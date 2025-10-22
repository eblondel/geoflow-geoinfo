handle_rfb_entities <- function(handler, source, config){
	
	domain = "www.fao.org"
	
	if(!require("XML")){
		stop("XML package is required")
	}
	if(!require("httr")){
		stop("httr package is required")
	}
	if(!require("sf")){
		stop("sf package is required")
	}
	if(!require("fdi4R")){
		remotes::install_github("fdiwg/fdi4R")
		require("fdi4R")
	}
	
	#software required for handling these entities
	WFS <- config$software$input$wfs
	if(is.null(WFS)) stop("RFB entities handler requires a WFS software to load RFB shapes")

	#query RFBs
	rfb_url <- "https://www.fao.org/figis/monikerj/figismapdata?format=xml&callback=FigisMap.loadStaticMapData"
	xml <- xmlParse(content(GET(rfb_url), "text"), encoding = "UTF-8")
	rfbs <- getNodeSet(xml, "//rfb/rfb")

	#compile entities
	entities = lapply(rfbs, function(rfb){
		
		rfb_name <- tolower(xmlGetAttr(rfb, "name"))
		print(rfb_name)
		print(which(sapply(rfbs, function(x){xmlGetAttr(x, "name")}) == toupper(rfb_name)))
		rfb_id <- sprintf("fao-rfb-map-%s",rfb_name)
		#fid 
		fid <- xmlGetAttr(rfb, "fid")
		#style
		style <- xmlGetAttr(rfb,"style")
		
		#-----------------------------------------------------------------------------------------------------------------
		#compute extent
		rfb_sf <- WFS$getFeatures("fifao:RFB_COMP_CLIP", cql_filter = sprintf("RFB='%s'", toupper(rfb_name)))
		sf::st_crs(rfb_sf) = 4326
		
		rfb_bbox = fdi4R::optimize_bbox(rfb_sf)
		rfb_bbox_sf = fdi4R::bbox_to_sf(xmin = rfb_bbox[1], ymin = rfb_bbox[2], xmax = rfb_bbox[3], ymax = rfb_bbox[4])
		rfb_center <- sf::st_point_on_surface(rfb_bbox_sf)
		bbox_string <- paste0(c(rfb_bbox$xmin,rfb_bbox$xmax,rfb_bbox$ymin,rfb_bbox$ymax),collapse=",")
		bbox_center <- paste(sf::st_coordinates(rfb_center), collapse=",")
		
		#links
		#---------------------------------------------------------------------------------------------
		#geonetwork links
		rfb_md_link_html <- sprintf("https://www.fao.org/fishery/geonetwork?uuid=%s",rfb_id)
		rfb_md_link_xml <- sprintf("https://www.fao.org/fishery/geonetwork/srv/eng/csw?service=CSW&request=GetRecordById&Version=2.0.2&elementSetName=full&outputSchema=https://www.isotc211.org/2005/gmd&id=%s", rfb_id)
		#NFI links
		rfb_sheet_link_html <- sprintf("https://%s/fishery/organization/%s",domain, rfb_name)
		rfb_viewer_link <- sprintf("https://%s/fishery/geoserver/factsheets/rfbs.html?rfb=%s&extent=%s&center=%s&prj=4326", domain, toupper(rfb_name), bbox_string, bbox_center)
		
		
		rfb_entity <- geoflow_entity$new()
		rfb_entity$addDate("publication", Sys.Date())
		rfb_entity$addDate("revision", Sys.Date())
		rfb_entity$setIdentifier("id", rfb_id)
		#title
		rfb_title <- xmlGetAttr(xmlChildren(rfb)$descriptor, "title")
		Encoding(rfb_title) <- "UTF-8"
		rfb_title <- paste("Geographic Area of Competence of", rfb_title)
		rfb_entity$setTitle("title", rfb_title)
		#descriptions
		rfb_abstract <- rfb_title #TODO improve if we can fetch actual area description
		rfb_entity$setDescription("abstract", rfb_abstract)
		#other descriptions?
		rfb_entity$setDescription("purpose", "The main objective of the Regional Fishery Bodies is to ensure the sustainable exploitation of marine and freshwater resources by the establishment of a system of international regulation and by the development of marketing activities in conformity with the objectives of its members.")
		rfb_entity$setDescription("info", "Regional Fishery Bodies (RFBs) are a mechanism through which States or organizations that are parties to an international fishery agreement or (\"agreement\" is fundamental, and different from arrangement) arrangement work together towards the conservation, management and/or development of fisheries. Some RFBs, especially those with an ecosystem mandate, work with seabirds, etc that are connected with fisheries but are not fish stocks per se.The mandates of RFBs vary. Some RFBs have an advisory mandate, and provide advice, decisions or coordinating mechanisms that are not binding on their members. Some RFBs have a management mandate, these are called Regional Fisheries Management Organizations (RFMOs). They adopt fisheries conservation and management measures that are binding on their members.The functions of RFBs also vary. They can include the collection, analysis and dissemination of information and data, coordinating fisheries management through joint schemes and mechanisms, serving as a technical and policy forum, and taking decisions relating to the conservation, management, development and responsible use of the resources.The difference between a \"regional fishery body\" and a \"regional fishery arrangement\" is that the former has established a Secretariat that operates under a governing body of member States and the latter does not have.")
		
		#creators
		fao <- geoflow_contact$new(); fao$setId("fao"); fao$setRole("owner"); rfb_entity$addContact(fao);
		AG <- geoflow_contact$new(); AG$setId("aureliano.fentile@fao.org"); AG$setRole("pointOfContact"); rfb_entity$addContact(AG);
		EB <- geoflow_contact$new(); EB$setId("emmanuel.blondel@fao.org"); EB$setRole("pointOfContact"); rfb_entity$addContact(EB);
		
		#creating subjects
		#topics
		topic_subj <- geoflow_subject$new(); topic_subj$setKey("topic");
		topic_subj$addKeyword("boundaries")
		rfb_entity$addSubject(topic_subj)
		#General
		gen_subj <- geoflow_subject$new(); gen_subj$setKey("theme"); gen_subj$setName("General");
		gen_subj$addKeyword("FAO", "https://www.fao.org")
		gen_subj$addKeyword("Fisheries & Aquaculture Division", "https://www.fao.org/fishery")
		gen_subj$addKeyword("fishery");
		gen_subj$addKeyword("fisheries");
		gen_subj$addKeyword("ocean management");
		rfb_entity$addSubject(gen_subj);
		#FAO
		fao_subj <- geoflow_subject$new(); fao_subj$setKey("theme"); fao_subj$setName("FAO"); 
		fao_subj$addKeyword(rfb_id, rfb_md_link_html)
		rfb_entity$addSubject(fao_subj)
		#INSPIRE
		inspire_subj <- geoflow_subject$new(); inspire_subj$setKey("theme"); inspire_subj$setName("GEMET - INSPIRE themes, version 1.0");
		inspire_subj$addKeyword("Area management/restriction/regulation zones and reporting units", "https://www.eionet.europa.eu/gemet/en/inspire-theme/am")
		inspire_subj$setDate("publication", as.Date("2008-06-01"))
		rfb_entity$addSubject(inspire_subj)
		
		#extents
		wkt <- sf::st_as_text(rfb_bbox_sf$geom)
		rfb_entity$setSpatialExtent(wkt, crs = 4326)
		#srid
		rfb_entity$setSrid(4326)
		
		#relations
		layerName <- paste0("RFB_",toupper(rfb_name))
		thumbnail <- geoflow_relation$new()
		thumbnail$setKey("thumbnail")
		thumbnail$setDescription("Map Overview")
		thumbnail$setLink(paste0("https://",domain,"/fishery/geoserver/wms?service=WMS&version=1.1.0&request=GetMap&layers=fifao:UN_CONTINENT2,rfb:",layerName,"&bbox=",paste(rfb_bbox, collapse=","),"&width=600&height=300&srs=EPSG:4326&format=image%2Fpng"))
		rfb_entity$addRelation(thumbnail)
		
		RFB_FIGIS <- geoflow_relation$new()
		RFB_FIGIS$setKey("http")
		RFB_FIGIS$setDescription("FAO Regional Fishery Bodies")
		RFB_FIGIS$setLink(sprintf("https://%s/fishery/collection/organization",domain))
		rfb_entity$addRelation(RFB_FIGIS)
		
		RFB_FS <- geoflow_relation$new()
		RFB_FS$setKey("http")
		RFB_FS$setDescription("Factsheet - Summary description")
		RFB_FS$setLink(rfb_sheet_link_html)
		rfb_entity$addRelation(RFB_FS)
		
		RFB_VIEWER <- geoflow_relation$new()
		RFB_VIEWER$setKey("http")
		RFB_VIEWER$setDescription("FAO Regional Fishery Bodies (GIS Viewer)")
		RFB_VIEWER$setLink(rfb_viewer_link)
		rfb_entity$addRelation(RFB_VIEWER)
		
		#Rights
		useLim1 <- geoflow_right$new()
		useLim1$setKey("use"); useLim1$setValues("The terms and conditions are available at https://www.fao.org/contact-us/terms/en")
		rfb_entity$addRight(useLim1);
		useLim2 <- geoflow_right$new()
		
			
		lic = geoflow_right$new()
		lic$setKey("license");lic$setValues("cc-by-4.0")
		
		rfb_citation <- sprintf("Usage subject to mandatory citation: © FAO, %s. FAO Regional Fishery Bodies. %s. In: FAO Fisheries and Aquaculture Division [online]. Accessed on <DD Month YYYY>. %s License: CC-BY 4.0",
						format(Sys.Date(), "%Y"), rfb_title, rfb_md_link_html)
		useLim2$setKey("use"); useLim2$setValues(rfb_citation)
		rfb_entity$addRight(useLim2);
		useLim3 <- geoflow_right$new()
		useLim3$setKey("use"); useLim3$setValues("This work is made available under the Creative Commons Attribution-4.0 International license (CC BY 4.0; https://creativecommons.org/licenses/by/4.0/legalcode.en). By using this database, you agree to be bound by the terms of this license and the FAO Statistical Database Terms of Use (https://www.fao.org/contact-us/terms/db-terms-of-use/en/)")
		rfb_entity$addRight(useLim3)
		useLim4 <- geoflow_right$new()
		useLim4$setKey("use"); useLim4$setValues("Disclaimer: The designations employed and the presentation of material in the map(s) are for illustration only and do not imply the expression of any opinion whatsoever on the part of FAO concerning the legal or constitutional status of any country, territory or sea area or concerning the delimitation of frontiers or boundaries.")
		rfb_entity$addRight(useLim4)
		useConst1 <- geoflow_right$new(); useConst1$setKey("useConstraint"); useConst1$setValues("license");
		rfb_entity$addRight(useConst1)
		
		#Data object
		data_obj <- geoflow_data$new()
		datasource <- "RFB_COMP_CLIP.zip"
		attr(datasource, "uri") <- "data/rfb/RFB_COMP_CLIP.zip"
		data_obj$setSource(datasource)
		data_obj$setSourceType("shp")
		data_obj$setUpload(FALSE)
		data_obj$setUploadType('shp')
		data_obj$setUploadSource('RFB_COMP_CLIP')
		data_obj$setLayername(paste0("RFB_",toupper(rfb_name)))
		data_obj$setCqlFilter(sprintf("RFB = '%s'",toupper(rfb_name)))
		data_obj$addStyle(style)
		rfb_entity$setData(data_obj);
		return(rfb_entity)
	})
	return(entities)
}