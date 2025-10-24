handle_species_entities <- function(handler, source, config){
	
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
	if(is.null(WFS)) stop("SPECIES entities handler requires a WFS software to load SPECIES shapes")


	#query species
	species_url <- "https://www.fao.org/fishery/geoserver/factsheets/js/specieslist.xml"
	xml <- xmlParse(content(GET(species_url), "text"), encoding = "UTF-8")
	species_list <- getNodeSet(xml, "//item")
	
	#compile entities
	entities = lapply(species_list, function(species){
		
		a3c <- tolower(xmlGetAttr(species, "a3c"))
		species_id <- sprintf("fao-species-map-%s",a3c)
		#fid 
		fid <- xmlGetAttr(species, "FigisID")
		#style
		styles <- c("852D36", "A60314", "FF031C", "FA485B", "F58E98", "CC8914", "FAA616", "FAC002", "F0E92E", "FAFA5F")
		style <- paste0("species_style_", sample(styles,1))
		
		#-----------------------------------------------------------------------------------------------------------------
		#compute extent
		species_sf <- WFS$getFeatures("fifao:SPECIES_DIST", cql_filter = URLencode(sprintf("ALPHACODE='%s'", toupper(a3c))))
		sf::st_crs(species_sf) = 4326
		
		species_bbox = fdi4R::optimize_bbox(species_sf)
		species_bbox_sf = fdi4R::bbox_to_sf(xmin = species_bbox[1], ymin = species_bbox[2], xmax = species_bbox[3], ymax = species_bbox[4])
		species_center <- sf::st_point_on_surface(species_bbox_sf)
		bbox_string <- paste0(c(species_bbox[1],species_bbox[3],species_bbox[2],species_bbox[4]),collapse=",")
		bbox_center <- paste(sf::st_coordinates(species_center), collapse=",")
		
		#links
		#---------------------------------------------------------------------------------------------
		#geonetwork links
		species_md_link_html <- sprintf("https://www.fao.org/fishery/geonetwork?uuid=%s",species_id)
		species_md_link_xml <- sprintf("https://www.fao.org/fishery/geonetwork/srv/eng/csw?service=CSW&request=GetRecordById&Version=2.0.2&elementSetName=full&outputSchema=https://www.isotc211.org/2005/gmd&id=%s", species_id)
		#NFI links
		species_sheet_link_html <- sprintf("https://%s/fishery/species/%s",domain, fid)
		a3c_for_viewer <- paste0(toupper(a3c),"-")
		if(xmlGetAttr(species, "mar")==1) a3c_for_viewer <- paste0(a3c_for_viewer, "m")
		if(xmlGetAttr(species, "inl")==1) a3c_for_viewer <- paste0(a3c_for_viewer, "i")
		species_viewer_link <- sprintf("https://%s/fishery/geoserver/factsheets/species.html?species=%s&extent=%s&center=%s&prj=4326", domain, a3c_for_viewer, bbox_string, bbox_center)
		
		#taxonomic properties
		#---------------------------------------------------------------------------------------------
		species_en <- xmlGetAttr(species, "en")
		species_lt <- xmlGetAttr(species, "lt")
		species_family = xmlGetAttr(species, "family")
		species_order = xmlGetAttr(species, "order")
		
		#geoflow entity
		species_entity <- geoflow_entity$new()
		species_entity$addDate("publication", Sys.Date())
		species_entity$addDate("revision", Sys.Date())
		species_entity$setIdentifier("id", species_id)
		#title
		species_title <- sprintf("FAO aquatic species distribution map of %s", species_lt)
		if(!is.null(species_en)) if(species_en != "") species_title <- paste0(species_title," (", species_en,")")
		Encoding(species_title) <- "UTF-8"
		species_entity$setTitle("title", species_title)
		#descriptions
		species_entity$setDescription("abstract", "The main sources of information for the species distribution are the habitat description and geographic range contained in the published FAO Catalogues of Species (more details at https://www.fao.org/fishery/fishfinder ). Terms used in the descriptive context of the FAO Catalogues were converted in standard depth, geographic and ecological regions and inserted into a Geographic Information System.")
		#other descriptions?
		species_entity$setDescription("purpose", "The distribution can help identifying areas of higher biodiversity or species associated with vulnerable marine habitats.")
		species_entity$setDescription("info", "As fisheries are fundamentally spatially distributed, responsible management requires a solid understanding of the underlying spatial dimension of fishery resources. Geographical Information Systems (GIS) provide the technology for mapping the distribution of aquatic resources and to identify the spatial relationship with, their environment, the fishery management units, the production systems, etc. which can support decision-making. FAO Fisheries and Aquaculture Department plays a unique role in providing such information which helps mapping global trends. All information organized and disseminated by the FAO Fisheries and Aquaculture Department includes a geographic dimension as an essential element for monitoring fisheries at global level and for supporting decision-making for FAO Members concerned with global fisheries governance.")
		
		#creators
		fao <- geoflow_contact$new(); fao$setId("fao"); fao$setRole("owner"); species_entity$addContact(fao);
		AG <- geoflow_contact$new(); AG$setId("aureliano.fentile@fao.org"); AG$setRole("pointOfContact"); species_entity$addContact(AG);
		EB <- geoflow_contact$new(); EB$setId("emmanuel.blondel@fao.org"); EB$setRole("pointOfContact"); species_entity$addContact(EB);
		
		#creating subjects
		#topic
		topic_subj <- geoflow_subject$new(); topic_subj$setKey("topic");
		topic_subj$addKeyword("biota")
		if(xmlGetAttr(species, "mar")==1) topic_subj$addKeyword("oceans")
		if(xmlGetAttr(species, "inl")==1) topic_subj$addKeyword("inlandWaters")
		species_entity$addSubject(topic_subj);
		#General
		gen_subj <- geoflow_subject$new(); gen_subj$setKey("theme"); gen_subj$setName("General");
		gen_subj$addKeyword("FAO", "https://www.fao.org")
		gen_subj$addKeyword("fishery");
		gen_subj$addKeyword("fisheries");
		gen_subj$addKeyword("aquatic species distribution");
		gen_subj$addKeyword("biodiversity")
		species_entity$addSubject(gen_subj);
		#FAO
		fao_subj <- geoflow_subject$new(); fao_subj$setKey("theme"); fao_subj$setName("FAO"); 
		fao_subj$addKeyword(species_id, species_md_link_html)
		species_entity$addSubject(fao_subj)
		#ASFIS
		asfis_subj <- geoflow_subject$new(); asfis_subj$setKey("theme"); asfis_subj$setName("ASFIS");
		asfis_subj$setDate("publication", as.Date("2025-06-20"))
		asfis_subj$addKeyword(toupper(a3c))
		if(!is.null(species_en)) if(species_en != "") asfis_subj$addKeyword(species_en)
		asfis_subj$addKeyword(species_lt)
		asfis_subj$addKeyword(species_family)
		asfis_subj$addKeyword(species_order)
		species_entity$addSubject(asfis_subj)
		#WoRMS
		mapping_asfis_worms = readr::read_csv("https://raw.githubusercontent.com/fdiwg/fdi-mappings/refs/heads/main/global-to-global/cl_mapping_species_asfis_worms.csv")
		map_result = mapping_asfis_worms[mapping_asfis_worms$src_code == toupper(a3c),]
		if(nrow(map_result)>0){
			worms_subj <- geoflow_subject$new(); worms_subj$setKey("theme"); worms_subj$setName("World Register of Marine Species (WoRMS)")
			worms_subj$setDate("revision", Sys.Date())
			aphiaID = map_result[1,]$trg_code
			worms_record = worrms::wm_record(aphiaID)
			attr(aphiaID, "uri") = worms_record$url
			worms_subj$addKeyword(aphiaID)
			worms_subj$addKeyword(worms_record$lsid)
			worms_subj$addKeyword(worms_record$scientificname)
			if(!is.na(worms_record$valid_AphiaID)) worms_subj$addKeyword(worms_record$valid_AphiaID)
			if(!is.na(worms_record$valid_name)) worms_subj$addKeyword(worms_record$valid_name)
			worms_subj$addKeyword(worms_record$authority)
			if(!is.na(worms_record$isMarine)) if(worms_record$isMarine == 1) worms_subj$addKeyword("marine")
			if(!is.na(worms_record$isBrackish)) if(worms_record$isBrackish == 1) worms_subj$addKeyword("brackish")
			if(!is.na(worms_record$isFreshwater)) if(worms_record$isFreshwater == 1) worms_subj$addKeyword("freshwater")
			if(!is.na(worms_record$isTerrestrial)) if(worms_record$isTerrestrial == 1) worms_subj$addKeyword("terrestrial")
			species_entity$addSubject(worms_subj)
		}
		
		#INSPIRE
		inspire_subj <- geoflow_subject$new(); inspire_subj$setKey("theme"); inspire_subj$setName("GEMET - INSPIRE themes, version 1.0");
		inspire_subj$addKeyword("Species distribution", "https://www.eionet.europa.eu/gemet/en/inspire-theme/sd")
		inspire_subj$setDate("publication", as.Date("2008-06-01"))
		species_entity$addSubject(inspire_subj)
		
		#extents
		wkt <- sf::st_as_text(species_bbox_sf$geom)
		species_entity$setSpatialExtent(wkt, crs = 4326)
		#srid
		species_entity$setSrid(4326)
		
		#relations
		#relations
		layerName <- paste0("SPECIES_DIST_",toupper(a3c))
		thumbnail <- geoflow_relation$new()
		thumbnail$setKey("thumbnail")
		thumbnail$setDescription("Map Overview")
		thumbnail$setLink(paste0("https://",domain,"/fishery/geoserver/wms?service=WMS&version=1.1.0&request=GetMap&layers=fifao:UN_CONTINENT2,species:",layerName,"&bbox=",paste(species_bbox, collapse=","),"&width=600&height=300&srs=EPSG:4326&format=image%2Fpng"))
		species_entity$addRelation(thumbnail)
		
		SPECIES_NFI <- geoflow_relation$new()
		SPECIES_NFI$setKey("http")
		SPECIES_NFI$setDescription("Compilation of Aquatic Species Distribution Maps of Interest to Fisheries")
		SPECIES_NFI$setLink("https://www.fao.org/fishery/en/collection/fish_dist_map")
		species_entity$addRelation(SPECIES_NFI)
		
		SPECIES_FS <- geoflow_relation$new()
		SPECIES_FS$setKey("http")
		SPECIES_FS$setDescription("Factsheet - Summary description")
		SPECIES_FS$setLink(species_sheet_link_html)
		species_entity$addRelation(SPECIES_FS)
		
		SPECIES_VIEWER <- geoflow_relation$new()
		SPECIES_VIEWER$setKey("http")
		SPECIES_VIEWER$setDescription("Aquatic Species Distribution Maps (GIS Viewer)")
		SPECIES_VIEWER$setLink(species_viewer_link)
		species_entity$addRelation(SPECIES_VIEWER)
		
		#Rights
		useLim1 <- geoflow_right$new()
		useLim1$setKey("use"); useLim1$setValues("The terms and conditions are available at https://www.fao.org/contact-us/terms/en")
		species_entity$addRight(useLim1);
		
		useLim2 <- geoflow_right$new()
		species_citation <- sprintf("Usage subject to mandatory citation: © FAO, %s. Aquatic Species Distribution Maps. %s. In: FAO Fisheries and Aquaculture Division [online]. Accessed on <DD Month YYYY>. %s License: CC-BY 4.0",
						format(Sys.Date(), "%Y"), species_title, species_md_link_html)
		useLim2$setKey("use"); useLim2$setValues(species_citation)
		species_entity$addRight(useLim2);
		
		useLim3 <- geoflow_right$new()
		useLim3$setKey("use"); useLim3$setValues(sprintf("Use of web map (WMS) and feature (WFS) protocol in web-applications subject to mention of provenance (Metadata link: %s) and FAO map attribution.", species_md_link_html))
		species_entity$addRight(useLim3)
		
		useLim4 <- geoflow_right$new()
		useLim4$setKey("use"); useLim4$setValues("This work is made available under the Creative Commons Attribution-4.0 International license (CC BY 4.0; https://creativecommons.org/licenses/by/4.0/legalcode.en). By using this database, you agree to be bound by the terms of this license and the FAO Statistical Database Terms of Use (https://www.fao.org/contact-us/terms/db-terms-of-use/en/)")
		species_entity$addRight(useLim4)
		
		useLim5 <- geoflow_right$new()
		useLim5$setKey("use"); useLim5$setValues("Disclaimer: The designations employed and the presentation of material in the map(s) are for illustration only and do not imply the expression of any opinion whatsoever on the part of FAO concerning the legal or constitutional status of any country, territory or sea area or concerning the delimitation of frontiers or boundaries.")
		species_entity$addRight(useLim5)
		useConst1 <- geoflow_right$new(); useConst1$setKey("useConstraint"); useConst1$setValues("license");
		species_entity$addRight(useConst1)
		
		#Data object
		data_obj <- geoflow_data$new()
		data_obj$setUpload(FALSE)
		data_obj$setUploadType('dbquery')
		data_obj$setLayername(paste0("SPECIES_DIST_",toupper(a3c)))
		
		sql <- sprintf("SELECT * FROM SPECIES_DIST WHERE ALPHACODE = '%s'", toupper(a3c))
		sqlfilename <- paste0("SPECIES_DIST.sql")
		fileConn<-file(sqlfilename)
		writeLines(sql, fileConn)
		close(fileConn)
		
		datasource <- "SPECIES_DIST.sql"
		attr(datasource, "uri") <- file.path(getwd(), sqlfilename)
		data_obj$setSource(datasource)
		data_obj$setSql(sql)
		data_obj$setGeometryType("MultiPolygon")
		data_obj$setGeometryField("geom")
		data_obj$addStyle(style)
		species_entity$setData(data_obj);
		
		return(species_entity)
	})
	return(entities)
}