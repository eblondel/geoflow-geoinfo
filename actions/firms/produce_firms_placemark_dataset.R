produce_firms_placemark_dataset <- function(action, entity, config){
	opts <- action$options

	#packages
	#-------------------------------------------------------------------------------------------------------
	require(ows4R)
	require(sf)
	require(lwgeom)
	require(RFirmsGeo)
	require(gt)
	
	#host (needed to init OGC WFS client, pointing to test-figisapps or prod -www.fao.org)
	IS_REVIEW = FALSE
	HOST = paste0(unlist(strsplit(attr(entity$data$source[[1]], "uri"), ".org/"))[1], ".org")
	if(HOST == "https://fisheries.review.fao.org") IS_REVIEW = TRUE
	
	#WFS
	WFS = ows4R::WFSClient$new(
		url = file.path(HOST, "fishery", "geoserver/wfs"),
		serviceVersion = "1.0.0",
		logger = "INFO"
	)
	
	#domain (fishery|resource)
	firmsDomain = switch(entity$data$source[[1]],
		"firmsfisheries.json" = "fishery",
		"firmsmarineresources.json" = "resource"
	)
	firmsDomainName <- switch(firmsDomain,"fishery" = "Fisheries","resource" = "Marine Resources")
	
	#read inventories
	doc = jsonlite::read_json(file.path("data", entity$data$source[[1]]))
	refs = sapply(doc, function(x){x$document$inventoryId})
	
	#produce dataset (all geo-references are taken from production Geoserver)
	result = RFirmsGeo::buildSpatialDataset(host = "https://www.fao.org", domain = firmsDomain, doc = doc)
	refs_not_in_result = refs[!refs %in% result$FIGIS_ID]
	
	#shapefile filename
	filename = unlist(strsplit(entity$data$uploadSource[[1]], ".zip"))[1]
	
	#prepare statistics / delta with previous version
	#--------------------------------------------------------------------------------------------------
	#Check previous version
	ft_old <- WFS$capabilities$findFeatureTypeByName(filename, F)
	result_old <- ft_old$getFeatures()
	
	#result_in_old
	refId = "FIGIS_ID"
	if("OLD_ID" %in% names(result)) refId = "OLD_ID"
	result_in_old <- result[result[[refId]] %in% result_old$FIGIS_ID,]
	sf::st_crs(result_in_old) <- sf::st_crs(result_old)
	
	#reports
	dir.create("reports")
	#new records
	new_records <- as.data.frame(result[!result[[refId]] %in% result_old$FIGIS_ID,])
	new_records_table <- NULL
	new_records_filename <- NULL
	if(nrow(new_records)>0){
		new_records$x <- NULL
		new_records$y <- NULL
		new_records$FACTSHEET <- sprintf('%s/fishery/firms/%s/%s', HOST, firmsDomain, new_records$INV_OBS_ID)
		new_records$VIEWER <- sprintf('%s/fishery/geoserver/factsheets/firms.html?layer=%s&feat=%s', HOST, firmsDomain, new_records$FIGIS_ID)
		new_records_filename <- sprintf("FIRMS_MapViewer_%s_%s_new_records_%s.xlsx", if(IS_REVIEW) "REVIEW" else "PROD", firmsDomainName, Sys.Date())
		writexl::write_xlsx(new_records, file.path("reports", new_records_filename))
		new_records_rep <- new_records
		new_records_rep$FACTSHEET <- sprintf('<a href="%s">Link</a>', new_records_rep$FACTSHEET)
		new_records_rep$VIEWER <- sprintf('<a href="%s">Link</a>', new_records_rep$VIEWER)
		new_records_rep$the_geom = NULL
		new_records_table <- new_records_rep %>% gt::gt() %>% 
			gt::fmt_markdown(columns = vars(FACTSHEET)) %>%
			gt::fmt_markdown(columns = vars(VIEWER)) %>%
			gt::tab_options(
				summary_row.background.color = "#FFFEEE",
				row_group.background.color = "#E6EFFC",
				table.font.size = "small",
				heading.title.font.size = "small",
				heading.subtitle.font.size = "x-small",
				row_group.font.size = "small",
				column_labels.font.size = "small",
				data_row.padding = "5px"
			) %>% gt::as_raw_html()
	}

	#updates
	updated_records <- as.data.frame(result_in_old[!apply(sf::st_within(result_in_old, sf::st_buffer(result_old, dist = 0.1)), 1, any),])
	updated_records_table <- NULL
	updated_records_filename <- NULL
	if(nrow(updated_records)>0){
		updated_records$geometry <- NULL
		updated_records$FACTSHEET <- sprintf('%s/fishery/firms/%s/%s', HOST, firmsDomain, updated_records$INV_OBS_ID)
		updated_records$VIEWER <- sprintf('%s/fishery/geoserver/factsheets/firms.html?layer=%s&feat=%s', HOST, firmsDomain, updated_records$FIGIS_ID)
		updated_records_filename <- sprintf("FIRMS_MapViewer_%s_%s_updated_records_%s.xlsx", if(IS_REVIEW) "REVIEW" else "PROD", firmsDomainName, Sys.Date())
		writexl::write_xlsx(updated_records, file.path("reports", updated_records_filename))
		updated_records_rep <- updated_records
		updated_records_rep$FACTSHEET <- sprintf('<a href="%s">Link</a>', updated_records_rep$FACTSHEET)
		updated_records_rep$VIEWER <- sprintf('<a href="%s">Link</a>', updated_records_rep$VIEWER)
		updated_records_rep$the_geom = NULL
		updated_records_table <- updated_records_rep %>% gt::gt() %>% 
			gt::fmt_markdown(columns = vars(FACTSHEET)) %>%
			gt::fmt_markdown(columns = vars(VIEWER)) %>%
			gt::tab_options(
				summary_row.background.color = "#FFFEEE",
				row_group.background.color = "#E6EFFC",
				table.font.size = "small",
				heading.title.font.size = "small",
				heading.subtitle.font.size = "x-small",
				row_group.font.size = "small",
				column_labels.font.size = "small",
				data_row.padding = "5px"
			) %>% gt::as_raw_html()	 
	}
	
	#deleted/unpublished records
	deleted_records <- as.data.frame(result_old[!result_old$FIGIS_ID %in% result[[refId]],])
	deleted_records$OLD_ID = deleted_records$FIGIS_ID
	deleted_records_table <- NULL
	deleted_records_filename <- NULL
	if(nrow(deleted_records)>0){
		deleted_records$gml_id <- NULL
		deleted_records$geometry <- NULL
		deleted_records$the_geom <- NULL
		deleted_records$FACTSHEET <- sprintf('%s/firms/%s/%s/%s', if(IS_REVIEW) {"https://figisapps.fao.org"}else{"https://firms.fao.org"}, firmsDomain, deleted_records$OLD_ID, deleted_records$LANG) #TODO refactor once migration finished
		deleted_records$VIEWER <- "-"
		deleted_records_filename <- sprintf("FIRMS_MapViewer_%s_%s_deleted_records_%s.xlsx", if(IS_REVIEW) "REVIEW" else "PROD", firmsDomainName, Sys.Date())
		writexl::write_xlsx(deleted_records, file.path("reports", deleted_records_filename))
		deleted_records_rep <- deleted_records
		deleted_records_rep$FACTSHEET <- sprintf('<a href="%s">Link</a>', deleted_records_rep$FACTSHEET)
		deleted_records_rep$VIEWER <- sprintf('<a href="%s">Link</a>', deleted_records_rep$VIEWER)
		deleted_records_table <- deleted_records_rep %>% gt::gt() %>% 
			gt::fmt_markdown(columns = vars(FACTSHEET)) %>%
			gt::fmt_markdown(columns = vars(VIEWER)) %>%
			gt::tab_options(
				summary_row.background.color = "#FFFEEE",
				row_group.background.color = "#E6EFFC",
				table.font.size = "small",
				heading.title.font.size = "small",
				heading.subtitle.font.size = "x-small",
				row_group.font.size = "small",
				column_labels.font.size = "small",
				data_row.padding = "5px"
			) %>% gt::as_raw_html()	 
	}
	
	#result by type
	result_by_type <- aggregate(result$FIGIS_ID, by = list(category = result$CATEGORY), length); colnames(result_by_type)[2] <- "count"
	result_old_by_type <- aggregate(result_old$FIGIS_ID, by = list(category = result_old$CATEGORY), length); colnames(result_old_by_type)[2] <- "count"
	result_by_type <- merge(result_by_type, result_old_by_type, by = "category", all.x = TRUE, all.y = TRUE)
	if(nrow(new_records)>0){
		added <- aggregate(new_records$FIGIS_ID, by = list(category = new_records$CATEGORY), length); colnames(added)[2] <- "count"
		result_by_type <- merge(result_by_type, added, by = "category", all.x = TRUE, all.y = TRUE)
	}else{
		result_by_type$added <- 0L
	}
	if(nrow(deleted_records)>0){
		deleted <- aggregate(deleted_records$FIGIS_ID, by = list(category = deleted_records$CATEGORY), length); colnames(deleted)[2] <- "count"
		result_by_type <- merge(result_by_type, deleted, by = "category", all.x = TRUE, all.y = TRUE)
	}else{
		result_by_type$deleted <- 0L
	}
	colnames(result_by_type) <- c("category", "after", "before", "added", "deleted")
	result_by_type$added[is.na(result_by_type$added)] <- 0L
	result_by_type$deleted[is.na(result_by_type$deleted)] <- 0L
	result_by_type$diff <- sapply(1:nrow(result_by_type), function(i){ 
		rec <- result_by_type[i,]
		outrec <- ""
		if(rec$added != 0) {
			outrec <- paste0("(+",rec$added,")")
		}
		if(rec$deleted != 0) {
			outrec <- paste0(outrec, "(-", rec$deleted,")")
		}
		return(outrec)
	})
	result_by_type$added <- NULL
	result_by_type$deleted <- NULL
	colnames(result_by_type) <- c("Type", "Number of records", "Previously", "Changes")
	result_by_type_table <- result_by_type %>%
		gt::gt() %>%
		  gt::cols_label(Changes = "") %>%
		  gt::as_raw_html()
		

	#result by agency
	result_by_agency <- aggregate(result$FIGIS_ID, by = list(agency = result$AGENCY), length); colnames(result_by_agency)[2] <- "count"
	result_old_by_agency <- aggregate(result_old$FIGIS_ID, by = list(agency = gsub(" ", "",result_old$AGENCY)), length); colnames(result_old_by_agency)[2] <- "count"
	result_by_agency <- merge(result_by_agency, result_old_by_agency, by = "agency", all.x = TRUE, all.y = TRUE)
	if(nrow(new_records)>0){
		added <- aggregate(new_records$FIGIS_ID, by = list(agency = new_records$AGENCY), length); colnames(added)[2] <- "count"
		result_by_agency <- merge(result_by_agency, added, by = "agency", all.x = TRUE, all.y = TRUE)
	}else{
		result_by_agency$added <- 0L
	}
	if(nrow(deleted_records)>0){
		deleted <- aggregate(deleted_records$FIGIS_ID, by = list(agency = deleted_records$AGENCY), length); colnames(deleted)[2] <- "count"
		result_by_agency <- merge(result_by_agency, deleted, by = "agency", all.x = TRUE, all.y = TRUE)
	}else{
		result_by_agency$deleted <- 0L
	}
	colnames(result_by_agency) <- c("agency", "after", "before", "added", "deleted")
	result_by_agency$added[is.na(result_by_agency$added)] <- 0L
	result_by_agency$deleted[is.na(result_by_agency$deleted)] <- 0L
	result_by_agency$diff <- sapply(1:nrow(result_by_agency), function(i){ 
		rec <- result_by_agency[i,]
		outrec <- ""
		if(rec$added != 0) {
			outrec <- paste0("(+",rec$added,")")
		}
		if(rec$deleted != 0) {
			outrec <- paste0(outrec, "(-", rec$deleted,")")
		}
		return(outrec)
	})
	result_by_agency$added <- NULL
	result_by_agency$deleted <- NULL
	if(any(is.na(result_by_agency$after))) result_by_agency[is.na(result_by_agency$after),]$after = "–"
	if(any(is.na(result_by_agency$before))) result_by_agency[is.na(result_by_agency$before),]$before = "–"
	colnames(result_by_agency) <- c("Agency", "Number of records", "Previously", "Changes")
	result_by_agency_table <- result_by_agency %>%
		gt::gt() %>%
		  gt::cols_label(Changes = "") %>%
		  gt::as_raw_html()

	#resources
	entity$addResource("is_review", IS_REVIEW)
	entity$addResource("host", HOST)
	entity$addResource("firmsDomain", firmsDomain)
	entity$addResource("firmsDomainName", firmsDomainName)
	entity$addResource("result", result)
	entity$addResource("result_old", result_old)
	entity$addResource("result_by_type_table", result_by_type_table)
	entity$addResource("result_by_agency_table", result_by_agency_table)
	entity$addResource("new_records", new_records)
	entity$addResource("new_records_filename", new_records_filename)
	entity$addResource("new_records_table", new_records_table)
	entity$addResource("deleted_records", deleted_records)
	entity$addResource("deleted_records_filename", deleted_records_filename)
	entity$addResource("deleted_records_table", deleted_records_table)
	entity$addResource("updated_records", updated_records)
	entity$addResource("updated_records_filename", updated_records_filename)
	entity$addResource("updated_records_table", updated_records_table)

	#EXPORT
	#--------------------------------------------------------------------------------------------------
	sf::st_crs(result) <- 4326
	sf::st_write(result, file.path("data", paste0(filename,".shp")))
	setwd("data")
	zip::zipr(entity$data$uploadSource[[1]], files = list.files(pattern = filename))
	setwd("..")
	entity$data$features = result

	#UPLOAD TO GEOSERVER (if REVIEW)
	#--------------------------------------------------------------------------------------------------
	GS = config$software$output$geoserver
	if(IS_REVIEW){
	  GS$uploadShapefile(
	    ws = "firms", ds = "firms_shapefiles", 
	    endpoint = "file", configure = "none", update = "overwrite",
	    filename = file.path("data", entity$data$uploadSource[[1]])
	  )
	}
	
}