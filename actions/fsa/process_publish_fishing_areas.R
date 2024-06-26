#
# Scripts to publish the fishing areas GIS data & metadata to GeoServer/Geonetwork
#
# @author eblondel
# @date 2018/09/25
#
config$logger.info("============================================================================================")
config$logger.info("PUBLISH main FAO areas datasets...")
config$logger.info("============================================================================================")


#packages
#--------------------------------------------------------------------------------------------

#options
#--------------------------------------------------------------------------------------------
Sys.setlocale("LC_ALL", "en_US.UTF-8")
options(encoding="UTF-8")
options(stringsAsFactors = FALSE)

#functions
#--------------------------------------------------------------------------------------------


#business logic
#----------------------------------------------------------------------------------


GS <- config$software$output$geoserver
GSCONFIG <- config$software$output$geoserver_config
gs_ws_name <- GSCONFIG$properties$workspace
gs_ds_name <- GSCONFIG$properties$datastore

#publish raw layers (all)
#------------------------
path <- paste(getwd(), "data",sep="/")
shapefiles_to_upload <- list.files(path, pattern = ".zip", full.names = TRUE)
for(shapefile in shapefiles_to_upload){
	
	#geoserver featuretype
	shp_splits <- unlist(strsplit(shapefile,"/"))
	gs_ft_name <- unlist(strsplit(shp_splits[length(shp_splits)],".zip"))[1]
	featureType <- GSFeatureType$new()
	gs_ft_newname <- gs_ft_name
	if(gs_ft_name == "FAO_AREAS") gs_ft_newname <- "FAO_AREAS_NOCOASTLINE"
	if(gs_ft_name == "FAO_AREAS_ERASE") gs_ft_newname <- "FAO_AREAS"
	featureType$setName(gs_ft_newname)
	featureType$setNativeName(gs_ft_name)
	featureType$setAbstract(gs_ft_newname)
	featureType$setTitle(gs_ft_newname)
	featureType$setSrs("EPSG:4326")
	featureType$setNativeCRS("EPSG:4326")
	featureType$setEnabled(TRUE)
	featureType$setProjectionPolicy("REPROJECT_TO_DECLARED")
	featureType$setLatLonBoundingBox(-180,-90,180,90, crs = "EPSG:4326")
	featureType$setNativeBoundingBox(-180,-90,180,90, crs ="EPSG:4326")
	
	#action on GS featuretype
	ft <- GS$getFeatureType(gs_ws_name, gs_ds_name, gs_ft_newname)
	if(!is(ft, "GSFeatureType")){
		config$logger.info(sprintf("Creating feature type '%s'", gs_ft_newname))
		ft_created <- GS$createFeatureType(gs_ws_name, gs_ds_name, featureType)
	}else{
		config$logger.info(sprintf("Updating feature type '%s'", gs_ft_newname))
		ft_updated <- GS$updateFeatureType(gs_ws_name, gs_ds_name, featureType)
	}
	
	#geoserver layer
	layer <- GSLayer$new()
	layer$setName(gs_ft_newname)
	layer$setDefaultStyle("all_fao_areas_style2")
	
	#action on GS layer
	lyr <- GS$getLayer(gs_ft_newname)
	if(!is(lyr, "GSLayer")){
		config$logger.info(sprintf("Creating layer '%s'", gs_ft_newname))
		lyr_created <- GS$createLayer(layer)	
	}else{
		config$logger.info(sprintf("Updating layer '%s'", gs_ft_newname))
		lyr_updated <- GS$updateLayer(layer)
	}	
}

#publish main draft layer (whatever status)
#--------------------------------------
gs_ft_name_fifao <- "FAO_AREAS"
featureType_fifao <- GSFeatureType$new()
featureType_fifao$setName(gs_ft_name_fifao)
featureType_fifao$setNativeName("FAO_AREAS_ERASE")
featureType_fifao$setAbstract("A FAO AREAS layer, erased by UN CONTINENT and containing all FAO Areas (all levels) officially endorsed by CWP or in draft stage")
featureType_fifao$setTitle("FAO Areas and its breakdown")
featureType_fifao$setSrs("EPSG:4326")
featureType_fifao$setNativeCRS("EPSG:4326")
featureType_fifao$setEnabled(TRUE)
featureType_fifao$setProjectionPolicy("REPROJECT_TO_DECLARED")
featureType_fifao$setLatLonBoundingBox(-180,-90,180,90, crs = "EPSG:4326")
featureType_fifao$setNativeBoundingBox(-180,-90,180,90, crs ="EPSG:4326")

#action on GS featuretype
ft <- GS$getFeatureType(gs_ws_name, gs_ds_name, gs_ft_name_fifao)
if(!is(ft, "GSFeatureType")){
	config$logger.info(sprintf("Creating feature type '%s'", gs_ft_name_fifao))
	ft_created <- GS$createFeatureType(gs_ws_name, gs_ds_name, featureType_fifao)
}else{
	config$logger.info(sprintf("Updating feature type '%s'", gs_ft_name_fifao))
	ft_updated <- GS$updateFeatureType(gs_ws_name, gs_ds_name, featureType_fifao)
}
	
#geoserver layer
layer_fifao <- GSLayer$new()
layer_fifao$setName(gs_ft_name_fifao)
layer_fifao$setDefaultStyle("all_fao_areas_style2")
layer_fifao$addStyle("generic")

#action on GS layer
lyr_fifao <- GS$getLayer(gs_ft_name_fifao)
if(!is(lyr_fifao, "GSLayer")){
	config$logger.info(sprintf("Creating layer '%s'", gs_ft_name_fifao))
	lyr_created <- GS$createLayer(layer_fifao)	
}else{
	config$logger.info(sprintf("Updating layer '%s'", gs_ft_name_fifao))
	lyr_updated <- GS$updateLayer(layer_fifao)
}	


#publish main public layer (status = 1)
#--------------------------------------
gs_ft_name_cwp <- "FAO_AREAS_CWP"
featureType <- GSFeatureType$new()
featureType$setName(gs_ft_name_cwp)
featureType$setNativeName("FAO_AREAS_ERASE")
featureType$setAbstract("A FAO AREAS layer, erased by UN CONTINENT and containing all FAO Areas (all levels) officially endorsed by CWP")
featureType$setTitle("FAO Areas and its breakdown (endorsed by CWP)")
featureType$setSrs("EPSG:4326")
featureType$setNativeCRS("EPSG:4326")
featureType$setEnabled(TRUE)
featureType$setProjectionPolicy("REPROJECT_TO_DECLARED")
featureType$setLatLonBoundingBox(-180,-90,180,90, crs = "EPSG:4326")
featureType$setNativeBoundingBox(-180,-90,180,90, crs ="EPSG:4326")
featureType$setCqlFilter("F_STATUS = 1")

#action on GS featuretype
ft <- GS$getFeatureType(gs_ws_name, gs_ds_name, gs_ft_name_cwp)
if(!is(ft, "GSFeatureType")){
	config$logger.info(sprintf("Creating feature type '%s'", gs_ft_name_cwp))
	ft_created <- GS$createFeatureType(gs_ws_name, gs_ds_name, featureType)
}else{
	config$logger.info(sprintf("Updating feature type '%s'", gs_ft_name_cwp))
	ft_updated <- GS$updateFeatureType(gs_ws_name, gs_ds_name, featureType)
}
	
#geoserver layer
layer <- GSLayer$new()
layer$setName(gs_ft_name_cwp)
layer$setDefaultStyle("all_fao_areas_style2")
layer$addStyle("generic")

#action on GS layer
lyr <- GS$getLayer(gs_ft_name_cwp)
if(!is(lyr, "GSLayer")){
	config$logger.info(sprintf("Creating layer '%s'", gs_ft_name_cwp))
	lyr_created <- GS$createLayer(layer)	
}else{
	config$logger.info(sprintf("Updating layer '%s'", gs_ft_name_cwp))
	lyr_updated <- GS$updateLayer(layer)
}	

#publish main public layer (status = 1) NO COASTLINE
#--------------------------------------
gs_ft_name_cwp <- "FAO_AREAS_CWP_NOCOASTLINE"
featureType <- GSFeatureType$new()
featureType$setName(gs_ft_name_cwp)
featureType$setNativeName("FAO_AREAS")
featureType$setAbstract("A FAO AREAS layer, not erased by UN_CONTINENT and containing all FAO Areas (all levels) officially endorsed by CWP")
featureType$setTitle("FAO Areas and its breakdown (endorsed by CWP) - no coastline")
featureType$setSrs("EPSG:4326")
featureType$setNativeCRS("EPSG:4326")
featureType$setEnabled(TRUE)
featureType$setProjectionPolicy("REPROJECT_TO_DECLARED")
featureType$setLatLonBoundingBox(-180,-90,180,90, crs = "EPSG:4326")
featureType$setNativeBoundingBox(-180,-90,180,90, crs ="EPSG:4326")
featureType$setCqlFilter("F_STATUS = 1")

#action on GS featuretype
ft <- GS$getFeatureType(gs_ws_name, gs_ds_name, gs_ft_name_cwp)
if(!is(ft, "GSFeatureType")){
	config$logger.info(sprintf("Creating feature type '%s'", gs_ft_name_cwp))
	ft_created <- GS$createFeatureType(gs_ws_name, gs_ds_name, featureType)
}else{
	config$logger.info(sprintf("Updating feature type '%s'", gs_ft_name_cwp))
	ft_updated <- GS$updateFeatureType(gs_ws_name, gs_ds_name, featureType)
}
	
#geoserver layer
layer <- GSLayer$new()
layer$setName(gs_ft_name_cwp)
layer$setDefaultStyle("all_fao_areas_style2")
layer$addStyle("generic")

#action on GS layer
lyr <- GS$getLayer(gs_ft_name_cwp)
if(!is(lyr, "GSLayer")){
	config$logger.info(sprintf("Creating layer '%s'", gs_ft_name_cwp))
	lyr_created <- GS$createLayer(layer)	
}else{
	config$logger.info(sprintf("Updating layer '%s'", gs_ft_name_cwp))
	lyr_updated <- GS$updateLayer(layer)
}	

#publish layers for FIGIS, by each level of FAO areas
#----------------------------------------------------
figis_fsa_layers <- list(
	FAO_MAJOR = list(
		source = "FAO_AREAS_ERASE_SINGLEPART",
		title = "FAO Major areas",
		cql_filter = "F_LEVEL = 'MAJOR'",
		defaultStyle = "Main_FAO_style"
	),
	FAO_MAJOR_Labels = list(
		source = "FAO_AREAS",
		title = "FAO Major areas labels",
		cql_filter = "F_LEVEL = 'MAJOR'",
		defaultStyle = "Main_FAO_style_labels_viewer",
		styles = list("Main_FAO_style_labels_factsheet")
	),
	FAO_SUB_AREA = list(
		source = "FAO_AREAS_ERASE_SINGLEPART",
		title = "FAO Subareas",
		cql_filter = "F_LEVEL = 'SUBAREA'",
		defaultStyle = "all_fao_areas_style"
	),
	FAO_DIV = list(
		source = "FAO_AREAS_ERASE_SINGLEPART",
		title = "FAO Divisions",
		cql_filter = "F_LEVEL = 'DIVISION'",
		defaultStyle = "all_fao_areas_style"
	),
	FAO_SUB_DIV = list(
		source = "FAO_AREAS_ERASE_SINGLEPART",
		title = "FAO Sub-divisions",
		cql_filter = "F_LEVEL = 'SUBDIVISION'",
		defaultStyle = "all_fao_areas_style"
	),
	FAO_SUB_UNIT = list(
		source = "FAO_AREAS_ERASE_SINGLEPART",
		title = "FAO Sub-units",
		cql_filter = "F_LEVEL = 'SUBUNIT'",
		defaultStyle = "all_fao_areas_style"
	)
)

for(figis_fsa_layername in names(figis_fsa_layers)){
	ft_name <- figis_fsa_layername
	ft_name_props <- figis_fsa_layers[[ft_name]]
	featureType <- GSFeatureType$new()
	featureType$setName(ft_name)
	featureType$setNativeName(ft_name_props$source)
	featureType$setTitle(ft_name_props$title)
	featureType$setAbstract(ft_name_props$title)
	featureType$setSrs("EPSG:4326")
	featureType$setNativeCRS("EPSG:4326")
	featureType$setEnabled(TRUE)
	featureType$setProjectionPolicy("REPROJECT_TO_DECLARED")
	featureType$setLatLonBoundingBox(-180,-90,180,90, crs = "EPSG:4326")
	featureType$setNativeBoundingBox(-180,-90,180,90, crs ="EPSG:4326")
	featureType$setCqlFilter(ft_name_props$cql_filter)

	#action on GS featuretype
	ft <- GS$getFeatureType(gs_ws_name, gs_ds_name, ft_name)
	if(!is(ft, "GSFeatureType")){
		config$logger.info(sprintf("Creating feature type '%s'", ft_name))
		ft_created <- GS$createFeatureType(gs_ws_name, gs_ds_name, featureType)
	}else{
		config$logger.info(sprintf("Updating feature type '%s'", ft_name))
		ft_updated <- GS$updateFeatureType(gs_ws_name, gs_ds_name, featureType)
	}
		
	#geoserver layer
	layer <- GSLayer$new()
	layer$setName(ft_name)
	layer$setDefaultStyle(ft_name_props$defaultStyle)
	if(!is.null(ft_name_props$styles)) for(style in ft_name_props$styles) layer$addStyle(style)
	
	#action on GS layer
	lyr <- GS$getLayer(ft_name)
	if(!is(lyr, "GSLayer")){
		config$logger.info(sprintf("Creating layer '%s'", ft_name))
		lyr_created <- GS$createLayer(layer)	
	}else{
		config$logger.info(sprintf("Updating layer '%s'", ft_name))
		lyr_updated <- GS$updateLayer(layer)
	}	
}

#publish fsa workspace layers
#------------------------
#path <- paste(getwd(), "data",sep="/")
#shapefiles_to_upload <- list.files(path, pattern = ".zip", full.names = TRUE)
#for(shapefile in shapefiles_to_upload){
#	
#	#geoserver featuretype
#	shp_splits <- unlist(strsplit(shapefile,"/"))
#	gs_ft_name <- unlist(strsplit(shp_splits[length(shp_splits)],".zip"))[1]
#	if(gs_ft_name == "FAO_AREAS_SINGLEPART") next;
#	gs_ft_newname <- gs_ft_name
#	if(gs_ft_name == "FAO_AREAS") gs_ft_newname <- "FAO_AREAS_NOCOASTLINE"
#	if(gs_ft_name == "FAO_AREAS_ERASE") gs_ft_newname <- "FAO_AREAS"
#	
#	featureType <- GSFeatureType$new()
#	featureType$setName(gs_ft_newname)
#	featureType$setNativeName(gs_ft_name)
#	featureType$setAbstract(gs_ft_newname)
#	featureType$setTitle(gs_ft_newname)
#	featureType$setSrs("EPSG:4326")
#	featureType$setNativeCRS("EPSG:4326")
#	featureType$setEnabled(TRUE)
#	featureType$setProjectionPolicy("REPROJECT_TO_DECLARED")
#	featureType$setLatLonBoundingBox(-180,-90,180,90, crs = "EPSG:4326")
#	featureType$setNativeBoundingBox(-180,-90,180,90, crs ="EPSG:4326")
#	featureType$setCqlFilter("F_STATUS = 1")
#	
#	#action on GS featuretype
#	ft <- GS$getFeatureType(gs_ws_name,gs_ds_name, gs_ft_newname)
#	if(!is(ft, "GSFeatureType")){
#		config$logger.info(sprintf("Creating feature type '%s'", gs_ft_newname))
#		ft_created <- GS$createFeatureType(gs_ws_name,gs_ds_name, featureType)
#	}else{
#		config$logger.info(sprintf("Updating feature type '%s'", gs_ft_newname))
#		ft_updated <- GS$updateFeatureType(gs_ws_name,gs_ds_name, featureType)
#	}
#	
#	#geoserver layer
#	layer <- GSLayer$new()
#	layer$setName(gs_ft_newname)
#	layer$setDefaultStyle("all_fao_areas_style2")
#	
#	#action on GS layer
#	lyr <- GS$getLayer(gs_ft_newname)
#	if(!is(lyr, "GSLayer")){
#		config$logger.info(sprintf("Creating layer '%s'", gs_ft_newname))
#		lyr_created <- GS$createLayer(layer)	
#	}else{
#		config$logger.info(sprintf("Updating layer '%s'", gs_ft_newname))
#		lyr_updated <- GS$updateLayer(layer)
#	}	
#}


