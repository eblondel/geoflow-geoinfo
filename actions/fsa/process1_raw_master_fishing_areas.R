#
# Scripts to manage the fishing areas GIS dataset
# This includes the production of:
# - normalized dataset FAO_AREAS (not erased)
# - normalized dataset FAO_AREAS_SINGLEPART (with Polygon features instead of MultiPolygon)
# - normalized dataset and erased by continent FAO_AREAS_ERASE
# 
# The derivate products handle the status of eac area, whether it is
# officially endorsed by CWP, or still at draft stage
#
# @author eblondel
# @date 2015/10/27
#
config$logger$INFO("============================================================================================")
config$logger$INFO("UPLOAD/PUBLISH FAO areas MASTER dataset...")
config$logger$INFO("============================================================================================")

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

#connect to GeoServer
GS <- config$software$output$geoserver
GSCONFIG <- config$software$output$geoserver_config
gs_ws_name <- GSCONFIG$properties$workspace
gs_ds_name <- GSCONFIG$properties$datastore
#connect to WFS
WFS <- config$software$input$wfs

#upload the FAO_AREAS_MASTER
gs_master <- "FAO_AREAS_MASTER"
gs_master_shpzip <- file.path(config$wd, "data/fsa", paste0(gs_master,".zip"))
config$logger$INFO(sprintf("Uploading FSA master shapefile '%s'...", gs_master_shpzip))
uploaded <- GS$uploadShapefile(
  gs_ws_name, gs_ds_name, endpoint = "file",configure = "none",
  update = "overwrite", gs_master_shpzip, "UTF-8"
)

# layer creation/update FAO_AREAS_MASTER
config$logger$INFO(sprintf("Publishing FSA master layer '%s'...", gs_master))
featureType <- GSFeatureType$new()
featureType$setName(gs_master)
featureType$setNativeName(gs_master)
featureType$setAbstract("Master FAO areas data layer. Used for Shapes maintenance")
featureType$setTitle(gs_master)
featureType$setSrs("EPSG:4326")
featureType$setNativeCRS("EPSG:4326")
featureType$setEnabled(TRUE)
featureType$setProjectionPolicy("REPROJECT_TO_DECLARED")
featureType$setLatLonBoundingBox(-180,-90,180,90, crs = "EPSG:4326")
featureType$setNativeBoundingBox(-180,-90,180,90, crs ="EPSG:4326")

#action on GS featuretype
ft <- GS$getFeatureType(gs_ws_name, gs_ds_name, gs_master)
if(!is(ft, "GSFeatureType")){
	config$logger$INFO(sprintf("Creating feature type '%s'", gs_master))
	ft_created <- GS$createFeatureType(gs_ws_name, gs_ds_name, featureType)
}else{
	config$logger$INFO(sprintf("Updating feature type '%s'", gs_master))
	ft_updated <- GS$updateFeatureType(gs_ws_name, gs_ds_name, featureType)
}

#geoserver layer
layer <- GSLayer$new()
layer$setName(gs_master)
layer$setDefaultStyle("generic")

#action on GS layer
lyr <- GS$getLayer(gs_master)
if(!is(lyr, "GSLayer")){
	config$logger$INFO(sprintf("Creating layer '%s'", gs_master))
	lyr_created <- GS$createLayer(layer)	
}else{
	config$logger$INFO(sprintf("Updating layer '%s'", gs_master))
	lyr_updated <- GS$updateLayer(layer)
}

#we reload the wfs capabilities to discover newly added FAO_AREAS_MASTER
#config$software$wfs$reloadCapabilities()

config$logger$INFO("Successful master data processing")
config$logger.warn(sprintf("We are here: %s", getwd()))
