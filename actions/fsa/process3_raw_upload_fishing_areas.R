#
# Scripts to upload the fishing areas GIS datasets to GeoServer
#
# @author eblondel
# @date 2018/09/25
#
config$logger.info("============================================================================================")
config$logger.info("UPLOAD main FAO areas datasets...")
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

#upload shapefiles
shapefiles_to_upload <- list.files(getwd(), pattern = ".zip", full.names = TRUE)
for(shapefile in shapefiles_to_upload){

	#shapefile upload
	config$logger.info(sprintf("Uploading shapefile '%s'...", shapefile))
	uploaded <- GS$uploadShapefile(
	  gs_ws_name, gs_ds_name, endpoint = "file",configure = "none",
	  update = "overwrite", shapefile, "UTF-8"
	)
	
	#shapefile clone upload into fsa workspace
	#This clone will be used for FSA GEMS products
	shp_splits <- unlist(strsplit(shapefile,"/"))
	gs_ft_name <- unlist(strsplit(shp_splits[length(shp_splits)],".zip"))[1]
	if(!(gs_ft_name %in% c("FAO_AREAS_SINGLEPART","FAO_AREAS_ERASE_SINGLEPART", "FAO_AREAS_ERASE_SINGLEPART_LOWRES"))){
		uploaded <- GS$uploadShapefile(
		  gs_ws_name, gs_ds_name, endpoint = "file",configure = "none",
		  update = "overwrite", shapefile, "UTF-8"
		)
	}
	
}
	

