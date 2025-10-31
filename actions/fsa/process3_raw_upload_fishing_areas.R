#
# Scripts to upload the fishing areas internal GIS datasets to GeoServer
# Public datasets are not uploaded with this script, but with a dedicated geoflow
# (See config_nfis_fao_areas_main.json)
#
# @author eblondel
#
config$logger$INFO("============================================================================================")
config$logger$INFO("UPLOAD main FAO areas datasets...")
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

GS <- config$software$output$geoserver
GSCONFIG <- config$software$output$geoserver_config
gs_ws_name <- GSCONFIG$properties$workspace
gs_ds_name <- GSCONFIG$properties$datastore

#upload shapefiles
shapefiles_to_upload <- list.files(file.path(getwd(),"data/fsa/outputs/internal"), pattern = ".zip", full.names = TRUE)
for(shapefile in shapefiles_to_upload){

	#shapefile upload
	config$logger$INFO(sprintf("Uploading shapefile '%s'...", shapefile))
	uploaded <- GS$uploadShapefile(
	  gs_ws_name, gs_ds_name, endpoint = "file",configure = "none",
	  update = "overwrite", shapefile, "UTF-8"
	)
	
}
	

