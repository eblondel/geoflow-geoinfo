require(googledrive)
require(sf)

fishery_register <- function(config){
	data = googledrive::drive_download(googledrive::drive_find("grsf_fishery_records_sf_points.gpkg")$id, overwrite = TRUE)
	sp = sf::st_read(data$name)
	out <- data.frame(
		code = sp$uuid,
		uri = sp$url,
		label = sp$short_name,
		definition = sp$grsf_name,
		stringsAsFactors = FALSE
	)
	out <- out[!is.na(out$code),]
	return(out)
}

resource_register <- function(config){
	
	data = googledrive::drive_download(googledrive::drive_find("grsf_resource_records_sf_points.gpkg")$id, overwrite = TRUE)
	sp = sf::st_read(data$name)
	out <- data.frame(
		code = sp$uuid,
		uri = sp$url,
		label = sp$short_name,
		definition = sp$grsf_name,
		stringsAsFactors = FALSE
	)
	out <- out[!is.na(out$code),]
	return(out)
}
