require(googledrive)
require(sf)

fishery_register <- function(config){
	data = googledrive::drive_download(googledrive::drive_find("grsf_fishery_placemarks.gpkg")$id, overwrite = TRUE)
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
	
	data = googledrive::drive_download(googledrive::drive_find("grsf_resource_placemarks.gpkg")$id, overwrite = TRUE)
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

type_register <- function(config){
  data = googledrive::drive_download(googledrive::drive_find("grsf_resource_placemarks.gpkg")$id, overwrite = TRUE)
  sp = sf::st_read(data$name)
  out <- data.frame(
    code = sp$type,
    uri = NA,
    label = sp$type,
    definition = NA,
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$code),]
  return(out)
}

sdg_flag <- function(config){
  data = googledrive::drive_download(googledrive::drive_find("grsf_resource_placemarks.gpkg")$id, overwrite = TRUE)
  sp = sf::st_read(data$name)
  out <- data.frame(
    code = sp$sdg_flag,
    uri = NA,
    label = sp$sdg_flag,
    definition = NA,
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$code),]
  return(out)
}

