require(googledrive)
require(sf)

area_system_owner_register = function(config){
  data = googledrive::drive_download(googledrive::drive_find("grsf_areas.gpkg")$id, overwrite = TRUE)
  cl = sf::st_read(data$name)
  out <- data.frame(
    code = cl$system_owner_code,
    uri = NA,
    label = cl$system_owner_name,
    definition = NA,
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$code),]
  return(out)
}

area_country_register = function(config){
  data = googledrive::drive_download(googledrive::drive_find("grsf_areas.gpkg")$id, overwrite = TRUE)
  cl = sf::st_read(data$name)
  iso3list = toupper(unique(cl$country))
  cl = readr::read_csv("https://raw.githubusercontent.com/fdiwg/fdi-codelists/refs/heads/main/global/cwp/cl_country_and_territory_iso3.csv")
  cl = cl[cl$code %in% iso3list,]
  cl$code = tolower(cl$code)
  out <- data.frame(
    code = cl$code,
    uri = cl$uri,
    label = cl$label,
    definition = cl$definition,
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$code),]
  return(out)
}

area_system_owner_register = function(config){
  data = googledrive::drive_download(googledrive::drive_find("grsf_areas.gpkg")$id, overwrite = TRUE)
  cl = sf::st_read(data$name)
  cl = unique(cl[,c("system_owner_code","system_owner_name")])
  out <- data.frame(
    code = cl$system_owner_code,
    uri = NA,
    label = cl$system_owner_name,
    definition = NA,
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$code),]
  return(out)
}

area_code_system_register = function(config){
  data = googledrive::drive_download(googledrive::drive_find("grsf_areas.gpkg")$id, overwrite = TRUE)
  cl = sf::st_read(data$name)
  cl = unique(cl[,c("code_system","code_system_name")])
  out <- data.frame(
    code = cl$code_system,
    uri = NA,
    label = cl$code_system_name,
    definition = NA,
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$code),]
  return(out)
}

area_code_register = function(config){
  data = googledrive::drive_download(googledrive::drive_find("grsf_areas.gpkg")$id, overwrite = TRUE)
  cl = sf::st_read(data$name)
  cl = unique(cl[,c("area_code","area_name")])
  out <- data.frame(
    code = cl$area_code,
    uri = NA,
    label = cl$area_name,
    definition = NA,
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$code),]
  return(out)
}

fishery_register <- function(config){
	data = googledrive::drive_download(googledrive::drive_find("grsf_fishery_placemarks.gpkg")$id, overwrite = TRUE)
	sp = sf::st_read(data$name)
	out <- data.frame(
		code = sp$uuid,
		uri = sp$url,
		label = sp$short_name,
		definition = NA,
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
		definition = NA,
		stringsAsFactors = FALSE
	)
	out <- out[!is.na(out$code),]
	return(out)
}

