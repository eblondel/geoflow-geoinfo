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
config$logger.info("============================================================================================")
config$logger.info("MANAGE FAO areas datasets...")
config$logger.info("============================================================================================")

#packages
#--------------------------------------------------------------------------------------------
require(sf)
require(terra)
require(magrittr)

#options
#--------------------------------------------------------------------------------------------
Sys.setlocale("LC_ALL", "en_US.UTF-8")
options(encoding="UTF-8")
options(stringsAsFactors = FALSE)

#functions
#--------------------------------------------------------------------------------------------

#get fishery statistical area levels
getFisheryStatAreas <- function(){
	fisheryStatAreas <- data.frame(
		propertyName = c("F_AREA", "F_SUBAREA", "F_DIVISION", "F_SUBDIVIS", "F_SUBUNIT"),
		levelName = c("MAJOR", "SUBAREA", "DIVISION", "SUBDIVISION", "SUBUNIT"),
		stringsAsFactors = FALSE
	)
	return(fisheryStatAreas)
}

#dissolve feature
dissolveFeature <- function(area, features){
	
	#proceed to the build
	areaCode = NULL
	subarea = NULL
	div = NULL
	subdiv = NULL
	
	#process the geometry
	out.sp <- sf::st_make_valid(sf::st_union(features))
	
	getCode = function(code){
	  if(is(code, "factor")) code = as.character(code)
	  if(length(code)>1) code = unique(code)[1]
	  if(startsWith(code, "_")) substr(code, 2, nchar(code)) else code
	}
	getStatus = function(code){
	  if(is(code, "factor")) code = as.character(code)
	  if(length(code)>1) code = unique(code)
	  if(startsWith(code, "_")) 0 else 1
	}
	
	print(getCode(features[[area$propertyName]]))
	print(getStatus(features[[area$propertyName]]))
	
	#handle attributes
	out.df <- switch(area$levelName,
		"MAJOR" = data.frame(
			"F_LEVEL" = "MAJOR",
			"F_CODE" = getCode(features[[area$propertyName]]),
			"F_STATUS" = getStatus(features[[area$propertyName]]),
			"F_NAME" = unique(features[["AREA_N"]])[1],
			"OCEAN" = unique(features[["OCEAN"]]),
			"SUBOCEAN" = unique(features[["SUBOCEAN"]]),
			"F_AREA" = getCode(features[[area$propertyName]]),
			"F_SUBAREA" = NA,
			"F_DIVISION" = NA,
			"F_SUBDIVIS" = NA,
			"F_SUBUNIT" = NA,
			stringsAsFactors = FALSE
		),
		"SUBAREA" = data.frame(
			"F_LEVEL" = "SUBAREA",
			"F_CODE" = getCode(features[[area$propertyName]]),
			"F_STATUS" = getStatus(features[[area$propertyName]]),
			"F_NAME" = unique(features[["SUBAREA_N"]])[1],
			"OCEAN" = unique(features[["OCEAN"]]),
			"SUBOCEAN" = unique(features[["SUBOCEAN"]]),
			"F_AREA" = unique(features[["F_AREA"]]),
			"F_SUBAREA" = getCode(features[[area$propertyName]]),
			"F_DIVISION" = NA,
			"F_SUBDIVIS" = NA,
			"F_SUBUNIT" = NA,
			stringsAsFactors = FALSE
		),
		"DIVISION" = data.frame(
			"F_LEVEL" = "DIVISION",
			"F_CODE" = getCode(features[[area$propertyName]]),
			"F_STATUS" = getStatus(features[[area$propertyName]]),
			"F_NAME" = unique(features[["DIVISION_N"]])[1],
			"OCEAN" = unique(features[["OCEAN"]]),
			"SUBOCEAN" = unique(features[["SUBOCEAN"]]),
			"F_AREA" = unique(features[["F_AREA"]]),
			"F_SUBAREA" = getCode(features[["F_SUBAREA"]]),
			"F_DIVISION" = getCode(features[[area$propertyName]]),
			"F_SUBDIVIS" = NA,
			"F_SUBUNIT" = NA,
			stringsAsFactors = FALSE
		),
		"SUBDIVISION" = data.frame(
			"F_LEVEL" = "SUBDIVISION",
			"F_CODE" = getCode(features[[area$propertyName]]),
			"F_STATUS" = getStatus(features[[area$propertyName]]),
			"F_NAME" = unique(features[["SUBDIVIS_N"]])[1],
			"OCEAN" = unique(features[["OCEAN"]]),
			"SUBOCEAN" = unique(features[["SUBOCEAN"]]),
			"F_AREA" = unique(features[["F_AREA"]]),
			"F_SUBAREA" = getCode(features[["F_SUBAREA"]]),
			"F_DIVISION" = getCode(features[["F_DIVISION"]]),
			"F_SUBDIVIS" = getCode(features[[area$propertyName]]),
			"F_SUBUNIT" = NA,
			stringsAsFactors = FALSE
		),
		"SUBUNIT" = data.frame(
			"F_LEVEL" = "SUBUNIT",
			"F_CODE" = getCode(features[[area$propertyName]]),
			"F_STATUS" = getStatus(features[[area$propertyName]]),
			"F_NAME" = unique(features[["SUBUNIT_N"]])[1],
			"OCEAN" = unique(features[["OCEAN"]]),
			"SUBOCEAN" = unique(features[["SUBOCEAN"]]),
			"F_AREA" = unique(features[["F_AREA"]]),
			"F_SUBAREA" = getCode(features[["F_SUBAREA"]]),
			"F_DIVISION" = getCode(features[["F_DIVISION"]]),
			"F_SUBDIVIS" = getCode(features[["F_SUBDIVIS"]]),
			"F_SUBUNIT" = getCode(features[[area$propertyName]]),
			stringsAsFactors = FALSE
		)
	)
	row.names(out.df) <- getCode(features[[area$propertyName]])
	
	out = NULL
	if(!is.null(out.sp)){
		out <- sf::st_sf(out.sp, out.df)		
	}
	
	return(out)
}


#area-based function to create new (dissolved) area
dissolveByFisheryArea <- function(area, features){

	#get unique list of codes
	areaCodes <- unique(features[[area$propertyName]])
	areaCodes <- areaCodes[!is.na(areaCodes)]
	
	#sub-collection
	sp.list <- lapply(areaCodes,
					  function(x){
						subcol <- features[!is.na(features[[area$propertyName]]) &
										   features[[area$propertyName]] == x,]
						out <- dissolveFeature(area, subcol)
						return(out)
					  })
	sp.list <- sp.list[!sapply(sp.list, is.null)]
	out.sp <- do.call("rbind",sp.list)
}

#main function to manage fishery stat areas
manageFisheryStatAreas <- function(features){

	areas <- getFisheryStatAreas()
	sp.list <- lapply(1:nrow(areas),
								function(x){
									area <- areas[x,]
									out <- dissolveByFisheryArea(area, features)
									return(out)
								})
	sp.list <- sp.list[!sapply(sp.list, is.null)]
	out.sp <- do.call("rbind", sp.list)
	return(out.sp)
}

#main function to add fishery stat area names
addFisheryStatAreaNames <- function(features, codelists){
	fsa.labels <- do.call("rbind", lapply(codelists, function(codelist){
		config$logger.info(sprintf("Current working directory: %s",getwd()))
		fsa.lab <- read.table(file.path(config$wd, "data/fsa", codelist), sep=",", h=TRUE)
		fields <- c("Code", "Name_En", "Name_Fr", "Name_Es")
		fsa.lab <- fsa.lab[,fields]
		colnames(fsa.lab) <- toupper(fields)
		return(fsa.lab)
	}))
	config$logger.info(sprintf("Successfuly read %s codelist tables", length(fsa.labels)))
	features$ID <- 1:nrow(features)
	featData <- merge(features, fsa.labels, by.x = "F_CODE", by.y = "CODE", all.x = TRUE, all.y = FALSE)
	featData <- featData[order(featData$ID),]
	features <- featData
	return(features)
}

#main function to erase fishery stat areas
eraseFisheryStatAreas <- function(features, eraser, computeSurfaces = TRUE){
  features = sf::st_make_valid(features)
  eraser = sf::st_make_valid(eraser)
	out <- sf::st_difference(features, eraser)
	if(computeSurfaces){
		out$SURFACE = as(sf::st_area(sf::st_transform(out, "+proj=eck4")), "numeric")
	}
	out$ID.1 = NULL
	out$ID = NULL
	out$gml_id = NULL
	return(out)
}

#business logic
#----------------------------------------------------------------------------------

#connect to WFS
WFS <- config$software$input$wfs

#read master data from the published source
data.sf <- WFS$getFeatures("fifao:FAO_AREAS_MASTER")
sf::st_crs(data.sf) = 4326
if(!is.null(data.sf)) config$logger.info("Successful fetching of master file through WFS")

# data <- as(data.sf, "Spatial")
# 
# #crop at North pole (-89.99 instead of -90) to avoid reprojection issues when exploiting the data
# config$logger.info("Crop data to fit global WGS84 extent")
# data <- raster::crop(data, raster::extent(-180,180,-89.99, 89.99))

#compute and export 'FAO_AREAS'
config$logger.info("Process master file (normalization)")
result <- manageFisheryStatAreas(data.sf)

fao_area_31 = result[result$F_AREA == 31,]


getParent = function(data){
  sapply(1:nrow(data), function(x){
    rec = data[x,]
    parent = switch(rec$F_LEVEL,
     "MAJOR" = NA,
     "SUBAREA" = rec$F_AREA,
     "DIVISION" = rec$F_SUBAREA,
     "SUBDIVISION" = rec$F_DIVISION,
     "SUBUNIT" = rec$F_SUBUNIT
    )
    return(parent)
  })
}

getLevel = function(data){
  sapply(1:nrow(data), function(x){
    rec = data[x,]
    lev = switch(rec$F_LEVEL,
      "MAJOR" = "area",
      "SUBAREA" = "subarea",
      "DIVISION" = "division",
      "SUBDIVISION" = "subdivision",
      "SUBUNIT" = "subunit"
    )
    return(lev)
  })
}

getFigisID = function(data){
  sapply(1:nrow(data), function(x){
    rec = data[x,]
    id = switch(rec$F_LEVEL,
                 "MAJOR" = paste0("fao_major:", rec$F_CODE),
                 "SUBAREA" = paste0("fao_sub_area:", rec$F_CODE),
                 "DIVISION" = paste0("fao_div:", rec$F_CODE),
                 "SUBDIVISION" = paste0("fao_sub_div:", rec$F_CODE),
                 "SUBUNIT" = paste0("fao_sub_unit:", rec$F_CODE)
    )
    return(id)
  })
}

#export_and_zip_features
export_and_zip_features = function(x, code, uri = NA, title, definition = title, dir = "outputs"){
  sf::st_write(x, paste0(code,".shp"))
  zip::zip(paste0(dir,"/",code,".zip"), files = list.files(".", paste0(code,"\\.")))
  meta = data.frame(code = code, uri = uri, label = title, definition = definition)
  readr::write_csv(meta, paste0(dir,"/register.csv"), append = T)
}


#manage area 31
area_31 = tibble::tibble(
  ID = paste0("fao:",fao_area_31$F_CODE),
  ID_old = fao_area_31$F_CODE,
  Code = fao_area_31$F_CODE,
  Parent_id_old = getParent(fao_area_31),
  Parent_id = paste0("fao:",getParent(fao_area_31)),
  Type = getLevel(fao_area_31),
  Name = fao_area_31$F_NAME,
  FigisID = getFigisID(fao_area_31),
  Codesystem = "fao",
  Description = "",
  TableOptions = "",
  Table = "",
  Images = "",
  Keywords = "",
  "Source Of information and URL" = "",
  "Additional reading" = "",
  "Environment" = "Marine areas",
  "Language" = "en",
  "CoverPage" = 640,
  CreationDate = Sys.Date(),
  ModifiedDate = Sys.Date(),
  RecordStatus = "PUBLISHED",
  CollectionCode = "CWP_HFS"
)
area_31 = area_31[-1,]


#inherit FAO area labels
config$logger.info("Inherit FAO area multi-lingual labels")
codelists <- c("CL_FI_WATERAREA_MAJOR.csv", "CL_FI_WATERAREA_SUBAREA.csv", "CL_FI_WATERAREA_DIVISION.csv", "CL_FI_WATERAREA_SUBDIVISION.csv", "CL_FI_WATERAREA_SUBUNIT.csv")
jobDirPath <- getwd()
setwd(config$wd)
result <- addFisheryStatAreaNames(result, codelists)

#add area31
result[result$F_AREA == 31 & result$F_LEVEL != "MAJOR",]$NAME_EN = area_31$Name
result[result$F_AREA == 31 & result$F_LEVEL != "MAJOR",]$NAME_FR = NA
result[result$F_AREA == 31 & result$F_LEVEL != "MAJOR",]$NAME_ES = NA
#harmonize F_NAME
result$F_NAME = result$NAME_EN


fsa_data_dir  = file.path(jobDirPath, "data/fsa")
setwd(fsa_data_dir)
data_files = list.files()
data_files = data_files[!startsWith(data_files,"CL_FI")]
data_files = data_files[!data_files %in% c("backup", "FAO_AREAS_MASTER.zip", "FAO_AREAS_INLAND.zip")]
unlink(data_files, force = T, recursive = T)
dir.create("outputs")
readr::write_csv(data.frame(code = character(0), uri = character(0), label = character(0), definition = character(0)), "outputs/register.csv")

config$logger.info(getwd())
config$logger.info(class(result))
sf::st_crs(result) = 4326
export_and_zip_features(result, code = "FAO_AREAS_NOCOASTLINE", title = "FAO statistical areas (Marine) - Multipolygons / No coastline (for use with custom coastline resolutions)")

#compute and export 'FAO_AREAS_ERASE'
config$logger.info("Compute/Export FAO areas layer erased by land")
continent.high <- WFS$getFeatures("fifao:UN_CONTINENT2_new")
sf::st_crs(continent.high) = 4326
result_erased_hr <- eraseFisheryStatAreas(result, continent.high)
export_and_zip_features(result_erased_hr, code = "FAO_AREAS_ERASE", title = "FAO statistical areas (Marine) - Multipolygons / Erased by coastline (intermediate resolution - aligned with UN countries & territories)")

continent.low <- WFS$getFeatures("fifao:UN_CONTINENT2")
sf::st_crs(continent.low) = 4326
result_erased_lr <- eraseFisheryStatAreas(result, continent.low)
export_and_zip_features(result_erased_lr, code = "FAO_AREAS_ERASE_LOWRES", title = "FAO statistical areas (Marine) - Multipolygons / Erased by coastline (low resolution - from former UNCS)")

#compute and export 'FAO_AREAS_SINGLEPART' ('FAO_AREAS' with Polygons instead of MultiPolygons)
config$logger.info("Compute/Export single part feature collections")
result_singlepart <- terra::vect(result) %>% terra::disagg() %>% sf::st_as_sf()
export_and_zip_features(result_singlepart, code = "FAO_AREAS_SINGLEPART", title = "FAO statistical areas (Marine) - Simple polygons / No coastline (for use with custom coastline resolutions)")

#compute and export 'FAO_AREAS_ERASE_SINGLEPART' ('FAO_AREAS_ERASE' with Polygons instead of MultiPolygons)
config$logger.info("Compute/Export single part feature collections")
result_erased_hr_singlepart <- terra::vect(result_erased_hr) %>% terra::disagg() %>% sf::st_as_sf()
export_and_zip_features(result_erased_hr_singlepart, code = "FAO_AREAS_ERASE_SINGLEPART", title = "FAO statistical areas (Marine) - Simple polygons / Erased by coastline (intermediate resolution - aligned with UN countries & territories)")
result_erased_lr_singlepart <- terra::vect(result_erased_lr) %>% terra::disagg() %>% sf::st_as_sf()
export_and_zip_features(result_erased_lr_singlepart, code = "FAO_AREAS_ERASE_SINGLEPART_LOWRES", title = "FAO statistical areas (Marine) - Simple polygons / Erased by coastline (low resolution - from former UNCS)")

#surfaces table
config$logger.info("Export FAO areas surface calculations")
readr::write_csv(as.data.frame(result_erased_hr)[,c("F_CODE", "SURFACE")],"fsa_surfaces.csv")

