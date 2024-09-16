function(action, entity, config){

#Install packages
require(sf)
require(geoflow)
require(geojsonsf)
library(dplyr)  # For data manipulation
library(ggplot2)  # For plotting

Geospatial_CQ_G3 <- entity$data$source[[1]]

#Extract GRSF competency query directly from GRSF website
grsf_records = read.csv(Geospatial_CQ_G3)

#Rename GRSF_semantic_id column to grsf_semantic_id:
names(grsf_records)[names(grsf_records) == "GRSF_semantic_id"] <- "grsf_semantic_id"
#Create status column (only if not existing) for follow same formulas as CQ G4:
# Check if the "status" column exists
if (!"status" %in% colnames(grsf_records)) {
  # Create the "status" column and fill with default value "approved"
  grsf_records$status <- "approved"
}

# Replace "eez:" with "wja:" in the "GRSF_semantic_id" column
grsf_records$grsf_semantic_id <- gsub("eez:", "wja:", grsf_records$grsf_semantic_id)

#Remove geo_polygon column:
grsf_records$geo_polygon <- NULL

#Separate areas from semantic_id in new column
grsf_records$record_area <- sub('\\+authority:.*$','',sub('^[^+]*\\+','', grsf_records$grsf_semantic_id))

#Get all areas from GitHub
all_features_source <- entity$data$source[[2]]
all_features = st_read(all_features_source)
#all_features = st_read("C:/Users/artur/OneDrive/Documents/GitHub/water_areas_shapefiles/all_areas.gpkg")
#Concatenate namespace and area_code for obtaining grsf_area_code in all_features
all_features$grsf_area_code <- paste(all_features$namespace,all_features$area_code,sep=":")

######PROVISIONAL UNTIL FIRMS, FISHSOURCE AND RAMLDB UPDATES AND GRSF REFRESH FOR NEW AREAS IMPLEMENTATION#####
  new_areas_source <- entity$data$source[[3]]
  new_areas <- readxl::read_excel(new_areas_source)
  #new_areas <- readxl::read_excel("C:/Users/artur/OneDrive/Desktop/new_areas_test.xlsx")
  new_areas$new_area = tolower(new_areas$new_area)
  grsf_records$record_area <- tolower(grsf_records$record_area)

  #Split 'record_area' into individual areas
  grsf_records$area_list <- strsplit(grsf_records$record_area, ";")

  #Function to check if all areas are in 'all_areas'
  check_areas <- function(area_list, all_features) {
    sapply(area_list, function(area) area %in% all_features$grsf_area_code)
  }

  #Apply the function to create a logical vector indicating matched areas
  grsf_records$all_areas_matched <- sapply(grsf_records$area_list, function(areas) {
    all(check_areas(areas, all_features))
  })

  #Extract records with unmatched areas
  unmatched_records <- grsf_records[!grsf_records$all_areas_matched, ]

  #Merge unmatched records with new_areas using 'uuid' to get new record areas
  merged_unmatched <- merge(unmatched_records, new_areas, by = "uuid", all.x = TRUE)

  #Replace the 'record_area' values in the original dataframe
  grsf_records$record_new_area <- grsf_records$record_area # Create a new column to hold updated areas

  # Ensure correct replacement based on uuid
  for(i in seq_len(nrow(merged_unmatched))) {
    uuid <- merged_unmatched$uuid[i]
    new_area <- merged_unmatched$new_area[i]
  
    # Update the corresponding record in grsf_records
    grsf_records$record_new_area[grsf_records$uuid == uuid] <- new_area
  }

  #replace as needed
  grsf_records$record_area <- ifelse(!is.na(grsf_records$record_new_area), 
                                     grsf_records$record_new_area, 
                                     grsf_records$record_area)

  # Convert the new area values to lowercase
  grsf_records$record_area <- tolower(grsf_records$record_area)
#########################################################################################################


##Geospatializing grsf_records data frame###########################

  # Function to check if geometry is valid
valid_geoms <- st_is_valid(all_features)
invalid_geoms <- all_features[!valid_geoms,]
#View(invalid_geoms)
if (nrow(invalid_geoms) > 0) {
  all_features <- st_make_valid(all_features)
  }

# Function to extract, combine polygons, and generate required outputs for given area codes
extract_combine_polygons_and_outputs <- function(area_codes, all_features) {
  # Convert area codes to lowercase
  area_codes_lower <- tolower(area_codes)
  all_features$grsf_area_code_lower <- tolower(all_features$grsf_area_code)
  
  # Filter GIS data for the given area codes (case-insensitive)
  filtered_data <- all_features[all_features$grsf_area_code_lower %in% area_codes_lower, ]
  
  # Extract geometries
  filtered_geoms <- filtered_data$geom
  
  # Combine geometries into a single multipolygon
  combined_multipolygon <- st_union(filtered_geoms)
  
  # Convert the combined multipolygon to well-known text (WKT) format
  combined_multipolygon_wkt <- st_as_text(combined_multipolygon)
  
  # Calculate centroid of the combined geometry
  centroid <- st_point_on_surface(combined_multipolygon)
  
  # Convert centroid to WKT format
  centroid_wkt <- st_as_text(centroid)
  
  # Determine publishability
  publishable <- ifelse(any(filtered_data$publishable == "no"), "no", "yes")
  
  # Calculate bounding box for the combined geometry
  bbox_wkt <- tryCatch({
    bbox <- st_bbox(combined_multipolygon)
    bbox_polygon <- st_polygon(list(matrix(c(
      bbox["xmin"], bbox["ymin"],
      bbox["xmin"], bbox["ymax"],
      bbox["xmax"], bbox["ymax"],
      bbox["xmax"], bbox["ymin"],
      bbox["xmin"], bbox["ymin"]
    ), ncol = 2, byrow = TRUE)))
    st_as_text(bbox_polygon)
  }, error = function(e) {
    message("Error calculating bounding box: ", e$message)
    "character(0)"  # Return empty string if there's an error with st_bbox
  })
  
  # Return a list with all required outputs
  return(list(multipolygon = st_sf(geometry = combined_multipolygon), 
              wkt = combined_multipolygon_wkt, 
              centroid_wkt = centroid_wkt,
              publishable = publishable,
              bbox_wkt = bbox_wkt))
}

results <- mapply(extract_combine_polygons_and_outputs, strsplit(grsf_records$record_area, ";"), list(all_features), SIMPLIFY = FALSE)

grsf_records$bbox_wkt <- sapply(results, function(x) x$bbox_wkt)
grsf_records$publishable <- sapply(results, function(x) x$publishable)
grsf_records$geo_polygon <- lapply(results, function(x) x$multipolygon)
grsf_records$geo_polygon_wkt <- sapply(results, function(x) x$wkt)
grsf_records$centroid <- sapply(results, function(x) x$centroid_wkt)


##Only approved records:
# Create SF object for grsf_approved_records centroids
grsf_approved_records_sf_centroid = sf::st_sf(
  uuid = grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0, ]$uuid,
  url = grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0, ]$url,
  grsf_semantic_id = grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0, ]$grsf_semantic_id,
  short_name = grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0, ]$short_name,
  grsf_name = grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0, ]$grsf_name,
  type = grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0, ]$type,
  traceability_flag = grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0, ]$traceability_flag,
  sdg_flag = grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0, ]$sdg_flag,
  geom = st_as_sfc(grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0, ]$centroid)
)

#Add fishery attributes if applicable:

if ("gear_code" %in% colnames(grsf_records)) {
  grsf_approved_records_sf_centroid$gear_code = grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0, ]$gear_code
}

if ("gear_type" %in% colnames(grsf_records)) {
  grsf_approved_records_sf_centroid$gear_type = grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0, ]$gear_type
}

if ("flag_code" %in% colnames(grsf_records)) {
  grsf_approved_records_sf_centroid$flag_code = grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0, ]$flag_code
}

if ("flag_type" %in% colnames(grsf_records)) {
  grsf_approved_records_sf_centroid$flag_type = grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0, ]$flag_type
}

if ("management_entity" %in% colnames(grsf_records)) {
  grsf_approved_records_sf_centroid$management_entity = grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0, ]$management_entity
}


st_crs(grsf_approved_records_sf_centroid)=4326
st_write(grsf_approved_records_sf_centroid, file.path(getwd(), "data", paste0(entity$identifiers$id, "_placemarks", ".gpkg")))
#upload to Googledrive
googledrive::drive_upload(file.path(getwd(), "data", paste0(entity$identifiers$id, "_placemarks", ".gpkg")), path = googledrive::as_dribble("fisheriesatlas/grsf/data"), overwrite = TRUE)


# Create SF object for non publishable (bbox)
# Function to check if a geometry is valid
is_valid_geometry <- function(wkt) {
  tryCatch({
    geom <- st_as_sfc(wkt)
    st_is_valid(geom)
  }, error = function(e) {
    FALSE
  })
}

# Apply the validation function
valid_geometries <- sapply(grsf_records$bbox_wkt, is_valid_geometry)


# Filter out invalid geometries
valid_indices <- grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0 & grsf_records$publishable == "no" & valid_geometries
grsf_approved_records_sf_bbox <- sf::st_sf(
  uuid = grsf_records[valid_indices, ]$uuid,
  url = grsf_records[valid_indices, ]$url,
  grsf_semantic_id = grsf_records[valid_indices, ]$grsf_semantic_id,
  short_name = grsf_records[valid_indices, ]$short_name,
  grsf_name = grsf_records[valid_indices, ]$grsf_name,
  type = grsf_records[valid_indices, ]$type,
  traceability_flag = grsf_records[valid_indices, ]$traceability_flag,
  sdg_flag = grsf_records[valid_indices, ]$sdg_flag,
  geom = st_as_sfc(grsf_records[valid_indices, ]$bbox_wkt)
)

#Add fishery attributes if applicable:

if ("gear_code" %in% colnames(grsf_records)) {
  grsf_approved_records_sf_bbox$gear_code = grsf_records[valid_indices, ]$gear_code
}

if ("gear_type" %in% colnames(grsf_records)) {
  grsf_approved_records_sf_bbox$gear_type = grsf_records[valid_indices, ]$gear_type
}

if ("flag_code" %in% colnames(grsf_records)) {
  grsf_approved_records_sf_bbox$flag_code = grsf_records[valid_indices, ]$flag_code
}

if ("flag_type" %in% colnames(grsf_records)) {
  grsf_approved_records_sf_bbox$flag_type = grsf_records[valid_indices, ]$flag_type
}

if ("management_entity" %in% colnames(grsf_records)) {
  grsf_approved_records_sf_bbox$management_entity = grsf_records[valid_indices, ]$management_entity
}


# Set CRS
st_crs(grsf_approved_records_sf_bbox) = 4326


# SF object for publishable (polygon)
grsf_approved_records_sf_polygons = sf::st_sf(
  uuid = grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0 &  grsf_records$publishable == "yes", ]$uuid,
  url = grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0 &  grsf_records$publishable == "yes", ]$url,
  grsf_semantic_id = grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0 &  grsf_records$publishable == "yes", ]$grsf_semantic_id,
  short_name = grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0 &  grsf_records$publishable == "yes", ]$short_name,
  grsf_name = grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0 &  grsf_records$publishable == "yes", ]$grsf_name,
  type = grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0 &  grsf_records$publishable == "yes", ]$type,
  traceability_flag = grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0 &  grsf_records$publishable == "yes", ]$traceability_flag,
  sdg_flag = grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0 &  grsf_records$publishable == "yes", ]$sdg_flag,
  geom = st_as_sfc(grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0 &  grsf_records$publishable == "yes", ]$geo_polygon_wkt)
)

#Add fishery attributes if applicable:

  if ("gear_code" %in% colnames(grsf_records)) {
    grsf_approved_records_sf_polygons$gear_code = grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0 &  grsf_records$publishable == "yes", ]$gear_code
  }

  if ("gear_type" %in% colnames(grsf_records)) {
    grsf_approved_records_sf_polygons$gear_type = grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0 &  grsf_records$publishable == "yes", ]$gear_type
  }

  if ("flag_code" %in% colnames(grsf_records)) {
    grsf_approved_records_sf_polygons$flag_code = grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0 &  grsf_records$publishable == "yes", ]$flag_code
  }

  if ("flag_type" %in% colnames(grsf_records)) {
    grsf_approved_records_sf_polygons$flag_type = grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0 &  grsf_records$publishable == "yes", ]$flag_type
  }

  if ("management_entity" %in% colnames(grsf_records)) {
    grsf_approved_records_sf_polygons$management_entity = grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0 &  grsf_records$publishable == "yes", ]$management_entity
  }

#Set CRS
st_crs(grsf_approved_records_sf_polygons)=4326

#Combine both SF objects
grsf_combined_sf <- rbind(grsf_approved_records_sf_bbox, grsf_approved_records_sf_polygons)

# Set CRS for the combined SF object
st_crs(grsf_combined_sf) = 4326

# Write the combined SF object to a file
st_write(grsf_combined_sf, file.path(getwd(), "data", paste0(entity$identifiers$id, "_polygons", ".gpkg")))
#Set working directory to DATA folder
setwd("data")
#ZIP geopackage for polygons
zip::zip(paste0(entity$identifiers$id, "_polygons", ".zip"), 
         paste0(entity$identifiers$id, "_polygons", ".gpkg"))
#back to the previous folder
setwd("..")

#Upload ZIP to googledrive
googledrive::drive_upload(file.path(getwd(), "data", paste0(entity$identifiers$id, "_polygons", ".zip")), path = googledrive::as_dribble("fisheriesatlas/grsf/data"), overwrite = TRUE)

}