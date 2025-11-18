function(action, entity, config){

#Install packages
require(sf)
require(geoflow)
require(geojsonsf)
library(purrr)
library(dplyr)  # For data manipulation
library(ggplot2)  # For plotting

#Get all areas from GitHub
#all_features_source <- entity$data$source[[2]]
#all_features = st_read(all_features_source)
all_features_simplified = st_read("C:/Users/artur/OneDrive/Documents/GitHub/water_areas_shapefiles/all_areas_simplified.gpkg")
all_features_simplified <- all_features_simplified %>%
  rename(use_limitation = useLimitation)
all_features_simplified <- cbind(ID = tolower(paste(all_features_simplified$namespace, all_features_simplified$area_code, sep=":")), all_features_simplified)
#all_features_simplified$useLimitation <- use_limitation

##Geospatializing grsf_records data frame###########################

  # Function to check if geometry is valid
valid_geoms <- st_is_valid(all_features_simplified)
invalid_geoms <- all_features_simplified[!valid_geoms,]
#View(invalid_geoms)
if (nrow(invalid_geoms) > 0) {
  all_features_simplified <- st_make_valid(all_features_simplified)
  }



# Geom as geometry column
st_geometry(all_features_simplified) <- "geom"

# Publishable features keeping original geometry
all_features_simplified_publishable <- all_features_simplified %>%
  filter(publishable == "yes")

# For non-publishable: replace geometry with its bounding box
bbox_geometries <- all_features_simplified %>%
  filter(publishable == "no") %>%
  pull(geom) %>%
  map(~ st_as_sfc(st_bbox(.x))[[1]])

# Ensure sfc object
bbox_sfc <- st_sfc(bbox_geometries, crs = st_crs(all_features_simplified))

# Reconstruct the non-publishable sf object
all_features_simplified_non_publishable <- all_features_simplified %>%
  filter(publishable == "no") %>%
  st_drop_geometry() %>%  # Remove original geom column
  mutate(geom = bbox_sfc) %>%
  st_as_sf()

# Combine both
grsf_areas <- bind_rows(
  all_features_simplified_publishable,
  all_features_simplified_non_publishable
)

# Ensure 'geom' is the geometry column again
st_geometry(grsf_areas) <- "geom"


#Delete empty geometries
all_features_simplified_public_errors <- grsf_areas[st_is_empty(grsf_areas), ]
View(all_features_simplified_public_errors)
grsf_areas <- grsf_areas[!st_is_empty(grsf_areas), ]


# Replace '&' with 'and' in all character columns (excluding geometry)
grsf_areas[] <- lapply(grsf_areas, function(col) {
  if (is.character(col)) {
    gsub("&", "and", col, fixed = TRUE)
  } else {
    col
  }
})

# Write the combined SF object to a file
st_write(grsf_areas, file.path(getwd(), "data", "grsf_areas.gpkg"))
#st_write(grsf_areas, "grsf_areas.gpkg", delete_layer = TRUE)

#Set working directory to DATA folder
setwd("data")
#ZIP geopackage for polygons
#zip::zip("grsf_areas.zip",
#         "grsf_areas.gpkg")
#back to the previous folder
setwd("..")

#Upload ZIP to googledrive
googledrive::drive_upload(file.path(getwd(), "data", "grsf_areas.gpkg"), path = googledrive::as_dribble("fisheriesatlas/grsf/data"), overwrite = TRUE)

}
