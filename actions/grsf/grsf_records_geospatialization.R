function(action, entity, config){

#Install packages
require(sf)
require(geoflow)
require(geojsonsf)
library(dplyr)  # For data manipulation
library(ggplot2)  # For plotting

Geospatial_CQ_G4 <- entity$data$source[[1]]

#Extract GRSF competency query directly from GRSF website
grsf_records = read.csv(Geospatial_CQ_G4)

if (!"ram_code" %in% names(grsf_records)) {
  grsf_records$ram_code <- NA_character_
}

  #Rename GRSF_semantic_id column to grsf_semantic_id:
  #names(grsf_records)[names(grsf_records) == "GRSF_semantic_id"] <- "grsf_semantic_id"
  #Create status column (only if not existing) for follow same formulas as CQ G4:

  ## Check if the "status" column exists
  #if (!"status" %in% colnames(grsf_records)) {
  #  # Create the "status" column and fill with default value "approved"
  #  grsf_records$status <- "approved"
  #}

# Replace "eez:" with "wja:" in the "GRSF_semantic_id" column
#grsf_records$grsf_semantic_id <- gsub("eez:", "wja:", grsf_records$grsf_semantic_id)

#Remove geo_polygon column:
#grsf_records$geo_polygon <- NULL

#Separate areas from semantic_id in new column
#grsf_records$record_area <- sub('\\+authority:.*$','',sub('^[^+]*\\+','', grsf_records$grsf_semantic_id))

#Get all areas from GitHub
all_features_source <- entity$data$source[[2]]
all_features = st_read(all_features_source)
#all_features = st_read("C:/Users/artur/OneDrive/Documents/GitHub/water_areas_shapefiles/all_areas.gpkg")
#Concatenate namespace and area_code for obtaining grsf_area_code in all_features
#all_features$grsf_area_code <- paste(all_features$namespace,all_features$area_code,sep=":")

######PROVISIONAL UNTIL FIRMS, FISHSOURCE AND RAMLDB UPDATES AND GRSF REFRESH FOR NEW AREAS IMPLEMENTATION#####
#  new_areas_source <- entity$data$source[[3]]
#  new_areas <- readxl::read_excel(new_areas_source)
#  #new_areas <- readxl::read_excel("C:/Users/artur/OneDrive/Desktop/new_areas_test.xlsx")
#  new_areas$new_area = tolower(new_areas$new_area)
#  grsf_records$record_area <- tolower(grsf_records$record_area)

  #Split 'record_area' into individual areas
#  grsf_records$area_list <- strsplit(grsf_records$record_area, ";")

  #Function to check if all areas are in 'all_areas'
#  check_areas <- function(area_list, all_features) {
#    sapply(area_list, function(area) area %in% all_features$grsf_area_code)
#  }

  #Apply the function to create a logical vector indicating matched areas
#  grsf_records$all_areas_matched <- sapply(grsf_records$area_list, function(areas) {
#    all(check_areas(areas, all_features))
#  })

  #Extract records with unmatched areas
#  unmatched_records <- grsf_records[!grsf_records$all_areas_matched, ]

  #Merge unmatched records with new_areas using 'uuid' to get new record areas
#  merged_unmatched <- merge(unmatched_records, new_areas, by = "uuid", all.x = TRUE)

  #Replace the 'record_area' values in the original dataframe
#  grsf_records$record_new_area <- grsf_records$record_area # Create a new column to hold updated areas

  # Ensure correct replacement based on uuid
#  for(i in seq_len(nrow(merged_unmatched))) {
#   uuid <- merged_unmatched$uuid[i]
#    new_area <- merged_unmatched$new_area[i]

    # Update the corresponding record in grsf_records
#    grsf_records$record_new_area[grsf_records$uuid == uuid] <- new_area
#  }

  #replace as needed
#  grsf_records$record_area <- ifelse(!is.na(grsf_records$record_new_area),
#                                     grsf_records$record_new_area,
#                                     grsf_records$record_area)

  # Convert the new area values to lowercase
#  grsf_records$record_area <- tolower(grsf_records$record_area)
#########################################################################################################

grsf_records$record_area <- sub('\\+authority:.*$','',sub('^[^+]*\\+','', grsf_records$grsf_semantic_id))
grsf_records$record_area <- tolower(grsf_records$record_area)

#all_features = st_read("C:/Users/artur/OneDrive/Documents/GitHub/water_areas_shapefiles/all_areas.gpkg")
all_features$grsf_area_code <- paste(all_features$namespace,all_features$area_code,sep=":")
all_features$grsf_area_code <- tolower(all_features$grsf_area_code)

new_areas <- readxl::read_excel("C:/Users/artur/OneDrive/Desktop/new_areas_test.xlsx")
grsf_records <- grsf_records %>%
  left_join(new_areas %>% select(uuid, correct_area), by = "uuid")


#check some specific areas
#selected_areas <- all_features %>%
# filter(namespace == "grsf")
#tmap_mode("view")  # Use "plot" for static map
#tm_shape(selected_areas) +
# tm_polygons(col = "lightgreen", border.col = "black") +
# tm_layout(title = "GRSF Areas: zaf_sc and zaf_wc")

#Remove grsf namespace (old polygons that need to be updated)
#all_features <- all_features %>%
#  filter(namespace != "grsf")

#Add updates already shared with RAMLDB
ramldb_updates = readxl::read_excel("C:/Users/artur/Downloads/2024.09_RAMLDB_areas_updates.xlsx")

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

#Correct areas
grsf_records <- grsf_records %>%
  mutate(updated_area = if_else(all_areas_matched, record_area, correct_area))
grsf_records$updated_area <- tolower(grsf_records$updated_area)

#Split 'updated_area' into individual areas
grsf_records$updated_area_list <- strsplit(grsf_records$updated_area, ";")

#Function to check if all areas are in 'all_areas'
check_updated_areas <- function(updated_area_list, all_features) {
  sapply(updated_area_list, function(area) area %in% all_features$grsf_area_code)
}

#Apply the function to create a logical vector indicating matched areas
grsf_records$all_updated_areas_matched <- sapply(grsf_records$updated_area_list, function(areas) {
  all(check_areas(areas, all_features))
})

#Extract records with unmatched areas
unmatched_records <- grsf_records[!grsf_records$all_updated_areas_matched, ]


unmatched_records <- unmatched_records %>%
  rowwise() %>%
  mutate(
    comments = paste(
      c(
        if (any(grepl("eez:", area_list))) 'namespace to be updated to "wja"',
        if (any(grepl("idn:", area_list))) 'namespace to be updated to "idn_wpp"',
        if (any(grepl("nzl:", area_list))) 'namespace to be updated to "nzl_qma"',
        if (any(grepl("esp:", area_list))) 'namespace to be updated to "esp_regbio"',
        if (any(grepl("wja:gbm", area_list))) 'Gambia code is GMB. Area code changed to "wja:GMB" in FishSource',
        if (any(grepl("can_dfo_pfmc", area_list))) 'Namespace updated to "can_dfo_pgma"',
        if (any(grepl("can_dfo_sfa_sca:29w a-d", area_list))) 'Areas to be researched and digitized',
        if (any(grepl("chl_subpesca:pmcb", area_list))) 'Area updated in FishSource to "chl_subpesca:PMBC"',
        if (any(grepl("aus_cwf:ts_smf", area_list))) 'TS_SMF area code not available in GitHub. Use "aus_cwf:TSF" instead',
        if (any(grepl("aus_qld_cf:egf", area_list))) '"aus_qld_cf:EGF" area code not available in GitHub. Check if any other area of "aus_qld_cf" should be used instead',
        if (any(grepl("mex_inapesca:nhan", area_list))) 'Correct area code is "mex_inapesca:NHAN1"',
        if (any(grepl("mex_pmp_bws", area_list))) 'FishSource area codes already updated to "mex_inapesca:BWS1;mex_inapesca:BWS4;mex_inapesca:BWS2;mex_inapesca:BWS3;mex_inapesca:BWS5". Need to refresh GRSF',
        if (any(grepl("mez_pns_z", area_list))) 'Areas already corrected in FishSource. Need to refresh GRSF',
        if (any(grepl("unk:moz", area_list))) 'update to areas with namespace "moz_area"',
        if (any(grepl("ind_fz", area_list))) 'Change to namespace "ind_cmfri"',
        if (any(grepl("gfcm:0", area_list))) 'GFCM GSAs codes 1-9 are without "0". For example, "gfcm:5" should be used instead of "gfcm:05"',
        if (any(grepl("gfcm:11", area_list))) 'GFCM GSA 11 does not exist, use "gfcm:11.1;gfcm:11.2" instead',
        if (any(grepl("21.5.zu", area_list))) 'FAO area code "21.5.zu" to be updated to "21.5.Z.u"',
        if (any(grepl("21.5.y.b", area_list))) 'FAO does not define 21.5.y.b NAFO subdivision. Update "fao:21.5.y.b" to "nafo:5YB"',
        if (any(grepl("27.4c", area_list))) 'Correct code already updated to "27.4.c" in FishSource',
        if (any(grepl("21.4r", area_list))) 'Correct code already updated to "21.4.r" in FishSource',
        if (any(grepl("21.5y", area_list))) 'Correct code already updated to "27.5.y" in FishSource',
        if (any(grepl("21.5z", area_list))) 'Correct code already updated to "27.5.z" in FishSource',
        if (any(grepl("other:", area_list))) 'Use correct namespace. Most cases are for FAO 31 subareas and need to be updated to "fao"',
        if (any(grepl("fao:21.4.w:fao:21.4.x", area_list))) '":" used instead of ";" at "fao:21.4.w:fao:21.4.x". Already solved at FishSource profile',
        if (any(grepl("fao:1.1.1", area_list))) 'Area updated in FishSource to "bra_reg:SE;bra_reg:S"',
        if (any(grepl("fao:31.2.6", area_list))) 'Area updated in FishSource to "usa_sw:FL"',
        if (any(grepl("fao:87.3.14", area_list))) 'FAO 87.3.14 does not exists, may be refering to 87.3.13',
        if (any(grepl("dummy", area_list))) 'FishSource to resolve "dummy:areas" issue',
        if (any(grepl("grsf:aus_gem_e", area_list))) 'To be updated to new area "aus_csiro:GEM_E"',
        if (any(grepl("grsf:aus_gem_w", area_list))) 'To be updated to new area "aus_csiro:GEM_W"',
        if (any(grepl("grsf:aus_wa_wcrl", area_list))) 'Area already updated in FishSource to "aus_wa_wcrlf:A", "aus_wa_wcrlf:B" and "aus_wa_wcrlf:C" as applicable. GRSF refresh needed',
        if (any(grepl("grsf:nzl", area_list))) 'New namespace "grsf_nzl" to be used. FishSource needs to be updated',
        if (any(grepl("aus_cwf:cs", area_list))) '"aus_cwf:CS does not exists, correct area code is "CSF". Change to "aus_cwf:CSF"',
        if (any(grepl("2357", fishsource_code))) 'FishSource area already updated to "usa_efh_gfmc:ReefFish". GRSF refresh needed',
        if (any(grepl("834", fishsource_code))) 'FishSource area already updated to "mex_inapesca:PS1;mex_inapesca:PS2;mex_inapesca:PS3;mex_inapesca:PS4;mex_inapesca:PS5". GRSF refresh needed',
        if (any(grepl("Salmon", short_name))) 'Salmon record. Decission to differentiate area not agreed.',
        if (any(grepl("salmon", area_list))) 'Salmon record. Decission to differentiate area not agreed.'
      ),
      collapse = "; "
    )
  ) %>%
  ungroup()

unmatched_records <- unmatched_records %>%
  # Join new_area from ramldb_updates
  left_join(ramldb_updates %>% select(ram_code, new_area), by = "ram_code") %>%

  # Update comments
  mutate(
    comments = case_when(
      !is.na(new_area) & (is.na(comments) | comments == "") ~ "RAMLDB area update shared with Daniel",
      !is.na(new_area) ~ paste0(comments, "; RAMLDB area update shared with Daniel"),
      TRUE ~ comments
    )
  )

unmatched_records$new_area <- tolower(unmatched_records$new_area)

# Correct '4c' to '4.c' only when it follows '27.'
library(stringr)

# Define the pattern replacements
patterns <- c(
  "(\\d)\\.4c" = "\\1.4.c",
  "(\\d)\\.4r" = "\\1.4.r",
  "(\\d)\\.5z" = "\\1.5.z",
  "(\\d)\\.5y" = "\\1.5.y",
  "87.3.14" = "87.3.13",
  "aus_cwf:cs" = "aus_cwf:csf",
  "can_dfo_pfmc:3c;can_dfo_pfmc:3d;can_dfo_pfmc:5a;can_dfo_pfmc:5b;can_dfo_pfmc:5c;can_dfo_pfmc:5d;can_dfo_pfmc:5e" = "can_dfo_pgma:3c;can_dfo_pgma:3d;can_dfo_pgma:5a;can_dfo_pgma:5b;can_dfo_pgma:5c;can_dfo_pgma:5d;can_dfo_pgma:5e",
  "chl_subpesca:pmcb" = "chl_subpesca:pmbc",
  "fmz:" = "eu_fmz:",
  "glfc:on" = "glfc:on_can;glfc:on_usa",
  "glfc:er" = "glfc:er_can;glfc:er_usa",
  "idn:" = "idn_wpp:",
  "mex_pmp_aps:" = "mex_inapesca:ps",
  "ind_fz:" = "ind_cmfri:",
  "wja:gbm" = "wja:gmb",
  "gfcm:0" = "gfcm:",
  "can_dfo_pfmc" = "can_dfo_pfma",
  "other:" = "fao:"
)

# Ensure new_area exists (if not, initialize it as NA)
if (!"new_area" %in% names(unmatched_records)) {
  unmatched_records$new_area <- NA
}

# Apply replacements only where new_area is NA or empty
unmatched_records$new_area <- ifelse(
  is.na(unmatched_records$new_area) | unmatched_records$new_area == "",
  str_replace_all(unmatched_records$record_area, patterns),
  unmatched_records$new_area
)



#Split 'updated_area' into individual areas
unmatched_records$new_area_list <- strsplit(unmatched_records$new_area, ";")

#Function to check if all areas are in 'all_areas'
check_new_areas <- function(new_area_list, all_features) {
  sapply(new_area_list, function(area) area %in% all_features$grsf_area_code)
}

#Apply the function to create a logical vector indicating matched areas
unmatched_records$all_new_areas_matched <- sapply(unmatched_records$new_area_list, function(areas) {
  all(check_areas(areas, all_features))
})

View(unmatched_records)


# First, pull in the 'new_area' from unmatched_records using a left join
grsf_records <- grsf_records %>%
  left_join(unmatched_records %>% select(uuid, new_area_unmatched = new_area), by = "uuid")


grsf_records <- grsf_records %>%
  mutate(
    new_area = updated_area,  # Step 1: initialize with correct_areas
    new_area = if_else(        # Step 2: override with new_unmatched_area if not empty
      !is.na(new_area_unmatched) & new_area_unmatched != "",
      new_area_unmatched,
      new_area
    )
  )

#Split 'new_area' into individual areas
grsf_records$new_area_list <- strsplit(grsf_records$new_area, ";")

#Function to check if all areas are in 'all_areas'
check_new_areas <- function(new_area_list, all_features) {
  sapply(new_area_list, function(area) area %in% all_features$grsf_area_code)
}

#Apply the function to create a logical vector indicating matched areas
grsf_records$all_new_areas_matched <- sapply(grsf_records$new_area_list, function(areas) {
  all(check_areas(areas, all_features))
})

grsf_records <- grsf_records %>%
  mutate(
    comments = case_when(
      grepl("(?i)salmon", short_name) ~ "Salmon record",  # (?i) makes it case-insensitive
      grepl("dummy", new_area, ignore.case = TRUE) ~ "FishSource to solve dummy areas",
      grepl("can_dfo_sfa_sca:29w a-d", new_area, ignore.case = TRUE) ~ "Areas to be researched and digitized",
      grepl("cf:egf", new_area, ignore.case = TRUE) ~ "aus_qld_cf:EGF area code not available in GitHub. Check if any other area of aus_qld_cf should be used instead",
      TRUE ~ NA_character_
    )
  )


grsf_records$record_area <- grsf_records$new_area


#######################################################################################################


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
  publishable_fao <- ifelse(any(filtered_data$publishable_fao == "no"), "no", "yes")

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
              publishable_fao = publishable_fao,
              bbox_wkt = bbox_wkt))
}

results <- mapply(extract_combine_polygons_and_outputs, strsplit(grsf_records$record_area, ";"), list(all_features), SIMPLIFY = FALSE)

grsf_records$bbox_wkt <- sapply(results, function(x) x$bbox_wkt)
grsf_records$publishable_fao <- sapply(results, function(x) x$publishable_fao)
grsf_records$geo_polygon <- lapply(results, function(x) x$multipolygon)
grsf_records$geo_polygon_wkt <- sapply(results, function(x) x$wkt)
grsf_records$centroid <- sapply(results, function(x) x$centroid_wkt)


##Only approved records:
# Create SF object for grsf_approved_records centroids
grsf_approved_records_sf_centroid = sf::st_sf(
  uuid = grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0, ]$uuid,
  url = paste0("https://i-marine.d4science.org/group/grsf/data-catalogue?path=/dataset/", grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0, ]$uuid),
  grsf_semantic_id = grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0, ]$grsf_semantic_id,
  short_name = gsub("&", "and", grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0, ]$short_name),
  grsf_name = gsub("&", "and", grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0, ]$grsf_name),
  type = grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0, ]$type,
  #traceability_flag = grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0, ]$traceability_flag,
  #sdg_flag = grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0, ]$sdg_flag,
  geom = st_as_sfc(grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0, ]$centroid)
)

#Add resource attributes if applicable:

if ("sdg_flag" %in% colnames(grsf_records)) {
  grsf_approved_records_sf_centroid$sdg_flag = grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0, ]$sdg_flag
}

#Add fishery attributes if applicable:

if ("traceability_flag" %in% colnames(grsf_records)) {
  grsf_approved_records_sf_centroid$traceability_flag = grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0, ]$traceability_flag
}

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

if ("management_entities" %in% colnames(grsf_records)) {
  grsf_approved_records_sf_centroid$management_entity = gsub("&", "and", grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0, ]$management_entities)
}


st_crs(grsf_approved_records_sf_centroid)=4326

#Delete empty geometries
grsf_approved_records_sf_centroid <- grsf_approved_records_sf_centroid[!st_is_empty(grsf_approved_records_sf_centroid), ]

# Replace '&' with 'and' in all character columns (excluding geometry)
grsf_approved_records_sf_centroid[] <- lapply(grsf_approved_records_sf_centroid, function(col) {
  if (is.character(col)) {
    gsub("&", "and", col, fixed = TRUE)
  } else {
    col
  }
})

st_write(grsf_approved_records_sf_centroid, file.path(getwd(), "data", paste0(entity$identifiers$id, "_placemarks", ".gpkg")))
#upload to Googledrive
googledrive::drive_upload(file.path(getwd(), "data", paste0(entity$identifiers$id, "_placemarks", ".gpkg")), path = googledrive::as_dribble("fisheriesatlas/grsf/data"), overwrite = TRUE)


# Create SF object for non publishable_fao (bbox)
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
valid_indices <- grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0 & grsf_records$publishable_fao == "no" & valid_geometries
grsf_approved_records_sf_bbox <- sf::st_sf(
  uuid = grsf_records[valid_indices, ]$uuid,
  url = paste0("https://i-marine.d4science.org/group/grsf/data-catalogue?path=/dataset/", grsf_records[valid_indices, ]$uuid),
  grsf_semantic_id = grsf_records[valid_indices, ]$grsf_semantic_id,
  short_name = grsf_records[valid_indices, ]$short_name,
  grsf_name = grsf_records[valid_indices, ]$grsf_name,
  type = grsf_records[valid_indices, ]$type,
  #traceability_flag = grsf_records[valid_indices, ]$traceability_flag,
  #sdg_flag = grsf_records[valid_indices, ]$sdg_flag,
  geom = st_as_sfc(grsf_records[valid_indices, ]$bbox_wkt)
)

#Add resource attributes if applicable:

if ("sdg_flag" %in% colnames(grsf_records)) {
  grsf_approved_records_sf_bbox$sdg_flag = grsf_records[valid_indices, ]$sdg_flag
}

#Add fishery attributes if applicable:

if ("traceability_flag" %in% colnames(grsf_records)) {
  grsf_approved_records_sf_bbox$traceability_flag = grsf_records[valid_indices, ]$traceability_flag
}

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

if ("management_entities" %in% colnames(grsf_records)) {
  grsf_approved_records_sf_bbox$management_entity = gsub("&", "and", grsf_records[valid_indices, ]$management_entities)
}


# Set CRS
st_crs(grsf_approved_records_sf_bbox) = 4326


# SF object for publishable_fao (polygon)
grsf_approved_records_sf_polygons = sf::st_sf(
  uuid = grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0 &  grsf_records$publishable_fao == "yes", ]$uuid,
  url = paste0("https://i-marine.d4science.org/group/grsf/data-catalogue?path=/dataset/", grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0 &  grsf_records$publishable_fao == "yes", ]$uuid),
  grsf_semantic_id = grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0 &  grsf_records$publishable_fao == "yes", ]$grsf_semantic_id,
  short_name = gsub("&", "and", grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0 &  grsf_records$publishable_fao == "yes", ]$short_name),
  grsf_name = gsub("&", "and", grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0 &  grsf_records$publishable_fao == "yes", ]$grsf_name),
  type = grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0 &  grsf_records$publishable_fao == "yes", ]$type,
  #traceability_flag = grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0 &  grsf_records$publishable_fao == "yes", ]$traceability_flag,
  #sdg_flag = grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0 &  grsf_records$publishable_fao == "yes", ]$sdg_flag,
  geom = st_as_sfc(grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0 &  grsf_records$publishable_fao == "yes", ]$geo_polygon_wkt)
)

#Add resource attributes if applicable:

  if ("sdg_flag" %in% colnames(grsf_records)) {
    grsf_approved_records_sf_polygons$sdg_flag = grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0 &  grsf_records$publishable_fao == "yes", ]$sdg_flag
  }

#Add fishery attributes if applicable:

  if ("traceability_flag" %in% colnames(grsf_records)) {
    grsf_approved_records_sf_polygons$traceability_flag = grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0 &  grsf_records$publishable_fao == "yes", ]$traceability_flag
  }

  if ("gear_code" %in% colnames(grsf_records)) {
    grsf_approved_records_sf_polygons$gear_code = grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0 &  grsf_records$publishable_fao == "yes", ]$gear_code
  }

  if ("gear_type" %in% colnames(grsf_records)) {
    grsf_approved_records_sf_polygons$gear_type = grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0 &  grsf_records$publishable_fao == "yes", ]$gear_type
  }

  if ("flag_code" %in% colnames(grsf_records)) {
    grsf_approved_records_sf_polygons$flag_code = grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0 &  grsf_records$publishable_fao == "yes", ]$flag_code
  }

  if ("flag_type" %in% colnames(grsf_records)) {
    grsf_approved_records_sf_polygons$flag_type = grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0 &  grsf_records$publishable_fao == "yes", ]$flag_type
  }

  if ("management_entities" %in% colnames(grsf_records)) {
    grsf_approved_records_sf_polygons$management_entity = gsub("&", "and", grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0 &  grsf_records$publishable_fao == "yes", ]$management_entities)
  }

#Set CRS
st_crs(grsf_approved_records_sf_polygons)=4326

#Combine both SF objects
grsf_combined_sf <- rbind(grsf_approved_records_sf_bbox, grsf_approved_records_sf_polygons)

# Set CRS for the combined SF object
st_crs(grsf_combined_sf) = 4326

#Delete empty geometries
grsf_combined_sf <- grsf_combined_sf[!st_is_empty(grsf_combined_sf), ]

# Replace '&' with 'and' in all character columns (excluding geometry)
grsf_combined_sf[] <- lapply(grsf_combined_sf, function(col) {
  if (is.character(col)) {
    gsub("&", "and", col, fixed = TRUE)
  } else {
    col
  }
})

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
