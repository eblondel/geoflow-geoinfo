function(action, entity, config){
  
  # Connect to WFS FAO Geoserver
  WFS_FAO <- ows4R::WFSClient$new(
    url = "https://www.fao.org/fishery/geoserver/wfs",
    serviceVersion = "1.0.0",
    logger = "INFO"
  )
  
  # Low-res continents layer
  continent_lowres <- WFS_FAO$getFeatures("fifao:UN_CONTINENT2") #TODO to move to a R fdi data package
  sf::st_crs(continent_lowres) = 4326
  continent_lowres <- sf::st_make_valid(continent_lowres)
  
  # Connect to Server
  #Identify server
  server = NA
  servers = entity$relations[sapply(entity$relations, function(x){x$key == "http"})]
  if(length(servers)>0){
    server = servers[[1]]$link
  }
  
  # Connect to WFS
  WFS <- ows4R::WFSClient$new(
    url = server,
    serviceVersion = "1.0.0",
    logger = "INFO"
  )
  
  #Get eez_land from WFS
  eez_land <- WFS$getFeatures(entity$data$source[[1]])
  sf::st_crs(eez_land) = 4326
  
  
  # Extract ABNJ from previous entity data WJA_level0.gpkg "../wja_level0/data/wja_level0.gpkg"
  
  wja_level0 <-sf::st_read("../wja_level0/data/wja_level0.gpkg")
  sf::st_crs(wja_level0) = 4326
  wja_level0 <- sf::st_make_valid(wja_level0)
  
  wja_abnj <- wja_level0 |>
    dplyr::filter(code == "abnj")
  
  
  # WJA_level1 
  
  #Standardize eez_land attribute table with code and urn
  eez_land <- eez_land |>
    dplyr::mutate(
      code = dplyr::case_when(
        
        # Antarctica special case 
        union == "Antarctica" ~ "ATA",
        
        # 1) Union EEZ and country / Landlocked country  → iso_sov1
        pol_type %in% c("Union EEZ and country", "Landlocked country") ~ dplyr::case_when(
          
          # iso_ter1 missing or empty → use iso_sov1
          is.na(iso_ter1) | !nzchar(iso_ter1) ~ iso_sov1,
          
          # iso_sov1 equals iso_ter1 → use iso_sov1
          iso_sov1 == iso_ter1 ~ iso_sov1,
          
          # iso_sov1 different from iso_ter1 → concatenate
          TRUE ~ paste(iso_sov1, iso_ter1, sep = "_")
        ),
        
        # 2) joint regime (EEZ) → iso_sov1_iso_sov2
        pol_type == "Joint regime (EEZ)" ~ dplyr::case_when(
          
          # iso_ter1 missing or empty → use iso_sov1
          is.na(iso_ter1) | !nzchar(iso_ter1) ~ paste(iso_sov1, iso_sov2, sep = "_"),
          
          # iso_sov1 equals iso_ter1 → use iso_sov1
          iso_sov1 == iso_ter1 ~ paste(iso_sov1, iso_sov2, sep = "_"),
          
          # iso_sov1 different from iso_ter1 → concatenate
          TRUE ~ paste(iso_sov1, iso_sov2, iso_ter1, sep = "_")
        ),
        
        # 3) Overlapping claim
        #    if iso_sov3 not empty → iso_sov1_iso_sov2_iso_sov3
        pol_type == "Overlapping claim" & !is.na(iso_sov3) & nzchar(iso_sov3) ~ 
          paste(iso_sov1, iso_sov2, iso_sov3, sep = "_"),
        
        #    if iso_sov3 is empty/NA → fall back to iso_sov1_iso_sov2
        pol_type == "Overlapping claim" ~ dplyr::case_when(
          
          # iso_ter1 missing or empty → use iso_sov1
          is.na(iso_ter1) | !nzchar(iso_ter1) ~ paste(iso_sov1, iso_sov2, sep = "_"),
          
          # iso_sov1 equals iso_ter1 → use iso_sov1
          iso_sov1 == iso_ter1 ~ paste(iso_sov1, iso_sov2, sep = "_"),
          
          # iso_sov1 different from iso_ter1 → concatenate
          TRUE ~ paste(iso_sov1, iso_sov2, iso_ter1, sep = "_")
        ),
        
        # default: NA
        TRUE ~ NA_character_
      ),
      urn = dplyr::case_when(
        # 1) Union EEZ and country / Landlocked country → iso_sov1
        pol_type %in% c("Union EEZ and country", "Landlocked country") ~ paste0("urn:fdi:code:cwp:wja:nja:iso3:", tolower(code)),
        
        # 2) joint regime (EEZ) → iso_sov1_iso_sov2
        pol_type == "Joint regime (EEZ)" ~ paste0("urn:fdi:code:cwp:wja:jr:iso3:", tolower(code)),
        
        # 3) Overlapping claim
        #    if iso_sov3 not empty → iso_sov1_iso_sov2_iso_sov3
        pol_type == "Overlapping claim" ~ paste0("urn:fdi:code:cwp:wja:oc:iso3:", tolower(code)),
        
        # default: NA
        TRUE ~ NA_character_
      )
    )
  eez_land <- sf::st_make_valid(eez_land)
  
  #Dissolve polygons with same code
  geom_col <- attr(eez_land, "sf_column")
  eez_land_cleaned <- eez_land |>
    dplyr::mutate(.area = as.numeric(sf::st_area(.data[[geom_col]]))) |>
    dplyr::group_by(code) |>
    dplyr::summarise(
      # for all non-geometry columns: take the value from the row with the largest area in this group
      across(
        -all_of(c(geom_col, ".area")),
        ~ .x[which.max(.area)]
      ),
      # for geometry: union
      !!geom_col := sf::st_union(.data[[geom_col]]),
      .groups = "drop"
    )
  
  # Remove landlocked countries
  eez_land_filtered <- eez_land_cleaned %>%
    dplyr::filter(pol_type != "Landlocked country")
  
  #Union continents into a single polygon
  continent_union <- continent_lowres %>%
    st_make_valid() %>%
    st_union()
  
  #Choose a projected CRS for the operation
  crs_proj <- 3857  # or another global projected CRS
  
  eez_proj       <- st_transform(eez_land_filtered, crs_proj)
  continent_proj <- st_transform(continent_union,   crs_proj)
  
  #Difference in projected CRS – vectorized over all rows
  eez_diff_proj <- st_difference(eez_proj, continent_proj)
  
  #Drop completely empty geometries, if any
  #eez_diff_proj <- eez_diff_proj %>%
  #  filter(!st_is_empty(the_geom))
  
  #Transform back to original CRS (lon/lat)
  eez <- st_transform(eez_diff_proj, st_crs(eez_land_filtered))

  #Create wja_nja
  wja_nja_iso3 <- eez %>%
    dplyr::transmute(
      ID = paste0("wja:",.data$code),
      code = .data$code,
      urn = .data$urn,
      label = .data$union,   # from column "union"
      type = .data$pol_type # from column "pol_type"
    )
  
  #Country iso3 codes and label
  cl_iso3 <- readr::read_csv(
    "https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/cwp/cl_country_and_territory_iso3.csv",
    show_col_types = FALSE
  ) %>% 
    dplyr::select(code, label)
  
  #Combine and replace iso3 name if applicable
  wja_nja_iso3 <- wja_nja_iso3 %>%
  dplyr::left_join(cl_iso3, by = "code", suffix = c("", "_cl")) %>%
    dplyr::mutate(
      # If the codelist has a label, use it; otherwise keep the original
      label = dplyr::coalesce(label_cl, label)
    ) %>%
    dplyr::select(-label_cl) %>%
    
    # Remove rows where code is "NA" or real NA
    dplyr::filter(!is.na(code), code != "NA")
  
  
  wja_nja_iso3 <- wja_nja_iso3 %>%
    dplyr::mutate(type = ifelse(type == "Union EEZ and country",
                         "Jurisdiction Area",
                         type))
  
  wja_nja_iso3 <- wja_nja_iso3 %>%
    dplyr::mutate(type = ifelse(type == "Joint regime (EEZ)",
                         "Joint regime",
                         type))
  
  #create WJA_level1 adding ABNJ
  
  # Standardize wja_abnj to match wja_nja_iso3 structure
  wja_abnj2 <- wja_abnj %>%
    # keep only the needed columns + geometry
    dplyr::transmute(
      ID,
      code,
      urn,
      label,
      type,
      geometry = .data$geom  # move geom into a column called "geometry"
    ) %>%
    sf::st_as_sf()                    # rebuild as sf with 'geometry' as sf column
  
  # geometries are in 'the_geom', rename that column to 'geometry'
  if ("the_geom" %in% names(wja_nja_iso3)) {
    wja_nja_iso3 <- wja_nja_iso3 %>%
      dplyr::rename(geometry = the_geom)
  }
  
  #'geometry' as the active geometry column
  st_geometry(wja_nja_iso3) <- "geometry"
  
  # wja_abnj has the real geom in 'the_geom'
  if ("the_geom" %in% names(wja_abnj)) {
    wja_abnj2 <- wja_abnj %>%
      dplyr::select(ID, code, urn, label, type, geom) %>%
      sf::st_as_sf()
  } else {
    wja_abnj2 <- wja_abnj %>%
      dplyr::select(ID, code, urn, label, type, geom) %>%
      sf::st_as_sf()
  }
  
  # 'geometry' as the geometry column
  sf::st_geometry(wja_abnj2) <- "geometry"
  
  
  #Combine into wja_level1
  wja_level1 <- dplyr::bind_rows(wja_nja_iso3, wja_abnj2)
  
  
  #SAVE GPKG
  wja_layer = get(entity$data$layername)
  wja_layer_path <- file.path("data", paste0(entity$data$layername, ".gpkg"))
  sf::st_write(wja_layer, wja_layer_path, delete_dsn = TRUE)
  
  
  #####
  #### 5) AUTO REGISTER (unchanged, with extended labels) ####
  data_files_register = NULL
  data_files <- list.files("data", pattern = "\\.gpkg$", full.names = FALSE)
  if (length(data_files) > 0) {
    
    register_df <- do.call(rbind, lapply(data_files, function(x) {
      code <- tools::file_path_sans_ext(x)
      uri  <- NA
      label <- entity$titles$title
      definition <- label
      
      data.frame(
        code = code,
        uri = uri,
        label = label,
        definition = definition,
        stringsAsFactors = FALSE
      )
    }))
    
    readr::write_csv(register_df, file.path("data", "register.csv"))
    data_files_register <- readr::read_csv(file.path("data", "register.csv"))
  }
  
  #TODO geoflow https://github.com/r-geoflow/geoflow/issues/421
  
  data_files <- list.files("data", full.names = TRUE)
  ext_data_files <- data_files[basename(data_files) != "register.csv"]
  
  #Keep ONLY intersection layers as geoflow_data children
  ext_data_files <- ext_data_files[grepl("\\.gpkg$", basename(ext_data_files))]
  
  entity$data$data <- lapply(ext_data_files, function(data_file){
    ext_data <- entity$data$clone(deep = TRUE)
    ext_data$dir <- NULL
    ext_data_src <- basename(data_file)
    ext_data_name <- unlist(strsplit(ext_data_src, "\\."))[1]
    ext_data_extension <- unlist(strsplit(ext_data_src, "\\."))[2]
    attr(ext_data_src, "uri") <- data_file
    ext_data$addSource(ext_data_src)
    uploadSource <- paste0(ext_data_name, ".", ext_data_extension)
    ext_data$setUploadSource(uploadSource)
    
    sourceType <- entity$data$sourceType
    if(is.null(entity$data$sourceType) || entity$data$sourceType == "other"){
      sourceType <- switch(ext_data_extension,
                           "shp" = "shp",
                           "gpkg" = "gpkg",
                           "tif" = "geotiff",
                           "csv" = "csv",
                           "parquet" = "parquet",
                           "other"
      )
    }
    if(!is.null(sourceType)){
      ext_data$setSourceType(sourceType)
    }
    if((is.null(entity$data$uploadType) || entity$data$uploadType == "other") && !is.null(sourceType)){
      if(sourceType != "zip") ext_data$setUploadType(sourceType)
      if(!is.null(ext_data$uploadType)) if(ext_data$uploadType == "geotiff") ext_data$setSpatialRepresentationType("grid")
    }
    
    hasStoreDeclared <- FALSE
    if(!is.null(config$software$output$geoserver_config$properties$store)) hasStoreDeclared <- TRUE
    if(!is.null(entity$data$store)) hasStoreDeclared <- TRUE
    if(!hasStoreDeclared) ext_data$setStore(ext_data_name)
    ext_data$setLayername(ext_data_name)
    
    # inherit layer metadata from data file register (if any)
    if(!is.null(data_files_register)){
      register_entry <- data_files_register[data_files_register$code == ext_data_name,]
      if(nrow(register_entry) > 0){
        register_entry <- register_entry[1L,]
        if(!is.na(register_entry$uri))        ext_data$setLayeruri(register_entry$uri)
        if(!is.na(register_entry$label))      ext_data$setLayertitle(register_entry$label)
        if(!is.na(register_entry$definition)) ext_data$setLayerdesc(register_entry$definition)
      }
    }
    
    return(ext_data)
  })
  
  entity$data$dir <- file.path(getwd(), "data")
  
  entity$enrichWithRelations(config)
  
  #entity$enrichWithData(config) #doesn't work to inherit features??? to investigate
  entity$data$data[[1]]$setFeatures(wja_layer)
  
  entity$setSpatialExtent(data = wja_layer)
  entity$setSpatialBbox(data = wja_layer)
  entity$setGeographicBbox()
  
  
}