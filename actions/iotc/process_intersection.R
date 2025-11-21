function(action, entity, config){
  
  # process iotc_stock_assessment_sa_master.gpkg
  #layer_master <- entity$data$features
  #layer_master_path <- entity$data$source[[1]] #source 1 is the local path of IOTC layer master
  #layer_master <- read.csv(layer_master_path)
  
  
  
  # Connect to WFS FAO Geoserver
  WFS <- ows4R::WFSClient$new(
    url = "https://www.fao.org/fishery/geoserver/wfs",
    serviceVersion = "1.0.0",
    logger = "INFO"
  )
  
  #layer_master could be retrieved from WFS also! entity$data$source[[1]] would be "iotc_assessment_sa_master"
  layer_master <- WFS$getFeatures(entity$data$source[[1]])
  #layer_master <- WFS$getFeatures(entity$data$source[[1]])
  
  # Grid 5deg x 5deg / ideally we will have a "cwp_grid_map" with all Grids
  #grid <- entity$data$source[[2]] #source[[2]] would be "cwp-grid-map-5deg_x_5deg"
  grid5 <- WFS$getFeatures(entity$data$source[[2]])
  sf::st_crs(grid5) <- 4326
  
  # Low-res continents layer /could be from a data[[3]]!!
  continent_lowres <- WFS$getFeatures("fifao:UN_CONTINENT2")
  sf::st_crs(continent_lowres) <- 4326
    
  # # High-res continents layer
  # continent_highres <- WFS$getFeatures("fifao:UN_CONTINENT2_new")
  # continent_highres <- sf::st_make_valid(continent_highres)
  # sf::st_crs(continent_highres) <- 4326
  
  # helper: safe difference that never breaks on empty result
  safe_difference <- function(geom, mask){
    diff <- try(sf::st_difference(geom, mask), silent = TRUE)
    if (inherits(diff, "try-error") || length(diff) == 0) {
      return(sf::st_geometrycollection())
    }
    g <- diff[[1]]
    if (sf::st_is_empty(g)) {
      return(sf::st_geometrycollection())
    }
    g
  }
  
  # union masks (much more efficient and stable)
  mask_lowres  <- sf::st_union(sf::st_geometry(continent_lowres))
  # mask_highres <- sf::st_union(sf::st_geometry(continent_highres))
  
  #### 1) CUT MAIN LAYER BY COASTLINES ####
  
  layer_lowres_sfc <- sf::st_sfc(
    lapply(seq_len(nrow(layer_master)), function(i){
      safe_difference(sf::st_geometry(layer_master[i, ]), mask_lowres)
    }),
    crs = 4326
  )
  layer_lowres <- sf::st_set_geometry(layer_master, layer_lowres_sfc)
  
  # layer_highres_sfc <- sf::st_sfc(
  #   lapply(seq_len(nrow(layer_master)), function(i){
  #     safe_difference(sf::st_geometry(layer_master[i, ]), mask_highres)
  #   }),
  #   crs = 4326
  # )
  # layer_highres <- sf::st_set_geometry(layer_master, layer_highres_sfc)
  
  sf::st_crs(layer_lowres)  <- 4326
  # sf::st_crs(layer_highres) <- 4326
  
  #### 2) CUT GRID5 BY COASTLINES (lowres / highres) ####
  
  grid5 <- sf::st_make_valid(grid5)
  
  grid5_lowres_sfc <- sf::st_sfc(
    lapply(seq_len(nrow(grid5)), function(i){
      safe_difference(sf::st_geometry(grid5[i, ]), mask_lowres)
    }),
    crs = 4326
  )
  
  grid5_lowres <- sf::st_set_geometry(grid5, grid5_lowres_sfc)
  sf::st_crs(grid5_lowres) <- 4326
  
  # grid5_highres_sfc <- sf::st_sfc(
  #   lapply(seq_len(nrow(grid5)), function(i){
  #     safe_difference(sf::st_geometry(grid5[i, ]), mask_highres)
  #   }),
  #   crs = 4326
  # )
  # grid5_highres <- sf::st_set_geometry(grid5, grid5_highres_sfc)
  # sf::st_crs(grid5_highres) <- 4326
  
  #### 3) INTERSECTIONS MAIN LAYERS x GRID5 (lowres / highres) ####
  
  # Make all layers valid before intersection
  layer_lowres    <- sf::st_make_valid(layer_lowres)
  # layer_highres   <- sf::st_make_valid(layer_highres)
  grid5_lowres    <- sf::st_make_valid(grid5_lowres)
  # grid5_highres   <- sf::st_make_valid(grid5_highres)
  
  # Add IDs to main layers (optional but useful)
  layer_lowres$main_id  <- seq_len(nrow(layer_lowres))
  # layer_highres$main_id <- seq_len(nrow(layer_highres))
  
  # Keep only needed grid attributes + IDs
  grid5_lowres_sub  <- grid5_lowres[, c("CWP_CODE", "GRIDTYPE")]
  # grid5_highres_sub <- grid5_highres[, c("CWP_CODE", "GRIDTYPE")]
  
  grid5_lowres_sub$grid_id  <- seq_len(nrow(grid5_lowres_sub))
  # grid5_highres_sub$grid_id <- seq_len(nrow(grid5_highres_sub))
  
  # Use an equal-area CRS for area calculations (ESRI:54009 / EPSG:6933 are ok; using 6933 here)
  area_crs <- 6933
  
  grid5_lowres_area  <- sf::st_transform(grid5_lowres_sub,  area_crs)
  # grid5_highres_area <- sf::st_transform(grid5_highres_sub, area_crs)
  
  grid5_lowres_sub$grid_area  <- as.numeric(sf::st_area(grid5_lowres_area))
  # grid5_highres_sub$grid_area <- as.numeric(sf::st_area(grid5_highres_area))
  
  #### 4) INTERSECTIONS MAIN LAYERS x GRID5 ####
  
  # LOWRES intersection: main attrs + CWP_CODE + GRIDTYPE + coverage %
  layer_lowres_grid5_int <- sf::st_intersection(layer_lowres, grid5_lowres_sub)
  # Compute intersection area in equal-area CRS
  ll_int_area <- sf::st_transform(layer_lowres_grid5_int, area_crs)
  layer_lowres_grid5_int$int_area <- as.numeric(sf::st_area(ll_int_area))
  # Bring grid_area
  layer_lowres_grid5_int$grid_area <- grid5_lowres_sub$grid_area[
    match(layer_lowres_grid5_int$grid_id, grid5_lowres_sub$grid_id)
  ]
  # % of grid5 polygon covered by main polygon
  layer_lowres_grid5_int$perc_grid5_covered <- ifelse(
    !is.na(layer_lowres_grid5_int$grid_area) & layer_lowres_grid5_int$grid_area > 0,
    100 * layer_lowres_grid5_int$int_area / layer_lowres_grid5_int$grid_area,
    NA_real_
  )
  
  # # HIGHRES intersection: main attrs + CWP_CODE + GRIDTYPE + coverage %
  # layer_highres_grid5_int <- sf::st_intersection(layer_highres, grid5_highres_sub)
  # # Compute intersection area in equal-area CRS
  # lh_int_area <- sf::st_transform(layer_highres_grid5_int, area_crs)
  # layer_highres_grid5_int$int_area <- as.numeric(sf::st_area(lh_int_area))
  # # Bring grid_area
  # layer_highres_grid5_int$grid_area <- grid5_highres_sub$grid_area[
  #   match(layer_highres_grid5_int$grid_id, grid5_highres_sub$grid_id)
  # ]
  # # % of grid5 polygon covered by main polygon
  # layer_highres_grid5_int$perc_grid5_covered <- ifelse(
  #   !is.na(layer_highres_grid5_int$grid_area) & layer_highres_grid5_int$grid_area > 0,
  #   100 * layer_highres_grid5_int$int_area / layer_highres_grid5_int$grid_area,
  #   NA_real_
  # )
  
  #### 4) SAVE GPKGs ####
  
  if(!dir.exists("data")) dir.create("data", recursive = TRUE)
  
  id <- entity$identifiers$id
  
  #lowres_path              <- file.path("data", paste0(id, "_lowres.gpkg"))
  #highres_path             <- file.path("data", paste0(id, "_highres.gpkg"))
  #grid5_lowres_path        <- file.path("data", paste0(id, "_grid5_lowres.gpkg"))
  #grid5_highres_path       <- file.path("data", paste0(id, "_grid5_highres.gpkg"))
  lowres_grid5_int_path    <- file.path("data", paste0(id, "_lowres_grid5_intersection.gpkg"))
  #highres_grid5_int_path   <- file.path("data", paste0(id, "_highres_grid5_intersection.gpkg"))
  
  #sf::st_write(layer_lowres,           lowres_path,           delete_dsn = TRUE)
  #sf::st_write(layer_highres,          highres_path,          delete_dsn = TRUE)
  #sf::st_write(grid5_lowres,           grid5_lowres_path,     delete_dsn = TRUE)
  #sf::st_write(grid5_highres,          grid5_highres_path,    delete_dsn = TRUE)
  sf::st_write(layer_lowres_grid5_int, lowres_grid5_int_path, delete_dsn = TRUE)
  # sf::st_write(layer_highres_grid5_int,highres_grid5_int_path,delete_dsn = TRUE)
  
  #### 5) AUTO REGISTER (unchanged, with extended labels) ####
  
  data_files <- list.files("data", pattern = "\\_intersection.gpkg$", full.names = FALSE)
  if (length(data_files) > 0) {
    
    register_df <- do.call(rbind, lapply(data_files, function(x) {
      code <- tools::file_path_sans_ext(x)
      uri  <- NA
      label <- code
      definition <- label
      
      if (grepl("master", x, ignore.case = TRUE)) {
        label <- "IOTC Stock assessment areas - Master layer (not cut with coastline)"
      } else if (grepl("lowres_grid5_intersection", x, ignore.case = TRUE)) {
        label <- "IOTC Stock assessment areas - Low-res coastline / 5x5 CWP grid intersection"
      # } else if (grepl("highres_grid5_intersection", x, ignore.case = TRUE)) {
      #   label <- "IOTC Stock assessment areas - High-res coastline / 5x5 CWP grid intersection"
      #} else if (grepl("grid5_lowres", x, ignore.case = TRUE)) {
      #  label <- "5x5 CWP grid - Cut with low-res coastline"
      #} else if (grepl("grid5_highres", x, ignore.case = TRUE)) {
      #  label <- "5x5 CWP grid - Cut with high-res coastline"
      #} else if (grepl("lowres", x, ignore.case = TRUE)) {
      #  label <- "IOTC Stock assessment areas - Layer cut with low-res coastline"
      #} else if (grepl("highres", x, ignore.case = TRUE)) {
      #  label <- "IOTC Stock assessment areas - Layer cut with high-res coastline"
      }
      
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
  }
  
  data_files_register <- readr::read_csv(file.path("data", "register.csv"))
  
  data_files <- list.files("data", full.names = TRUE)
  ext_data_files <- data_files[basename(data_files) != "register.csv"]
  
  #Keep ONLY intersection layers as geoflow_data children
  ext_data_files <- ext_data_files[grepl("_intersection\\.gpkg$", basename(ext_data_files))]
  
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
}
