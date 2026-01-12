function(action, entity, config){
  
  #option
  crop1 = if(!is.null(action$getOption("crop1"))) action$getOption("crop1") else FALSE
  crop2 = if(!is.null(action$getOption("crop2"))) action$getOption("crop2") else FALSE
  
  
  # Connect to WFS FAO Geoserver
  WFS <- ows4R::WFSClient$new(
    url = "https://www.fao.org/fishery/geoserver/wfs",
    serviceVersion = "1.0.0",
    logger = "INFO"
  )
  
  #layer1
  layer1 <- WFS$getFeatures(entity$data$source[[1]])
  sf::st_crs(layer1) = 4326
  
  #layer2
  layer2 <- WFS$getFeatures(entity$data$source[[2]])
  sf::st_crs(layer2) = 4326
  
  # Low-res continents layer
  continent_lowres <- WFS$getFeatures("fifao:UN_CONTINENT2") #TODO to move to a R fdi data package
  sf::st_crs(continent_lowres) = 4326
  continent_lowres <- sf::st_make_valid(continent_lowres)
  
  #TODO refactor intersection logic into a util function in fdi4R!!! https://github.com/fdiwg/fdi4R/issues/8
  
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
  
  # Use an equal-area CRS for area calculations
  area_crs <- "+proj=eck4"
  
  #### 1) PROCESS LAYER1 ####
  layer1_lowres = NULL
  if(crop1){
    config$logger$INFO("Croping layer 1 with low-res continent layer")
    layer1_lowres_sfc <- sf::st_sfc(
      lapply(seq_len(nrow(layer1)), function(i){
        safe_difference(sf::st_geometry(layer1[i, ]), mask_lowres)
      }),
      crs = 4326
    )
    layer1_lowres <- sf::st_set_geometry(layer1, layer1_lowres_sfc)
    sf::st_crs(layer1_lowres)  <- 4326
    layer1_lowres    <- sf::st_make_valid(layer1_lowres)
    layer1_lowres$surface1  <- as.numeric(sf::st_area( sf::st_transform(layer1_lowres,  area_crs) ))
    layer1$surface1 = layer1_lowres$surface1 #recover accurate surface1 in non-cut layer
  }else{
    layer1$surface1 = as.numeric(sf::st_area( sf::st_transform(layer1,  area_crs) ))
  }
  
    
  #### 2) PROCESS LAYER2 ####
  layer2_lowres = NULL
  if(crop2){
    config$logger$INFO("Croping layer 2 with low-res continent layer")
    layer2_lowres_sfc <- sf::st_sfc(
      lapply(seq_len(nrow(layer2)), function(i){
        safe_difference(sf::st_geometry(layer2[i, ]), mask_lowres)
      }),
      crs = 4326
    )
    layer2_lowres <- sf::st_set_geometry(layer2, layer2_lowres_sfc)
    sf::st_crs(layer2_lowres) <- 4326
    layer2_lowres    <- sf::st_make_valid(layer2_lowres)
    layer2_lowres$surface2  <- as.numeric(sf::st_area( sf::st_transform(layer2_lowres,  area_crs) ))
    layer2$surface2 = layer2_lowres$surface2 #recover accurate surface2 in non-cut layer
  }else{
    layer2$surface2 = as.numeric(sf::st_area( sf::st_transform(layer2,  area_crs) ))
  }
  
  #### 3) INTERSECTIONS MAIN LAYERS x GRID5 (lowres / highres) ####
  
  layer1_lowres_to_intersect = if(crop1) layer1_lowres else layer1 
  layer2_lowres_to_intersect = if(crop2) layer2_lowres else layer2
  if(crop1 & crop2){
    layer1_lowres_to_intersect = layer1
  }
  # Keep only needed attributes + IDs
  layer1_lowres_to_intersect  <- layer1_lowres_to_intersect[, c(entity$data$sourceFid[[1]], "surface1")]
  layer1_lowres_to_intersect$layer1 = entity$data$source[[1]]
  layer1_lowres_to_intersect$code1 = layer1_lowres_to_intersect[[entity$data$sourceFid[[1]]]]
  layer1_lowres_to_intersect = layer1_lowres_to_intersect[,c("layer1", "code1", "surface1")]
  layer2_lowres_to_intersect  <- layer2_lowres_to_intersect[, c(entity$data$sourceFid[[2]], "surface2")]
  layer2_lowres_to_intersect$layer2 = entity$data$source[[2]]
  layer2_lowres_to_intersect$code2 = layer2_lowres_to_intersect[[entity$data$sourceFid[[2]]]]
  layer2_lowres_to_intersect = layer2_lowres_to_intersect[,c("layer2", "code2", "surface2")]
  
  #exclude empty geometries
  layer1_lowres_to_intersect = layer1_lowres_to_intersect[!sf::st_is_empty(layer1_lowres_to_intersect),]
  layer2_lowres_to_intersect = layer2_lowres_to_intersect[!sf::st_is_empty(layer2_lowres_to_intersect),]

  #### 4) INTERSECTIONS MAIN LAYERS x GRID5 ####
  layer1_lowres_to_intersect = sf::st_make_valid(layer1_lowres_to_intersect)
  layer2_lowres_to_intersect = sf::st_make_valid(layer2_lowres_to_intersect)
  
  # LOWRES intersection: main attrs + CWP_CODE + GRIDTYPE + coverage %
  layer_lowres_int <- sf::st_intersection(layer1_lowres_to_intersect, layer2_lowres_to_intersect)
  # Compute intersection area in equal-area CRS
  layer_lowres_int$surface <- as.numeric(sf::st_area( sf::st_transform(layer_lowres_int, area_crs) ))
  # % of surface
  layer_lowres_int$surface1_percent <- ifelse(
    !is.na(layer_lowres_int$surface1),
    round(100 * layer_lowres_int$surface / layer_lowres_int$surface1, 2),
    NA_real_
  )
  layer_lowres_int$surface2_percent <- ifelse(
    !is.na(layer_lowres_int$surface2),
    round(100 * layer_lowres_int$surface / layer_lowres_int$surface2, 2),
    NA_real_
  )
  layer_lowres_int = layer_lowres_int[sf::st_geometry_type(layer_lowres_int) %in% c("POLYGON", "MULTIPOLYGON"),]
  
  layer_lowres_int = layer_lowres_int[,c("layer1","code1","surface1","layer2","code2","surface2","surface","surface1_percent","surface2_percent")]
  
  #### 4) SAVE GPKGs ####
  layer_lowres_int_path    <- file.path("data", paste0(entity$data$layername, ".gpkg"))
  sf::st_write(layer_lowres_int, layer_lowres_int_path, delete_dsn = TRUE)
  
  #### 5) AUTO REGISTER (unchanged, with extended labels) ####
  data_files_register = NULL
  data_files <- list.files("data", pattern = entity$data$layername, full.names = FALSE)
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
  ext_data_files <- ext_data_files[grepl(entity$data$layername, basename(ext_data_files))]
  
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
  entity$data$data[[1]]$setFeatures(layer_lowres_int)
  
  entity$setSpatialExtent(data = layer_lowres_int)
  entity$setSpatialBbox(data = layer_lowres_int)
  entity$setGeographicBbox()
  
}
