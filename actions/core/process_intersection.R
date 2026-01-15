function(action, entity, config){
  
  #option
  crop1 = if(!is.null(action$getOption("crop1"))) action$getOption("crop1") else FALSE
  crop2 = if(!is.null(action$getOption("crop2"))) action$getOption("crop2") else FALSE
  
  #THIS WORKS INSTEAD OF LINES 10-15
  WFS <- config$software$input$wfs
  
  # Connect to WFS FAO Geoserver
  #WFS <- ows4R::WFSClient$new(
  #  url = "https://www.fao.org/fishery/geoserver/wfs",
  #  serviceVersion = "1.0.0",
  #  logger = "INFO"
  #)
  
  # Low-res continents layer
  continent_lowres <- WFS$getFeatures("fifao:UN_CONTINENT2") #TODO to move to a R fdi data package
  sf::st_crs(continent_lowres) = 4326
  continent_lowres <- sf::st_make_valid(continent_lowres)
  
  #tRFMO code to crop layers into tRFMO area of competence
  #trfmo = NA
  #trfmos = entity$contacts[sapply(entity$contacts, function(x){x$role == "owner"})]
  #if(length(owners)>0) trfmo = trfmos[[1]]
  
  #Other servers
  server1 = NA
  server2 = NA
  servers = entity$relations[sapply(entity$relations, function(x){x$key == "http"})]
  if(length(servers)>0){
    server1 = servers[[1]]$link
    server2 = servers[[2]]$link
  }
  #server1 = entity$subjects$server[[1]] #https://www.fao.org/fishery/geoserver/wfs
  #server1 = "https://www.fao.org/fishery/geoserver/wfs"
  #server2 = entity$subjects$server[[2]] #https://geo.vliz.be/geoserver/MarineRegions/wfs
  #server2 = "https://geo.vliz.be/geoserver/MarineRegions/wfs"
  
  # Connect to WFS1 server
  WFS1 <- ows4R::WFSClient$new(
    url = server1,
    serviceVersion = "1.0.0",
    logger = "INFO"
  )
  
  # Connect to WFS2 server
  WFS2 <- ows4R::WFSClient$new(
    url = server2,
    serviceVersion = "1.0.0",
    logger = "INFO"
  )
  
  #layer1
  layer1 <- WFS1$getFeatures(entity$data$source[[1]])
  sf::st_crs(layer1) = 4326
  layer1$code1 = layer1[[entity$data$sourceFid[[1]]]]
  #dissolve per code
  geom_col <- attr(layer1, "sf_column")
  layer1 <- layer1 %>%
    dplyr::group_by(code1) %>%
    dplyr::summarise(
      dplyr::across(-dplyr::all_of(geom_col), dplyr::first),
      .groups = "drop"
    )
  layer1 <- sf::st_make_valid(layer1)
  
  #layer2
  layer2 <- WFS2$getFeatures(entity$data$source[[2]])
  sf::st_crs(layer2) = 4326
  layer2$code2 = layer2[[entity$data$sourceFid[[2]]]]
  #dissolve per code
  geom_col <- attr(layer2, "sf_column")
  layer2 <- layer2 %>%
    dplyr::group_by(code2) %>%
    dplyr::summarise(
      dplyr::across(-dplyr::all_of(geom_col), dplyr::first),
      .groups = "drop"
    )
  layer2 <- sf::st_make_valid(layer2)
  
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
  #mask_lowres  <- sf::st_union(sf::st_geometry(continent_lowres))
  mask_lowres <- continent_lowres
  
  # Use an equal-area CRS for area calculations
  area_crs <- "+proj=eck4"
  
  #### 1) PROCESS LAYER1 ####
  layer1_lowres = NULL
  #if(crop1){
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
  #}else{
  #  layer1$surface1 = as.numeric(sf::st_area( sf::st_transform(layer1,  area_crs) )) #if crop1 = FALSE
  #}
  
    
  #### 2) PROCESS LAYER2 ####
  layer2_lowres = NULL
  #if(crop2){
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
  #}else{
  #  layer2$surface2 = as.numeric(sf::st_area( sf::st_transform(layer2,  area_crs) )) #if crop1 = FALSE
  #}
  
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
  layer1_lowres_to_intersect = st_make_valid(layer1_lowres_to_intersect[!sf::st_is_empty(layer1_lowres_to_intersect),])
  layer2_lowres_to_intersect = st_make_valid(layer2_lowres_to_intersect[!sf::st_is_empty(layer2_lowres_to_intersect),])

  #### 4) INTERSECTIONS MAIN LAYERS x GRID5 ####
  
  # LOWRES intersection: main attrs + CWP_CODE + GRIDTYPE + coverage %
  layer_lowres_int <- sf::st_intersection(layer1_lowres_to_intersect, layer2_lowres_to_intersect)
       
      #SOLVE ISSUE WITH GEOMETRY COLLECTIONS
        
        layer_lowres_int <- sf::st_make_valid(layer_lowres_int)
        
        # Add stable row id
        layer_lowres_int$..row_id <- seq_len(nrow(layer_lowres_int))
        
        #Extract only polygonal pieces (POLYGON or MULTIPOLYGON)
        #    - GEOMETRYCOLLECTION → polygon parts extracted
        #    - LINESTRING/POINT → dropped
        poly_parts <- sf::st_collection_extract(layer_lowres_int, "POLYGON")
        
        # Dissolve polygon pieces per original row
        poly_diss <- poly_parts %>%
          dplyr::group_by(..row_id) %>%
          dplyr::summarise(.groups = "drop")
        
        #Recover the original attribute columns (except geometry)
        attrs <- sf::st_drop_geometry(layer_lowres_int)
        
        #Restore attributes to dissolved polygons
        layer_lowres_int_poly <- poly_diss %>%
          dplyr::left_join(attrs, by = "..row_id") %>%
          dplyr::select(-..row_id)
        
        #Final result
        layer_lowres_int <- layer_lowres_int_poly
  
  
  # Compute intersection area in equal-area CRS
  layer_lowres_int$surface <- as.numeric(sf::st_area( sf::st_transform(layer_lowres_int, area_crs) ))
  # % of surface
  layer_lowres_int$surface1_percent <- ifelse(
    !is.na(layer_lowres_int$surface1),
    100 * layer_lowres_int$surface / layer_lowres_int$surface1,
    NA_real_
  )
  layer_lowres_int$surface2_percent <- ifelse(
    !is.na(layer_lowres_int$surface2),
    100 * layer_lowres_int$surface / layer_lowres_int$surface2,
    NA_real_
  )
  #layer_lowres_int = layer_lowres_int[sf::st_geometry_type(layer_lowres_int) %in% c("POLYGON", "MULTIPOLYGON"),]
  #layer_lowres_int <- sf::st_collection_extract(layer_lowres_int, "POLYGON")
  
  layer_lowres_int = layer_lowres_int[,c("layer1","code1","surface1","layer2","code2","surface2","surface","surface1_percent","surface2_percent")]
  
  # Remove possible duplicates
  #key_cols <- c(
  #  "layer1", "code1", "surface1",
  #  "layer2", "code2", "surface2",
  #  "surface", "surface1_percent", "surface2_percent"
  #)
  #layer_lowres_int <- layer_lowres_int[
  #  !duplicated(sf::st_drop_geometry(layer_lowres_int)[key_cols]),
  #]
  
  
  #### 4) SAVE GPKGs ####
  layer_lowres_int_path    <- file.path("data", paste0(entity$data$layername, ".gpkg"))
  sf::st_write(layer_lowres_int, layer_lowres_int_path, delete_dsn = TRUE)
  
  #### BOUNDING BOX UPDATE
  
  
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
  #ext_data_files <- ext_data_files[grepl("_intersection\\.gpkg$", basename(ext_data_files))]
  ext_data_files <- ext_data_files[basename(ext_data_files) == paste0(entity$data$layername, ".gpkg")]
  
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
  
  
  #entity$enrichWithData(config) #doesn't work to inherit features??? to investigate
  entity$data$data[[1]]$setFeatures(layer_lowres_int)
  
  entity$setSpatialExtent(data = layer_lowres_int)
  entity$setSpatialBbox(data = layer_lowres_int)
  entity$setGeographicBbox()
  
  entity$enrichWithRelations(config)
  
}
