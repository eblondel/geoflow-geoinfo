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
  
  # WJA_level0
  
  #From VLIZ layer to WJA_NJAs dissolved
  eez_land_buffer = sf::st_union(sf::st_make_valid(eez_land))
  #eez_land_buffer_multiparts = sf::st_cast(eez_land_buffer, "POLYGON")
  wja_nja_land <- smoothr::fill_holes(eez_land_buffer, threshold = units::set_units(10000000000, "m^2"))
  wja_nja <- st_difference(wja_nja_land, continent_lowres)
  wja_nja <- st_as_sf(data.frame(
                          ID = "wja:nja",
                          code = "nja",
                          urn = "urn:fdi:code:cwp:wja:nja",
                          label = "National Jurisdiction Area",
                          type = "National Jurisdiction Area",
                          geomtry = wja_nja))
  
  #Create WJA_ABNJ area
  pts = matrix(c(-180,-90,-180,90,180,90,180,-90,-180,-90),ncol=2, byrow=TRUE)
  poly = sf::st_sf(geom = sf::st_sfc(sf::st_polygon(list(pts)), crs = 4326))
  hs = sf::st_difference(poly, eez_land_buffer)
  hs = sf::st_cast(hs, "POLYGON")
  wja_abnj = hs[sf::st_area(hs)>units::as_units(10000000000,"m2"),]
  wja_abnj = sf::st_union(sf::st_make_valid(wja_abnj))
  wja_abnj <- st_as_sf(data.frame(
                          ID = "wja:abnj",
                          code = "abnj",
                          urn = "urn:fdi:code:cwp:wja:abnj",
                          label = "Areas Beyond National Jurisdiction (High Seas)",
                          type = "Area Beyond National Jurisdiction",
                          geometry = wja_abnj))
  
  #Combine into wja_level0
  wja_level0 <- dplyr::bind_rows(wja_nja, wja_abnj)
  
  
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