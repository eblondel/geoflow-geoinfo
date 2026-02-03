function(action, entity, config){
  
  #process iotc_stock_assessment_sa_master.gpkg
  layer_master = entity$data$features
  sf::st_crs(layer_master) = 4326
  
  #Connect to WFS FAO Geoserver
  WFS = ows4R::WFSClient$new(
    url = "https://www.fao.org/fishery/geoserver/wfs",
    serviceVersion = "1.0.0",
    logger = "INFO"
  )
  
  #Low-res continents layer
  continent_lowres = WFS$getFeatures("fifao:UN_CONTINENT2")
  continent_lowres = sf::st_make_valid(continent_lowres)
  sf::st_crs(continent_lowres) = 4326
  
  #High-res continents layer
  continent_highres = WFS$getFeatures("fifao:UN_CONTINENT2_new")
  continent_highres = sf::st_make_valid(continent_highres)
  sf::st_crs(continent_highres) = 4326
  
  #Cut by lowres coastline
  layer_lowres_sfc = do.call(
    sf::st_sfc, 
    lapply(seq_len(nrow(layer_master)), function(i) {
      (sf::st_difference(sf::st_geometry(layer_master[i, ]), continent_lowres))[[1]]
    })
  )
    layer_lowres = sf::st_set_geometry(layer_master, layer_lowres_sfc)
  
  #Cut by highres coastline
  layer_highres_sfc = do.call(
    sf::st_sfc, 
    lapply(seq_len(nrow(layer_master)), function(i) {
    (sf::st_difference(sf::st_geometry(layer_master[i, ]), continent_highres))[[1]]
    })
  )
  layer_highres = sf::st_set_geometry(layer_master, layer_highres_sfc)
  
  sf::st_crs(layer_lowres)  = 4326
  sf::st_crs(layer_highres) = 4326
  
  #Save GPKGs based on entity ID in data folder
  id <- entity$identifiers$id
  lowres_path  <- file.path("data", paste0(id, "_lowres.gpkg"))
  highres_path <- file.path("data", paste0(id, "_highres.gpkg"))
  sf::st_write(layer_lowres, lowres_path)
  sf::st_write(layer_highres, highres_path)
  
  #Automatically create register.csv
  data_files <- list.files("data", pattern = "\\.gpkg$", full.names = FALSE)
  if (length(data_files) > 0) {
    
    register_df <- do.call(rbind, lapply(data_files, function(x) {
      code <- tools::file_path_sans_ext(x)  # remove .gpkg
      uri  <- NA
      
      # label / definition based on filename
      label <- code
      definition <- label
      if (grepl("master", x, ignore.case = TRUE)) {
        label = paste0(entity$titles$title ," - Master layer (not cut with coastline)")
        definition <- label
      } else if (grepl("lowres", x, ignore.case = TRUE)) {
        label = paste0(entity$titles$title ," - Layer cut with low-res coastline")
        definition <- label
      } else if (grepl("highres", x, ignore.case = TRUE)) {
        label = paste0(entity$titles$title , " - Layer cut with high-res coastline")
        definition <- label
      }
      
      data.frame(
        code = code,
        uri = uri,
        label = label,
        definition = definition,
        stringsAsFactors = FALSE
      )
    }))
    
    # write it to ./data/register.csv
    readr::write_csv(register_df, file.path("data", "register.csv"))
  }
  data_files_register = readr::read_csv(file.path("data", "register.csv"))
  
  data_files = list.files("data", full.names = TRUE)
  ext_data_files = data_files[basename(data_files) != "register.csv"]
  entity$data$data <- lapply(ext_data_files, function(data_file){
    ext_data <- entity$data$clone(deep = TRUE) #clone parent geoflow_data to inherit all needed properties
    ext_data$dir <- NULL
    ext_data_src <- basename(data_file)
    ext_data_name <- unlist(strsplit(ext_data_src, "\\."))[1]
    ext_data_extension <- unlist(strsplit(ext_data_src, "\\."))[2]
    attr(ext_data_src, "uri") <- data_file
    ext_data$addSource(ext_data_src)
    uploadSource = paste0(ext_data_name, ".", ext_data_extension)
    ext_data$setUploadSource(uploadSource)
    sourceType <- entity$data$sourceType
    if(is.null(entity$data$sourceType) || entity$data$sourceType == "other"){
      sourceType <- switch(ext_data_extension,
                           "shp" = "shp",
                           "gpkg" = "gpkg",
                           "tif" = "geotiff",
                           "csv" = "csv",
                           "parquet" = "parquet",
                           "other" #including zip that will be resolved later when entity is enriched with data
      )
    }
    if(!is.null(sourceType)){
      ext_data$setSourceType(sourceType)
    }
    if((is.null(entity$data$uploadType) || entity$data$uploadType == "other") && !is.null(sourceType)){
      print(sourceType)
      if(sourceType != "zip") ext_data$setUploadType(sourceType)
      if(!is.null(ext_data$uploadType)) if(ext_data$uploadType == "geotiff") ext_data$setSpatialRepresentationType("grid")
    }

    hasStoreDeclared <- FALSE
    if(!is.null(config$software$output$geoserver_config$properties$store)) hasStoreDeclared <- TRUE
    if(!is.null(entity$data$store)) hasStoreDeclared <- TRUE
    if(!hasStoreDeclared) ext_data$setStore(ext_data_name)
    ext_data$setLayername(ext_data_name)
    
    #accessors <- list_data_accessors(raw = TRUE)
    #accessor <- accessors[sapply(accessors, function(x){x$id == entity$data$access})][[1]]
    
    #inherit layer metadata from data file register (if any)
    if(!is.null(data_files_register)){
      register_entry = data_files_register[data_files_register$code == ext_data_name,]
      if(nrow(register_entry)>0){
        register_entry = register_entry[1L,]
        if(!is.na(register_entry$uri)) ext_data$setLayeruri(register_entry$uri)
        if(!is.na(register_entry$label)) ext_data$setLayertitle(register_entry$label)
        if(!is.na(register_entry$definition)) ext_data$setLayerdesc(register_entry$definition)
      }
    }
    
    return(ext_data)
  })
  
  entity$data$dir = file.path(getwd(), "data") #make sure entity$data$data is used for geoserver (waiting for r-geoflow/geoflow#422)
  
  entity$enrichWithRelations(config)
  
}