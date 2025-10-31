function(action, entity, config){
  
  #process iotc_stock_assessment_sa_master.gpkg
  layer_master = entity$data$features
  #iotc_layer_master = st_read("./data/iotc/iotc_stock_assessment_sa_master.gpkg")
  #iotc_layer_master_path = entity$data$source
  #iotc_layer_master_path = "C:/Users/artur/OneDrive/Documents/GitHub/geoflow-geoinfo/data/iotc/iotc_stock_assessment_sa_master.gpkg"
  #iotc_layer_master = st_read(iotc_layer_master_path)
  
  #Connect to WFS FAO Geoserver
  WFS = ows4R::WFSClient$new(
    url = "https://www.fao.org/fishery/geoserver/wfs",
    serviceVersion = "1.0.0",
    logger = "INFO"
  )
  
  #Low-res continents layer
  continent_lowres = WFS$getFeatures("fifao:UN_CONTINENT2")
  sf::st_crs(continent_lowres) = 4326
  
  #High-res continents layer
  continent_highres = WFS$getFeatures("fifao:UN_CONTINENT2_new")
  continent_highres = st_make_valid(continent_highres)
  sf::st_crs(continent_highres) = 4326
  
  #Cut by lowres coastline
  layer_lowres_sfc = do.call(
    sf::st_sfc, 
    lapply(seq_len(nrow(layer_master)), function(i) {
      (st_difference(st_geometry(layer_master[i, ]), continent_lowres))[[1]]
    })
  )
    layer_lowres = st_set_geometry(layer_master, layer_lowres_sfc)
  
  #Cut by highres coastline
  layer_highres_sfc = do.call(
    sf::st_sfc, 
    lapply(seq_len(nrow(layer_master)), function(i) {
    (st_difference(st_geometry(layer_master[i, ]), continent_highres))[[1]]
    })
  )
  layer_highres = st_set_geometry(layer_master, layer_highres_sfc)
  
  st_crs(layer_lowres)  = 4326
  st_crs(layer_highres) = 4326
  
  #Save GPKGs based on entity ID in data folder
  id <- entity$identifiers$id
  lowres_path  <- file.path("data", paste0(id, "_lowres.gpkg"))
  highres_path <- file.path("data", paste0(id, "_highres.gpkg"))
  st_write(layer_lowres, lowres_path)
  st_write(layer_highres, highres_path)
  
  #Automatically create register.csv
  data_files <- list.files("data", pattern = "\\.gpkg$", full.names = FALSE)
  
  if (length(data_files) > 0) {
    
    register_df <- do.call(rbind, lapply(data_files, function(x) {
      code <- tools::file_path_sans_ext(x)  # remove .gpkg
      uri  <- "https://data.iotc.org/reference/latest/domain/admin/"
      label <- code
      
      # definition based on filename
      if (grepl("master", x, ignore.case = TRUE)) {
        definition <- "Master layer not cut with coastline"
      } else if (grepl("lowres", x, ignore.case = TRUE)) {
        definition <- "Layer cut with low-res coastline"
      } else if (grepl("highres", x, ignore.case = TRUE)) {
        definition <- "Layer cut with high-res coastline"
      } else {
        definition <- ""
      }
      
      data.frame(
        code = code,
        uri = uri,
        label = label,
        definition = definition,
        stringsAsFactors = FALSE
      )
    }))
    
    # write it to ./data/register.csv (overwrite each run)
    readr::write_csv(register_df, file.path("data", "register.csv"))
  }
  
  #Need to produce a list with one geoflow_data per gpkg
  #geoflow_data_list = list(entity$data)
  #Main data object:
  #main_data_obj = geoflow_data_list[[1]]

  #Clone and process for each:
  #entity$data$data = lapply(list.files("./data", pattern = ".gpkg"), function(x){
    #gpkg_data_obj = main_data_obj$clone(deep = TRUE)
    #look at geoflow geoflow_data.R/line 520 -> https://github.com/r-geoflow/geoflow/blob/master/R/geoflow_data.R#L520
    #...
    #return(gpkg_data_obj)
  #})
  
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
    
    #detect presence of data files register
    data_files_register <- NULL
    data_files_register_file = data_files[basename(data_files) == "register.csv"]
    if(length(data_files_register_file)>0){
      data_files_register_file = data_files_register_file[1]
      target_register_file = data_files_register_file
      #if(!is.null(accessor)){
        #target_register_file <- file.path(tempdir(), "register.csv")
        #accessor$download(
          #resource = data_files_register_file,
          #file = "register.csv", 
          #path = target_register_file,
          #software = accessor_software,
          #unzip = FALSE
        #)
      #}
    }
    
    #data_files_register = readr::read_csv("data/register.csv")
    data_files_register = as.data.frame(readr::read_csv(target_register_file))
    register_colnames = c("code","uri","label","definition")
    if(!all(register_colnames %in% colnames(data_files_register))){
      stop("A data files register has been found but doesn't follow the standard structure (code,uri,label,definition)") 
    }
    
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


    
    
    
##############################################################################3
    #Clone for lowres object
    #lowres_data_obj = main_data_obj$clone(deep = TRUE)
    #New object source, sourceType, upload flag, uploadType and uploadSource:
    #lowres_data_obj$source = "iotc_layer_lowres.gpkg@./data/iotc_layer_lowres.gpkg"
    #lowres_data_obj$sourceType = "gpkg"
    #lowres_data_obj$upload = TRUE
    #lowres_data_obj$uploadType = "gpkg"
    #lowres_data_obj$uploadSource = "iotc_layer_lowres"
    
    #Clone for highres object
    #highres_data_obj = main_data_obj$clone(deep = TRUE)
    #New object source, sourceType, upload flag, uploadType and uploadSource:
    #highres_data_obj$source = "iotc_layer_highres.gpkg@./data/iotc_layer_highres.gpkg"
    #highres_data_obj$sourceType = "gpkg"
    #highres_data_obj$upload = TRUE
    #highres_data_obj$uploadType = "gpkg"
    #highres_data_obj$uploadSource = "iotc_layer_highres"
    
  #Attach new objects to the geoflow_data_list:
  #geoflow_data_list = c(
    #geoflow_data_list,            #original including only the master
    #list(lowres_data_obj),
    #list(highres_data_obj)
  #)
  
  #Append to entity$data list
  #entity$data = geoflow_data_list
  
  #entity$??(lowres_data_obj) #add to entity?
  #entity$??(highres_data_obj) #add to entity?
  
}