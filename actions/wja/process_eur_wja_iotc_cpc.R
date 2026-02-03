function(action, entity, config){
  
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
  
  #Get wja_level1 layer from WFS
  wja_level1 <- WFS$getFeatures(entity$data$source[[1]])
  sf::st_crs(wja_level1) = 4326
  
  #Create eur_wja
  eu <- c(
    "AUT","BEL","BGR","HRV","CYP","CZE","DNK","EST","FIN","FRA",
    "DEU","GRC","HUN","IRL","ITA","LVA","LTU","LUX","MLT",
    "NLD","POL","PRT","ROU","SVK","SVN","ESP","SWE"
  )
  
  eu_wja <- vapply(
    wja_level1[["code"]],
    FUN = function(code_txt) {
      # handle NA/empty
      if (is.na(code_txt) || !nzchar(code_txt)) return(FALSE)
      
      parts <- unlist(strsplit(as.character(code_txt), "_", fixed = TRUE), use.names = FALSE)
      
      has_eu  <- any(parts %in% eu, na.rm = TRUE)
      has_atf <- any(parts == "ATF", na.rm = TRUE)
      
      has_eu && !has_atf
    },
    FUN.VALUE = logical(1)
  )
  
  eu_wja <- wja_level1[eu_wja, ]
  
  # Filter by pol_type nja
  eu_wja_nja <- eu_wja |>
    dplyr::filter(.data$type == "JA")

  # Dissolve 
  eu_ja_buffer <- sf::st_union(sf::st_make_valid(eu_wja_nja))
  eu_ja <- st_as_sf(data.frame(
                          ID = "wja:eur",
                          code = "EUR",
                          urn = "urn:fdi:code:cwp:wja:nja:eur",
                          label = "European Union Jurisdiction Area",
                          type = "JA",
                          geometry = eu_ja_buffer))
  
  # Function to replace any EU country iso3 code with EUR:
  replace_eu_with_eur <- function(x, eu_codes) {
    # Replace EU ISO3 codes only when they are full tokens delimited by start/end, ':' or '_'
    pattern <- base::paste0(
      "(?<=^|[:_])(",
      base::paste(eu_codes, collapse = "|"),
      ")(?=[:_]|$)"
    )
    
    base::vapply(
      x,
      FUN = function(s) {
        if (base::is.na(s) || !base::nzchar(s)) return(s)
        base::gsub(pattern, "EUR", s, perl = TRUE)
      },
      FUN.VALUE = character(1)
    )
  }
  
  #eur_oc Overlapping Claims
  # Filter by pol_type
  oc_eu <- eu_wja |>
    dplyr::filter(.data$type == "OC")
  
  oc_eu <- oc_eu |>
    dplyr::mutate(
      ID   = replace_eu_with_eur(.data$ID, eu),
      code = replace_eu_with_eur(.data$code, eu),
      urn = tolower(
        replace_eu_with_eur(
          base::toupper(.data$urn),
          eu
        )
      )
    )
  
  #eur_jr Joint Regimes
  jr_eu <- eu_wja |>
    dplyr::filter(.data$type == "JR")
  
  jr_eu <- jr_eu |>
    dplyr::mutate(
      ID   = replace_eu_with_eur(.data$ID, eu),
      code = replace_eu_with_eur(.data$code, eu),
      urn = tolower(
        replace_eu_with_eur(
          base::toupper(.data$urn),
          eu
        )
      )
    )
  
  #Combine into eur_wja
  oc_eu <- oc_eu |>
    dplyr::rename(geometry = geom) |>    # only if they have geom
    sf::st_as_sf()
  
  jr_eu <- jr_eu |>
    dplyr::rename(geometry = geom) |>    # only if they have geom
    sf::st_as_sf()
  
  wja_level1_eur_iotc_cpc <- dplyr::bind_rows(eu_ja, oc_eu, jr_eu)
  
  
  #SAVE GPKG
  wja_layer = get(entity$data$layername)
  wja_layer = wja_layer[, colnames(wja_layer)[!startsWith(colnames(wja_layer), "gml_id")]] #remove gml_ids columns
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