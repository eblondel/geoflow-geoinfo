function(action, entity, config){
  
  WFS_FAO = ows4R::WFSClient$new(
    url = "https://www.fao.org/fishery/geoserver/fifao/wfs",
    serviceVersion = "1.0.0",
    logger = "INFO"
  )
  fao_areas = WFS_FAO$getFeatures("fifao:FAO_AREAS_ERASE_LOWRES")
  
  #get data, enrich with the bbox, and remove geometry
  sf.data = entity$data$features
  sf.data$bbox = sapply(sf.data$geom, function(x){
    b = sf::st_bbox(x)
    g = sf::st_polygon(list(matrix(c(b$xmin,b$ymin, b$xmin,b$ymax, b$xmax,b$ymax, b$xmax,b$ymin, b$xmin,b$ymin),ncol=2, byrow=TRUE)))
    as(sf::st_as_text(g),"character") 
  })
  sf.data$geom = NULL
  
  #vocabulary
  readr::write_csv(sf.data, file.path("data", "grsf_areas_vocabulary.csv"))
  
  #vocabulary for NFI
  nfi_data = tibble::tibble(
    ID = sf.data$ID,
    Code = sf.data$area_code,
    Parent_id = sapply(1:nrow(sf.data), function(i){
      if(sf.data[i,]$namespace == "fao"){
        fao_area = fao_areas[fao_areas$F_CODE == sf.data[i,]$area_code,]
        if(nrow(fao_area)>0){
          level = fao_area$F_LEVEL
          switch(level,
                 "MAJOR" = "",
                 "SUBAREA" = fao_area$F_AREA,
                 "DIVISION" = fao_area$F_SUBAREA,
                 "SUBDIVISION" = fao_area$F_DIVISION,
                 "SUBUNIT" = fao_area$F_SUBDIVIS
          )
        }else{
          ""
        }
      }else{
        ""
      }
    }),
    Type = sapply(1:nrow(sf.data), function(i){
      if(sf.data[i,]$namespace == "fao"){
        fao_area = fao_areas[fao_areas$F_CODE == sf.data[i,]$area_code,]
        if(nrow(fao_area) > 0){
          level = fao_area$F_LEVEL
          switch(level,
                 "MAJOR" = "area",
                 "SUBAREA" = "subarea",
                 "DIVISION" = "division",
                 "SUBDIVISION" = "subdivision",
                 "SUBUNIT" = "subunit"
          )
        }else{
          "area" 
        }
      }else{
        "nonfaoarea"
      }
    }),
    Name = sf.data$area_name,
    FigisID = "",
    Codesystem = sf.data$namespace,
    Description = "",
    TableOptions = "",
    Table = "",
    Images = "",
    Keywords = "",
    "Source of information" = sf.data$citation,
    "Additional reading" = "",
    Environment = "Marine areas",
    Language = "en",
    "Cover page" = "",
    CreationDate = "",
    ModifiedDate = "",
    RecordStatus = "PUBLISHED",
    CollectionCode = "",
    BoundingBox = sf.data$bbox
  )
  readr::write_csv(nfi_data, file.path("data", "grsf_areas_vocabulary_nfi.csv"))
  
}