#fsa_register_area
fsa_register_area <- function(config){
  fsa = sf::st_read("data/fsa/FAO_AREAS_ERASE.shp")
  fsa_inland = sf::st_read("data/fsa/FAO_AREAS_INLAND.shp")
  fsa = rbind(fsa, fsa_inland)
  out <- data.frame(
    code = fsa$F_CODE,
    uri = unlist(lapply(1:nrow(fsa), function(i){
      dat = fsa[i,]
      baseuri = NULL
      if(dat$F_LEVEL %in% c("MAJOR","SUBAREA", "DIVISION")){
        baseuri = sprintf("https://www.fao.org/fishery/en/area/fao:%s", dat$F_CODE)
      }else{
        baseuri = sprintf("https://www.fao.org/fishery/en/area/fao:%s", dat$F_DIVISION)
        if(dat$F_LEVEL %in% c("SUBDIVISION","SUBUNIT")){
          baseuri = paste0(baseuri, "#fao:", dat$F_CODE)
        }
      }
      return(baseuri)
    })),
    label = fsa$NAME_EN,
    definition = NA
  )
  return(out)
}

#fsa_register_area_level
fsa_register_area_level <- function(config){
  fsa = sf::st_read("data/fsa/FAO_AREAS_ERASE.shp")
  fsa_levels = unique(fsa$F_LEVEL)
  out = data.frame(
    code = fsa_levels,
    uri = NA,
    label = sapply(fsa_levels, function(x){
      switch(x,
             "MAJOR" = "Major areas",
             "SUBAREA" = "Subareas",
             "DIVISION" = "Divisions",
             "SUBDIVISION" = "Sub-divisions",
             "SUBUNIT" = "Sub-units"
             )
    }),
    definition = NA
  )
  return(out)
}

#fsa_register_area_status
fsa_register_area_status <- function(config){
  fsa = sf::st_read("data/fsa/FAO_AREAS_ERASE.shp")
  fsa_status = unique(fsa$F_STATUS)
  out = data.frame(
    code = fsa_status,
    uri = NA,
    label = sapply(fsa_status, function(x){
      switch(x,
             "endorsed" = "Endorsed",
             "draft" = "Draft"
      )
    }),
    definition = NA
  )
  return(out)
}

#fsa_register_area_ocean
fsa_register_area_ocean <- function(config){
  fsa = sf::st_read("data/fsa/FAO_AREAS_ERASE.shp")
  oceans = unique(fsa$OCEAN)
  out = data.frame(
    code = oceans,
    uri = NA,
    label = oceans,
    definition = NA
  )
  return(out)
}
