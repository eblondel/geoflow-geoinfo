function(action, entity, config){
  
  #process iotc_stock_assessment_sa_master.gpkg
  master = entity$data$features
  #cut by lowres coastline
  #cut by highres coastline

  
  #here we have 3 gpkgs
  #need to produce one geoflow_data per gpkg
  main_data_obj = entity$data
  
  entity$data$data = lapply(list.files("./data", pattern = ".gpkg"), function(x){
    gpkg_data_obj = main_data_obj$clone(deep = TRUE)
    #look at geoflow geoflow_data.R/line 520 -> https://github.com/r-geoflow/geoflow/blob/master/R/geoflow_data.R#L520
    #...
    return(gpkg_data_obj)
  })
  
}