iotc_codes_register <- function(config){
  req <- readr::read_csv("C:/Users/artur/OneDrive/Documents/GitHub/geoflow-geoinfo/registers/cl_iotc_codes.csv", guess_max = 0)
  out <- as.data.frame(req)[, c("code", "uri", "label", "definition")]
  out <- out[!is.na(out$code), ]
  return(out)
}