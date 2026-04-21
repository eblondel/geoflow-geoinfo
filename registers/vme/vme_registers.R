closure_type_register <- function(config){
  req <- readr::read_csv("./registers/vme/cl_closure_types.csv", guess_max = 0)
  out <- as.data.frame(req)[, c("code", "uri", "label", "definition")]
  out <- out[!is.na(out$code), ]
  return(out)
}