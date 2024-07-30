fdi_cwp_register <- function(config, codelist){
	req = readr::read_csv(sprintf("https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/global/cwp/%s", codelist), guess_max = 0)
	out <- as.data.frame(req)[,c("code", "uri", "label", "definition")]
	out <- out[!is.na(out$code),]
	return(out)
}
species_register <- function(config){
	fdi_cwp_register(config, "cl_asfis_species.csv")
}
country_and_territory_register <- function(config){
	fdi_cwp_register(config, "cl_country_and_territory_iso3.csv")
}
georegion_register <- function(config){
	fdi_cwp_register(config, "cl_un_georegions.csv")
}
water_area_register <- function(config){
	fdi_cwp_register(config, "cl_fao_areas.csv")
}
water_area_type_register <- function(config){
	fdi_cwp_register(config, "cl_main_water_area_types.csv")
}
gear_type_register <- function(config){
	fdi_cwp_register(config, "cl_isscfg_gear.csv")
}

aggregation_method_register <- function(config){
  out <- data.frame(
    code = c("none", "none_withgeom", "avg_by_year", "sum"),
    uri = NA,
    label = c("None", "None (with geometry)", "Yearly average", "Sum"),
    definition = NA
  )
  return(out)
}
