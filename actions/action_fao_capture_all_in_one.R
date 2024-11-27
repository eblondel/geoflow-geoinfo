function(action, entity, config){
  
  opts <- action$options
  
  library(dplyr)
  library(rvest)
  library(utils)
  library(xml2)
  
  print("Entity action")
  print(action)

  locale<-Sys.setlocale("LC_TIME")
  Sys.setlocale("LC_TIME","English_United States")
  
  url<- attr(entity$data$source[[1]], "uri")
  
  dataset <- url %>%
    read_html()%>%
    html_node(xpath = '//table[1]')%>%
    html_table(fill=TRUE)%>%
    filter(`Last modified`!="")%>%
    mutate(Dataset=sub("_[^_]+$", "",Name))%>%
    filter(Dataset== "Capture")%>%
    mutate(`Last modified`=as.POSIXct(`Last modified`,format="%d-%b-%Y %H:%M"))%>%
    group_by(Dataset)%>%
    filter(`Last modified`==max(`Last modified`))%>%
    mutate(link=file.path(url,Name))
  
  Sys.setlocale("LC_TIME",locale)
    
    link<-dataset$link
    name<-dataset$Name
    
    path<-file.path(tempdir(),name)
    
    unziped<-gsub(".zip","",path)
    
    download.file(url = link, destfile = path)
    
    utils::unzip(zipfile = path, exdir = unziped, unzip = getOption("unzip"))
    
    folder_file<-list.files(unziped)
    
    file<-folder_file[grepl('*VALUE\\.csv|*Value\\.csv|*QUANTITY\\.csv|*Quantity\\.csv|*quantity\\.csv', folder_file)]
    
    file.copy(from=file.path(unziped,file),to=file.path(getwd(),"data",file),overwrite = T)

    fao_n<-readr::read_csv(file.path(getwd(), "data", file))

names(fao_n)<-c("country_id","species","water_area","unit","year","measurement_value","measurement_status")

fao_n <- subset(fao_n, unit == "Q_tlw")

fao_n<-subset(fao_n, select=-c(unit))

#Be sure to keep 2 character length format to area code
#fao_n$water_area<-sprintf("%02d", as.numeric(fao_n$water_area))

#Be sure to keep 3 character length format to flag code
#fao_n$country_id<-sprintf("%03d", as.numeric(fao_n$country_id))

#AREA
area_ref<-readr::read_csv("https://raw.githubusercontent.com/fdiwg/fdi-mappings/main/global-to-global/cl_mapping_fao_areas_main_water_area_types.csv")
area_ref <-subset(area_ref,select=c(src_code,trg_code))
names(area_ref)<-c("water_area","water_area_type")

#COUNTRY
link_m49_iso3 <-readr::read_csv("https://raw.githubusercontent.com/fdiwg/fdi-mappings/main/global-to-global/cl_mapping_country_and_territory_m49_country_and_territory_iso3.csv")
link_m49_iso3 <-subset(link_m49_iso3,select=c(src_code,trg_code))
names(link_m49_iso3)<-c("country_id","country_and_territory")

#GEOREGION
link_iso3_georegion <-readr::read_csv("https://raw.githubusercontent.com/fdiwg/fdi-mappings/main/global-to-global/cl_mapping_country_and_territory_iso3_un_georegions.csv")
link_iso3_georegion <-subset(link_iso3_georegion,select=c(src_code,trg_code))
names(link_iso3_georegion)<-c("country_and_territory","georegion")

country_ref<-merge(link_m49_iso3,link_iso3_georegion)

fao_n<-merge(fao_n,area_ref,all.x = T,all.y=F)
fao_n<-merge(fao_n,country_ref,all.x = T,all.y=F)

fao_n<-subset(fao_n,select=-c(country_id))

fao_n<- fao_n[,c("country_and_territory", "georegion", "species","water_area","measurement_value","water_area_type","year","measurement_status")]

#Data filter
#fao_n <- subset(fao_n, measurement_value != 0)
fao_n$year<-as.integer(fao_n$year)

fao_n$measurement<-"catch"
fao_n$measurement_type<-"NC"
fao_n$measurement_unit<-"t"

fao_n<- fao_n[c("country_and_territory","georegion", "species", "water_area","water_area_type", "year","measurement","measurement_type","measurement_value","measurement_unit","measurement_status")]

#case of Chinese provinces statistics
#make sure chinese statistics are all counted (including those from provinces)
#fao_n[fao_n$flag %in% c("MAC","HKG","TWN"),]$flag <- "CHN"
#chn_data <- fao_n[fao_n$flag == "CHN",]
#mac_data <- chn_data
#mac_data$flag <- "MAC"
#hkg_data <- chn_data
#hkg_data$flag <- "HKG"
#twn_data <- chn_data
#twn_data$flag <- "TWN"
#fao_n <- do.call("rbind", list(fao_n, mac_data, hkg_data, twn_data))

if(!is.null(opts$water_areas)){
	config$logger.info(opts$water_areas)
	fao_n = fao_n[fao_n$water_area %in% opts$water_areas,]
	config$logger.info("Count records")
	config$logger.info(nrow(fao_n))
}

entity$data$features<-fao_n
#Db connection and export
writeWorkflowJobDataResource(entity=entity,config=config,type="dbtable",useFeatures=TRUE,useUploadSource=TRUE,createIndexes=TRUE)

fao_n<-sf::st_read(dsn = config$software$output$dbi,query = sprintf("SELECT * FROM %s", entity$data$uploadSource[[1]]))
country<-sf::st_read(dsn = config$software$output$dbi,query = "SELECT geometry, iso_3 FROM countries")
fao<-merge(fao_n,country,by.x="country_and_territory",by.y="iso_3",all.x=T,all.y=F)
entity$data$features<-fao
#Enrich Metadata
dimensions<-c("year","country_and_territory","georegion","species","water_area","water_area_type","aggregation_method")

default_values <- list()
for(dimension in dimensions){
    regexpValue <- switch(dimension,
                          "year" = "^[\\w. +]+$",
                          "country_and_territory" = "^[\\w. +]+$",
                          "georegion" = "^[\\w. +]+$",
                          "species" = "^[\\w. +]+$",
                          "water_area" = "^[\\w. +]+$",
                          "water_area_type" = "^[\\w. +]+$",
                          "aggregation_method"="^[none|none_withgeom|avg_by_year|sum]+$",
                          "^[\\w. +]+$"
    )
    defaultValue <-switch(dimension,
                          "year" = paste(unique(fao_n$year),  collapse="+"),
                          "country_and_territory" = paste(unique(fao_n$country_and_territory),  collapse="+"),
                          "georegion" = paste(unique(fao_n$georegion),  collapse="+"),
                          "species" = paste(unique(fao_n$species),  collapse="+"),
                          "water_area" = paste(unique(fao_n$water_area),  collapse="+"),
                          "water_area_type" = paste(unique(fao_n$water_area_type),  collapse="+"),
                          "aggregation_method" = "sum",
                          paste(dbGetQuery(con, sprintf("select distinct %s from %s.%s",dimension, schema, pid, dimension))[,1], collapse="+")
    )
    print("--------------")
    print(dimension)
    print(regexpValue)
    print(defaultValue)
    default_values <- c(default_values, defaultValue)
    entity$data$setParameter(dimension, dimension, regexpValue, defaultValue)
}
}













