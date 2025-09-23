function(action, entity, config){
  download.file(
    url = "https://raw.githubusercontent.com/GRSF/water_areas_shapefiles/refs/heads/main/changelog.md",
    destfile = file.path("data", "changelog.md"),
    mode = "wb"
  )
}