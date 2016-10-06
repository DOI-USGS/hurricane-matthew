process.matthew_counties <- function(viz){
  library(rgdal)
  library(rgeos)
  states <- c("FL","GA","SC","NC")
  
  epsg_code <- '+init=epsg:3086' 
  shp.path <- tempdir()
  unzip(viz[['depends']], exdir = shp.path)
  counties = readOGR(shp.path, layer='US_Counties')
  counties = spTransform(countiesCRS(epsg_code))
  
  counties <- counties[counties$STATE %in% states, ]
  saveRDS(counties, viz[['location']])
}