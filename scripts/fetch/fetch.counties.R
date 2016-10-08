fetch.counties <- function(viz){

  library(httr)

  query <- 'http://cida.usgs.gov/gdp/geoserver/wfs?service=WFS&request=GetFeature&typeName=derivative:US_Counties&outputFormat=shape-zip&version=1.0.0'
  file <- GET(query, write_disk(viz[['location']], overwrite=T), progress())
  
}

fetch.states <- function(viz){
  
  library(httr)
  
  query <- 'http://cida.usgs.gov/gdp/geoserver/wfs?service=WFS&request=GetFeature&typeName=derivative:CONUS_States&outputFormat=shape-zip&version=1.0.0'
  file <- GET(query, write_disk(viz[['location']], overwrite=T), progress())
  
}
