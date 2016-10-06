visualize.matthew_map <- function(id, fileout='plot_out.svg'){
  library(rgdal)
  library(rgeos)
  library(httr)
  library(dplyr)
  
  
  states <- 'FL'
  
  destination = tempfile(pattern = 'counties', fileext='.zip')
  epsg_code <- '+init=epsg:3086' 
  query <- 'http://cida.usgs.gov/gdp/geoserver/wfs?service=WFS&request=GetFeature&typeName=derivative:US_Counties&outputFormat=shape-zip&version=1.0.0'
  file <- GET(query, write_disk(destination, overwrite=T), progress())
  shp.path <- tempdir()
  unzip(destination, exdir = shp.path)
  counties = readOGR(shp.path, layer='US_Counties') %>% 
    spTransform(CRS(epsg_code))
  
  counties <- counties[counties$STATE %in% states, ]
  
  library(svglite)
  
  svglite::svglite(fileout)
  par(mai=c(0,0,0,0), omi=c(0,0,0,0))
  plot(counties)
  dev.off()
  
  library(xml2)
  svg <- read_xml(fileout)
  
  
  # let this thing scale:
  xml_attr(svg, "preserveAspectRatio") <- "xMinYMin meet" 
  
  # clean up junk that svglite adds:
  r <- xml_find_all(svg, '//*[local-name()="rect"]')
  .junk <- lapply(r, xml_remove)
  
  
  # find the paths for the counties, add IDs
  p <- xml_find_all(svg, '//*[local-name()="path"]')
  
  for (i in 1:length(counties)){
    xml_attr(p[[i]], 'id') <- paste0('FIP-', as.character(counties$FIPS[i]))
    xml_attr(p[[i]], 'style') <- NULL
    xml_attr(p[[i]], 'clip-path') <- NULL
  }
  
  write_xml(svg, fileout)
}