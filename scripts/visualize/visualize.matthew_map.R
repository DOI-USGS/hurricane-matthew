visualize.matthew_map <- function(viz){
  
  counties <- readData(viz[['depends']])
  library(svglite)
  
  svglite::svglite(viz[['location']])
  par(mai=c(0,0,0,0), omi=c(0,0,0,0))
  sp::plot(counties)
  dev.off()
  
  library(xml2)
  svg <- read_xml(viz[['location']])
  
  
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
  
  write_xml(svg, viz[['location']])
}