visualize.matthew_map <- function(viz){
  
  counties <- readData(viz[['depends']][1])
  flowlines <- readData(viz[['depends']][2])
  library(svglite)
  
  svglite::svglite(viz[['location']])
  par(mai=c(0,0,0,0), omi=c(0,0,0,0))
  sp::plot(counties)
  sp::plot(flowlines, add=TRUE)
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
    xml_attr(p[[i]], 'class') <- 'county-polygon'
    xml_attr(p[[i]], 'style') <- NULL
    xml_attr(p[[i]], 'clip-path') <- NULL
  }
  
  g.rivers <- xml_add_child(svg, 'g', id='rivers','class'='river-polyline')
  pl <- xml_find_all(svg, '//*[local-name()="polyline"]')
  for (i in 1:length(pl)){
    xml_add_child(g.rivers, 'polyline', points = xml_attr(pl[i], 'points'))
  }
  xml_remove(pl)
  write_xml(svg, viz[['location']])
}