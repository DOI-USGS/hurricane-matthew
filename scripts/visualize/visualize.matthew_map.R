visualize.matthew_map <- function(viz){
  
  counties <- readData(viz[['depends']][1])
  flowlines <- readData(viz[['depends']][2])
  states <- readData(viz[['depends']][3])
  track <- readData(viz[['depends']][4])
  precipData <- readData(viz[['depends']][5])
  
  library(svglite)
  
  svglite::svglite(viz[['location']])
  par(mai=c(0,0,0,0), omi=c(0,0,0,0))
  sp::plot(counties)
  sp::plot(flowlines, add=TRUE)
  sp::plot(states, add=TRUE)
  sp::plot(track, add=TRUE)
  dev.off()
  
  library(xml2)
  svg <- read_xml(viz[['location']])
  
  
  # let this thing scale:
  xml_attr(svg, "preserveAspectRatio") <- "xMinYMin meet" 
  vb <- strsplit(xml_attr(svg, 'viewBox'),'[ ]')[[1]]
  
  r <- xml_find_all(svg, '//*[local-name()="rect"]')
  
  xml_add_sibling(xml_children(svg)[[1]], 'rect', .where='before', width=vb[3], height=vb[4], class='background')

  # clean up junk that svglite adds:
  .junk <- lapply(r, xml_remove)
  
  
  # find the paths for the counties, add IDs
  p <- xml_find_all(svg, '//*[local-name()="path"]')
  
  num.time <- 10
  num.bins <- 9
  
  for (i in 1:length(counties)){
    steps <- paste('prcp', 1:num.time, sep='-')
    time.classes <- paste(steps, sample(1:num.bins, num.time, replace=TRUE), sep='-', collapse=' ') # these are fake
    xml_attr(p[[i]], 'id') <- paste0('FIP-', as.character(counties$FIPS[i]))
    xml_attr(p[[i]], 'class') <- sprintf('county-polygon %s', time.classes)
    xml_attr(p[[i]], 'style') <- NULL
    xml_attr(p[[i]], 'clip-path') <- NULL
  }
  for (j in i:length(p)){
    xml_attr(p[[j]], 'class') <- 'state-polygon'
  }
  
  g.rivers <- xml_add_child(svg, 'g', id='rivers','class'='river-polyline')
  g.track <- xml_add_child(svg, 'g', id='track','class'='track-polyline')
  pl <- xml_find_all(svg, '//*[local-name()="polyline"]')
  for (i in 1:length(flowlines)){
    xml_add_child(g.rivers, 'polyline', points = xml_attr(pl[i], 'points'))
  }
  for (j in i:length(pl)){
    xml_add_child(g.track, 'polyline', points = xml_attr(pl[j], 'points'))
  }
  d <- xml_find_all(svg, '//*[local-name()="defs"]')
  xml_remove(pl)
  xml_remove(d)
  write_xml(svg, viz[['location']])
}