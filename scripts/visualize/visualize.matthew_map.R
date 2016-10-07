visualize.matthew_map <- function(viz){
  
  counties <- readData(viz[['depends']][1])
  flowlines <- readData(viz[['depends']][2])
  states <- readData(viz[['depends']][3])
  track <- readData(viz[['depends']][4])
  col.bins <- readData(viz[['depends']][5])
  storm <- readData(viz[['depends']][6])
  library(svglite)
  library(dplyr)
  
  svglite::svglite(viz[['location']])
  par(mai=c(0,0,0,0), omi=c(0,0,0,0))
  sp::plot(counties)
  sp::plot(flowlines, add=TRUE)
  sp::plot(states, add=TRUE)
  sp::plot(track, add=TRUE)
  plot(storm, pch=20, add=TRUE)
  fip.cd <- as.character(counties$FIPS[counties@plotOrder])
  dev.off()
  # m3 <- map('county', regions = precipData$county_mapname, 
  #           add = TRUE, fill = TRUE, col = precipData$cols)
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
  cr <- xml_find_all(svg, '//*[local-name()="circle"]')
  
  num.time <- group_by(col.bins, fips) %>% tally %>% .$n %>% unique()
  if (length(num.time) != 1){
    stop('all of the counties dont have the same number of timesteps!')
  }
  
  for (i in 1:length(counties)){
    steps <- paste('prcp', 1:num.time, sep='-')
    bins <- filter(col.bins, fips == fip.cd[i]) %>% .$cols
    time.classes <- paste(steps, bins, sep='-', collapse=' ') 
    xml_attr(p[[i]], 'id') <- paste0('FIP-', fip.cd[i])
    xml_attr(p[[i]], 'class') <- sprintf('county-polygon %s', time.classes)
    xml_attr(p[[i]], 'style') <- NULL
    xml_attr(p[[i]], 'clip-path') <- NULL

  }
  
  for (j in i:length(p)){
    xml_attr(p[[j]], 'class') <- 'state-polygon'
  }

  g.rivers <- xml_add_child(svg, 'g', id='rivers','class'='river-polyline')
  g.storm <- xml_add_child(svg, 'g', id='storm','class'='storm-dots')
  g.track <- xml_add_child(svg, 'g', id='track','class'='track-polyline')
  pl <- xml_find_all(svg, '//*[local-name()="polyline"]')
  for (i in (length(pl)+1 - length(track)): length(pl)){
    xml_add_child(g.track, 'polyline', points = xml_attr(pl[i], 'points'))
  }
  for (i in 1:(length(pl)- length(track))){
    xml_add_child(g.rivers, 'polyline', points = xml_attr(pl[i], 'points'))
  }
  for (i in 1:length(cr)){
    xml_add_child(g.storm, 'circle', cx = xml_attr(cr[i], 'cx'), cy = xml_attr(cr[i], 'cy'), id=paste0('storm-',i), r='8', class='hidden')
  }
  d <- xml_find_all(svg, '//*[local-name()="defs"]')
  xml_remove(pl)
  xml_remove(d)
  xml_remove(cr)
  write_xml(svg, viz[['location']])
}