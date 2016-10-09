# viz <- yaml.load_file("viz.yaml")
# viz <- viz$visualize
# viz <- viz[[1]]

visualize.matthew_map <- function(viz){
  
  counties <- readData(viz[['depends']][1])
  flowlines <- readData(viz[['depends']][2])
  states <- readData(viz[['depends']][3])
  track <- readData(viz[['depends']][4])
  col.bins <- readData(viz[['depends']][5])
  storm <- readData(viz[['depends']][6])
  gages <- readData(viz[['depends']][7])
  prctl <- readData(viz[['depends']][8])
  legend.bins <- readData(viz[['depends']][9])
  legend.breaks <- readData(viz[['depends']][10])
  spark.sites <- readData(viz[['depends']][11])
  
  library(svglite)
  library(dplyr)
  
  svglite::svglite(viz[['location']])
  par(mai=c(0,0,0,0), omi=c(0,0,0,0))
  sp::plot(counties)
  sp::plot(flowlines, add=TRUE)
  sp::plot(states, add=TRUE)
  sp::plot(track, add=TRUE)
  sp::plot(gages, pch=20, add=TRUE)
  sp::plot(storm, pch=20, add=TRUE)
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
  
  for (j in (i+1):length(p)){
    xml_attr(p[[j]], 'class') <- 'state-polygon'
  }

  g.rivers <- xml_add_child(svg, 'g', id='rivers','class'='river-polyline')
  g.track <- xml_add_child(svg, 'g', id='track','class'='track-polyline')
  g.storm <- xml_add_child(svg, 'g', id='storm','class'='storm-dots')
  g.legend <- xml_add_child(svg, 'g', id='precip-legend','class'='legend', transform='translate(155,353)scale(0.8)')
  g.watermark <- xml_add_child(svg, 'g', id='usgs-watermark',transform=sprintf('translate(2,%s)scale(0.25)', as.character(as.numeric(vb[4])-40)))
  
  pl <- xml_find_all(svg, '//*[local-name()="polyline"]')
  for (i in (length(pl)+1 - length(track)): length(pl)){
    xml_add_child(g.track, 'polyline', points = xml_attr(pl[i], 'points'))
  }
  for (i in 1:(length(pl)- length(track))){
    xml_add_child(g.rivers, 'polyline', points = xml_attr(pl[i], 'points'))
  }
  
  
  cnt = 0; # count how many actually have data
  min.x <- NA
  max.x <- NA
  for (i in 1:length(gages)){
    svg.points <- filter(spark.sites, site_no == gages$site_no[i]) %>% .$points
    if (!is.null(svg.points) && !is.na(svg.points[1])){
      cnt = cnt+1
      xs <- sapply(strsplit(strsplit(svg.points,'[ ]')[[1]], '[,]'), function(x) as.numeric(x[1]))
      min.x <- min(xs, min.x, na.rm = TRUE)
      max.x <- max(xs, max.x, na.rm = TRUE)
    }
  }
  d <- xml_find_all(svg, '//*[local-name()="defs"]')
  xml_remove(d)
  d <- xml_add_child(svg, 'defs') 
  cp <- xml_add_child(d, 'clipPath', id="spark-clip")
  xml_add_child(cp, 'rect', x=as.character(min.x), height=vb[4], width = as.character(max.x-min.x), id="spark-clip-rect")
  m = xml_add_child(d, 'mask', id="spark-opacity", x="0", y="-1", width="1", height="3", maskContentUnits="objectBoundingBox")
  xml_add_child(m, 'rect', x="0", y="-1", width="1", height="3", style="fill-opacity: 0.25; fill: white;", id='spark-light-mask')
  xml_add_child(m, 'rect', x="0", y="-1", width="0", height="3", style="fill-opacity: 1; fill: white;", id='spark-full-mask')
  xml_add_child(svg, 'rect', x='580', y='5', width="132", height='40', fill='white', stroke='grey', class='legend-box', 'fill-opacity'='0.4')
  xml_add_child(svg, 'text', x='646', y='30', 'USGS stream', dy='-0.5em', 'text-anchor'='middle')
  xml_add_child(svg, 'text', x='646', y='30', 'gage discharge', dy='0.5em', "text-anchor"='middle')
  
  ys <- seq(50,as.numeric(vb[4])-20, length.out = cnt)
  cnt = 0;
  for (i in 1:length(gages)){ # FRAGILE - assumes all gages are on the map!!
    
    svg.points <- filter(spark.sites, site_no == gages$site_no[i]) %>% .$points
    if (!is.null(svg.points) && !is.na(svg.points[1])){
      cnt = cnt+1
      g.dot <- xml_add_child(g.storm, 'g', transform=sprintf('translate(%s,%s)', cx = xml_attr(cr[i], 'cx'), cy = xml_attr(cr[i], 'cy')))
      xml_add_child(g.dot, 'circle', id=sprintf('nwis-%s',gages$site_no[i]), r='3', class='nwis-dot',
                    onclick=sprintf("window.open('http://waterdata.usgs.gov/nwis/uv?site_no=%s','_blank')", gages$site_no[i]),
                    onmouseover=sprintf("document.getElementById('sparkline-%s').setAttribute('class', 'sparkline-bold')", gages$site_no[i]),
                    onmouseout=sprintf("document.getElementById('sparkline-%s').setAttribute('class', 'sparkline');hovertext(' ');", gages$site_no[i]),
                    onmousemove=sprintf("hovertext('NWIS %s',evt);",gages$site_no[i]))
      g.dot <- xml_add_child(g.storm, 'g', transform=sprintf('translate(%s,%s)', "570", ys[cnt])) 
      xml_add_child(g.dot, 'polyline', points = svg.points[1], class='sparkline', id=paste0('sparkline-',gages$site_no[i]), 
                    onclick=sprintf("window.open('http://waterdata.usgs.gov/nwis/uv?site_no=%s','_blank')", gages$site_no[i]),
                    onmouseover=sprintf("document.getElementById('nwis-%s').setAttribute('class', 'nwis-bold');", gages$site_no[i]),
                    onmouseout=sprintf("document.getElementById('nwis-%s').setAttribute('class', 'nwis-dot');hovertext(' ');", gages$site_no[i]),
                    style="mask: url(#spark-opacity)",
                    'clip-path'="url(#spark-clip)", onmousemove=sprintf("hovertext('NWIS %s',evt);",gages$site_no[i]))
    }
    
  }
  storm.i <- length(storm)
  for (i in length(cr):(length(gages)+1)){ # assumes that LAST of the storm is on the map!!
    xml_add_child(g.storm, 'circle', cx = xml_attr(cr[i], 'cx'), cy = xml_attr(cr[i], 'cy'), id=paste0('storm-',storm.i), r='8', class='storm-dot')
    storm.i <- storm.i - 1
  }
  
  xml_add_child(g.legend, 'rect', x="-8", y="-8", width='175', height='235', fill='white', stroke='grey', class='legend-box', 'fill-opacity'='0.4')
  xml_add_child(g.legend, 'text', 'Legend', 'class'='legend-title', dy='0.75em')
  
  ys <- as.character(seq(24, 160, length.out = length(legend.bins)))
  box.w <- '12'
  for (i in 1:length(legend.bins)){
    xml_add_child(g.legend, 'rect', 'height'=box.w, 'width'=box.w, y = ys[i], id=paste0('precip-bin-',i), fill=legend.bins[i], class='precip-legend-bin')
    leg.txt <- ifelse(i == length(legend.breaks), sprintf('> %s inches per hour', legend.breaks[i]), sprintf('%s to %s', legend.breaks[i], legend.breaks[i+1]))
    xml_add_child(g.legend, 'text', x=box.w, 'dx'="0.5em", y=as.character(as.numeric(ys[i])+as.numeric(box.w)/2), 'dy'= "0.33em", class='precip-legend-text', leg.txt)
  }
  xml_add_child(g.legend, 'path', d=sprintf('M-4,%s h%s',as.character(as.numeric(ys[i])+30), 20), class='track-polyline')
  xml_add_child(g.legend, 'circle', cx = as.character(as.numeric(box.w)/2), r='8', class='storm-dot-legend', cy = as.character(as.numeric(ys[i])+30), class='storm-legend-dot')
  xml_add_child(g.legend, 'text', x=box.w, 'dx'="0.5em", y=as.character(as.numeric(ys[i])+30), 'dy'= "0.33em", class='storm-legend-text', "Hurricane Matthew")
  
  xml_add_child(g.legend, 'circle', cx = as.character(as.numeric(box.w)/2), r='3', cy = as.character(as.numeric(ys[i])+53), class='nwis-legend-dot')
  xml_add_child(g.legend, 'text', x=box.w, 'dx'="0.5em", y=as.character(as.numeric(ys[i])+53), 'dy'= "0.33em", class='nwis-legend-text', "USGS stream gage")
  
  xml_add_child(svg, 'text', ' ', id='timestamp-text', class='time-text', x="490", y="320", 'text-anchor'="middle")
  
  usgs.d="m234.95 15.44v85.037c0 17.938-10.132 36.871-40.691 36.871-27.569 0-40.859-14.281-40.859-36.871v-85.04h25.08v83.377c0 14.783 6.311 20.593 15.447 20.593 10.959 0 15.943-7.307 15.943-20.593v-83.377h25.08m40.79 121.91c-31.058 0-36.871-18.27-35.542-39.03h25.078c0 11.462 0.5 21.092 14.282 21.092 8.472 0 12.62-5.482 12.62-13.618 0-21.592-50.486-22.922-50.486-58.631 0-18.769 8.968-33.715 39.525-33.715 24.42 0 36.543 10.963 34.883 36.043h-24.419c0-8.974-1.492-18.106-11.627-18.106-8.136 0-12.953 4.486-12.953 12.787 0 22.757 50.493 20.763 50.493 58.465 0 31.06-22.75 34.72-41.85 34.72m168.6 0c-31.06 0-36.871-18.27-35.539-39.03h25.075c0 11.462 0.502 21.092 14.285 21.092 8.475 0 12.625-5.482 12.625-13.618 0-21.592-50.494-22.922-50.494-58.631 0-18.769 8.969-33.715 39.531-33.715 24.412 0 36.536 10.963 34.875 36.043h-24.412c0-8.974-1.494-18.106-11.625-18.106-8.144 0-12.955 4.486-12.955 12.787 0 22.757 50.486 20.763 50.486 58.465 0 31.06-22.75 34.72-41.85 34.72m-79.89-46.684h14.76v26.461l-1.229 0.454c-3.816 1.332-8.301 2.327-12.453 2.327-14.287 0-17.943-6.645-17.943-44.177 0-23.256 0-44.348 15.615-44.348 12.146 0 14.711 8.198 14.933 18.107h24.981c0.198-23.271-14.789-36.043-38.42-36.043-41.021 0-42.52 30.724-42.52 60.954 0 45.507 4.938 63.167 47.12 63.167 9.784 0 25.36-2.211 32.554-4.18 0.436-0.115 1.212-0.596 1.212-1.216v-59.598h-38.612v18.09"
  wave.d="m48.736 55.595l0.419 0.403c11.752 9.844 24.431 8.886 34.092 2.464 6.088-4.049 33.633-22.367 49.202-32.718v-10.344h-116.03v27.309c7.071-1.224 18.47-0.022 32.316 12.886m43.651 45.425l-13.705-13.142c-1.926-1.753-3.571-3.04-3.927-3.313-11.204-7.867-21.646-5.476-26.149-3.802-1.362 0.544-2.665 1.287-3.586 1.869l-28.602 19.13v34.666h116.03v-24.95c-2.55 1.62-18.27 10.12-40.063-10.46m-44.677-42.322c-0.619-0.578-1.304-1.194-1.915-1.698-13.702-10.6-26.646-5.409-29.376-4.116v11.931l6.714-4.523s10.346-7.674 26.446 0.195l-1.869-1.789m16.028 15.409c-0.603-0.534-1.214-1.083-1.823-1.664-12.157-10.285-23.908-7.67-28.781-5.864-1.382 0.554-2.7 1.303-3.629 1.887l-13.086 8.754v12.288l21.888-14.748s10.228-7.589 26.166 0.054l-0.735-0.707m68.722 12.865c-4.563 3.078-9.203 6.203-11.048 7.441-4.128 2.765-13.678 9.614-29.577 2.015l1.869 1.797c0.699 0.63 1.554 1.362 2.481 2.077 11.418 8.53 23.62 7.303 32.769 1.243 1.267-0.838 2.424-1.609 3.507-2.334v-12.234m0-24.61c-10.02 6.738-23.546 15.833-26.085 17.536-4.127 2.765-13.82 9.708-29.379 2.273l1.804 1.729c0.205 0.19 0.409 0.375 0.612 0.571l-0.01 0.01 0.01-0.01c12.079 10.22 25.379 8.657 34.501 2.563 5.146-3.436 12.461-8.38 18.548-12.507l-0.01-12.165m0-24.481c-14.452 9.682-38.162 25.568-41.031 27.493-4.162 2.789-13.974 9.836-29.335 2.5l1.864 1.796c1.111 1.004 2.605 2.259 4.192 3.295 10.632 6.792 21.759 5.591 30.817-0.455 6.512-4.351 22.528-14.998 33.493-22.285v-12.344"
  xml_add_child(g.watermark,'path', d=usgs.d, onclick="window.open('https://www2.usgs.gov/water/','_blank')", 'class'='watermark')
  xml_add_child(g.watermark,'path', d=wave.d, onclick="window.open('https://www2.usgs.gov/water/','_blank')", 'class'='watermark')
  
  g.tool <- xml_add_child(svg,'g',id='tooltip-group')
  xml_add_child(g.tool, 'rect', id="tooltip-box", height="1.5em", fill='white')#, class="hidden")
  xml_add_child(g.tool, 'path', id="tooltip-point", d="M-6,-11 l6,10 l6,-11", class="tooltip-box")
  xml_add_child(g.tool, 'text', id="tooltip-text", dy="-1.1em", 'text-anchor'="middle", class="tooltip-text-label", " ")
  
  xml_remove(pl)
  xml_remove(cr)
  write_xml(svg, viz[['location']])
}