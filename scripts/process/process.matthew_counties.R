process.matthew_counties <- function(viz){
  library(rgeos)
  states <- c("FL","GA","SC","NC")
  
  epsg_code <- '+init=epsg:3086' 
  counties <- readData(viz[['depends']])
  counties <- counties[counties$STATE %in% states, ]
  FIPs <- as.character(counties$FIPS)
  counties <- rgeos::gSimplify(counties, 0.001)
  counties <- spTransform(counties, CRS(epsg_code))
  counties <- SpatialPolygonsDataFrame(counties, data = data.frame(FIPS=FIPs), match.ID = FALSE)
  
  saveRDS(counties, viz[['location']])
}

process.matthew_states <- function(viz){
  library(rgeos)
  skip.states <- c("Florida","Georgia","South Carolina","North Carolina")
  
  epsg_code <- '+init=epsg:3086' 
  states <- readData(viz[['depends']])
  states <- states[!states$STATE %in% skip.states, ]
  states <- rgeos::gSimplify(states, 0.01)
  states <- spTransform(states, CRS(epsg_code))
  
  saveRDS(states, viz[['location']])
}

process.matthew_track <- function(viz){
  library(rgeos)
  library(sp)
  
  counties <- readData(viz[['depends']][2])
  track <- readData(viz[['depends']][1])
  track <- spTransform(track, CRS(proj4string(counties)))
  
  # here do "over" analysis for masking?
  
  saveRDS(track, viz[['location']])
}

process.matthew_sites <- function(viz){
  library(rgeos)
  library(sp)
  library(dplyr)
  
  counties <- readData(viz[['depends']][2])
  sites <- readData(viz[['depends']][1]) %>% 
    arrange(desc(dec_lat_va))
  track <- readData(viz[['depends']][3])
  buffered.track <- gBuffer( track, width=150000, byid=TRUE )
  pts <- cbind(sites$dec_long_va, sites$dec_lat_va)
  sites <- SpatialPointsDataFrame(pts, proj4string=CRS("+proj=longlat +datum=WGS84"), 
                                     data = sites %>% select(site_no, station_nm) %>% data.frame)
  sites <- spTransform(sites, CRS(proj4string(counties)))
  overlap <- gContains(buffered.track, sites, byid = TRUE) %>% rowSums() %>% as.logical()
  
  # here do "over" analysis for masking?
  
  saveRDS(sites[overlap, ], viz[['location']])
}

process.timesteps <- function(viz){
  #"classifyBins",'storm-location'
  library(dplyr)
  library(jsonlite)
  times <- readData(viz[['depends']][1]) %>% select(DateTime) %>% 
    unique() %>% .$DateTime %>% as.POSIXct %>% format('%b %d %I:%M %p')
  cat(jsonlite::toJSON(list(times=times)), file = viz[['location']])
}
library(svglite)
library(xml2)
grab_spark <- function(vals){
  
  x = svglite::xmlSVG({
    par(omi=c(0,0,0,0), mai=c(0,0,0,0))
    plot(vals, type='l', axes=F, ann=F)
  }, height=0.2, width=2)
  xml2::xml_attr(xml2::xml_find_first(x, '//*[local-name()="polyline"]'),'points')
}

process.discharge_sparks <- function(viz){
  library(dplyr)
  disch <- readData(viz[['depends']][1])
  times <- readData(viz[['depends']][2]) %>% .$times %>% 
    as.POSIXct(format = '%b %d %I:%M %p', tz= "America/New_York")
  interp_q <- function(x,y){
    approx(x, y, xout = times)$y %>% grab_spark
  }
  sparks <- group_by(disch, site_no) %>% 
    summarize(points = interp_q(dateTime, Flow_Inst))
  saveRDS(sparks, viz[['location']])
}
process.storm_location <- function(viz){
  
  library(rgeos)
  library(sp)
  library(foreign)
  library(dplyr)
  
  shp.path <- file.path(tempdir(), 'tmp')
  if (!dir.exists(shp.path)){
    dir.create(shp.path)
  }
  unzip('cache/matthew.zip', exdir = shp.path)
  
  as.time <- function(YEAR, MONTH, DAY, HHMM){
    as.POSIXct(sprintf('%s-%s-%s %s', YEAR, MONTH, DAY, HHMM), format='%Y-%b-%d %H%M', tz="America/New_York")
  }
  
  dbf.file <- file.path(shp.path, 'al142016_pts.dbf')
  shp.data <- foreign::read.dbf(dbf.file) %>% 
    filter(STORMNAME=="MATTHEW") %>% mutate(DateTime = as.time(YEAR, MONTH, DAY, HHMM)) %>% 
    select(LAT, LON, DateTime, INTENSITY)
  
  unlink(shp.path)
  
  t.out <- seq(as.POSIXct("2016-10-5 00:00:00", tz="America/New_York"), by='hours', to = Sys.time())
  lat.out <- approx(shp.data$DateTime, shp.data$LAT, xout = t.out)$y
  lon.out <- approx(shp.data$DateTime, shp.data$LON, xout = t.out)$y
  pts <- cbind(lon.out[!is.na(lon.out)], lat.out[!is.na(lon.out)])
  location <- SpatialPoints(pts, proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  counties <- readData(viz[['depends']][2])
  
  location <- spTransform(location, CRS(proj4string(counties)))
  
  saveRDS(location, viz[['location']])
}