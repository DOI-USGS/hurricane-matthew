process.sites <- function(viz){
  library(rgeos)
  library(sp)
  required <- c("depends", "location")
  checkRequired(viz, required)
  
  discharge <- readData(viz[['depends']][1])
  counties <- readData(viz[['depends']][2])
  
  sites <- discharge$sites
  
  coordinates(sites) <- ~ dec_lon_va + dec_lat_va
  proj4string(sites) <- CRS("+proj=longlat +ellps=GRS80 +no_defs")
  sites <- spTransform(sites, CRS(proj4string(counties)))
  
  saveRDS(sites, viz[['location']])
}