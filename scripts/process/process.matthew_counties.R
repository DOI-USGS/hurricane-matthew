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