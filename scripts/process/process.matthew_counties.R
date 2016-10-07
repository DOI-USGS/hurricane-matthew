process.matthew_counties <- function(viz){
  library(rgeos)
  states <- c("FL","GA","SC","NC")
  
  epsg_code <- '+init=epsg:3086' 
  counties <- readData(viz[['depends']])
  counties = spTransform(counties, CRS(epsg_code))
  
  counties <- counties[counties$STATE %in% states, ]
  saveRDS(counties, viz[['location']])
}