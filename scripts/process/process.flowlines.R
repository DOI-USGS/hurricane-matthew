process.flowlines <- function(viz){
  library(rgeos)
  library(sp)
  
  counties <- readData(viz[['depends']][2])
  flowlines <- readData(viz[['depends']][1])
  flowlines <- spTransform(flowlines, CRS(proj4string(counties)))
  
  # here do "over" analysis for masking?
  
  saveRDS(flowlines, viz[['location']])
}