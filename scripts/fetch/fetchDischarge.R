#fetch NWIS iv data, downsample to hourly

fetch.discharge <- function(viz){
  library(dataRetrieval)
  library(lubridate)
  library(dplyr)
  
  required <- c("depends", "start.date", "location")
  checkRequired(viz, required)
  
  sites <- readData(viz[['depends']])
  start.date <-  as.Date(viz[["start.date"]])
  n.sites <- nrow(sites)
  n.bins <- 5
  site.bins <- 1:(n.sites/n.bins)
  
  discharge <- data.frame()
  
  for(i in 1:(n.bins+1)){
    
    site.to.call <- sites$site_no[(i*site.bins)]
    site.to.call <- site.to.call[!is.na(site.to.call)]
    stDV <- renameNWISColumns(readNWISdata(service="iv",
                                           parameterCd="00060",
                                           sites = site.to.call,
                                           startDate = start.date,
                                           tz = "America/New_York"))
    
    discharge <- bind_rows(discharge, stDV)
  }
  
  #downsample to hourly: TODO: get precip data?
  discharge <- filter(discharge, minute(dateTime)==0)
  
  location <- viz[['location']]
  saveRDS(discharge, file=location)
}




