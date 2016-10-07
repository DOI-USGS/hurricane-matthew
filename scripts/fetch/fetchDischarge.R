#fetch NWIS iv data, downsample to hourly

fetch.discharge <- function(viz){
  hitNWIS <- function(states, startDate, endDate){
    for(st in states){
      stDV <- renameNWISColumns(readNWISdata(service="iv",
                                             parameterCd="00060",
                                             stateCd = st,
                                             startDate = startDate,
                                             endDate = endDate,
                                             tz = "America/New_York"))
      if(st != states[1]){
        storm.data <- full_join(storm.data,stDV)
        sites <- full_join(sites, attr(stDV, "siteInfo"))
      } else {
        storm.data <- stDV
        sites <- attr(stDV, "siteInfo")
      }
    }
    
    #downsample to hourly
    storm.data <- filter(storm.data, minute(dateTime)==0)
    
    #stats service
    reqBks <- seq(1,nrow(sites),by=10)
    statData <- data.frame()
    for(i in reqBks) {
      getSites <- sites$site_no[i:(i+9)]
      currentSites <- readNWISstat(siteNumbers = getSites,
                                   parameterCd = "00060", 
                                   statReportType="daily",
                                   statType=c("p10","p25","p50","p75","p90","mean"))
      statData <- rbind(statData,currentSites)
    }
    
    #NOTE: won't deal with crossing months
    statData.storm <- statData[statData$month_nu == month(startDate) & 
                                 statData$day_nu >= day(startDate) & 
                                 statData$day_nu <= day(endDate),]
    
    finalJoin <- left_join(storm.data,statData.storm)
    finalJoin <- left_join(finalJoin,sites) 
    
    #remove sites without current data 
    #finalJoin <- finalJoin[!is.na(finalJoin$Flow),] 
    return(finalJoin)
  }#end
  
  library(dataRetrieval)
  library(dplyr)
  library(lubridate)
  
  #TODO: get dates & states from a yaml
  startDate <- as.Date("2016-10-5")
  endDate <- Sys.Date()
  states <- c("FL","GA","SC","NC")
  
  qData <- hitNWIS(states = states, startDate = startDate, endDate = endDate)
  location <- viz[['location']]
  write.csv(qData, file=location, row.names = FALSE)
}




