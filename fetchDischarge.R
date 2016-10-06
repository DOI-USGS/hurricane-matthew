#fetch NWIS iv data, downsample to hourly
#get site stats 
#classify in process step?

fetch.discharge <- function(viz){
  hitNWIS <- function(states, startDate, endDate){
    for(st in states){
      
      stDV <- renameNWISColumns(readNWISdata(service="iv",
                                             parameterCd="00060",
                                             stateCd = st,
                                             startDate = startDate,
                                             endDate = ))
      if(st != states[1]){
        storm.data <- full_join(storm.data,stDV)
        sites <- full_join(sites, attr(stDV, "siteInfo"))
      } else {
        storm.data <- stDV
        sites <- attr(stDV, "siteInfo")
      }
    }
    
    #stats service
    reqBks <- seq(1,nrow(sites),by=10)
    statData <- data.frame()
    for(i in reqBks) {
      getSites <- sites$site_no[i:(i+9)]
      currentSites <- readNWISstat(siteNumbers = getSites,
                                   parameterCd = "00060", 
                                   statReportType="daily",
                                   statType=c("p10","p25","p50","p75","p90","mean"),
                                   startDate = startDate, endDate = endDate)
      statData <- rbind(statData,currentSites)
    }
    
    finalJoin <- left_join(storm.data,statData)
    finalJoin <- left_join(finalJoin,sites) 
    
    #remove sites without current data 
    finalJoin <- finalJoin[!is.na(finalJoin$Flow),] 
    
  }#end
  library(dataRetrieval)
  library(dplyr)
  #TODO: call it
  
}




#retrieve stats data, dealing with 10 site limit to stat service requests




#classify current discharge values
finalJoin$class <- NA
finalJoin$class <- ifelse(is.na(finalJoin$p25), 
                          ifelse(finalJoin$Flow > finalJoin$p50_va, "darkorange1","greenyellow"),
                          ifelse(finalJoin$Flow < finalJoin$p25_va, "cyan",
                                 ifelse(finalJoin$Flow > finalJoin$p75_va, "red2","green4")))
return(finalJoin)
}
