process.classify <- function(viz){
  library(dplyr)
  library(lubridate)
  #this is just what was left over from my script that wasn't fetch
  discharge <- readData(viz[['depends']][1])
  stats <- readData(viz[['depends']][2])
  
  discharge <- mutate(discharge,
                      month_nu = as.integer(month(dateTime)+1),
                      day_nu = day(dateTime))%>%
    rename(Flow = Flow_Inst)
  
  finalJoin <- left_join(discharge,stats, by=c("site_no"="site_no",
                                               "agency_cd"="agency_cd",
                                               "month_nu"="month_nu",
                                               "day_nu"="day_nu")) 
  
  #classify current discharge values
  finalJoin$class <- NA
  finalJoin$class[finalJoin$Flow > finalJoin$p75_va] <- "navy"
  finalJoin$class[finalJoin$Flow < finalJoin$p25_va] <- "red2"
  
  finalJoin$class[finalJoin$Flow > finalJoin$p25_va & 
                    finalJoin$Flow <= finalJoin$p50_va] <- "green4"
  
  finalJoin$class[finalJoin$Flow > finalJoin$p50_va &
                    finalJoin$Flow <= finalJoin$p75_va] <- "blue"
  
  finalJoin$class[is.na(finalJoin$class) & 
                    finalJoin$Flow > finalJoin$p50_va] <- "cyan"
  
  finalJoin$class[is.na(finalJoin$class) & 
                    finalJoin$Flow < finalJoin$p50_va] <- "yellow"
  
  saveRDS(finalJoin, viz[['location']])
}