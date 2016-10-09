process.classifyBins <- function(viz){
  library(dplyr)
  
  #need to get bins
  
  colSteps <- readData(viz[['depends']][1]) #vector of actual color palette codes
  precipData <- readData(viz[['depends']][2]) #actual data
  precip_breaks <- seq(0,0.5, length.out = length(colSteps))
  

  precipData$precipVal <- precipData$precipVal/25.4 #convert mm to inches
  
  precipData <- precipData %>% mutate(cols = cut(precipVal, breaks = precip_breaks, labels = FALSE)) %>% 
    mutate(cols = ifelse(precipVal > tail(precip_breaks,1), length(colSteps), cols)) %>% 
    mutate(cols = ifelse(is.na(cols), 1, cols), cols = as.character(cols)) %>% select(fips, DateTime, cols)
    
  
  #want to cut down precipData to only relevant info?
  
  saveRDS(object = precipData, file = viz[['location']])
}