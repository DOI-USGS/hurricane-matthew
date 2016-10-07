process.classifyBins <- function(viz){
  library(dplyr)
  
  #need to get bins
  
  colSteps <- readData(viz[['depends']][1]) #vector of actual color palette codes
  precipData <- readData(viz[['depends']][2]) #actual data
  precip_breaks <- #???
  

  precipData$precipVal <- precipData$precipVal/25.4 #convert mm to inches
  precipData <- precipData %>% mutate(cols = cut(precipVal, breaks = precip_breaks, labels = colSteps, right=FALSE)) %>% 
    mutate(cols = as.character(cols))
  
  #want to cut down precipData to only relevant info?
  
  saveRDS(object = precipData, file = viz[['location']])
}