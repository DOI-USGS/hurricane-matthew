process.classifyBins <- function(viz){
  #need to get bins
  colSteps #vector of actual color palette codes
  
  precip_breaks
  
  #load precip data
  precipData$precipVal <- precipData$precipVal/25.4 #convert mm to inches
  precipData <- totalPrecip %>% mutate(cols = cut(cumprecip, breaks = precip_breaks, labels = colSteps, right=FALSE)) %>% 
    mutate(cols = as.character(cols))
}