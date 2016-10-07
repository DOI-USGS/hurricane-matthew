#fetch precip data w/geoknife
fetch.precip <- function(viz){
  getPrecip <- function(states, startDate, endDate){
    
    wg_s <- webgeom(geom = 'derivative:US_Counties', attribute = 'STATE')
    wg_c <- webgeom(geom = 'derivative:US_Counties', attribute = 'COUNTY')
    wg_f <- webgeom(geom = 'derivative:US_Counties', attribute = 'FIPS')
    county_info <- data.frame(state = query(wg_s, 'values'), county = query(wg_c, 'values'), 
                              fips = query(wg_f, 'values'), stringsAsFactors = FALSE) %>% 
      unique() 
    
    counties_fips <- county_info %>% filter(state %in% states) %>%
      mutate(state_fullname = tolower(state.name[match(state, state.abb)])) %>%
      mutate(county_mapname = paste(state_fullname, tolower(county), sep=",")) %>%
      mutate(county_mapname = unlist(strsplit(county_mapname, split = " county")))
    
    stencil <- webgeom(geom = 'derivative:US_Counties',
                       attribute = 'FIPS',
                       values = counties_fips$fips)
    
    fabric <- webdata(url = 'http://cida.usgs.gov/thredds/dodsC/stageiv_combined', 
                      variables = "Total_precipitation_surface_1_Hour_Accumulation", 
                      times = c(startDate, endDate))
    
    job <- geoknife(stencil, fabric, wait = TRUE, REQUIRE_FULL_COVERAGE=FALSE)
    check(job)
    precipData <- result(job, with.units=TRUE)
    precipData2 <- precipData %>% 
      select(-variable, -statistic, -units) %>% 
      gather(key = fips, value = precipVal, -DateTime) %>% 
      left_join(counties_fips, by="fips")
    
    return(precipData2)
  }
  
  library(dplyr)
  library(geoknife)
  library(tidyr)
  
  #TODO: get dates and states from a yaml
  startDate <- as.POSIXct("2016-10-5 00:00:00", tz="America/New_York")
  endDate <- as.POSIXct("2016-10-6 23:00:00", tz = "America/New_York")
  attr(startDate, 'tzone') <- "UTC"
  attr(endDate, 'tzone') <- "UTC"
  
  states <- c("FL","GA","SC","NC")
  
  precip <- getPrecip(states, startDate, endDate)
  attr(precip$DateTime, 'tzone') <- "America/New_York" #back to eastern
  location <- viz[['location']]
  write.csv(precip, file=location, row.names = FALSE)
}



