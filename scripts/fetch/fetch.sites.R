#fetch NWIS site data, downsample to hourly

fetch.sites <- function(viz){
  library(dataRetrieval)
  library(dplyr)
  library(rgdal)
  
  required <- c("states", "location")
  checkRequired(viz, required)
  
  states <- viz[["states"]] 
  site_sum_all <- data.frame()
  
  for(i in states){
    sites <- readNWISdata(service = "site",
                          seriesCatalogOutput=TRUE,
                          parameterCd="00060",
                          stateCd = i)
    sites_sum <- filter(sites, parm_cd == "00060",
                        data_type_cd == "uv") %>%
      select(site_no, station_nm, dec_lat_va, dec_long_va) %>%
      data.frame() %>%
      filter(!is.na(dec_lat_va)) %>%
      filter(!is.na(dec_long_va))
    site_sum_all <- bind_rows(site_sum_all, sites_sum)
  }
  
  coordinates(site_sum_all) <- ~ dec_long_va + dec_lat_va
  proj4string(site_sum_all) <- CRS("+proj=longlat +ellps=GRS80 +no_defs")
  
  saveRDS(site_sum_all, viz[["location"]])
  
}