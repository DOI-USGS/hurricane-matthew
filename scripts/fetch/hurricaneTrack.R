#' Pull the shapefile(s) for a hurricane and writes the file(s) to disk
#' viz fields:
#' ocean: al or ep
#' stormnum: 01-n
#' year: 2016
fetch.hurricaneTrack <- function(viz) {
  required <- c("location", "ocean", "stormnum", "year")
  checkRequired(viz, required)
  
  nhc.url <- "http://www.nhc.noaa.gov/gis/best_track/%s%s%s_best_track.zip"
  download.url <- sprintf(nhc.url, viz[['ocean']], viz[['stormnum']], viz[['year']])
  
  resp <- httr::GET(url = download.url)
  if (status_code(resp) >= 400) {
    stop("Error downloading file ", message_for_status(resp))
  }
  cat(content(resp, type = raw), file = viz[['location']])
}