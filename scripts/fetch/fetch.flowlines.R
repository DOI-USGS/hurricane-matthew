#' Pull the shapefile(s) for flowlines and writes the file(s) to disk
#' viz fields:
#' mapRange: al or ep
#' location: 01-n
fetch.flowlines <- function(viz) {
  required <- c("location", "mapRange","streamorder")
  checkRequired(viz, required)
  
  postURL <- "http://cida.usgs.gov/nwc/geoserver/nhdplus/ows"
  
  filterXML <- paste0('<?xml version="1.0"?>',
                      '<wfs:GetFeature xmlns:wfs="http://www.opengis.net/wfs" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:gml="http://www.opengis.net/gml" service="WFS" version="1.1.0" outputFormat="shape-zip" xsi:schemaLocation="http://www.opengis.net/wfs http://schemas.opengis.net/wfs/1.1.0/wfs.xsd">',
                      '<wfs:Query xmlns:feature="http://gov.usgs.cida/nhdplus" typeName="feature:nhdflowline_network" srsName="EPSG:4326">',
                      '<ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">',
                      '<ogc:And>',
                      '<ogc:PropertyIsGreaterThan>',
                      '<ogc:PropertyName>streamorde</ogc:PropertyName>',
                      '<ogc:Literal>',viz[['streamorder']],'</ogc:Literal>',
                      '</ogc:PropertyIsGreaterThan>',
                      '<ogc:BBOX>',
                      '<ogc:PropertyName>the_geom</ogc:PropertyName>',
                      '<gml:Envelope>',
                      '<gml:lowerCorner>',viz[["mapRange"]][3]," ",viz[["mapRange"]][1],'</gml:lowerCorner>',
                      '<gml:upperCorner>',viz[["mapRange"]][4]," ",viz[["mapRange"]][2],'</gml:upperCorner>',
                      '</gml:Envelope>',
                      '</ogc:BBOX>',
                      '</ogc:And>',
                      '</ogc:Filter>',
                      '</wfs:Query>',
                      '</wfs:GetFeature>')
  
  resp <- POST(postURL, body = filterXML, write_disk(viz[['location']], overwrite=T))
  if (status_code(resp) >= 400) {
    stop("Error downloading file ", message_for_status(resp))
  }
  cat(content(resp, type = raw), file = viz[['location']])
}


library(httr)
