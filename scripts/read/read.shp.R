readData.shp <- function(viz){
  library(rgdal)
  shp.path <- file.path(tempdir(), 'tmp')
  if (!dir.exists(shp.path)){
    dir.create(shp.path)
  }
  unzip(viz[['location']], exdir = shp.path)
  layer <- tools::file_path_sans_ext(list.files(shp.path, pattern='*.shp'))[1]
  data.out = readOGR(shp.path, layer=layer)
  unlink(shp.path, recursive = TRUE)
  return(data.out)
}

readData.json <- function(viz){
  jsonlite::fromJSON(viz[['location']])
}