process.precip_colors <- function(viz){
  library(RColorBrewer)
  library(jsonlite)
  cols <- RColorBrewer::brewer.pal(viz[['bins']], viz[['pallete']])
  json <- lapply(1:viz[['bins']], function(x) {
    out <- list(cols[x])
    names(out) <- paste0('bin-', x)
    return(out)
    })
  cat(jsonlite::toJSON(json), file = viz[['location']])
}