#' Transform data.table to raster*
#' @param dt data.table with x, y coordinates
#' @param CRSobj crs object
#' @param filename NULL, else raster* will be written to specified file
#' @return returns a data.table object
#' @export
dt_to_raster <- function(dt, CRSobj, filename = NULL) {
  if(any(!c("x", "y") %in% colnames(dt))) {
    stop("dt needs planar coordinates in x, y format")
  }
  coordinates(dt) = ~x+y
  proj4string(dt) <- CRSobj
  gridded(dt) = TRUE
  if(ncol(dt) > 1) {
    dtr <- brick(dt)
  } else {
    dtr <- brick(dt)
  }
  if(!is.null(filename)) dtr <- writeRaster(dtr, filename)
  return(dtr)
}


