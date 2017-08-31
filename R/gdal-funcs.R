#' @title Run gdal_calc.py
#' 
#' @description Allows gdal_calc.py to be run, with limited options, for arbitrary calculations
#' @param cstr String providing the raster calculation to be performed, e.g. "(A == 1) * 10"
#' @param x a list of rasters, or filenames of rasters, where list element names match names in cstr
#' @param filename output filename to write, with gdal compliant filename extension
#' @param type The gdal data type, e.g. "UInt16" (default), "Byte", etc.
#' @param gdformat The gdal format for output. Default if "GTiff" for geotiff
#' @param overwrite Option to overwrite existing file name. Default is TRUE
#' @return The calculated raster
#' @export
gdal_calc <- function(cstr, x, filename, type = "UInt16", gdformat = "GTiff", overwrite = TRUE) {
  dang <- Sys.time()
  rnm <- function(j) if(is.character(j)) j else if(class(j)[1] == "RasterLayer") j@file@name
  rnms <- paste("-", names(x), " ",  sapply(x, rnm), sep = "", collapse = " ")
  calc_str <- paste("gdal_calc.py ", rnms, " --outfile=", filename, " --type=", type, " --format=", 
                    gdformat, paste(ifelse(overwrite == TRUE, " --overwrite ", " ")), "--calc='", cstr, "'", 
                    sep = "")
  print(paste("Running calculation", cstr))
  system(calc_str)
  print(paste("finished in", Sys.time() - dang))
} 
