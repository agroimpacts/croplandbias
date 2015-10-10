
#' Calculate bias statistics from simple list of rasters of spatial errors
#' @description A variant of bias_stats in which error can be doubly weighted, 
#' singly weighted, or not at all
#' @param ref Raster containing reference weights
#' @param awgts Raster defining area weights, for aggregation
#' @param rerror List of error rasters
#' @param evalnm Name of value encoded in reval's cells (for column header)
#' @param fun Function to evaluate--should be able to handle weights
#' @param rweight Apply reference weighting or not
#' @param aweight Apply area weighting or not
#' @param rnd Rounding value to apply to statistic
#' @param trim_wgt Remove column of weights from output table
#' @return A data.table of output statistics under evalnm
#' @details This can calculate weighted or unweighted statistics
#' @examples 
#' double weighting, proof
#' a <- c(1, 0.5)
#' w1 <- c(0.5, 1) 
#' w2 <- c(1, 0.5)
#' sum(a * (w1 * w2) / sum(w1 * w2)) == mean(a)  # equal to straight mean
#' 
#' # equal to weighted.mean
#' a <- c(1, 0.5)
#' w1 <- c(0.5, 1) 
#' # w2 <- c(1, 1)
#' w2 <- c(2, 2)
#' sum(a * (w1 * w2) / sum(w1 * w2)) == weighted.mean(a, w1)
#' 
#' # toy data
#' ref <- rerror <- awgts <- raster(nrow = 10, ncol = 10)
#' ref[] <- runif(ncell(ref))
#' rerror[] <- runif(ncell(ref))
#' rerror <- rerror - ref
#' names(rerror) <- "rerror"
#' awgts[] <- sample(5:10, size = ncell(ref), replace = TRUE)
#' wm <- function(x, w) stats::weighted.mean(x, w)
#' # no weight by ref, check against raster version
#' bias_statsw(ref, awgts, rerror, fun = wm, evalnm = "rerror", 
#'            rweight = FALSE, rnd = 4) ==
#'  round(weighted.mean(getValues(rerror), getValues(awgts)), 4)
#' 
#' # no weight by area, check against raster version
#' bias_statsw(ref, awgts, rerror, fun = wm, evalnm = "rerror", aweight = FALSE, 
#'             rnd = 4) == 
#'   round(weighted.mean(getValues(rerror), getValues(ref)), 4)
#' 
#' # double weighted, check against raster version
#' bias_statsw(ref, awgts, rerror, fun = wm, evalnm = "rerror", rnd = 4) == 
#'   round(weighted.mean(getValues(rerror), getValues(ref) * getValues(awgts)), 4)
#' 
#' # no weights, check against raster version
#' bias_statsw(ref, awgts, rerror, fun = wm, evalnm = "rerror", rweight = FALSE, 
#'             aweight = FALSE, rnd = 4) == round(cellStats(rerror, mean), 4)
#' 
#' # check behavior with a sum
#' bias_statsw(ref, awgts, rerror, fun = sum, evalnm = "rerror", rweight = FALSE, 
#'             aweight = FALSE, rnd = 4) == 
#'   bias_statsw(ref, awgts, rerror, fun = sum, evalnm = "rerror", rnd = 4) 
#' bias_statsw(ref, awgts, rerror, fun = sum, evalnm = "rerror", rnd = 4) == 
#'   round(cellStats(rerror, sum), 4)
#' @import data.table
#' @export
bias_statsw <- function(ref, awgts, rerror, evalnm, fun, rnd = 2, 
                        rweight = TRUE, aweight = TRUE, trim_wgt = TRUE) {
  if(rweight == FALSE) ref[!is.na(ref)] <- 1  # to turn of ref weights
  if(aweight == FALSE) awgts[!is.na(awgts)] <- 1  # turn off area weights
  if(fun(1:2) == 3) {  # if function is sum, turn off all weights
    ref[!is.na(ref)] <- 0  
    awgts[!is.na(awgts)] <- 0  
  }
  s <- stack(c(ref, awgts, rerror))
  names(s) <- c("ref", "awgts", names(rerror))
  DT <- na.omit(as.data.table.raster(s, xy = TRUE))
  DT[, wgt := ref * awgts]  # calculate weights from ref and area weights
  nms <- c(evalnm, "wgt")
  odt <- round(DT[, lapply(.SD, fun, wgt), .SDcols = nms], rnd)
  if(trim_wgt == TRUE) odt[, wgt := NULL]
  odt
}

#' Calculate weighted bias statistics from list of rasters
#' @param refl 1-level list of reference rasters
#' @param awgtsl 1-level list of area weighting rasters
#' @param rerrorl 2-level list of spatial error rasters 
#' @param evalnm Name of value encoded in reval's cells (for column header)
#' @param fun Function to evaluate--should be able to handle weights
#' @param rnd Rounding value to apply to statistic
#' @param rweight Apply reference weighting or not
#' @param aweight Apply area weighting or not
#' @param rnd Rounding value to apply to statistic
#' @param trim_wgt Remove column of weights from output table
#' @return A list of data.table output statistics
#' @details This function runs the bias_statsw function over a list of rasters
#' @export
bias_statsw_list <- function(refl, awgtsl, rerrorl, evalnm, fun, rnd = 2, 
                        rweight = TRUE, aweight = TRUE, trim_wgt = TRUE, 
                        silent = FALSE) {
  olnames <- names(rerrorl)
  ol <- lapply(olnames, function(x) {
    ref <- refl[[x]]
    awgts <- awgtsl[[x]]
    rerror <- rerrorl[[x]]
    bstats <- bias_statsw(ref, awgts, rerror, evalnm, fun, rnd, rweight, 
                          aweight, trim_wgt)
  })
  named_out(ol, olnames)
  names(ol) <- olnames
  DTO <- cbind("ol" =  olnames, rbindlist(ol))
}
    




