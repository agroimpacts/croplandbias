#' Calculate weighted bias statistics from raster
#' @param rbins Raster of bins within which statistic should be calculated
#' @param rwgts Raster defining weighting of pixel towards calculation of stat
#' @param rmask Binary masking raster 
#' @param reval Raster containing the bias statistics to evaluate
#' @param fun Function to evaluate--should be able to handle weights
#' @param stnm Name of statistic (for output column header)
#' @param evalnm Name of value encoded in reval's cells (for column header)
#' @param weighted TRUE or FALSE, whether statistic is weighted or not
#' @param rnd Rounding value to apply to statistic
#' @param trim_wgt Remove column of weights from output table
#' @return A data.table of output statistics
#' @details The statistic of interest is calculated across i) the entire masked 
#' in area of the raster, ii) the entire non-NA part of the raster (masked in 
#' and masked-out), iii) within each bin in the masked in-area, iv) within each 
#' bin for the masked and un-masked area. There are thus two output statistics 
#' for each category (whole raster, and for each bin), the second of which is 
#' indicated by the suffix ".nm", meaning it was calculated for all non-NA areas.
#' If an unweighted statistic is required, then for a mean value rwgts should be
#' a raster of zeros
#' @import data.table
#' @export
bias_stats <- function(rbins, rwgts, rmask, reval, fun, stnm, evalnm, weighted, 
                       rnd = 2, trim_wgt = TRUE) {
  # stack raster bins, cropland bins, and landcover set
  if(fun(1:2) == 3) rwgts[] <- 0  # rwgts to 0 for sum
  if(weighted == FALSE & fun(1:2) == 1.5) rwgts[] <- 1  # rwgts = 1 for mean   
  s <- stack(list(rbins, rwgts, rmask, reval))
  names(s) <- c("bin", "wgt", "mask", evalnm)
  DT <- na.omit(as.data.table.raster(s, xy = TRUE))
  setkey(DT, "bin")
  fr1 <- data.table("bvals" = c(stnm, paste0(stnm, ".nm")))
  fr2 <- data.table("bin" = "all", "N" = nrow(DT[mask == 1, ]))  # unmasked N
  fr3 <- data.table("bin" = "all", "N" = nrow(DT)) # masked N
  binlnm <- DT[, .N, by = bin]  # n obs per bin, all non-NAs
  binl <- DT[mask == 1, .N, by = bin]  # n unmasked obs per bin
  nms <- c(evalnm, "wgt")
  a <- list(round(DT[mask == 1, lapply(.SD, fun, wgt), .SDcols = nms], rnd), 
            round(DT[, lapply(.SD, fun, wgt), .SDcols = nms], rnd))
  dtl <- list(DT[mask == 1, lapply(.SD, fun, wgt), by = bin, .SDcols = nms],
              DT[, lapply(.SD, fun, wgt), by = bin, .SDcols = nms])
  b <- lapply(1:length(dtl), function(x) {
    dto <- cbind(fr1[x], round(dtl[[x]], rnd))
    setkey(dto, "bin")
    dto
  })  
  odt <- rbind(cbind(fr1, rbind(cbind(rbind(fr2, fr3), rbindlist(a)))), 
               rbind(binl[b[[1]]], binlnm[b[[2]]]))
  if(trim_wgt == TRUE) odt[, wgt := NULL]
  return(odt)
}

#' Calculate weighted bias statistics from list of rasters
#' @param rbinsl 1-level list of binning rasters
#' @param rwgtsl 1-level list of rasters defining pixel weightings
#' @param rmaskl 2-level list of binary masking rasters 
#' @param revall 2-level list of raster containing bias statistics to evaluate
#' @param fun Function to evaluate--should be able to handle weights
#' @param stnm Name of statistic (for output column header)
#' @param evalnm Name of value encoded in reval's cells (for column header)
#' @param weighted TRUE or FALSE, whether statistic is weighted or not
#' @param rnd Rounding value to apply to statistic
#' @param trim_wgt Remove column of weights from output table
#' @return A list of data.table of output statistics
#' @details This function runs the bias_stats function over a list of rasters
#' @export
bias_stats_list <- function(rbinsl, rwgtsl, rmaskl, revall, fun, stnm, evalnm, 
                            weighted, rnd = 2, trim_wgt = TRUE, silent = FALSE){

  if(is.null(names(rbinsl))) olnames <- 1:length(rbinsl)
  if(!is.null(names(rbinsl))) olnames <- names(rbinsl)
  if(is.null(names(revall))) ilnames <- 1:length(revall[[1]])
  if(!is.null(names(revall))) ilnames <- names(revall[[1]])

    # Reshape list if needed
  if(any(olnames %in% ilnames) & all(names(revall[[1]]) %in% olnames)) {
    ilnames <- names(revall)
    revall <- lapply(olnames, function(x) {
      named_out(lapply(ilnames, function(y) revall[[y]][[x]][[1]]), ilnames)
    })
    names(revall) <- olnames
  }
  ol <- lapply(olnames, function(x) {  # level
    if(silent == FALSE) print(paste(".", x))
    il <- lapply(ilnames, function(y) {
      if(silent == FALSE) print(paste("...", y))
      reval <- revall[[x]][[y]][[1]]  # hack in case there is a 3rd level
      rmask <- rmaskl[[x]][[y]]
      rwgts <- rwgtsl[[x]]
      rbins <- rbinsl[[x]]
      bstats <- bias_stats(rbins = rbins, rwgts = rwgts, rmask = rmask, 
                         reval = reval, fun = fun, stnm = stnm, evalnm = evalnm,
                         weighted = weighted, rnd = rnd, trim_wgt = trim_wgt)
    })
    named_out(il, ilnames)
  })
  names(ol) <- olnames
  DTO <- rbindlist(lapply(olnames, function(i) {
    cbind("ol" = i, rbindlist(lapply(ilnames, function(j) {
      cbind("il" = j, ol[[i]][[j]])
    })))
  }))
  return(DTO)
}


