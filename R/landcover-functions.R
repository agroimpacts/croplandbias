# Processing functions
#' Aggregates a raster or list of rasters using a vector of factors
#' 
#' @param fact Vector of integers giving factors to aggregate by
#' @param rlist List of rasters to be aggregated
#' @return List of rasters aggregated by specified factors, with original raster or list prepended to it 
aggregate_rast_list <- function(fact, rlist) {
  aggs <- lapply(fact, function(x) {
    print(paste("aggregating by factor of", x))
    rl <- lapply(rlist, function(j) {
      aggregate(j, fact = x)
    })
    names(rl) <- names(rlist)
    return(rl)
  })
  aggs2 <- vector("list", length(fact) + 1)
  names(aggs2) <- paste("f", c(1, fact), sep = "")
  aggs2[[1]] <- rlist
  for(i in 2:length(aggs2)) aggs2[[i]] <- aggs[[i - 1]]
  return(aggs2)
}

#' Takes difference between single raster and each raster in list of rasters, organized by aggregation level
#' 
#' @param ind Vector of integers used to index into first level of raster lists
#' @param subtractor Two level list of rasters, with first level being aggregation level
#' @param subtractee Raster/raster list to difference against, with same structure as subtractor
#' @param whichdiff Index or name determing which raster from rdifflist per factor level will be used to diff
#' @param abs TRUE or FALSE, for whether actual or absolute difference should be taken (default is FALSE)
diff_rast_list <- function(ind, subtractor, subtractee, whichdiff, abs = FALSE) {
  diffs <- lapply(ind, function(x) {
    print(paste("differencing at level", x))
    rl <- lapply(1:length(subtractor[[x]]), function(j) {
      if(abs == TRUE) {
        abs(subtractee[[x]][[whichdiff]] - subtractor[[x]][[j]])
      } else {
        subtractee[[x]][[whichdiff]] - subtractor[[x]][[j]]
      }
    })
    names(rl) <- names(subtractor[[x]])
    return(rl)
  })
  names(diffs) <- names(subtractor)[ind]
  return(diffs)
}

#' Groups the values of one raster into bins that correspond to ranges of values in another raster
#' 
#' @param bin.rast The "master raster", or that which has the values defining the bin ranges
#' @param rast.to.bin The raster whose values will be binned
#' @param bins A vector defining the bin ranges, e.g. seq(0, 100, 10) for bins of 10%
#' @return 
#' @details This requires the two inputs to be of the same extent in order for values to correspond
bin_values <- function(bin.rast, rast.to.bin, bins = binv) {
  binmat <- rbind(bins[-length(bins)], bins[-1])  # create range matrix
  binmat[2, ncol(binmat)] <- binmat[1, 1] + max(binmat) * 0.0001  # so that no values escape conditionals
  br_v <- values(bin.rast) 
  rtb_v <- values(rast.to.bin)
  bindims <- dim(binmat)
  vals <- lapply(1:bindims[2], function(x) {
    ind <- which((br_v >= binmat[1, x]) & (br_v < binmat[2, x]))  # Figure out which bin.rast values are in bins
    binned <- rtb_v[ind]
    cbind("bin" = x, "ind" = ind, "val" = binned)
  })
  lind <- (1:bindims[2])[-which(sapply(vals, function(x) dim(x)[2]) != 3)]  # which list elements have values
  outmat <- do.call(rbind, vals[lind])
  return(outmat)
}

#' Function to run various statistics over different lists produced by bin_value
#' 
#' @param stat.list Output list created by bin_values function
#' @param fun.list List of functions, passed as e.g., list(mean, sd), or list(mean)
#' @param bins A vector defining the bin ranges, e.g. seq(0, 100, 10) for bins of 10%
#' @return 
#' @details List of output tables, one table per level, in a list corresponding to different statistics
bin_stats <- function(stat.list, fun.list, bins) {
  stat_list <- lapply(fun.list, function(FUN) {  # Outer list is function to be applied
    level_list <- lapply(1:length(stat.list), function(x) {  # first inner list on level
      print(paste("processing level", x))
      sat_stats <- lapply(stat.list[[x]], function(j) {  # second inner list on sensor
        stats <- sapply(1:(length(bins) - 1), function(y) {  # stat processed on each sensor within each level
          #o <- sapply(fun.list[[i]], function(FUN) FUN(j[j[, 1] == y, 3], na.rm = TRUE))
          o <- FUN(j[j[, 1] == y, 3], na.rm = TRUE)
        })
      })
      out_stats <- do.call(cbind, sat_stats)
    })
    names(level_list) <- paste("L", 1:length(stat.list), sep = "")
    return(level_list)
  })
  return(stat_list)
}




