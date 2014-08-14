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



