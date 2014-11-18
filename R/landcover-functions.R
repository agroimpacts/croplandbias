#' Block processing function to assign new values to different layers in brick
#' 
#' @param x Brick with layers containing binary landcover classes
#' @param y Vector containing the values to assign to each layer in brick (thus one element per layer)
#' @param fname Output file name for reclassified brick
#' @return Raster with altered values
#' @export
assignLCPct <- function(x, y, fname) {
  if(length(y) != nlayers(x)) stop("Vector must have same length as number of layers")
  out <- brick(x, values=FALSE)
  out <- writeStart(out, filename=fname)
  bs <- blockSize(x)
  for (i in 1:bs$n) {
    print(paste("Block", i, "of", bs$n))
    v <- getValues(x, row=bs$row[i], nrows=bs$nrows[i])
    v <- t(t(v) * y)
    out <- writeValues(out, v, bs$row[i])
  }
  out <- writeStop(out)
  return(out)
}

#' Aggregates a raster or list of rasters using a vector of factors
#' 
#' @param fact Vector of integers giving factors to aggregate by
#' @param rlist List of rasters to be aggregated
#' @return List of rasters aggregated by specified factors, with original raster or list prepended to it 
#' @export
aggregate_rast_list <- function(fact, rlist, fun = mean) {
  aggs <- lapply(fact, function(x) {
    print(paste("aggregating by factor of", x))
    rl <- lapply(rlist, function(j) {
      aggregate(j, fact = x, fun = fun)
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

#' @title Apply math on two lists
#' @description Apply arbitrary math to two objects within two lists 
#' @param ilist A 3 element list of list indices (names or index numbers). See details. 
#' @param list1 A two-level list
#' @param list2 A two-level list
#' @param expr A character vector describing the calculation between each list element, e.g. "a - b"
#' @param silent TRUE (default) or FALSE, to print (or not) which level of list is being operated on
#' @details This allows aribtrary operations to be performed on lists with two levels. The expression that 
#' is passed to the function requires that "a" reference the element within list1, and "b" the element within 
#' list2 that has the operation applied to it. As written, this performs operations on nested loops (lapply), 
#' with the outer driven by the index in ilist[[1]], the second level driven by ilist[[2]], and the innermost
#' by ilist[[3]]. The innermost loop performs the operation specified by eval on each list element, where 
#' a = list1[[ilist[[2]]]][[ilist[[3]]]], and b = list2[[ilist[[2]]]][[ilist[[1]]]]. The expression must 
#' therefore always have a and b as its variables. Input lists should be equal on their first dimensions. If 
#' one can understand the complex looping structure, it should be possible to shape lists and indices to use
#' the function differently 
#' @examples
#' library(raster)
#' list1 <- lapply(1:5, function(x) lapply(1:2, function(j) {
#'   r <- raster(nrow = 10, ncol = 10)
#'   r[] <- sample(1:20, size = ncell(r), replace = TRUE)
#'   r
#' }))
#' list2 <- lapply(1:5, function(x) lapply(1:6, function(j) {
#'   r <- raster(nrow = 10, ncol = 10)
#'   r[] <- sample(1:2, size = ncell(r), replace = TRUE)
#'   r
#' }))
#' o <- rast_list_math(ilist = list(1:6, 1:5, 1:2), list1 = list1, list2 = list2, expr = "a * b", 
#'                     silent = FALSE)
#' plot(o[[1]][[2]][[1]])
#' plot(o[[5]][[2]][[1]])
#' @export
rast_list_math <- function(ilist, list1, list2, expr, silent = TRUE) {
  l1 <- lapply(ilist[[1]], function(x) {
    if(silent == FALSE) print(paste("outer:", x))
    l2 <- lapply(ilist[[2]], function(j) {
      if(silent == FALSE) print(paste("  middle:", j))
      l3 <- lapply(ilist[[3]], function(k) {
        a <- list1[[j]][[k]] 
        b <- list2[[j]][[x]]
        if(silent == FALSE) print(paste("    inner:", k))
        r <- eval(parse(text = expr))
      })
      names(l3) <- ilist[[3]]
      l3
    })
    names(l2) <- ilist[[2]]
    l2
  })
  names(l1) <- ilist[[1]]
  return(l1)
}

# #' Groups the values of one raster into bins that correspond to ranges of values in another raster
# #' 
# #' @param bin.rast The "master raster", or that which has the values defining the bin ranges
# #' @param rast.to.bin The raster whose values will be binned
# #' @param bins A vector defining the bin ranges, e.g. seq(0, 100, 10) for bins of 10\%
# #' @return List of differences between rasters
# #' @details This requires the two inputs to be of the same extent in order for values to correspond 
# #' @export
# bin_values <- function(bin.rast, rast.to.bin, bins) {
#   binmat <- rbind(bins[-length(bins)], bins[-1])  # create range matrix
#   binmat[2, ncol(binmat)] <- binmat[1, 1] + max(binmat) * 0.0001  # so that no values escape conditionals
#   br_v <- values(bin.rast) 
#   rtb_v <- values(rast.to.bin)
#   bindims <- dim(binmat)
#   vals <- lapply(1:bindims[2], function(x) {
#     ind <- which((br_v >= binmat[1, x]) & (br_v < binmat[2, x]))  # which bin.rast values are in bins
#     binned <- rtb_v[ind]
#     cbind("bin" = x, "ind" = ind, "val" = binned)
#   })
#   lind <- (1:bindims[2])[-which(sapply(vals, function(x) dim(x)[2]) != 3)]  # which list elements have values
#   outmat <- do.call(rbind, vals[lind])
#   return(outmat)
# }
# 
# #' Applies bin_values to list
# #' 
# #' @param bin.rast The "master raster", or that which has the values defining the bin ranges
# #' @param which.rast Index of raster in bin.rast level 2 on which to do the binning
# #' @param rast.to.bin The raster whose values will be binned
# #' @param bins A vector defining the bin ranges, e.g. seq(0, 100, 10) for bins of 10\%
# #' @param filename A filename without any extension. Defaults to null, but if provided writes results to rda  
# #' @return List of values extracted from rasters corresponding to different bins, in memory or written to disk 
# #' @details This requires the two inputs to be of the same extent in order for values to correspond. The list 
# #' will not be returned is a filename is specified, which is a good options for saving disk space. 
# #' @export
# extract_bin_values <- function(bin.rast, which.rast, rast.to.bin, bins, filename = "none") {
#   bin_val_list <- lapply(1:length(bin.rast), function(x) {
#     out <- lapply(rast.to.bin[[x]], function(j) b <- bin_values(bin.rast[[x]][[which.rast]], j, bins))
#     names(out) <- names(rast.to.bin[[x]])
#     return(out)
#   })
#   if(filename != "none") {
#     save(bin_val_list, file = paste(filename, ".rda", sep = ""))
#     rm(bin_val_list)
#   } else {
#     return(bin_val_list)
#   }
# } 
# 
# #' Function to run various statistics over different lists produced by bin_value
# #'  
# #' @param stat.list Output list created by bin_values function
# #' @param fun.list List of functions, passed as e.g. list(mean, sd), or list(mean)
# #' @param bins A vector defining the bin ranges, e.g. seq(0, 100, 10) for bins of 10\%
# #' @return List of specified statistics applied per bin value
# #' @details List of output tables, one table per level, in a list corresponding to different statistics  
# #' @export
# bin_stats <- function(stat.list, fun.list, bins) {
#   stat_list <- lapply(fun.list, function(FUN) {  # Outer list is function to be applied
#     level_list <- lapply(1:length(stat.list), function(x) {  # first inner list on level
#       print(paste("processing level", x))
#       sat_stats <- lapply(stat.list[[x]], function(j) {  # second inner list on sensor
#         stats <- sapply(1:(length(bins) - 1), function(y) {  # stat processed on each sensor within each level
#           #o <- sapply(fun.list[[i]], function(FUN) FUN(j[j[, 1] == y, 3], na.rm = TRUE))
#           o <- FUN(j[j[, 1] == y, 3], na.rm = TRUE)
#         })
#       })
#       out_stats <- do.call(cbind, sat_stats)
#     })
#     names(level_list) <- paste("L", 1:length(stat.list), sep = "")
#     return(level_list)
#   })
#   return(stat_list)
# }
# 
# #' Function to reshape list output created by extract_bin_values
# #' 
# #' @param diff.list Output list created by extract_bin_values function
# #' @param sens.ind Index vector, named or numbered, for sensors (lowest level of list)
# #' @param level.vec A vector defining the number of aggregation levels (second level of list)
# #' @param bin.len The total number of bins used in extract_bin_values
# #' @return A reshaped list, by sensor, factor, then bin
# #' @details This function assumes that the binned differences for each sensor were calculated from multiple
# #' difference permutations, e.g. there should be three elements of the binned difference list, one created by 
# #' the 2011 GTI dataset, one by the mean of the 2007 and 2011 datasets, and one from the 2007 set.   
# #' @export
# reshape_diff_list <- function(diff.list, sens.ind, level.vec, bin.len) {
#   resh <- lapply(sens.ind, function(v) {
#     print(v)
#     agg <- lapply(1:length(level.vec), function(x) {
#       bin_pool <- lapply(1:bin.len, function(z) {  # apply across bins
#         dat <- do.call(rbind, lapply(1:length(diff.list), function(y) diff.list[[y]][[x]][[v]]))
#         v  <- dat[dat[, "bin"] == z, "val"]  # pool bias values
#       })
#       names(bin_pool) <- paste("b", 1:bin.len, sep = "")
#       return(bin_pool)
#     })
#     names(agg) <- level.vec
#     return(agg)
#   })
#   names(resh) <- sens.ind
#   return(resh)
# }
# 
# #' Function to reshape list output created by reshape_diff_list, in this case to do one further pooling
# #' 
# #' @param resh.diff.list Output list created by reshape_diff_list
# #' @param sens.pat Index vector of character patterns for the difference bins that need to be further pooled
# #' @param level.vec A vector defining the number of aggregation levels (second level of list)
# #' @param bin.len The total number of bins used in extract_bin_values
# #' @return A reshaped list, by sensor, factor, then bin
# #' @details This function was created to do one further level of pooling of the binned differences resulting 
# #' from the MODIS and GlobCover high, medium, and low variants. It assumes that each level of the list is 
# #' named.  
# #' @export
# reshape_reshape_list <- function(resh.diff.list, sens.pat, level.vec, bin.len) {
#   resh <- lapply(sens.pat, function(pat) {
#     print(pat)
#     resh_comb <- lapply(1:length(level.vec), function(x) {
#       bin_pool <- lapply(1:bin.len, function(y) {
#         unlist(lapply(grep(pat, names(resh.diff.list)), function(z) resh.diff.list[[z]][[x]][[y]]))
#       })
#       names(bin_pool) <- paste("b", 1:bin.len, sep = "")
#       return(bin_pool)
#     })
#     names(resh_comb) <- level.vec
#     return(resh_comb)
#   })
#   names(resh) <- sens.pat
#   return(resh)
# }







