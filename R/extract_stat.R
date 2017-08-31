#' Extract specific statistics from bias_stats data.table
#' @param ols vector with value(s) to subset from outer list (ol) variable
#' @param ils vector with value(s) to subset from inner list (il) variable
#' @param bins vector naming the statistical aggregation level to subset
#' @param stat statistic to subset
#' @param val Column header containing the value of interest (N or bias stat)
#' @param dt Name of the bias_stat produced data.table
#' @param type "density" for normal mode, "nodensity" for density independent
#' @return Subset of statistics from bias_stat data.table
#' @details The type parameter allows for density independent and density 
#' dependent statistics to be extracted. If density independent, the mean is 
#' calculated as the mean of bin-wise means.  
#' @export
extract_stat <- function(ols, ils, bins, stat, val, dt, type = "density") {
  ii <- rbindlist(lapply(ols, function(i) {
    jj <- rbindlist(lapply(ils, function(j) {
      if(type == "density") {
        v <- dt[ol == i & il == j & bin == bins & bvals == stat, val, 
                with = FALSE]
      } else if(type == "nodensity") {
        v <- dt[ol == i & il == j & bin != "all" & bvals == stat, 
                lapply(.SD, mean), .SDcols = val]
      }
      if(nrow(v) == 0) v <- rbindlist(list(v, as.list(NA)))
      vd <- data.table(v)
    }))
    jjdt <- cbind("ol" = i, "il" = ils, jj)
  }))
  setkey(ii, "ol")
  return(ii)
}