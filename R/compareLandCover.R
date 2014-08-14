##############################################################################################################
# Title      : compareLandCover.R
# Purpose    : Comparing landcover datasets against GTI's
# Author     : Lyndon Estes
# Draws from : 
# Used by    : 
# Notes      : Created 11/8/2014
##############################################################################################################

suppressMessages(library(raster))
suppressMessages(library(rgdal))
# install_github("hadley/devtools")
suppressMessages(library(devtools))
# install_github('PrincetonUniversity/lmisc', auth_token = "cbce2de1b9375802ce518dfd9057b5120ffef6a9")
library(lmisc)
source("/u/sandbox/afmap/R/KMLAccuracyFunctions.R")

# Path variables
rasterOptions(tmpdir = "/var/scratch/lestes")  # Change tempdir to scratch
p.alyze <- "/u/lestes/analyses/SAcropland/data/"
p.fig <- "/u/lestes/analyses/SAcropland/figures/"

# load sa.r, sa.shp, gti_hort, gti_nohort, globsa_list_sum, modsa_list_sum, glcsa.m, salc1km.m,
load(full_path(p.alyze, "landcover_objects.rda"))
salc <- salc1km.m * 100

# Create mean of gtis from different time periods
gti_nohort2 <- c(gti_nohort2, calc(stack(gti_nohort), mean))
names(gti_nohort2) <- c("gti2007", "gti2011", "gtimu")

# Processing functions
#' Aggregates a list of rasters using a vector of factors
#' 
#' @param fact Vector of integers giving factors to aggregate by
#' @param rlist List of rasters to be aggregated
aggregate_rast_list <- function(fact, rlist) {
  aggs <- lapply(fact, function(x) {
    print(paste("aggregating by factor of", x))
    rl <- lapply(rlist, function(j) {
      aggregate(j, fact = x)
    })
    names(rl) <- names(rlist)
    return(rl)
  })
  names(aggs) <- paste("f", fact, sep = "")
  return(aggs)
}

#' Takes difference between single raster and each raster in list of rasters, organized by aggregation level
#' 
#' @param fact Vector of integers used to aggregate raster list, here providing list index
#' @param rlist Two level list of rasters, with first level being aggregation level
#' @param rdifflist Raster list to difference against, with same structure as rlist
#' @param whichdiff Index or name determing which raster from rdifflist per factor level will be used to diff
#' @param abs TRUE or FALSE, for whether actual or absolute difference should be taken (default is FALSE)
diff_rast_list <- function(fact, raster.list, diff.raster, whichdiff, abs = FALSE) {
  diffs <- lapply(1:length(fact), function(x) {
    print(paste("differencing aggregates at factor of", fact[x]))
    rl <- lapply(1:length(rlist[[x]]), function(j) {
      if(abs == TRUE) {
        abs(rdifflist[[x]][[whichdiff]] - rlist[[x]][[j]])
      } else {
        rdifflist[[x]][[whichdiff]] - rlist[[x]][[j]]
      }
    })
    names(rl) <- names(rlist[[x]])
    return(rl)
  })
  names(diffs) <- paste("f", fact, sep = "")
  return(diffs)
}

# Aggregate raster
fact <- c(5, 10, 20, 50, 100, 200, 300, 600)
lc_agg <- aggregate_rast_list(fact, lclist)
lc_agg2 <- vector("list", length(fact) + 1)
names(lc_agg2) <- paste("f", c(1, fact), sep = "")
lc_agg2[[1]] <- lclist
for(i in 2:length(lc_agg2)) lc_agg2[[i]] <- lc_agg[[i - 1]]
rm(lc_agg)

gti_nh_agg <- aggregate_rast_list(fact, gti_nohort2)
gti_nh_agg2 <- vector("list", length(fact) + 1)
names(gti_nh_agg2) <- paste("f", c(1, fact), sep = "")
gti_nh_agg2[[1]] <- gti_nohort2
for(i in 2:length(gti_nh_agg2)) gti_nh_agg2[[i]] <- gti_nh_agg[[i - 1]]
rm(gti_nh_agg)


# Differences
# 1 km
lclist <- c(salc, globsa_list_sum, modsa_list_sum, glcsa.m)
names(lclist) <- c("sa30", "globmin", "globmu", "globmax", "modmin", "modmu", "modmax", "glcsh")
diff2011.nh <- lapply(lclist, function(x) gti_nohort2[[2]] - x)

# Figure 1
# Plotting colors
cols <- colorRampPalette(c("red", "grey80", "blue4"))
brks <- seq(-100, 100, 10)
ncol <- length(brks) - 1
legtext <- "% Difference"
cx <- 1.5
lcol <- "black"
mcap <- c("SA Landcover", "GlobCover min", "GlobCover mean", "GlobCover max", "MODIS min", "MODIS mean", 
          "MODIS max", "GLC-Share")

pdf(full_path(p.fig, "Figure1.pdf"), height = 12, width = 8)
par(mfrow = c(4, 2), mar = c(0, 0, 2, 0), oma = c(7, 0, 0, 0))
for(i in 1:8) {
  plot(sa.shp, lty = 0)
  plot(diff2011.nh[[i]], add = TRUE, col = cols(ncol), breaks = brks, legend = FALSE)
  mtext(mcap[i], side = 3, cex = cx)
}
flex_legend(ncuts = length(brks) - 1, legend.text = legtext, legend.vals = brks, 
            longdims = c(0.2, 0.8), shortdims = c(0.04, 0.01), colvec = cols(length(brks) - 1), 
            srt = c(270, 0), horiz = TRUE, textside = "bottom", legend.pos = c(4, 10), 
            leg.adj = list(c(0.25, 0), c(0.5, -0.5)), cex.val = cx, textcol = lcol, bordercol = lcol)
dev.off()

# Process difference at different resolutions
# Landcover lists
fact <- c(5, 10, 20, 50, 100, 200, 300, 600)
lclist_agg <- lapply(fact, function(x) {
  print(paste("aggregating by factor of", x))
  rl <- lapply(lclist, function(j) {
    aggregate(j, fact = x)
  })
  names(rl) <- names(lclist)
  return(rl)
})
names(lclist_agg) <- paste("km", fact, sep = "")
  
# GTI set
gti_nohort_agg <- lapply(fact, function(x) {
  print(paste("aggregating by factor of", x))
  rl <- lapply(gti_nohort2, function(j) {
    aggregate(j, fact = x)
  })
  names(rl) <- names(gti_nohort2)
  return(rl)
})
names(gti_nohort_agg) <- paste("km", fact, sep = "")

# Difference between the two
diff2011_agg <- lapply(1:length(fact), function(x) {
  print(paste("differencing aggregates at factor of", fact[x]))
  rl <- lapply(1:length(lclist_agg[[x]]), function(j) {
    gti_nohort_agg[[x]]$gti2011 - lclist_agg[[x]][[j]]
  })
  names(rl) <- names(lclist_agg[[x]])
  return(rl)
})
names(diff2011_agg) <- paste("km", fact, sep = "")

absdiff2011_agg <- lapply(1:length(fact), function(x) {
  print(paste("differencing aggregates at factor of", fact[x]))
  rl <- lapply(1:length(lclist_agg[[x]]), function(j) {
    abs(gti_nohort_agg[[x]]$gti2011 - lclist_agg[[x]][[j]])
  })
  names(rl) <- names(lclist_agg[[x]])
  return(rl)
})
names(absdiff2011_agg) <- paste("km", fact, sep = "")

k <- 4
qv <- values(gti_nohort2$gti2011)
qt <- quantile(qv, seq(0, 1, 0.05), na.rm = TRUE)
qt <- seq(0, 100, 5)
qcl <- rbind(qt[-length(qt)], qt[-1])
vals <- lapply(1:ncol(qcl), function(x) {
  ind <- which((qv >= qcl[1, x]) & (qv < qcl[2, x]))
  v <- values(diff2011.nh$globmu) 
  vv <- v[ind]
})

plot(1:length(vals), sapply(vals, mean))
plot(gti_nohort2$gti2011, diff2011.nh$globmu)


plot(values(gti_nohort_agg[[k]]$gti2011), values(diff2011_agg[[k]]$globmu))
plot(gti_nohort_agg[[k]]$gti2011, diff2011_agg[[k]]$globmu, add = TRUE, col = "red")








