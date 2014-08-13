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
install_github('PrincetonUniversity/lmisc', auth_token = "cbce2de1b9375802ce518dfd9057b5120ffef6a9")
library(lmisc)
source("/u/sandbox/afmap/R/KMLAccuracyFunctions.R")

# Change tempdir to scratch
rasterOptions(tmpdir = "/var/scratch/lestes")
dirPath <- function(path, fname) paste(path, fname, sep = "")  # Directory path function
p.alyze <- "/u/lestes/analyses/SAcropland/data/"

# load sa.r, sa.shp, gti_hort, gti_nohort, globsa_list_sum, modsa_list_sum, glcsa.m, salc1km.m,
load(dirPath(p.alyze, "landcover_objects.rda"))
salc <- salc1km.m * 100

# Plotting colors
cols <- colorRampPalette(c("red", "grey80", "blue4"))
brks <- seq(-100, 100, 10)
ncol <- length(brks) - 1

# Differences
# 1 km
lclist <- c(salc, globsa_list_sum, modsa_list_sum, glcsa.m)
names(lclist) <- c("sa30", "globmin", "globmu", "globmax", "modmin", "modmu", "modmax", "glc-sh")

diff2011.nh <- lapply(lclist, function(x) gti_nohort[[2]] - x)

par(mfrow = c(4, 4), mar = c(0, 0, 0, 0))
for(i in 1:8) {
  plot(sa.shp, lty = 0)
  plot(diff2011.nh[[i]], add = TRUE, col = cols(ncol), breaks = brks, legend = FALSE)
  #plot(sa.shp, add = TRUE)
}

plot(modsa_list_sum[[2]] - modsa_list_sum[[2]])

cellStats(diff2011.nh[[8]], mean)

plot(diff2011.nh[[i]], col = cols(ncol), breaks = brks)
hist(diff2011.nh[[i]])
?raster::plot








