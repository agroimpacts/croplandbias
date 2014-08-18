##############################################################################################################
# Title      : compare-land-cover.R
# Purpose    : Comparing landcover datasets against GTI's
# Author     : Lyndon Estes
# Draws from : 
# Used by    : 
# Notes      : Created 11/8/2014
##############################################################################################################

library(raster)
library(rgdal)
if(any(c("lmisc", "devtools") %in% rownames(installed.packages()) == FALSE)) {
  install.packages("devtools")
  library(devtools)
  install_github("hadley/devtools")  # updates to most recent
  install_github('PrincetonUniversity/lmisc', auth_token = "cbce2de1b9375802ce518dfd9057b5120ffef6a9")
}
library(lmisc)
source("R/landcover-functions.R")

# Path variables
p_root <- proj_root("SAcropland")
# rasterOptions(tmpdir = "/var/scratch/lestes")  # Change tempdir to scratch (if on server)
p_data <- full_path(p_root, "SAcropland/data/")


# Load sa.shp, gti_hort, gti_nohort, globsa_list_sum, modsa_list_sum, glcsa.m, salc1km.m,
# Landcover sets
salc <- raster(full_path(p_data, "salc_ag_masked.tif")) * 100
globsa_list_sum <- lapply(full_path(p_data, dir(p_data, "globSA*.*1kmrect_sum.tif")[c(2:3, 1)]), raster)
modsa_list_sum <- lapply(full_path(p_data, dir(p_data, "mod_1*.*1kmrect_sum.tif")[c(2:3, 1)]), raster)
glcsa <- raster(full_path(p_data, "glcsa_masked.tif"))
lclist <- c(salc, globsa_list_sum, modsa_list_sum, glcsa)  # into list
names(lclist) <- c("sa30", "globmin", "globmu", "globmax", "modmin", "modmu", "modmax", "glcsh")

# Other set
load(full_path(p_data, "MZshapes.Rdata"))  
gti_nohort <- lapply(full_path(p_data, dir(p_data, "cover*.*sum_nh.tif")), raster)
gti_hort <- lapply(full_path(p_data, dir(p_data, "cover*.*sum_h.tif")), raster)

# Create aggregates and differences (no need to redo if already done)
if(!file.exists(full_path(p_data, "difference_grids.rda"))) {  # do if not done
  # Create mean of gtis from different time periods
  gti_nohort2 <- c(gti_nohort, calc(stack(gti_nohort), mean))
  names(gti_nohort2) <- c("gti2007", "gti2011", "gtimu")
  
  # Aggregate rasters
  fact <- c(5, 10, 20, 50, 100, 200, 300, 600)
  lc_agg <- aggregate_rast_list(fact, lclist)   # landcover rasters 
  gti_nh_agg <- aggregate_rast_list(fact, gti_nohort2)  # GTI rasters
  
  # Differences
  # Actual 
  diff2011_act <- diff_rast_list(1:length(lc_agg), subtractor = lc_agg, subtractee = gti_nh_agg, whichdiff = 2)
  diff2011_abs <- diff_rast_list(1:length(lc_agg), subtractor = lc_agg, subtractee = gti_nh_agg, whichdiff = 2, 
                                 abs = TRUE)
  save(diff2011_act, diff2011_abs, lc_agg, gti_nh_agg, file = full_path(p_data, "difference_grids.rda"))
} else {
  load(full_path(p_data, "difference_grids.rda"))  # load if already done
}  

# Process difference at different resolutions
binv <- seq(0, 100, 5)  # bins
# qt <- quantile(seq(0, 1, 0.05), na.rm = TRUE)
act_diff_2011_bin <- lapply(1:length(gti_nh_agg), function(x) {
  out <- lapply(diff2011_act[[x]], function(j) b <- bin_values(gti_nh_agg[[x]]$gti2011, j, binv))
  names(out) <- names(diff2011_act[[x]])
  out
})
abs_diff_2011_bin <- lapply(1:length(gti_nh_agg), function(x) {
  out <- lapply(diff2011_abs[[x]], function(j) b <- bin_values(gti_nh_agg[[x]]$gti2011, j, binv))
  names(out) <- names(diff2011_abs[[x]])
  out
})

# Calculate statistics from them
act_diff_2011_stats <- bin_stats(act_diff_2011_bin, list("mu" = mean, "sd" = sd), binv)
abs_diff_2011_stats <- bin_stats(abs_diff_2011_bin, list("mu" = mean, "sd" = sd), binv)

# Country-scale means as well, to get country-scale bias
sa_mu <- cellStats(gti_nh_agg$f1$gti2011, mean)
lc_sa_diff_act <- lapply(lclist, function(x) sa_mu - cellStats(x, mean))
lc_sa_diff_abs <- lapply(lclist, function(x) abs(sa_mu - cellStats(x, mean)))

save(sa_mu, lc_sa_diff_act, lc_sa_diff_abs, act_diff_2011_stats, abs_diff_2011_stats, 
     file = full_path(p_data, "difference_stats.rda"))


