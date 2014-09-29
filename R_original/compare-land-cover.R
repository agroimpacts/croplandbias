##############################################################################################################
# Title      : compare-land-cover.R
# Purpose    : Differencing landcover datasets against GTI at different resolutions
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

# Load globsa_list_sum, modsa_list_sum, glcsa.m, salc1km.m,
# Landcover sets
salc <- raster(full_path(p_data, "salc_ag_masked.tif")) * 100
globsa_list_sum <- lapply(full_path(p_data, dir(p_data, "globSA*.*1kmrect_sum.tif")[c(2:3, 1)]), raster)
modsa_list_sum <- lapply(full_path(p_data, dir(p_data, "mod_1*.*1kmrect_sum.tif")[c(2:3, 1)]), raster)
glcsa <- raster(full_path(p_data, "glcsa_masked.tif"))
lclist <- c(salc, globsa_list_sum, modsa_list_sum, glcsa)  # into list
names(lclist) <- c("sa30", "globmin", "globmu", "globmax", "modmin", "modmu", "modmax", "glcsh")

# Other set
load(full_path(p_data, "MZshapes.Rdata"))  
paths <- full_path(p_data, dir(p_data, paste("cover*.*sum_nh_[subs|stand]*", sep = "")))[c(2, 4, 1, 3)]
gti_nohort <- lapply(paths, raster)
paths <- full_path(p_data, dir(p_data, paste("cover*.*sum_h_[subs|stand]*", sep = "")))[c(2, 4, 1, 3)]
gti_hort <- lapply(paths, raster)

# Create aggregates and differences (no need to redo if already done)
if(!file.exists(full_path(p_data, "difference_grids.rda"))) {  # do if not done
  # Create mean of gtis from different time periods
  #gti_nohort2 <- c(gti_nohort, calc(stack(gti_nohort), mean))
  gti_nohort2 <- c(gti_nohort, calc(stack(gti_nohort[1:2]), mean), calc(stack(gti_nohort[3:4]), mean))
  names(gti_nohort2) <- c("gti2007", "gti2011", "gti2007_ns", "gti2011_ns", "gtimu", "gtimu_ns")
  
  # Aggregate rasters
  fact <- c(5, 10, 20, 50, 100, 200, 300, 600)
  lc_agg <- aggregate_rast_list(fact, lclist)   # landcover rasters 
  gti_nh_agg <- aggregate_rast_list(fact, gti_nohort2)  # GTI rasters
      
  # Differences
  # Actual
  diff_list_act <- lapply(1:6, function(x) {
    print(paste("processing", x))
    diffr <- diff_rast_list(1:length(lc_agg), subtractor = lc_agg, subtractee = gti_nh_agg, whichdiff = x)
  })
  names(diff_list_act) <- c("d2007", "d2007_ns", "d2011", "d2011_ns", "dmu", "dmu_ns")  
  #diff2007_act <- diff_rast_list(1:length(lc_agg), subtractor = lc_agg, subtractee = gti_nh_agg, whichdiff = 1)
  #diff2007_ns_act <- diff_rast_list(1:length(lc_agg), lc_agg, gti_nh_agg, 3)
  #diff2011_act <- diff_rast_list(1:length(lc_agg), subtractor = lc_agg, subtractee = gti_nh_agg, 2)
  #diff2011_ns_act <- diff_rast_list(1:length(lc_agg), lc_agg, gti_nh_agg, 4)
  #diffmu_act <- diff_rast_list(1:length(lc_agg), lc_agg, gti_nh_agg, 5)
  #diffmu_ns_act <- diff_rast_list(1:length(lc_agg), lc_agg, gti_nh_agg, 6)

  # Absolute
  diff_list_abs <- lapply(1:6, function(x) {
    print(paste("processing", x))
    diffr <- diff_rast_list(1:length(lc_agg), subtractor = lc_agg, subtractee = gti_nh_agg, whichdiff = x, 
                            abs = TRUE)
  })
  names(diff_list_abs) <- c("d2007", "d2007_ns", "d2011", "d2011_ns", "dmu", "dmu_ns")  
  #diff2007_abs <- diff_rast_list(1:length(lc_agg), lc_agg, gti_nh_agg, 1, abs = TRUE)
  #diff2007_ns_act <- diff_rast_list(1:length(lc_agg), lc_agg, gti_nh_agg, 3, abs = TRUE)
  #diff2011_abs <- diff_rast_list(1:length(lc_agg), lc_agg, gti_nh_agg, 2, abs = TRUE)
  #diff2011_ns_abs <- diff_rast_list(1:length(lc_agg), lc_agg, gti_nh_agg, 4, abs = TRUE)
  #diffmu_abs <- diff_rast_list(1:length(lc_agg), lc_agg, gti_nh_agg, 5, abs = TRUE)
  #diffmu_ns_abs <- diff_rast_list(1:length(lc_agg), lc_agg, gti_nh_agg, 6, abs = TRUE)
  
  # Between 2007 and 2011 GTI
  diff_list_gti <- diff_rast_list(1:length(gti_nh_agg), gti_nh_agg, gti_nh_agg, 4)
  
  #save(diff2011_act, diff2011_ns_act, diffmu_act, diffmu_ns_act, diff2011_abs, diff2011_ns_abs, diffmu_abs, 
  #     diffmu_ns_abs, lc_agg, gti_nh_agg, file = full_path(p_data, "difference_grids.rda"))
  save(diff_list_act, diff_list_abs, diff_list_gti, file = full_path(p_data, "difference_grid_lists.rda"))
} else {
  load(full_path(p_data, "difference_grids.rda"))  # load if already done
}  

# Process difference at different resolutions
binv <- seq(0, 100, 5)  # bins

# GTI versus GTI (to measure difference in the different permutations of GTI)
gti_diff_bin <- extract_bin_values(gti_nh_agg, "gti2011_ns", diff_list_gti, binv) 

# Actual landcover differences relative to GTI
act_2011_bin <- extract_bin_values(gti_nh_agg, "gti2011", diff2011_act, binv)
act_2011_ns_bin <- extract_bin_values(gti_nh_agg, "gti2011_ns", diff2011_ns_act, binv)
act_mu_bin <- extract_bin_values(gti_nh_agg, "gtimu", diffmu_act, binv)
act_mu_ns_bin <- extract_bin_values(gti_nh_agg, "gtimu_ns", diffmu_ns_act, binv)

# Absolute landcover differences relative to GTI
abs_2011_bin <- extract_bin_values(gti_nh_agg, "gti2011", diff2011_abs, binv)
abs_2011_ns_bin <- extract_bin_values(gti_nh_agg, "gti2011_ns", diff2011_ns_abs, binv)
abs_mu_bin <- extract_bin_values(gti_nh_agg, "gtimu", diffmu_abs, binv)
abs_mu_ns_bin <- extract_bin_values(gti_nh_agg, "gtimu_ns", diffmu_ns_abs, binv)

# Save to make space
lapply(list("act_2011_bin", "act_2011_ns_bin", "act_mu_bin", "act_mu_ns_bin", 
            "abs_2011_bin", "abs_2011_ns_bin", "abs_mu_bin", "abs_mu_ns_bin"), function(x) {
        l <- get(x)      
        save(l, file = full_path(p_data, paste(x, ".rda", sep = "")))      
})
rm(diff2011_act, diff2011_ns_act, diffmu_act, diffmu_ns_act, diff2011_abs, diff2011_ns_abs, diffmu_abs, 
   diffmu_ns_abs, lc_agg)
rm(act_2011_bin, act_2011_ns_bin, act_mu_bin, act_mu_ns_bin, abs_2011_bin, abs_2011_ns_bin, abs_mu_bin, 
   abs_mu_ns_bin)

# Calculate statistics from them
act_diff_2011_stats <- bin_stats(act_diff_2011_bin, list("mu" = mean, "sd" = sd), binv)
abs_diff_2011_stats <- bin_stats(abs_diff_2011_bin, list("mu" = mean, "sd" = sd), binv)

# Country-scale means as well, to get country-scale bias
sa_mu <- cellStats(gti_nh_agg$f1$gti2011, mean)
lc_sa_diff_act <- lapply(lclist, function(x) sa_mu - cellStats(x, mean))
lc_sa_diff_abs <- lapply(lclist, function(x) abs(sa_mu - cellStats(x, mean)))

save(sa_mu, lc_sa_diff_act, lc_sa_diff_abs, act_diff_2011_stats, abs_diff_2011_stats, 
     file = full_path(p_data, "difference_stats.rda"))


