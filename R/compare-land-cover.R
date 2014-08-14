##############################################################################################################
# Title      : compareLandCover.R
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

# Path variables
p_root <- proj_root("SAcropland")
rasterOptions(tmpdir = "/var/scratch/lestes")  # Change tempdir to scratch (if on server)
p_data <- full_path(p_root, "data/")
p_fig <- full_path(p_root, "figures/")

# load sa.r, sa.shp, gti_hort, gti_nohort, globsa_list_sum, modsa_list_sum, glcsa.m, salc1km.m,
load(full_path(p_data, "landcover_objects.rda"))
salc <- salc1km.m * 100

# Create mean of gtis from different time periods
gti_nohort2 <- c(gti_nohort2, calc(stack(gti_nohort), mean))
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








