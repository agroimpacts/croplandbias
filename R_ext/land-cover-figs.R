##############################################################################################################
# Title      : SA-land-cover-figs.R
# Purpose    : Figures created from SA land cover comparison
# Author     : Lyndon Estes
# Draws from : 
# Used by    : 
# Notes      : Created 18/8/2014
##############################################################################################################

library(lmisc)
source("R/landcover-functions.R")

# Path variables
p_root <- proj_root("SAcropland")
p_fig <- full_path(p_root, "SAcropland/figures/")
p_data <- full_path(p_root, "SAcropland/data/")

# Load necessary datasets
load(full_path(p_data, "MZshapes.Rdata"))  # SA shape
load(full_path(p_data, "difference_grids.rda"))  # difference rasters
load(full_path(p_data, "difference_stats.rda"))  # difference stats

# Figure 1
# Plotting colors
cols <- colorRampPalette(c("red", "grey80", "blue4"))
brks <- c(-100.1, -75, -50, -25, -10, 0, 10, 25, 50, 75, 100.1)
n_cols <- length(brks) - 1
legtext <- "% Difference"
cx <- 1.5
lcol <- "black"
mcap <- c("SA Landcover", "GlobCover min", "GlobCover mean", "GlobCover max", "MODIS min", "MODIS mean", 
          "MODIS max", "GLC-Share")
pdf(full_path(p_fig, "Figure1.pdf"), height = 12, width = 8)
par(mfrow = c(4, 2), mar = c(0, 0, 2, 0), oma = c(7, 0, 0, 0))
for(i in 1:8) {
  plot(sa.shp, lty = 0)
  plot(diff2011_act$f1[[i]], add = TRUE, col = cols(n_cols), breaks = brks, legend = FALSE)
  mtext(mcap[i], side = 3, cex = cx)
}
flex_legend(ncuts = length(brks) - 1, legend.text = legtext, legend.vals = round(brks), 
            longdims = c(0.2, 0.8), shortdims = c(0.04, 0.01), colvec = cols(length(brks) - 1), 
            srt = c(270, 0), horiz = TRUE, textside = "bottom", legend.pos = c(4, 5), 
            leg.adj = list(c(0.25, 0), c(0.5, -0.5)), cex.val = cx, textcol = lcol, bordercol = lcol)
dev.off()

# Figure 2
pdf(full_path(p_fig, "Figure2.pdf"), height = 12, width = 8)
par(mfrow = c(4, 2), mar = c(0, 0, 2, 0), oma = c(7, 0, 0, 0))
for(i in 1:8) {
  plot(sa.shp, lty = 0)
  plot(diff2011_act$f20[[i]], add = TRUE, col = cols(n_cols), breaks = brks, legend = FALSE)
  mtext(mcap[i], side = 3, cex = cx)
}
flex_legend(ncuts = length(brks) - 1, legend.text = legtext, legend.vals = round(brks), 
            longdims = c(0.2, 0.8), shortdims = c(0.04, 0.01), colvec = cols(length(brks) - 1), 
            srt = c(270, 0), horiz = TRUE, textside = "bottom", legend.pos = c(4, 10), 
            leg.adj = list(c(0.25, 0), c(0.5, -0.5)), cex.val = cx, textcol = lcol, bordercol = lcol)
dev.off()

# Figure 3 - plot of actual differences with scale
v <- act_diff_2011_stats$mu
cols <- c("red", rep("green4", 3), rep("blue4", 3), "black")
lty <- c(1, 2, 1, 3, 2, 1, 3, 1)
pts <- c(16, 3, 16, 4, 3, 16, 4, 16)
x <- 1:nrow(v$L1)
f <- c(1, 5, 10, 20, 50, 100, 200, 300, 600)
lvec <- paste(seq(0, 100, 5)[-21], seq(0, 100, 5)[-1], sep = "-")
mlab <- paste(c(1, 5, 10, 20, 50, 100, 200, 300, 600), "km")
leg <- c("SA 30", "GCov min", "GCov mean", "GCov max", "Mod min", "Mod mean", "Mod max", "GLC-share")

pdf(full_path(p_fig, "Figure3.pdf"), height = 8, width = 8)
par(mfrow = c(3, 3), oma = c(5, 3, 0, 0), mar = c(1, 1, 0, 0))
ylim <- c(-20, 80)
for(i in 1:length(v)) {
  plot(x[c(1, length(x))], ylim, pch = "", axes = FALSE, xlab = "% cropcover", ylab = "")
  lines(c(-1, 21), c(0, 0), lty = 4)
  mtext(mlab[i], side = 3, line = -2, adj = 0.05)
  for(j in 1:ncol(v[[i]])) {
    pv <- which(!is.na(v[[i]][, j]))
    #lines(pv, v[[i]][pv, j], col = cols[j], lty = lty[j])
    points(pv, v[[i]][pv, j], col = cols[j], pch = pts[j], cex = 1)
  }
  if(i %in% 7:9) {
    mtext("% cropcover", side = 1, line = 4)
    axis(1, at = 1:20, labels = lvec, las = 2, mgp = c(1, 0.7, 0), tcl = -0.3)
  } else {
    axis(1, at = 1:20, labels = FALSE, las = 2, mgp = c(1, 0.7, 0), tcl = -0.3)
  }
  if(i %in% c(1, 4, 7)) {
    mtext("% bias", side = 2, line = 2)
    axis(2, at = seq(ylim[1], ylim[2], 10), labels = seq(ylim[1], ylim[2], 10), las = 2, mgp = c(1, 0.7, 0), 
         tcl = -0.3)
  } else {
    axis(2, at = seq(ylim[1], ylim[2], 10), labels = FALSE, las = 2, mgp = c(1, 0.7, 0), 
         tcl = -0.3)
  } 
}
legend("right", leg, col = cols, pch = pts, bty = "n")
dev.off()        

# Figure 4 - plot of absolute differences with scale
v <- abs_diff_2011_stats$mu
pdf(full_path(p_fig, "Figure4.pdf"), height = 8, width = 8)
par(mfrow = c(3, 3), oma = c(5, 3, 0, 0), mar = c(1, 1, 0, 0))
ylim <- c(0, 80)
for(i in 1:length(v)) {
  plot(x[c(1, length(x))], ylim, pch = "", axes = FALSE, xlab = "% cropcover", ylab = "")
  lines(c(-1, 21), c(0, 0), lty = 4)
  mtext(mlab[i], side = 3, line = -2, adj = 0.05)
  for(j in 1:ncol(v[[i]])) {
    pv <- which(!is.na(v[[i]][, j]))
    points(pv, v[[i]][pv, j], col = cols[j], pch = pts[j], cex = 1)
  }
  if(i %in% 7:9) {
    mtext("% cropcover", side = 1, line = 4)
    axis(1, at = 1:20, labels = lvec, las = 2, mgp = c(1, 0.7, 0), tcl = -0.3)
  } else {
    axis(1, at = 1:20, labels = FALSE, las = 2, mgp = c(1, 0.7, 0), tcl = -0.3)
  }
  if(i %in% c(1, 4, 7)) {
    mtext("% bias", side = 2, line = 2)
    axis(2, at = seq(ylim[1], ylim[2], 10), labels = seq(ylim[1], ylim[2], 10), las = 2, mgp = c(1, 0.7, 0), 
         tcl = -0.3)
  } else {
    axis(2, at = seq(ylim[1], ylim[2], 10), labels = FALSE, las = 2, mgp = c(1, 0.7, 0), 
         tcl = -0.3)
  } 
}
legend("right", leg, col = cols, pch = pts, bty = "n")
dev.off()        


# plot(x[c(1, length(x))], ylim, pch = "")
# for(j in 1:length(v)) {
#   pv <- which(!is.na(v[[j]][, 1]))
#   points(pv, v[[j]][pv, i], col = cols[j], pch = 0, cex = log(1.5 * f[j]))
# }

# sapply(1:20, function(x) {
#   mean(act_diff_2011_bin[[1]]$glcsh[act_diff_2011_bin[[1]]$glcsh[, 1] == x, 3], na.rm = TRUE)
# })



