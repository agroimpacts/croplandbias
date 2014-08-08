##############################################################################################################
# Title      : fetchLandCover.R
# Purpose    : Downloading and pre-processing of global landcover products
# Author     : Lyndon Estes
# Draws from : 
# Used by    : 
# Notes      : 
##############################################################################################################

library(gdalUtils)
library(RCurl)
suppressMessages(library(RPostgreSQL))
suppressMessages(library(rgdal))
suppressMessages(library(rgeos))
suppressMessages(library(raster))
source("/u/sandbox/afmap/R/KMLAccuracyFunctions.R")

dirPath <- function(path, fname) paste(path, fname, sep = "")

p.alyze <- "/u/lestes/analyses/SAcropland/data/"
setwd(p.alyze)

# Get SA shape
system("wget http://dl.dropboxusercontent.com/u/31890709/MZshapes.Rdata")
load("MZshapes.Rdata")
sa.buf <- gBuffer(sa.shp, width = 3000)

### Download landcover data
setwd("/u/lestes/spatial_data/")
# FAO GLC share
url <- "http://www.fao.org/geonetwork/srv/en/resources.get?id=47948&fname=GlcShare_v10_02.zip&access=private"
download.file(url, method="auto", destfile = "glc_share.zip")
unzip("glc_share.zip", exdir= "glc_share")

# Globcover 2009
url <- "http://due.esrin.esa.int/globcover/LandCover2009/Globcover2009_V2.3_Global_.zip"
download.file(url, method="auto", destfile = "globcover_2009.zip")
unzip("globcover_2009.zip", exdir= "globcover_2009")

# MODIS landcover 
# First create points where want MODIS data for MODIS tools
# GetBands(GetProducts()[1])
tiles <- expand.grid("h" = c(19, 20), "v" = c(11, 12))
tiles <- apply(tiles, 1, function(x) paste("h", x[1], "v", x[2], sep = ""))
url <- "http://e4ftl01.cr.usgs.gov/MOTA/MCD12Q1.051/2011.01.01/"
tile.names <- getURL(url, verbose=TRUE, dirlistonly = TRUE) 
tile.names1 <- strsplit(tile.names, "alt")[[1]]
tile.names2 <- unique(substr(tile.names1, 20, 64))
modnames <- tile.names2[sapply(tiles, function(x) grep(x, tile.names2))]
lapply(modnames, function(x) {
  url <- paste("http://e4ftl01.cr.usgs.gov/MOTA/MCD12Q1.051/2011.01.01/", x, sep = "")
  download.file(url, method="auto", destfile = x)
})
dir.create("MCD12Q1")
file.copy(from = dir(pattern = "hdf"), to = paste(getwd(), "MCD12Q1", dir(pattern = "hdf"), sep = "/"))
file.remove(dir(pattern = "hdf"))

# SA National landcover
url <- "http://planet.uwc.ac.za/BGISdownloads/landcover_2009.zip"
download.file(url, method="auto", destfile = "sa_landcover_2009.zip")
unzip("sa_landcover_2009.zip", exdir= "sa_landcover_2009")

# file.remove(dir(pattern = "zip"))

### Process downloaded landcover data to SA extent
# Connection
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "SouthAfricaSandbox", user = "postgis", password = "P0stG1S")

# Albers
prjsrid <- 97490
prj.sql <- paste("select proj4text from spatial_ref_sys where srid=", prjsrid, sep = "")
prjstr <- dbGetQuery(con, prj.sql)$proj4text

# GCS
gcsid <- 4326
gcs.sql <- paste("select proj4text from spatial_ref_sys where srid=", gcsid, sep = "")
gcsstr <- dbGetQuery(con, gcs.sql)$proj4text

# Sinusoidal

# Convert SA to GCS for cropping extent
sa.buf.gcs <- spTransform(sa.buf, CRSobj = CRS(gcsstr))
#save(sa.buf.gcs, file = "/u/lestes/analyses/SAcropland/data/sa_buf_gcs.rda")
#load("/u/lestes/analyses/SAcropland/data/sa_buf_gcs.rda")
glcpath <- "/u/lestes/spatial_data/glc_share/"
glc <- raster(dirPath(glcpath, "glc_shv10_02.Tif"))
projection(glc) <- gcsstr
glcSA <- crop(glc, y=sa.buf.gcs, file = dirPath(glcpath, "glcSA.tif"))  # crop to extent of SA
gdalwarp(srcfile = glcSA@file@name, dstfile = dirPath(glcpath, "glcSA_alb.tif"), t_srs = prjstr, 
         tr = c(1000, 1000), r = "bilinear")
glcSA_alb <- raster(dirPath(glcpath, "glcSA_alb.tif"))
# glcSA_alb <- writeRaster(glcSA_alb, file = dirPath(glcpath, "glcSA_alb.grd"), overwrite = TRUE)

globpath <- "/u/lestes/spatial_data/globcover_2009/"
glob <- raster(dirPath(globpath, "GLOBCOVER_L4_200901_200912_V2.3.tif"))
globSA <- crop(glob, y=sa.buf.gcs)  # crop to extent of SA
#writeRaster(globSA, file = "/u/lestes/analyses/SAcropland/data/globSA.tif")
#writeRaster(globSA, file = "/u/lestes/spatial_data/globcover_2009/globSA.tif")
gdalwarp(srcfile = globSA@file@name, dstfile = dirPath(globpath, "globSA_alb.tif"), t_srs = prjstr, 
         tr = c(300, 300))
#writeRaster(globSA, file = "/u/lestes/spatial_data/globcover_2009/globSA_alb.tif")
globSA_alb <- raster(dirPath(globpath, "globSA_alb.tif"))
#writeRaster(globSA, file = "/u/lestes/spatial_data/globcover_2009/globSA_alb.grd")

moddir <- "/u/lestes/spatial_data/MCD12Q1/"
modis <- dir(moddir)

# gdal_translate(src_dataset = modis[1], dst_dataset= "modis1.tif", sd_index= 1)  
# r <- raster("modis1.tif")
# plot(r)

# Translate to geotiff
lapply(1:5, function(x) {
  batch_gdal_translate(infiles = modis, outdir = moddir, 
                       outsuffix = paste("_LC_", x, ".tif", sep = ""), sd_index = x)  
})
sdnames <- lapply(1:5, function(x) dir(pattern  = paste("LC_", x, ".tif", sep = "")))
# r <- raster(sdnames[[1]][1])

# warp to albers, mosaic, and clip to SA extent
lapply(1:5, function(x) {
  print(paste("processing type", x))
  gdalwarp(srcfile = sdnames[[x]], dstfile = paste("modis_type_", x, "_alb.tif", sep = ""), 
           t_srs = prjstr, tr = res(r), te = bbox(sa.buf)[1:4])
})  

mod_1 <- raster(dirPath(moddir, "modis_type_1_alb.tif"))
# mod_2 <- raster("modis_type_2_alb.tif")
# mod_3 <- raster("modis_type_3_alb.tif")

# plot(mod_1)
# plot(sa.buf, add = TRUE)

# sdnames <- lapply(1:5, function(x) unname(sapply(modis, function(y) gdalUtils::get_subdatasets(y)[x])))
# lapply(1:5, function(x) {
#   gdalbuildvrt(gdalfile = sdnames[[x]], output.vrt=paste("modis_type", x, ".vrt", sep = ""))
# })

# gdalwarp(srcfile = "modis_type1.vrt", dstfile = "modis_type1_alb.tif", of = "VRT", t_srs = prjstr, 
#          te = bbox(sa.buf)[1:4])

# r <- raster("modis_type1_alb.tif")
# file.remove(dir(pattern = "alb.tif"))

# Create 1 km grid from sa1kilo to rectify grids against that (should have saved from previous code) 
sql <- paste("SELECT gid, id, ST_AsText(ST_Centroid(geom)) as center FROM sa1kilo")
geom.tab <- dbGetQuery(con, sql)
coord.mat <- do.call(rbind, lapply(1:nrow(geom.tab), function(y) {
  strip <- gsub("POINT|\\(|\\)", "", geom.tab[y, 3])
  coord.mat <- do.call(rbind, strsplit(strsplit(strip, ",")[[1]], " "))
}))
class(coord.mat) <- "numeric"
point.tab <- cbind(geom.tab[, 1:2], coord.mat)
colnames(point.tab)[3:4] <- c("x", "y") 
pointsXYZ <- point.tab[, c(3:4, 2)]
sa.r <- rasterFromXYZ(pointsXYZ)
projection(sa.r) <- sa.buf@proj4string
sa.r <- writeRaster(sa.r, filename = dirPath(p.alyze, "sagrid.tif"), overwrite = TRUE)
sa.r <- raster("/u/lestes/analyses/SAcropland/data/sagrid.tif")
# writeRaster(sa.r, dirPath(p.alyze, "sagrid.grd"))
# sa.r <- raster(dirPath(p.alyze, "sagrid.grd"))

# Now to extract the relevant classes from each landcover set--need binary sets to aggregate counts
# MODIS, using main classification scheme (IGBP-DIS)
mod_1_crop <- stack(mod_1 == 12, mod_1 == 14)
mod_1_cropb <- brick(mod_1_crop)
names(mod_1_crop) <- c("cropland", "mosaic")
mod_1_cropb.agg <- aggregate(mod_1_cropb, fact = 2, FUN = sum, na.rm = TRUE)  # aggregate to 900m
mod_1_cropb.1km <- resample(mod_1_cropb.agg, sa.r, method = "bilinear")  # resample to sa 1 km
save(mod_1_cropb.1km, file = dirPath(p.alyze, "mod_crop_alb_rect.rda"))
# load(dirPath(p.alyze, "mod_crop_alb_rect.rda"))


# Globcover: class 11 = irrigated cropland, class 14, rainfed, 20 is 50-70% cropland, 30 is 20-50%
# l <- lapply(c(11, 14, 20, 30), function(x) r <- globSA_alb == x)
# s <- stack(l)
rasterOptions(maxmemory = 1e+09)
globSA_crop <- stack(globSA_alb == 11, globSA_alb == 14, globSA_alb == 20, globSA_alb == 30) 
# globSA_cropb <- brick(globSA_crop)
globSA_cropb <- brick(globSA_crop, filename = dirPath(p.alyze, "globSA_crop.grd"), overwrite = TRUE)
# rm(s, l)

minset <- c(100, 100, 50, 20)
maxset <- c(100, 100, 70, 50)
meanset <- c(100, 100, 60, 35)

plot(globSA_crop[[2]])
x <- globSA_crop
y <- minset
assignLCPct <- function(x, y, fname) {
  if(length(y) != nlayers(x)) stop("Vector must have same length as number of layers")
  out <- brick(x, values=FALSE)
  out <- writeStart(out, filename=fname)
  bs <- blockSize(x)
  i <- 4
  for (i in 1:bs$n) {
    print(paste("Block", i, "of", bs$n))
    v <- getValues(x, row=bs$row[i], nrows=bs$nrows[i])
    #class(v) <- "numeric"
    v <- t(t(v) * y)
    out <- writeValues(out, v, bs$row[i])
  }
  out <- writeStop(out)
  return(out)
}

rasterOptions(chunksize = 1e+07)
globsa_min <- assignLCPct(globSA_crop, minset, fname = dirPath(p.alyze, "globSA_300_min.grd"))
plot(globsa_min[[2]])
summary(globsa_min[[2]])

v <- values(globSA_crop, row = 1000:1200)
t(sapply(1:nrow(v), function(x) v[x, ] * minset))[4000:4500, ]
globsa_min <- lapply(1:4, function(x) r <- globSA_crop[[x]] * minset[x])
globsa_mu <- stack(lapply(1:4, function(x) globSA_crop[[x]] * meanset[x]))
globsa_max <- stack(lapply(1:4, function(x) globSA_crop[[x]] * maxset[x]))

globSA_crop_mu <- globSA_crop_mu[[3]] * 0.6
globSA_crop_mu[[3]] <- globSA_crop_mu[[3]] * 0.6
globSA_crop <- stack(globSA_alb == 11, globSA_alb == 14, globSA_alb == 20, globSA_alb == 30)
names(globSA_crop) <- c("irrigated", "rainfed", "crop60", "crop35")
globSA_cropb <- brick(globSA_crop)
globSA_cropb.agg <- aggregate(globSA_cropb, fact = 3, FUN = sum, na.rm = TRUE)  # aggregate to 900m
globSA_cropb.1km <- resample(globSA_cropb.agg, sa.r, method = "bilinear")  # resample to sa 1 km
save(globSA_cropb.1km, file = dirPath(p.alyze, "glob_crop_alb_rect.rda"))
# load(dirPath(p.alyze, "glob_crop_alb_rect.rda"))

# save(globSA_cropb, mod_1_cropb, glcSA, file = "/u/lestes/analyses/SAcropland/data/sa_landcover_grids_alb.rda")
# load("/u/lestes/analyses/SAcropland/data/sa_landcover_grids_alb.rda")

# GLC share
# glcSA_alb <- raster(dirPath(glcpath, "glcSA_alb.grd"))
# plot(glcSA_alb)
glcsa <- resample(glcSA_alb, sa.r, method = "bilinear", 
                  filename = dirPath(p.alyze, "glcSA_alb_rect.tif"))
# glcsa <- resample(glcSA_alb, sa.r, method = "bilinear", 
#                   filename = dirPath(p.alyze, "glcSA_alb_rect.grd"))
# glcsa <- raster(dirPath(p.alyze, "glcSA_alb_rect.grd"))
save(glcsa, file = dirPath(p.alyze, "glc_crop_alb_rect.rda"))
load(dirPath(p.alyze, "glc_crop_alb_rect.rda"))

cover2007 <- brick(dirPath(p.alyze, "cover2007.tif"))
cover2011 <- brick(dirPath(p.alyze, "cover2011.tif"))

# Now let's convert each landcover set to percent of cropland per 1 km
# glc is already there
glcsa.m <- mask(glcsa, sa.r, file = dirPath(p.alyze, "glcsa_masked.tif"))  # apply mask

# Globcover will have a mean, a max, and a min set
minset <- c(1, 1, 0.5, 0.2)
maxset <- c(1, 1, 0.7, 0.5)
meanset <- c(1, 1, 0.6, 0.35)
globsa.min <- stack(lapply(1:4, function(x) globSA_cropb.1km[[x]] * minset[x]))


