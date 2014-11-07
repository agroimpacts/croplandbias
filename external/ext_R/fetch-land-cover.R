##############################################################################################################
# Title      : fetchLandCover.R
# Purpose    : Downloading and pre-processing of global landcover products
# Author     : Lyndon Estes
# Draws from : 
# Used by    : 
# Notes      : This was primarily run on mapping africa server. Download locations for major landcover data 
#              needs to be altered for runs on other platforms
##############################################################################################################

library(gdalUtils)
library(RCurl)
library(RPostgreSQL)
library(rgdal)
library(rgeos)
library(raster)
library(lmisc)

# source("/u/sandbox/afmap/R/KMLAccuracyFunctions.R")

# Change tempdir to scratch
# rasterOptions(tmpdir = "/var/scratch/lestes")
full_path <- function(path, fname) paste(path, fname, sep = "")  # Directory path function
p_root <- proj_root("SAcropland")
p_fig <- full_path(p_root, "SAcropland/figures/")
p_data <- full_path(p_root, "SAcropland/data/")

### Download and process downloaded landcover data to SA extent
setwd(p_data)

# Get SA shape
if(!file.exists("MZshapes.Rdata")) {
  system("wget http://dl.dropboxusercontent.com/u/31890709/MZshapes.Rdata")
}
load("MZshapes.Rdata")
sa.buf <- gBuffer(sa.shp, width = 3000)

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

# Convert SA to GCS for cropping extent
sa.buf.gcs <- spTransform(sa.buf, CRSobj = CRS(gcsstr))
#save(sa.buf.gcs, file = "/u/lestes/analyses/SAcropland/data/sa_buf_gcs.rda")
#load("/u/lestes/analyses/SAcropland/data/sa_buf_gcs.rda")

# Create 1 km grid from sa1kilo to rectify grids against that (should have saved from previous code) 
if(!file.exists(full_path(p_data, "sagrid.tif"))) {
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
  sa_r <- rasterFromXYZ(pointsXYZ)
  projection(sa_r) <- sa.buf@proj4string
  sa_r <- writeRaster(sa_r, filename = full_path(p_data, "sagrid.tif"), overwrite = TRUE)
} else {
  sa_r <- raster(full_path(p_data, "sagrid.tif"))
}

# GLC share
if(!file.exists(full_path(p_data, "glcsa_masked.tif"))) {  # see if file exists, to avoid redoing step 
  setwd("/u/lestes/spatial_data/")
  glcpath <- "/u/lestes/spatial_data/glc_share/"
  url <- "http://www.fao.org/geonetwork/srv/en/resources.get?id=47948&fname=GlcShare_v10_02.zip&access=private"
  download.file(url, method="auto", destfile = "glc_share.zip")
  unzip("glc_share.zip", exdir= "glc_share")
  glc <- raster(full_path(glcpath, "glc_shv10_02.Tif"))
  projection(glc) <- gcsstr
  glcSA <- crop(glc, y=sa.buf.gcs, file = full_path(glcpath, "glcSA.tif"))  # crop to extent of SA
  gdalwarp(srcfile = glcSA@file@name, dstfile = full_path(glcpath, "glcSA_alb.tif"), t_srs = prjstr, 
           tr = c(1000, 1000), r = "bilinear")

  # Warp to SA grid
  glcsa <- resample(glcSA_alb, sa.r, method = "bilinear", 
                    filename = full_path(p_data, "glcSA_alb_rect.tif"))
  glcsa_m <- mask(glcsa, sa.r, file = full_path(p_data, "glcsa_masked.tif"), overwrite = TRUE)
} else {
  glcsa_m <- raster(full_path(p_data, "glcsa_masked.tif"))
}

# Globcover 2009
globpath <- "/u/lestes/spatial_data/globcover_2009/"
if(!file.exists(full_path(globpath, "globSA_alb.tif"))) {
  url <- "http://due.esrin.esa.int/globcover/LandCover2009/Globcover2009_V2.3_Global_.zip"
  download.file(url, method="auto", destfile = "globcover_2009.zip")
  unzip("globcover_2009.zip", exdir= "globcover_2009")
  glob <- raster(full_path(globpath, "GLOBCOVER_L4_200901_200912_V2.3.tif"))
  globSA <- crop(glob, y=sa.buf.gcs)  # crop to extent of SA
  gdalwarp(srcfile = globSA@file@name, dstfile = full_path(globpath, "globSA_alb.tif"), t_srs = prjstr, 
           tr = c(300, 300))
  globSA_alb <- raster(full_path(globpath, "globSA_alb.tif"))
} else {
  globSA_alb <- raster(full_path(globpath, "globSA_alb.tif"))
}

# MODIS landcover 
moddir <- "/u/lestes/spatial_data/MCD12Q1/"
if(!file.exists(full_path(moddir, "modis_type_1_alb.tif"))) {
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
  # file.remove(dir(pattern = "zip"))
  
  modis <- dir(moddir)
  # gdal_translate(src_dataset = modis[1], dst_dataset= "modis1.tif", sd_index= 1)  
  # r <- raster("modis1.tif")
  # plot(r)
  
  # Translate to geotiff
  lapply(1:5, function(x) {
    batch_gdal_translate(infiles = modis, outdir = moddir, 
                         outsuffix = paste("_LC_", x, ".tif", sep = ""), sd_index = x)  
  })
  sdnames <- lapply(1:5, function(x) dir(moddir, pattern  = paste("LC_", x, ".tif", sep = "")))
  # r <- raster(sdnames[[1]][1])
  
  # warp to albers, mosaic, and clip to SA extent
  lapply(1:5, function(x) {
    print(paste("processing type", x))
    gdalwarp(srcfile = sdnames[[x]], dstfile = paste("modis_type_", x, "_alb.tif", sep = ""), 
             t_srs = prjstr, tr = res(r), te = bbox(sa.buf)[1:4])
  })  
  
  # lapply(sdnames[2:5], function(x) file.remove(paste(moddir, x, sep = "")))  # removing types 2-5 for space
  # lapply(2:5, function(x) file.remove(paste(moddir, "modis_type_", x, "_alb.tif", sep = "")))
  
  # sdnames <- lapply(1:5, function(x) unname(sapply(modis, function(y) gdalUtils::get_subdatasets(y)[x])))
  # lapply(1:5, function(x) {
  #   gdalbuildvrt(gdalfile = sdnames[[x]], output.vrt=paste("modis_type", x, ".vrt", sep = ""))
  # })
  
  # gdalwarp(srcfile = "modis_type1.vrt", dstfile = "modis_type1_alb.tif", of = "VRT", t_srs = prjstr, 
  #          te = bbox(sa.buf)[1:4])
  
  mod_1 <- raster("modis_type1_alb.tif")
  # file.remove(dir(pattern = "alb.tif"))
} else {
  mod_1 <- raster(full_path(moddir, "modis_type_1_alb.tif"))
}
 
# Process SA National landcover dataset, including warps to SA 1km grid
salcpath <- "/u/lestes/spatial_data/sa_landcover_2009/"
if(!file.exists(full_path(p_data, "salc_ag_masked.tif"))) {
  url <- "http://planet.uwc.ac.za/BGISdownloads/landcover_2009.zip"
  download.file(url, method="auto", destfile = "sa_landcover_2009.zip")
  unzip("sa_landcover_2009.zip", exdir= "sa_landcover_2009")
  
  #cext <- extent(c(500000, 510000, -2910000, -2900000))  # crops to small extent for examination
  setwd(salcpath)
  gdal_translate(src_dataset = "landcover", dst_dataset= "salandcover2009.tif", of = "GTiff")
  salc <- raster(full_path(salcpath, "salandcover2009.tif"))
  system(paste("rm -r", full_path(salcpath, "landcover")))
  file.remove("sa_landcover_2009.zip")
  # salc.sm <- crop(salc, cext)
  # plot(salc.sm)
  # rm(salc.sm)
  
  # system call to gdal_calc.py to extract landcover class 2, cultivation
  salconm <- "salc_ag.tif"
  pcalc <- paste("gdal_calc.py -A ", salc@file@name, " --outfile=", salconm, " --overwrite --calc='A==2'", 
                 sep = "")
  system.time(system(pcalc))  # 123 seconds, but was much faster before
  salc_ag <- raster(full_path(salcpath, salconm))
  # salc.sm <- crop(salc_ag, cext)
  # plot(salc.sm)
  # rm(salc.sm)

  # Aggregate SA landcover to 1 km resolution also, relying on gdal
  # salc_agtest <- crop(salc_ag, cext, file = full_path(salcpath, "salc_agtest.tif"))
  setwd(p_data)
  gdalwarp(srcfile = salc_ag@file@name, t_srs = projection(sa.r), dstfile = "salc_ag_1km.tif", 
           r = "average", ot = "Float32", srcnodata = 255, tr = c(1000, 1000), of = "GTiff", 
           verbose = TRUE, overwrite = TRUE)
  # agtest1 <- raster("salc_ag_test1km.tif")
  # agtet <- aggregate(salc_agtest, fact = 33, na.rm = TRUE)
  salc1km <- raster(full_path(p_data, "salc_ag_1km.tif"))
  salc1km_r <- resample(salc1km, sa.r, method = "bilinear")
  salc1km_m <- mask(salc1km.r, sa.r, filename = full_path(p_data, "salc_ag_masked.tif"), 
                    overwrite = TRUE)
  file.remove(full_path(salcpath, salconm))
  file.remove(full_path(p_data, "salc_ag_1km.tif"))
} else {
  salc1km_m <- raster(full_path(p_data, "salc_ag_masked.tif"))
} 

# KZN masking layer for sugarcane
if(!file.exists(full_path(p_data, "kzn_cane_masked.tif"))) {
  kznpath <- "/u/lestes/spatial_data/kzn_landcover/"
  setwd(kznpath)
  # copied over and unzipped out of script
  gdal_translate(src_dataset = "kznlc11v1w31", dst_dataset= "kznlandcover.tif", of = "GTiff")
  system(paste("rm -r", full_path(kznpath, "kznlc11v1w31")))
  kznlc <- raster(full_path(kznpath, "kznlandcover.tif"))

  # system call to gdal_calc.py to extract landcover class 2, cultivation
  kznlconm <- "kzn_cane.tif"  # file.remove(kznlconm)
  kzninnm <- strsplit(kznlc@file@name, "/")[[1]][length(strsplit(kznlc@file@name, "/")[[1]])]
  # kzninnm <- strsplit(kznlc.ss@file@name, "/")[[1]][length(strsplit(kznlc.ss@file@name, "/")[[1]])]
  pcalc <- paste("gdal_calc.py -A ", kzninnm, " --outfile=", kznlconm, 
                 " --overwrite --calc='(A==9)|(A==10)'", sep = "")
  system.time(system(pcalc))  # 49 seconds
  kzn_cane <- raster(full_path(kznpath, kznlconm))
  # cext <- extent(c(20000, 40000, -3220000, -3200000))  # crops to small extent for examination
  # kznlc.ss <- crop(kznlc, cext, filename = full_path(kznpath, "kznlandcover_crop.tif"), overwrite = TRUE)
  # plot(kznlc.ss)
  # scane1 <- (kznlc.ss == 9); scane2 <- (kznlc.ss == 10)
  # plot(scane1, add = TRUE, col = c("transparent", "red"))
  # plot(scane2, add = TRUE, col = c("transparent", "purple"))
  # chk <- crop(kzn_cane, cext)
  # plot(chk); plot(scane1, add = TRUE, col = c("transparent", "red"), legend = FALSE) 
  # plot(scane2, add = TRUE, col = c("transparent", "red"), legend = FALSE)
  # file.remove("kznlandcover_crop.tif")
  # rm(chk, kznlc.ss, scane1, scane2)
  file.remove("kznlandcover.tif")
  
  # Warp and resample to SA grid
  setwd(p_data)
  gdalwarp(srcfile = kzn_cane@file@name, t_srs = projection(sa.r), dstfile = "kzn_cane_1km.tif", 
           r = "average", ot = "Float32", srcnodata = 255, tr = c(1000, 1000), of = "GTiff", 
           verbose = TRUE, overwrite = TRUE)
  kzn_1km <- raster(full_path(p_data, "kzn_cane_1km.tif"))
  kzn_1km_r <- resample(kzn_1km, sa.r, method = "bilinear")
  kzn_1km_m <- mask(kzn_1km_r, sa.r, filename = full_path(p_data, "kzn_cane_masked.tif"), 
                    overwrite = TRUE)
} else {
  kzn_1km_m <- raster(full_path(p_data, "kzn_cane_masked.tif"))
} 
# plot(salc1km.m, )
# plot(kzn_1km.m > 0, add = TRUE, legend = FALSE, col = c("transparent", "red"))

setwd(p_data)
if(any(!file.exists(full_path(p_data, dir(p_data, pattern = "mod_1*.*1kmrect_sum.tif"))))) {
  # MODIS, using main classification scheme (IGBP-DIS)
  mod_1_crop <- stack(mod_1 == 12, mod_1 == 14)
  mod_1_cropb <- brick(mod_1_crop)
  names(mod_1_crop) <- c("cropland", "mosaic")
  
  # assign percentages
  minset <- c(100, 10)  # assume 10% is minimum crop cover for MODIS IGBP mosaic class
  meanset <- c(100, 35)  # assume 35% is mean crop cover
  maxset <- c(100, 60)  # assume 60% is mean crop cover  
  modsa_min <- assignLCPct(mod_1_cropb, minset, fname = full_path(p_data, "mod_1_min.tif"))
  modsa_mu <- assignLCPct(mod_1_cropb, meanset, fname = full_path(p_data, "mod_1_mu.tif"))
  modsa_max <- assignLCPct(mod_1_cropb, maxset, fname = full_path(p_data, "mod_1_max.tif"))
  
#   modsa_min <- brick(full_path(p_data, "mod_1_min.tif"))
#   modsa_mu <- brick(full_path(p_data, "mod_1_mu.tif"))
#   modsa_max <- brick(full_path(p_data, "mod_1_max.tif"))

  # aggregate, resample, mask to SA grid
  modsa_list <- lapply(list(modsa_min, modsa_mu, modsa_max), function(x) {
    nm <- gsub("//.tif", "", x@file@name)
    print(paste("Processing", nm))
    print("...aggregating")
    agg <- aggregate(x, fact = 2, na.rm = TRUE)  
    print("...resampling")
    crop1km <- resample(agg, sa.r, method = "bilinear")
    print("...masking")
    crop1km <- mask(crop1km, mask = sa.r, filename = paste(nm, "_1kmrect.tif", sep = ""), overwrite = TRUE)
    return(crop1km)
  })
  #modsa_list <- lapply(full_path(p_data, dir(p_data, pattern = "mod_1*.*1kmrect.tif")[c(2:3, 1)]), brick)
  
  # And them create single raster with summed proportions
  modsa_list_sum <- lapply(modsa_list, function(x) {
    nm <- paste(gsub("\\.tif", "", x@file@name), "_sum.tif", sep = "")
    paste(nm)
    calc(x, sum, filename = nm, overwrite = TRUE)
  })
} else {
  modsa_list_sum <- lapply(full_path(p_data, dir(p_data, "mod_1*.*1kmrect_sum.tif")[c(2:3, 1)]), raster)
}  

if(any(!file.exists(full_path(p_data, dir(p_data, "globSA*.*1kmrect_sum.tif"))))) {
  # Globcover: class 11 = irrigated cropland, class 14, rainfed, 20 is 50-70% cropland, 30 is 20-50%
  l <- lapply(c(11, 14, 20, 30), function(x) r <- globSA_alb == x)
  s <- stack(l)
  # globSA_cropb <- brick(s, filename = full_path(p_data, "globSA_crop.tif"), overwrite = TRUE)
  globSA_cropb <- brick(full_path(p_data, "globSA_crop.grd"))
  rm(s, l)
  
  # assign percentages
  minset <- c(100, 100, 50, 20)
  maxset <- c(100, 100, 70, 50)
  meanset <- c(100, 100, 60, 35)
  globsa_min <- assignLCPct(globSA_cropb, minset, fname = full_path(p_data, "globSA_300_min.tif"))
  globsa_mu <- assignLCPct(globSA_cropb, meanset, fname = full_path(p_data, "globSA_300_mu.tif"))
  globsa_max <- assignLCPct(globSA_cropb, maxset, fname = full_path(p_data, "globSA_300_max.tif"))
  glob300list <- lapply(dir(pattern = "globSA_300+.+tif")[c(2:3, 1)], raster)
 
  # Aggregate them and resample and mask to SA grid
  globsa_list <- lapply(list(globsa_min, globsa_mu, globsa_max), function(x) {
    nm <- gsub("_300|.tif", "", x@file@name)
    print(paste("Processing", nm))
    print("...aggregating")
    agg <- aggregate(x, fact = 3, na.rm = TRUE)  # equivalent to summing, dividing by 900
    print("...resampling")
    crop1km <- resample(agg, sa.r, method = "bilinear")
    print("...masking")
    crop1km <- mask(crop1km, mask = sa.r, filename = paste(nm, "_1kmrect.tif", sep = ""), 
                    overwrite = TRUE)
    return(crop1km)
  })
  # globsa_list <- lapply(dir(p_data, pattern = "glob+.+1kmrect.tif")[c(2:3, 1)], raster)
  
  # And them create single raster with summed proportions
  globsa_list_sum <- lapply(globsa_list, function(x) {
    nm <- paste(gsub("\\.tif", "", x@file@name), "_sum.tif", sep = "")
    paste(nm)
    calc(x, sum, filename = nm)
  })
} else {
  globsa_list_sum <- lapply(full_path(p_data, dir(p_data, "globSA*.*1kmrect_sum.tif")[c(2:3, 1)]), raster)
} 

# glcsa.m <- raster(full_path(p_data, "glcsa_masked.tif"))

# Bring in SA landcover sets and mask
cover2007 <- brick(full_path(p_data, "cover2007.tif"))
cover2011 <- brick(full_path(p_data, "cover2011.tif"))

# sum these also with and without horticulture
sumfun <- function(x) sum(x, na.rm = TRUE)
gti_nohort <- lapply(list(cover2007, cover2011), function(y) {
  r <- calc(y[[-6]], sumfun)
  r[r > 100] <- 100
  writeRaster(r, file = gsub("\\.tif", "sum_nh.tif", y@file@name), overwrite = TRUE)
}) 
gti_hort <- lapply(list(cover2007, cover2011), function(y) {
  r <- calc(y, sumfun)
  r[r > 100] <- 100
  writeRaster(r, file = gsub("\\.tif", "sum_h.tif", y@file@name), overwrite = TRUE)
}) 
# which(values(gti_hort[[1]] > 100) == 1) 
# gti_nohort <- lapply(full_path(p_data, dir(p_data, "cover*.*sum_nh.tif")), raster)
# gti_hort <- lapply(full_path(p_data, dir(p_data, "cover*.*sum_h.tif")), raster)

# And with and without communal farmlands
sust_mask_2011 <- cover2011[[2]] > 10  
sust_mask_2007 <- cover2007[[2]] > 10

# Set up SA mask including sugarcane -- add in Mpumalanga sugar cane when it becomes available
# Might also want to mask out protected areas, if GTI excluded them from consideration
m1 <- kzn_1km_m
m1[is.na(m1)] <- 0
m1[m1 > 0] <- NA
sa_masks <- sa_r > 0
sa_masks <- m1 + sa_masks
rm(m1)

# create additional masks for areas of subsistence agriculture
mask_list <- lapply(list(sust_mask_2007, sust_mask_2011), function(x) {
  m <- x
  m[is.na(m)] <- 0
  m[m > 0] <- NA
  mr <- m + sa_masks
  mr
})
mask_list <- c(sa_masks, mask_list)
names(mask_list) <- c("stand", "nosubs_2007", "nosubs_2011")
brick(stack(mask_list), file = full_path(p_data, "mask_brick.tif"), overwrite = TRUE)

# Fix values greater than 100% and apply masks
r <- gti_nohort[[1]]
lapply(1:2, function(x) {
  out_name <- paste(gsub("\\.tif", "", r@file@name), "_", names(mask_list)[x], ".tif", sep = "")
  mask(r, mask_list[[x]], file = out_name, overwrite = TRUE)
})  
r <- gti_nohort[[2]]
lapply(c(1, 3), function(x) {
  out_name <- paste(gsub("\\.tif", "", r@file@name), "_", names(mask_list)[x], ".tif", sep = "")
  mask(r, mask_list[[x]], file = out_name, overwrite = TRUE)
})  
r <- gti_hort[[1]]
lapply(1:2, function(x) {
  out_name <- paste(gsub("\\.tif", "", r@file@name), "_", names(mask_list)[x], ".tif", sep = "")
  mask(r, mask_list[[x]], file = out_name, overwrite = TRUE)
})  
r <- gti_hort[[2]]
lapply(c(1, 3), function(x) {
  out_name <- paste(gsub("\\.tif", "", r@file@name), "_", names(mask_list)[x], ".tif", sep = "")
  mask(r, mask_list[[x]], file = out_name, overwrite = TRUE)
})  

# lapply(gti_nohort, function(x) {
#   gti_nohort[c(1, 2)]
#   m_list <- lapply(1:length(mask_list), function(y) {
#     out_name <- paste(gsub("\\.tif", "", x@file@name), "_", names(mask_list)[y], ".tif", sep = "")
#     #mask(x, mask_list[[y]], file = out_name, overwrite = TRUE)
#   })
# })
# lapply(gti_hort, function(x) {
#   m_list <- lapply(1:length(mask_list), function(y) {
#     out_name <- paste(gsub("\\.tif", "", x@file@name), "_", names(mask_list)[y], ".tif", sep = "")
#     mask(x, mask_list[[y]], file = out_name, overwrite = TRUE)
#   })
# })

