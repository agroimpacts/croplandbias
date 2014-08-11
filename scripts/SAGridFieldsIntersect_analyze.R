##############################################################################################################
# Title      : SAGridFieldsIntersect_analyze.R
# Purpose    : Post-processing and analyzing results of intersections between fields and SA grid
# Author     : Lyndon Estes
# Draws from : SAGridFieldsIntersect_postgis.R; SAGridFieldsIntersect.R
# Used by    : 
# Notes      : 
##############################################################################################################

# Libraries
suppressMessages(library(RPostgreSQL))
suppressMessages(library(rgdal))
suppressMessages(library(rgeos))
suppressMessages(library(raster))
source("/u/sandbox/afmap/R/KMLAccuracyFunctions.R")

# Connection
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "SouthAfricaSandbox", user = "postgis", password = "P0stG1S")

# Projection information
prjsrid <- 97490
prj.sql <- paste("select proj4text from spatial_ref_sys where srid=", prjsrid, sep = "")
prjstr <- dbGetQuery(con, prj.sql)$proj4text

# table names
tab2007 <- "sa_fg_isect_2007"
tab2011 <- "sa_fg_isect_2011"

# Calculate area of each intersection
lapply(list(tab2007, tab2011), function(x) {  # add column for area
  d <- dbSendQuery(con, paste("ALTER TABLE",  x, "ADD COLUMN area numeric"))
})
lapply(list(tab2007, tab2011), function(x) {  # calculate area
  print(paste("processing", x))
  dbSendQuery(con, paste("UPDATE ", x, "SET area=ST_Area(geom)/10000")) 
})
lapply(list(tab2007, tab2011), function(x) {   # clean up tables
  print(paste("processing", x))
  dbSendQuery(con, paste("VACUUM ANALYZE",  x))
})

# vals <- lapply(list(tab2007, tab2011), function(x) {
#   d <- dbGetQuery(con, paste("SELECT gid FROM", x, "WHERE area > 0"))$gid
# })

# A small check (all the same)
## geoms <- dbGetQuery(con, paste("SELECT gid, ST_AsText(geom) FROM", tab2007, "WHERE gid < 10"))
# geoms <- dbGetQuery(con, paste("SELECT gid, ST_AsText(geom) FROM", tab2011, "WHERE gid < 10"))
# geoms.poly <- polyFromWkt(geom.tab=geoms, crs=prjstr)
# gArea(geoms.poly, byid=TRUE) / 10000
# # geoms.area <- dbGetQuery(con, paste("SELECT gid, area FROM", tab2007, "WHERE gid < 10"))
# geoms.area <- dbGetQuery(con, paste("SELECT gid, area FROM", tab2011, "WHERE gid < 10"))

# Starting calculating area for different classes
fld.types <- lapply(list(tab2007, tab2011), function(x) {   # clean up tables
  print(paste("processing", x))
  sql <- paste("Select distinct field_type from", x)
  fld.types <- dbGetQuery(con, sql)$field_type
})

types <- list(list("normal" = "ISNULL", "subsist" = "LIKE 'Sub%'", "irrig" = "LIKE '%Pivot%'", 
                   "fallow" = "SIMILAR TO 'OF%|Old%'", "small" = "LIKE 'SH%'", "hort" = "LIKE 'Horti%'", 
                   "strip" = "LIKE 'Strip%'"), 
              list("normal" = "LIKE 'Annual%'", "subsist" = "LIKE 'Subsistence%'", "irrig" = "LIKE 'Pivot%'", 
                   "fallow" = "LIKE 'Old%'", "small" = "LIKE 'Small%'", "hort" = "~* 'Horti%|Tea%|Shade%'", 
                   "strip" = "LIKE 'Strip%'"))

lapply(list(tab2007, tab2011), function(x) {  # add column for area
  d <- dbSendQuery(con, paste("ALTER TABLE",  x, "ADD COLUMN new_type varchar"))
})

tabnames <- c(tab2007, tab2011)
lapply(1:2, function(x) {
#lapply(1, function(x) {
  print(tabnames[x])
  lapply(1:7, function(y) {
  #lapply(4, function(y) {
    print(y)
    sql <- paste("UPDATE ", tabnames[x], " SET new_type = '", names(types[[x]][y]), "'", 
                 " WHERE field_type ",  types[[x]][y], sep = "")
    print(sql) 
    dbSendQuery(con, sql)
  })
})

# Vaccum analyze
lapply(list(tab2007, tab2011), function(x) {  
  print(paste("processing", x))
  dbSendQuery(con, paste("VACUUM ANALYZE",  x))
})

# Check rows
checkTabs <- lapply(1:2, function(x) {
  checkTab <- lapply(1:7, function(y) {
    print(y)
    sql <- paste("SELECT COUNT(*) FROM ", tabnames[x], " WHERE new_type = '", names(types[[x]][y]), "'",  
                 sep = "")
    print(sql) 
    dbGetQuery(con, sql)
  })
})

tabCts <- lapply(1:2, function(x) dbGetQuery(con, paste("SELECT COUNT(*) FROM ", tabnames[x])))  
cloudCt <- dbGetQuery(con, paste("SELECT COUNT(*) FROM", tabnames[1], "WHERE field_type = 'Clouds'"))  
# sapply(1:2, function(x) sum(unlist(checkTabs[[x]])))  # tab2007 + cloudCt = tabCts[1], 2011 all equal

# All balanced, let's move on to processing
# Extract areas by field type by time period
typenms <- names(types[[1]])  # all(names(types[[1]]) == names(types[[2]]))
typeAreas <- lapply(1:2, function(x) {
  print(tabnames[x])
  lapply(1:7, function(y) {
    sql <- paste("SELECT gid, grid_id, new_type, area FROM ", tabnames[x], " WHERE new_type = '", typenms[y], 
                 "'", sep = "") 
    print(sql) 
    d <- dbGetQuery(con, sql)
  })
})
# for(i in 1:7) print(which(typeAreas[[2]][[i]]$area == 100))

# IDs of 1 km cells having field intersections
gridIds <- lapply(1:2, function(x) unique(unlist(lapply(1:7, function(y) typeAreas[[x]][[y]]$grid_id))))

# Create grid 
sql <- paste("SELECT gid, id, ST_AsText(ST_Centroid(geom)) as center FROM sa1kilo")
geom.tab <- dbGetQuery(con, sql)
coord.mat <- do.call(rbind, lapply(1:nrow(geom.tab), function(y) {
  strip <- gsub("POINT|\\(|\\)", "", geom.tab[y, 3])
  coord.mat <- do.call(rbind, strsplit(strsplit(strip, ",")[[1]], " "))
}))
class(coord.mat) <- "numeric"
point.tab <- cbind(geom.tab[, 1:2], coord.mat)
# nrow(point.tab)  # 1225603
colnames(point.tab)[3:4] <- c("x", "y") 
pointsXYZ <- point.tab[, c(3:4, 2)]
sa.r <- rasterFromXYZ(pointsXYZ)

pointsXYZ.l <- lapply(1:2, function(x) pointsXYZ[pointsXYZ$id %in% gridIds[[x]], ])
areas <- lapply(1:2, function(x) {
  print(paste("Working on time", x))
  r <- lapply(1:7, function(y) {
    print(paste("Rasterizing areas for", typenms[y]))
    areas <- aggregate(area ~ grid_id, data = typeAreas[[x]][[y]][, c(2, 4)], FUN = sum) 
  })
})
# Check places where sum of area greater than 100 (probably overlaps at provincial boundaries)
gid_gt100 <- lapply(1:2, function(x) {
  sapply(1:7, function(y) areas[[x]][[y]][areas[[x]][[y]]$area > 100.001, 1])
})  

# An examination reveals it was duplicated polygons (solution: union bad records)
# geom.tab <- dbGetQuery(con, paste("SELECT grid_id, area, new_type, ST_AsText(geom) FROM ", tabnames[set], 
#                                   " WHERE grid_id in (", paste(gid_gt100[[set]][[type]], collapse = ","), ")",
#                                   sep = ""))
# geom.tab <- geom.tab[geom.tab$new_type == typenms[type], ]
# geom.poly <- polyFromWkt(geom.tab[, c(1, 4)], crs=prjstr)
# grid.tab <- dbGetQuery(con, paste("SELECT id, ST_AsText(geom) FROM sa1kilo WHERE id in (", 
#                                   paste(gid_gt100[[set]][[type]], collapse = ","), ")", sep = ""))
# grid.poly <- polyFromWkt(grid.tab, crs=prjstr)
# i <- 3
# plot(grid.poly[i, ])
# dpoly <- geom.poly[geom.poly$ID == grid.poly[i, ]$ID, ]
# for(j in 1:nrow(dpoly)) plot(dpoly[j, ], add = TRUE, col = rgb(1, 0, 0, alpha = 0.5))
# areas[[set]][[type]][areas[[set]][[type]]$grid_id == grid.poly[i, ]$ID, "area"]

# Fix the areas in those records greater than 100.001
# Calculate correct areas first by unioning polygons that overlap
fixedAreas <- lapply(1:2, function(x) {
  print(x)
  dset <- gid_gt100[[x]]  # x <- 1
  fixedArea <- lapply(1:7, function(y) {  # y <- 1
    grids2fix <- dset[[y]]
    if(length(grids2fix) > 0) {
      sql <- paste("SELECT grid_id, new_type, area, ST_AsText(geom) FROM ", tabnames[x],  
                   " WHERE grid_id in (", paste(grids2fix, collapse = ","), ") AND new_type = '", 
                   typenms[y], "'", sep = "") 
      geom.tab <- dbGetQuery(con, sql)
      geom.poly <- polyFromWkt(geom.tab[, c(1, 4)], crs=prjstr)
      fixedGrid <- t(sapply(1:length(grids2fix), function(z) {  # z <- 1
        g <- grids2fix[z]
        p <- geom.poly[geom.poly$ID == g, ]
        u <- gUnaryUnion(p)
        c("grid_id" = g, "area" = gArea(u) / 10000)
      }))
    } else {
      fixedGrid <- NULL
    }
  })
})

# Then reassign those corrected values into the corresponding grid_ids in areas
areasUpdate <- lapply(1:2, function(x) {
  areaUpdate <- lapply(1:7, function(y) {
    f <- fixedAreas[[x]][[y]]
    if(!is.null(f)) {
      up <- areas[[x]][[y]]
      up[which(up$grid_id %in% f[, 1]), "area"] <- f[, 2]
      up$area <- round(up$area, 2)
      up
    } else {
      up <- areas[[x]][[y]]
      up$area <- round(up$area, 2)
      up
    }
  })
})

# # Check that right ones were fixed in news dataset
# all(unlist(lapply(1:2, function(x) {
#   unlist(lapply(1:7, function(y) {
#     a <- areas[[x]][[y]]
#     all(a[which(round(a$area, 2) != round(areasUpdate[[x]][[y]]$area, 2)), "grid_id"] == gid_gt100[[x]][[y]])
#   }))
# })))  # yes, only those ids were adjusted

# Now, onto the areas. 
areas.rl <- lapply(1:2, function(x) {
  print(paste("Working on time", x))
  r <- lapply(1:7, function(y) {
    print(paste("Rasterizing areas for", typenms[y]))
    #areas <- aggregate(area ~ grid_id, data = typeAreas[[x]][[y]][, c(2, 4)], FUN = sum) 
    mdf <- merge(pointsXYZ.l[[x]], areasUpdate[[x]][[y]], by.x = "id", by.y = "grid_id")
    areas.r <- rasterFromXYZ(mdf[, 2:4])
    areas.rs <- resample(areas.r, sa.r, method = "ngb")
  })
})
# plot(areas.rl[[1]][[1]] > 100)

cover2007 <- brick(stack(areas.rl[[1]]))
cover2011 <- brick(stack(areas.rl[[2]]))
names(cover2007) <- typenms
names(cover2011) <- typenms
# plot(cover2011[[1]] - cover2007[[1]])

writeRaster(cover2007, file = "data/cover2007.tif", overwrite = TRUE)
writeRaster(cover2011, file = "data/cover2011.tif", overwrite = TRUE)
save(cover2007, cover2011, areasUpdate, areas.rl, file = "data/coverAreas.rda")

# e <- extent(sa.r)
# test.r <- resample(areas.rl[[1]][[7]], sa.r, method = "ngb")
# plot(areas.rl[[1]][[7]], xlim = c(-500000, -450000), ylim = c(-3540000, -3500000))
# plot(test.r, add = TRUE, xlim = c(-500000, -450000), ylim = c(-3540000, -3500000))

#### checks on rasterization accuracy
# set <- 1
set <- 2
# set <- 2
# set <- 1
# type <- 1
type <- 4
# type <- 2
# type <- 5
chkset <- typeAreas[[set]][[type]][, c(2, 4)]
areas <- aggregate(area ~ grid_id, data = chkset, FUN = sum) 
# nrow(areas) == length(unique(chkset$grid_id))
mdf <- merge(pointsXYZ.l[[set]], areas, by.x = "id", by.y = "grid_id")
# mdf[1:10, ]
areas.r <- rasterFromXYZ(mdf[, 2:4])

set.seed(234)
s <- mdf[sample(1:nrow(mdf), size = 5, replace = FALSE), ]
geom.tab <- dbGetQuery(con, paste("SELECT grid_id, area, new_type, ST_AsText(geom) FROM ", tabnames[set], 
                                  " WHERE grid_id in (", paste(s$id, collapse = ","), ")", sep = ""))
geom.tab <- geom.tab[geom.tab$new_type == typenms[type], ]
geom.tab[, 1:3]
geom.poly <- polyFromWkt(geom.tab[, c(1, 4)], crs=prjstr)
grid.tab <- dbGetQuery(con, paste("SELECT id, ST_AsText(geom) FROM sa1kilo WHERE id in (", 
                                  paste(s$id, collapse = ","), ")", sep = ""))
grid.poly <- polyFromWkt(grid.tab, crs=prjstr)
# geom.poly$area <- geom.tab$area
# geom.poly$type <- geom.tab$new_type
i <- 5
ext <- extent(grid.poly[grid.poly$ID == s$id[i], ])
plot(grid.poly[grid.poly$ID == s$id[i], ])
plot(geom.poly[geom.poly$ID == s$id[i], ], add = TRUE)
#plot(geom.poly[geom.poly$ID == s$id[i], ])
rc <- crop(sa.r, ext)
areas.rc1 <- crop(areas.r, ext)
areas.rc2 <- crop(areas.rl[[set]][[type]], ext)
plot(rc, add = TRUE, legend = FALSE)
plot(areas.rc1, add = TRUE, legend = FALSE)
plot(geom.poly[geom.poly$ID == s$id[i], ], add = TRUE, col = "red")
mtext(side=3, line = -2, values(rc))
mtext(side=3, line = -1, geom.poly$ID[geom.poly$ID == s$id[i]][1])
mtext(side=1, line = -2, values(areas.rc1))
mtext(side=1, line = -3, values(areas.rc2))
mtext(side=1, line = -1, sum(geom.tab$area[geom.tab$grid_id == s$id[i]]))

