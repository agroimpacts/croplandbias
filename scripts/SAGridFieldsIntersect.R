##############################################################################################################
# Title      : SAGridFieldsIntersect.R
# Purpose    : Calculating intersection area between GTI crop fields and SA 1 km grid
# Author     : Lyndon Estes
# Draws from : 
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

prjsrid <- 97490
prj.sql <- paste("select proj4text from spatial_ref_sys where srid=", prjsrid, sep = "")
prjstr <- dbGetQuery(con, prj.sql)$proj4text
  
fldname <- "gti_sa_2011_alb"
gridname <- "sa1kilo"

# Grab data list of SA 1 km grids
grid.sql <- "select id from sa1kilo"
grids <- dbGetQuery(con, grid.sql)$id

fld.sql <- paste("select gid from", fldname)
flds <- dbGetQuery(con, fld.sql)$gid

# Read in crop fields
chunk <- 10000
a <- seq(1, length(flds), chunk)
b <- c(a, a[length(a)] + (length(flds) %% chunk - 1))
f.inds <- cbind(b[-length(b)] + 1, b[-1])
f.inds[1] <- b[1]

field.centers <- lapply(1:nrow(f.inds), function(x) {
  print(x)
  sql <- paste("SELECT gid, ST_AsEWKT(ST_Centroid(geom)) as center FROM", fldname,  "where gid>=", 
               f.inds[x, 1], "and gid<=", f.inds[x, 2])
  geom.tab <- dbGetQuery(con, sql)
  geom.tab[, 2] <- gsub("^SRID=*.*;", "", geom.tab[, 2])
  coord.mat <- do.call(rbind, lapply(1:nrow(geom.tab), function(y) {
    strip <- gsub("POINT|\\(|\\)", "", geom.tab[y, 2])
    coord.mat <- do.call(rbind, strsplit(strsplit(strip, ",")[[1]], " "))
  }))
  class(coord.mat) <- "numeric"
  point.tab <- cbind(geom.tab[, 1], coord.mat)
})

field.centers.t <- as.data.frame(do.call(rbind, field.centers))
colnames(field.centers.t) <- c("gid", "x", "y")
field.centers.to[1:100, ]
field.centers.to <- field.centers.t[with(field.centers.t, order(y, x, decreasing = TRUE)), ]
proj4string(field.centers.to) <- prjstr
coordinates(field.centers.to) <- ~x + y
# plot(field.centers.to[1:20000, ], cex = 0.1, pch = 20)
# plot(field.centers.to)
# nrow(field.centers.to)

# bound.sql <- paste("SELECT ST_AsEWKT(ST_SetSRID(ST_Extent(geom),", prjsrid, ")) as table_extent FROM", 
#                    gridname)
# bound.box <- dbGetQuery(con, bound.sql)
# bound.box <- gsub("^SRID=*.*;", "", bound.box)
# bound.poly <- polyFromWkt(geom.tab=cbind(1, bound.box), crs=prjstr)
# plot(bound.poly)
# for(i in 1:length(field.bounds)) plot(field.bounds[[i]], add = TRUE)

# tst <- gsub("MULTIPOLYGON|\\(|\\)", "", flds.geom.tab[1, 2])

# plot(as(extent(bounds), "SpatialPolygons"))
# plot(fields[[1]][sample(1:nrow(fields[[1]]), nrow(fields[[1]]) * 0.1), ], add = TRUE)

sql <- paste("Select distinct catname_1 from", fldname)
fld.types <- dbGetQuery(con, sql)
fld.types <- unname(apply(fld.types, 1, tolower))
fld.types[grep("subsistence", fld.types)] <- "subsistence"
fld.types <- unique(fld.types)

# Read in SA grids
chunk <- 10000
a <- seq(1, length(grids), chunk)
b <- c(a, a[length(a)] + (length(grids) %% chunk - 1))
g.inds <- cbind(b[-length(b)] + 1, b[-1])
g.inds[1] <- b[1]

fld.1km.ints <- lapply(1, function(x) {
  print(x)
#   x <- 1
#   sql <- paste("select id, ST_AsEWKT(geom) from sa1kilo where id>=", g.inds[x, 1], " and id<=", 
#                g.inds[x, 2], sep = "")
#   sql <- paste("SELECT id, ST_AsEWKT(ST_Centroid(geom)) as center FROM", gridname,  "where id>=", 
#                g.inds[x, 1], "and id<=", g.inds[x, 2])
#   geom.tab <- dbGetQuery(con, sql)
#   geom.tab[, 2] <- gsub("^SRID=*.*;", "", geom.tab[, 2])
#   coord.mat <- do.call(rbind, lapply(1:nrow(geom.tab), function(y) {
#     strip <- gsub("POINT|\\(|\\)", "", geom.tab[y, 2])
#     coord.mat <- do.call(rbind, strsplit(strsplit(strip, ",")[[1]], " "))
#   }))
#   class(coord.mat) <- "numeric"
#   point.tab <- as.data.frame(cbind(geom.tab[, 1], coord.mat))
#   colnames(point.tab) <- c("gid", "x", "y")
#   point.ext <- as(extent(point.tab), "SpatialPolygons")
#   proj4string(point.ext) <- prjstr
  sql <- paste("SELECT ST_AsEWKT(ST_SetSRID(ST_Extent(geom),", prjsrid, ")) as table_extent FROM", 
               gridname, "where id>=", g.inds[x, 1], "and id<=", g.inds[x, 2])
  bound.box <- dbGetQuery(con, sql)
  bound.box <- gsub("^SRID=*.*;", "", bound.box)
  bound.poly <- polyFromWkt(geom.tab=cbind(1, bound.box), crs=prjstr)
#   plot(bound.poly)
#   plot(point.ext, add = TRUE, border = 'green')
#   
  field.sel <- gIntersects(field.centers.to, bound.poly, byid = TRUE)
  field.int <- as.data.frame(field.centers.to)[field.sel, ]
#   field.int[1:10, ]
#   plot(field.int, add = TRUE)
#   sql <- paste("SELECT gid, catname_1, ST_AsEWKT(geom) FROM ", fldname,  " where gid in (", 
#                paste(field.int$gid, collapse = ","), ")", sep = "") 
  sql <- paste("SELECT gid, catname_1 FROM ", fldname,  " where gid in (", 
               paste(field.int$gid, collapse = ","), ")", sep = "") 
  fld.type.tab <- dbGetQuery(con, sql)
  fld.type.tab[, 2] <- sapply(1:nrow(fld.type.tab), function(j) tolower(fld.type.tab[j, 2]))
#   fmatches <- match(geom.tab[, 2], fld.types, nomatch=999)
  fmatches <- lapply(fld.types, function(y) grep(y, fld.type.tab[, 2])) 
  names(fmatches) <- fld.types
  if(sum(sapply(fmatches, length)) != nrow(fld.type.tab)) stop("Some field types were missed!")
  
  lapply(fmatches, function(z) {
#     z <- fmatches[[1]]
    fld.sel <- fld.type.tab[z, ]
#     fld.sel <- fld.sel[1:3, ]

    sql <- paste("SELECT ", gridname, ".id, ST_AsEWKT(", gridname, ".geom) FROM ", gridname, " INNER JOIN ", 
                 fldname, " on st_intersects(", gridname, ".geom, ", fldname, ".geom) WHERE ", gridname, 
                 ".id>=", g.inds[x, 1], " AND ", gridname, ".id<=", g.inds[x, 2], " AND ", fldname, 
                 ".gid in (", paste(fld.sel[, 1], collapse = ","), ")", sep = "")
    grid.geom.tab <- dbGetQuery(con, sql)
    grid.geom.tab[, 2] <- gsub("^SRID=*.*;", "", grid.geom.tab[, 2])
    grid.geom.poly <- polyFromWkt(geom.tab=grid.geom.tab, crs=prjstr)
    sql <- paste("SELECT gid, ST_AsEWKT(geom) FROM ", fldname, " WHERE gid in (", 
                 paste(fld.sel[, 1], collapse = ","), ")", sep = "")
    fld.geom.tab <- dbGetQuery(con, sql)
    fld.geom.tab[, 2] <- gsub("^SRID=*.*;", "", fld.geom.tab[, 2])
    fld.geom.poly <- polyFromWkt(geom.tab=fld.geom.tab, crs=prjstr)
    fld.geom.union <- gUnaryUnion(fld.geom.poly)
    fld.geom.union <- gBuffer(fld.geom.union, width=0)
    if(gIsValid(fld.geom.union)) {
      grid.int <- gIntersection(grid.geom.poly, fld.geom.union, byid = TRUE)
      grid.geom.poly$area <- gArea(grid.int, byid = TRUE) / 10000
    }
  })

  fld.types <- unique(fld.types)
  geom.tab[, 3] <- gsub("^SRID=*.*;", "", geom.tab[, 3])
  
  
  
  
  fld.types
  
  
  
  
  geom.tab[1:10, ]
  
  
  
  coordinates(field.int) <- ~x + y
  
  
  
  plot(field.int)
  plot(point.ext, add = T)
  
  colnames(point.tab) <- c("gid", "x", "y")
  coordinates(point.tab) <- ~x + y
  proj4string(point.tab) <- prjstr
  gridded(point.tab) <- TRUE
  as(point.tab, "raster")
  plot(point.tab)
  class(point.tab)
  r <- raster(point.tab)
  
  plot(r)
  
  point.tab.t <- as.data.frame(do.call(rbind, field.centers))
  colnames(field.centers.t) <- c("gid", "x", "y")
  
  plot(point.tab, cex = 0.1, pch = 20)

  sql <- paste("select id, ST_AsEWKT(geom) from sa1kilo where id>=", g.inds[x, 1], " and id<=", 
               g.inds[x, 2], sep = "")
  geom.tab <- dbGetQuery(con, sql)
  geom.tab[, 2] <- gsub("^SRID=*.*;", "", geom.tab[, 2])
  geom.poly <- polyFromWkt(geom.tab=geom.tab, crs=prjstr)
  plot(geom.poly[1:10, ])
  plot(point.tab[1:10, ], add = TRUE)
  plot(r, add = TRUE)
  
  test <- over(point.tab, field.centers.to)
  test <- gIntersects(point.tab, field.centers.to)
  
  
  p1 <- as(extent(geom.poly), "SpatialPolygons")
  p2 <- as(extent(geom.poly), "SpatialPolygons")
  plot(p1)
  plot(p2, add = TRUE)
  p1@data
  
  plot(grids.poly)
  
})


field.bounds <-lapply(1:nrow(f.inds), function(x) {
  field.centers <-lapply(1:nrow(f.inds), function(x) {
    print(x)
    flds.sql <- paste("select gid, ST_AsEWKT(geom) from ", fldname, " where gid>=", f.inds[x, 1], " and gid<=", 
                      f.inds[x, 2], sep = "")
    flds.geom.tab <- dbGetQuery(con, flds.sql)
    flds.geom.tab[, 2] <- gsub("^SRID=*.*;", "", flds.geom.tab[, 2])
    #   flds.poly <- polyFromWkt(geom.tab=flds.geom.tab, crs=prjstr)
    #   coord.mat <- do.call(rbind, lapply(1:nrow(flds.geom.tab), function(x) {
    #     strip <- gsub("MULTIPOLYGON|\\(|\\)", "", flds.geom.tab[x, 2])
    #     coord.mat <- do.call(rbind, strsplit(strsplit(strip, ",")[[1]], " "))
    #   }))
    coord.mat <- do.call(rbind, lapply(1:nrow(flds.geom.tab), function(x) {
      strip <- gsub("MULTIPOLYGON|\\(|\\)", "", flds.geom.tab[x, 2])
      coord.mat <- do.call(rbind, strsplit(strsplit(strip, ",")[[1]], " "))
      class(coord.mat) <- "numeric"
      point.mu <- colMeans(coord.mat)
    }))
    #   
    #   class(coord.mat) <- "numeric"
    #   bounds <- c(apply(coord.mat, 2, min), apply(coord.mat, 2, max))
    #   bounds <- bounds[c(1, 3, 2, 4)]
    #   bound.poly <- as(extent(bounds), "SpatialPolygons")
    #   return(bound.poly)
  })
  