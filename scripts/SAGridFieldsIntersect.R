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

########################################################
# Grab data list of SA 1 km grids
grid.sql <- "select id from sa1kilo"
grids <- dbGetQuery(con, grid.sql)$id

fld.sql <- paste("select gid from", fldname)
flds <- dbGetQuery(con, fld.sql)$gid

# get field categories
sql <- paste("Select distinct catname_1 from", fldname)
fld.types <- dbGetQuery(con, sql)
fld.types <- unname(apply(fld.types, 1, tolower))
fld.types[grep("subsistence", fld.types)] <- "subsistence"
fld.types <- unique(fld.types)

sql <- paste("SELECT ST_AsEWKT(ST_SetSRID(ST_Extent(geom),", prjsrid, ")) as table_extent FROM", gridname)
bound.box <- dbGetQuery(con, sql)
bound.box <- gsub("^SRID=*.*;", "", bound.box)
bound.poly <- polyFromWkt(geom.tab=cbind(1, bound.box), crs=prjstr)
bound.ext <- extent(bound.poly)
chunk <- 20000
bound.incr <- (bound.ext@ymax - bound.ext@ymin) / chunk
bound.rem <- (bound.ext@ymax - bound.ext@ymin) %% chunk
ys <- seq(bound.ext@ymax, bound.ext@ymin, -chunk)
ysb <- c(ys, ys[length(ys)] - bound.rem)
yadd <- cbind(ys[-length(ysb)], ysb[-1])

split.polys <- lapply(1:nrow(yadd), function(x) {
  coords <- c(xmin = bound.ext@xmin, xmax = bound.ext@xmax, ymin = yadd[x, 2], ymax = yadd[x, 1])
})

# Pull out SA grid cells corresponding to polygon "chunks"
check.id.overlaps <- lapply(1:length(split.polys), function(x) {
  print(x)
  box.geom <- writeWKT(as(extent(split.polys[[x]]), "SpatialPolygons"))
  sql <- paste("SELECT ", gridname, ".id FROM ", gridname, " WHERE st_intersects('SRID=", prjsrid, ";", 
               box.geom, "'::geometry, ", gridname, ".geom)", sep = "")
  grid.geom.tab <- dbGetQuery(con, sql)$id
})
# length(unlist(check.id.overlaps)); length(unique(unlist(check.id.overlaps)))

# Find and remove overlapping polygons 
grid.ids <- lapply(2:length(check.id.overlaps), function(x) {
  check.id.overlaps[[x]][!check.id.overlaps[[x]] %in% check.id.overlaps[[x - 1]]]
})  
grid.ids.all <- vector("list", length(check.id.overlaps))
grid.ids.all[[1]] <- check.id.overlaps[[1]] 
for(i in 2:length(grid.ids.all)) grid.ids.all[[i]] <- grid.ids[[i - 1]]
# length(unlist(grid.ids.all))
#which(duplicated(unlist(grid.ids.all)))  # no duplicates, so theoretically no overlaps left

tick <- Sys.time()
fld.1km.ints <- lapply(1:length(grid.ids.all), function(x) {

  # select fields intersecting with grid ids within the box
  print(paste("Processing section", x))
  box.geom <- writeWKT(as(extent(split.polys[[x]]), "SpatialPolygons"))
  sql <- paste("SELECT ", fldname, ".gid FROM ", fldname, " WHERE st_intersects('SRID=", prjsrid, ";", 
               box.geom, "'::geometry, ", fldname, ".geom)", sep = "")
  fld.id.tab <- dbGetQuery(con, sql)$gid
  sql <- paste("SELECT ", gridname, ".id, ", fldname, ".gid, ", fldname, ".catname_1 ",  "FROM ", gridname, 
               " INNER JOIN ", fldname, " on st_intersects(", gridname, ".geom, ", fldname, ".geom) WHERE ", 
               gridname, ".id in (", paste(grid.ids.all[[x]], collapse = ","), ")", " AND ", fldname, 
               ".gid in (", paste(fld.id.tab, collapse = ","), ")", sep = "")
  system.time(fieldgrid.id.tab <- dbGetQuery(con, sql))
  fieldgrid.id.tab[, 3] <- sapply(1:nrow(fieldgrid.id.tab), function(j) tolower(fieldgrid.id.tab[j, 3]))
  fmatches <- lapply(fld.types, function(y) grep(y, fieldgrid.id.tab[, 3])) 
  names(fmatches) <- fld.types
  if(sum(sapply(fmatches, length)) != nrow(fieldgrid.id.tab)) stop("Some field types were missed!")

  ding <- Sys.time()
  fld.type.ints <- lapply(1:length(fmatches), function(j) {
    print(paste("Processing intersections with Class", j, ":", names(fmatches)[j]))
    fld.sel <- fieldgrid.id.tab[fmatches[[j]], ]
    if(nrow(fld.sel) == 0) {
      fld.ints <- 0
    } else {
      sql <- paste("SELECT id, ST_AsText(geom) FROM ", gridname, " WHERE id in (",
                 paste(fld.sel$id, collapse = ","), ")", sep = "")
      print("  fetching grids")
      grid.geom.tab <- dbGetQuery(con, sql)
      print("  fetching fields")
      sql <- paste("SELECT gid, ST_AsText(geom) FROM ", fldname, " WHERE gid in (",
                   paste(fld.sel$gid, collapse = ","), ")", sep = "")
      fld.geom.tab <- dbGetQuery(con, sql)
      
      fld.ints <- t(sapply(unique(fld.sel$id), function(k) {
        print(paste("processing grid", k))
        g <- polyFromWkt(geom.tab=grid.geom.tab[grid.geom.tab$id == k, ], crs=prjstr)
        fid <- fld.sel[fld.sel$id == k, "gid"]
        f <- polyFromWkt(geom.tab=fld.geom.tab[fld.geom.tab$gid %in% fid, ], crs=prjstr)
        if(!gIsValid(f)) {
          f.union <- gUnaryUnion(f)
          if(!gIsValid(f.union)) {
            print("Unioned geometry not valid, first try fix with gBuffer")
            f.union2 <- gBuffer(f.union, width=0)
            if(!gIsValid(f.union2)) {
              print("Still not valid, call pprepair to clean this one")
              td <- tempdir()
              tmpnmin <- strsplit(tempfile("poly", tmpdir = ""), "/")[[1]][2]
              tmpnmout <- strsplit(tempfile("poly", tmpdir = ""), "/")[[1]][2]
              writeOGR(f.union2, dsn = td, layer = tmpnmin, driver = "ESRI Shapefile")
              f.union3 <- callPprepair(td, spdfinname = tmpnmin, spdfoutname = tmpnmout, 
                                       crs = f.union2@proj4string)
              f.polys <- f.union3
            } else {
              print("Buffering work, geometry now valid")
              f.polys <- f.union2
            }
          } else {
            print("Unioned geometry is valid")
            f.polys <- f.union
          } 
        } else {
          f.polys <- f
        }
        #print("Processing intersections now")
        grid.int <- gIntersection(g, f.polys, byid = TRUE)
        farea <- gArea(grid.int, byid = TRUE) / 10000
        c("id" = k, "area" = unname(sum(farea)))  # sum areas to make for single record
      }))
    }
  })
  names(fld.type.ints) <- names(fmatches)
  dong <- Sys.time()
  dong - ding
})
tock <- Sys.time()
tock - tick

save(fld.1km.ints, file = 
     paste("/var/local/as/home/lestes/analyses/SAcropland/data/", fldname, "_intersects.rda", sep = ""))

