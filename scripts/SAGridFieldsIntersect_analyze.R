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
sapply(1:2, function(x) sum(unlist(checkTabs[[x]])))  # tab2007 + cloudCt = tabCts[1], 2011 all equal

# All balanced, let's move on to processing
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

typeAreas[[1]][[3]][1:10, ]



















