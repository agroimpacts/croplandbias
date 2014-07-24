##############################################################################################################
# Title      : SAGridFieldsIntersect_postgis.R
# Purpose    : Workflow between R, and postgis to clean up polygons and calculate intersections
# Author     : Lyndon Estes
# Draws from : SAGridFieldsIntersect_postgis.R; SAGridFieldsIntersect.R
# Used by    : 
# Notes      : 
##############################################################################################################

# Libraries
suppressMessages(library(RPostgreSQL))
suppressMessages(library(rgdal))
suppressMessages(library(rgeos))
# suppressMessages(library(raster))
source("/u/sandbox/afmap/R/KMLAccuracyFunctions.R")

test <- "Y"

if(test == "N") {
  args <- commandArgs(TRUE)
  fldname_in <- args[1]  # name of data table to create to hold cleaned fields in
  fldname_out <- args[2]  # name of data table to create to hold cleaned fields in
  grname_in <- args[3]  # name of input grid for comparison
  grname_out <- args[4]  # name of input grid for comparison
  isectname <- args[5]  # name of output table with intersections
} else if(test == "Y") {
  fldname_in <- "sa_flds_2011_alb"
  fldname_out <- "sa_flds_2011_alb_cl"
  isectname <- "sa_fg_isect_2011"
  grname_in <- "sa1kilo"  # name of input grid for comparison
  grname_out <- "sa_isect_grid"  # name of input grid for comparison
}

# Connection
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "SouthAfricaSandbox", user = "postgis", password = "P0stG1S")

prjsrid <- 97490
prj.sql <- paste("select proj4text from spatial_ref_sys where srid=", prjsrid, sep = "")
prjstr <- dbGetQuery(con, prj.sql)$proj4text

# First clean up SA fields datatables, starting with duplicate of it
if(!dbExistsTable(con, name = fldname_out)) {
  d <- dbSendQuery(con, paste("CREATE TABLE",  fldname_out, "AS SELECT gid, catname, geom FROM", fldname_in))
  d <- dbSendQuery(con, paste("CREATE INDEX gix_",  fldname_out, " ON ",  fldname_out, " USING GIST(geom)", 
                   sep = ""))
  d <- dbSendQuery(con, paste("VACUUM ANALYZE",  fldname_out))
  d <- dbSendQuery(con, paste("CLUSTER ", fldname_out, " USING gix_", fldname_out, sep = ""))
  d <- dbSendQuery(con, paste("ANALYZE",  fldname_out))
  d <- dbSendQuery(con, paste("ALTER TABLE",  fldname_out, "ADD COLUMN fixed integer"))
} else {
  print(paste(fldname_out, "exists, carry on")
}  

# Screen these for bad geometries, and fix
bad.flds <- dbGetQuery(con, paste("SELECT gid, st_AsText(geom) FROM ", fldname_out, " WHERE NOT st_isvalid(", 
                                  fldname_out, ".geom)", sep = ""))
# bad.flds <- dbGetQuery(con, paste("SELECT gid, st_AsText(geom) FROM ", "test_fields", " WHERE NOT st_isvalid(", 
#                                   "test_fields", ".geom)", sep = ""))
if(nrow(bad.flds) > 0) {
  clean.flds <- createCleanTempPolyfromWKT(bad.flds, crs = prjstr)$polygons
  fixit <- lapply(1:nrow(clean.flds), function(x) {
    print(x)
    poly <- clean.flds[x, ]
    if(gIsValid(poly)) {
      wkt <- c(poly$ID, writeWKT(poly))
      wkt[2] <- gsub("POLYGON \\(", "MULTIPOLYGON\\((", wkt[2])
      wkt[2] <- gsub("\\))", "\\)))", wkt[2])
    } else {
      stop("Problem")
    }
    sql <- paste("UPDATE ", fldname_out, " SET geom=ST_GeomFromEWKT('SRID=", prjsrid, ";", wkt[2], "'), ",
                 "fixed = 1 WHERE gid=", wkt[1], sep = "")
    res <- dbSendQuery(con, sql)
  })
  # Test again
  bad.flds2 <- dbGetQuery(con, paste("SELECT gid FROM ", fldname_out, " WHERE NOT st_isvalid(", 
                                     fldname_out, ".geom)", sep = ""))
  if(nrow(bad.flds2) > 0) stop("cleaning didn't get everything!")
} else {
  print(paste(fldname_out, "has no bad fields, carry on")
} 

# Set up new sa grid so that proper indexing can be applied
if(!dbExistsTable(con, name = grname_out)) {
  d <- dbSendQuery(con, paste("CREATE TABLE",  grname_out, "AS SELECT gid, id, geom FROM", grname_in))
  d <- dbSendQuery(con, paste("CREATE INDEX gix_",  grname_out, " ON ",  grname_out, " USING GIST(geom)", 
                              sep = ""))
  d <- dbSendQuery(con, paste("VACUUM ANALYZE",  grname_out))
  d <- dbSendQuery(con, paste("CLUSTER ", grname_out, " USING gix_", grname_out, sep = ""))
  d <- dbSendQuery(con, paste("ANALYZE",  grname_out))
  bad.grds <- dbGetQuery(con, paste("SELECT gid FROM ", grname_out, " WHERE NOT st_isvalid(", grname_out, 
                                    ".geom)", sep = ""))
  if(nrow(bad.grds) > 0) stop("Not all grids are clean!")
} else {
  print(paste(grname_out, "exists, carry on")
}  

# Now test intersection
# First set up the table needed
if(!dbExistsTable(con, name = isectname)) {
  sql <- paste("CREATE TABLE",  isectname, 
               "(gid serial primary key,", 
               "grid_id integer,", 
               "field_gid integer,",  
               "field_type varchar(80),", 
               "geom geometry(multipolygon,", prjsrid, ")",
               ");")
  d <- dbSendQuery(con, sql)
} else {
  print(paste(isectname, "exists, carry on")
} 

tick <- Sys.time()
print(paste("Processing intersection, starting at time", tick))
sql <- paste("INSERT INTO", isectname, "(grid_id, field_gid, field_type, geom)", 
             "SELECT", 
             "a.id AS grid_id,", 
             "b.gid AS field_gid,", "b.catname AS field_type,",  
             "CASE", 
             "WHEN ST_Within(a.geom,b.geom)", 
             "THEN a.geom", 
             "ELSE ST_Multi(ST_Intersection(a.geom,b.geom))", 
             "END AS geom", 
             "FROM", grname_out, "a",
             "JOIN", fldname_out, "b", 
             "ON ST_Intersects(a.geom, b.geom)")
system.time(d <- dbSendQuery(con, sql))  # 6.211
tock <- Sys.time()
print(paste("Finished intersections at ", tock, ", total time = ", tock - tick, sep = ""))




