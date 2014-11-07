##############################################################################################################
# Title      : SAGridFieldsIntersect_postgis_test.R
# Purpose    : Workflow between R, python, and postgis to clean up polygons and calculate intersections
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

# Set up test tables
# Test fields
tfldname <- "test_fields"
sql <- paste("CREATE TABLE test_fields AS SELECT gid, areaha, catname_1, field_id, geom FROM",
             fldname, "WHERE gid <= 20000")
res <- dbSendQuery(con, sql)
sql <- "CREATE INDEX field_gix ON test_fields USING GIST(geom)"
res <- dbSendQuery(con, sql)
sql <- "VACUUM ANALYZE test_fields"
res <- dbSendQuery(con, sql)
sql <- "CLUSTER test_fields USING field_gix"
res <- dbSendQuery(con, sql)
sql <- "ANALYZE test_fields"  
res <- dbSendQuery(con, sql)
sql <- "ALTER TABLE test_fields ADD COLUMN fixed integer"
res <- dbSendQuery(con, sql)

# Screen these for bad geometries, and fix
sql <- "SELECT gid, st_AsText(geom) FROM test_fields WHERE NOT st_isvalid(test_fields.geom)"
bad.flds <- dbGetQuery(con, sql)
# i <- 9
# bad.flds$gid
# plot(polyFromWkt(bad.flds[i, ], prjstr))
# plot(clean.flds[i, ], add = TRUE, border = "blue")
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
  sql <- paste("UPDATE test_fields SET geom=ST_GeomFromEWKT('SRID=", prjsrid, ";", wkt[2], "'), ",
               "fixed = 1 WHERE gid=", wkt[1], sep = "")
  res <- dbSendQuery(con, sql)
})

# Create test fields without fixing to compare
# sql <- paste("CREATE TABLE test_fields_compare AS SELECT gid, areaha, catname_1, field_id, geom FROM",
#              fldname, "WHERE gid <= 20000")
# res <- dbSendQuery(con, sql)

# Test grids
tgname <- "test_grids"
sql <- paste("CREATE TABLE", tgname, "AS SELECT DISTINCT sa1kilo.* FROM sa1kilo,", tfldname, 
             "WHERE ST_Intersects(sa1kilo.geom,", paste(tfldname, ".geom)", sep = ""))
d <- dbSendQuery(con, sql)
sql <- paste("CREATE INDEX ", tgname, "_gix ON ", tgname, " USING GIST(geom)", sep = "")
d <- dbSendQuery(con, sql)
d <- dbSendQuery(con, paste("VACUUM ANALYZE", tgname))
d <- dbSendQuery(con, paste("CLUSTER", tgname, "USING", paste(tgname, "_gix", sep = "")))
d <- dbSendQuery(con, paste("ANALYZE", tgname))

# Now test intersection
# First set up the table needed
tisectname <- "test_grid_isect"
sql <- paste("CREATE TABLE",  tisectname, 
             "(gid serial primary key,", 
             "grid_id integer,", 
             "field_gid integer,",  
             "field_type varchar(80),", 
             "geom geometry(multipolygon,", prjsrid, ")",
             ");")
d <- dbSendQuery(con, sql)

sql <- paste("INSERT INTO", tisectname, "(grid_id, field_gid, field_type, geom)", 
             "SELECT", 
             "a.id AS grid_id,", 
             "b.gid AS field_gid,", "b.catname_1 AS field_type,",  
             "CASE", 
             "WHEN ST_Within(a.geom,b.geom)", 
             "THEN a.geom", 
             "ELSE ST_Multi(ST_Intersection(a.geom,b.geom))", 
             "END AS geom", 
             "FROM", tgname, "a",
             "JOIN", tfldname, "b", 
             "ON ST_Intersects(a.geom, b.geom)")
system.time(d <- dbSendQuery(con, sql))  # 6.211

# Let's look at it now
# iid <- dbGetQuery(con, paste("SELECT grid_id, ST_AsText(geom) FROM", tisectname, "WHERE"))
gid <- dbGetQuery(con, paste("SELECT id FROM", tgname))
tg.ss <- dbGetQuery(con, paste("SELECT id, ST_AsText(geom) FROM ", tgname, " WHERE id in (", 
                               paste(gid$id[1:100], collapse = ","), ")", sep = ""))
tg.ss.poly <- polyFromWkt(tg.ss, prjstr)
ig.ss <- dbGetQuery(con, paste("SELECT grid_id, field_type, ST_AsText(geom) FROM ", tisectname, 
                               " WHERE grid_id in (", paste(gid$id[1:100], collapse = ","), ")", sep = ""))
ig.ss.polys <- lapply(unique(ig.ss$field_type), function(x) {
  dat <- ig.ss[ig.ss$field_type == x, c(1, 3)]
  ig.ss.poly <- polyFromWkt(dat, prjstr)  
})

i <- 1
plot(tg.ss.poly[i, ])
plot(ig.ss.polys[[1]][ig.ss.polys[[1]]$ID %in% tg.ss.poly$ID[i], ], add = TRUE)

plot(tg.ss.poly, col = "grey", lwd = 0.1)
plot(ig.ss.polys[[3]], add = TRUE, col = "red")
plot(ig.ss.polys[[1]], add = TRUE, col = "blue")
plot(ig.ss.polys[[2]], add = TRUE, col = "green")

