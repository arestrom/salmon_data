#======================================================================================
# Test of location upload strategies
#
# AS 2019-08-22
#======================================================================================

library(odbc)
library(DBI)
library(remisc)
library(sf)
library(glue)
library(tibble)
library(pool)

# Keep connections pane from opening
options("connectionObserver" = NULL)

# Define dsns
location_test_db = "location_test_db"
# Set up dsn connection
pool = pool::dbPool(drv = odbc::odbc(), timezone = "UTC", dsn = location_test_db)

# Checkout a connection
con = poolCheckout(pool)

# Test connection
dbListTables(con)

# Return to pool
poolReturn(con)

# Create data to insert to location table
location_id = remisc::get_uuid(1L)
location_name = "01-08-28-19"
created_by = Sys.getenv("USERNAME")

# Checkout a connection
con = poolCheckout(pool)
insert_loc_result = dbSendStatement(
  con, glue_sql("INSERT INTO location (",
                "location_id, ",
                "location_name, ",
                "created_by) ",
                "VALUES (",
                "?, ?, ?)"))
dbBind(insert_loc_result, list(location_id, location_name, created_by))
dbGetRowsAffected(insert_loc_result)
dbClearResult(insert_loc_result)
poolReturn(con)

# Create insert values for geometry table
horizontal_accuracy = 8L
comment_text = "Very sparse"
gid = 2L
lon = -123.300142
lat = 46.485271
#geom = "ST_GeomFromText('POINT(-123.300146  46.485270)', 2927)"
geom = glue("(select ST_GeomFromText('POINT({lon}  {lat})', 2927))")

qry = glue_sql("INSERT INTO location_coordinates ",
               "(location_id, horizontal_accuracy, comment_text, gid, geom, created_by) ",
               "VALUES ({location_id}, {horizontal_accuracy}, {comment_text}, {gid}, ST_GeomFromText('POINT({lon} {lat})', 2927), {created_by}) ",
               .con = con)

# Checkout a connection
con = poolCheckout(pool)
DBI::dbExecute(con, qry)
poolReturn(con)

#===================================================================
# Test without GID....Works
#===================================================================

# Create insert values for geometry table
horizontal_accuracy = 2L
comment_text = "Nada again"
lon = -123.300258
lat = 46.485279

qry = glue_sql("INSERT INTO location_coordinates ",
               "(location_id, horizontal_accuracy, comment_text, gid, geom, created_by) ",
               "VALUES ({location_id}, {horizontal_accuracy}, {comment_text}, ",
               "nextval('location_coordinates_gid_seq'::regclass), ",
               "ST_GeomFromText('POINT({lon} {lat})', 2927), {created_by}) ",
               .con = con)

# Checkout a connection
con = poolCheckout(pool)
DBI::dbExecute(con, qry)
poolReturn(con)

#===================================================================
# Test without GID....Does not work
#===================================================================

# Create insert values for geometry table
horizontal_accuracy = 2L
comment_text = "Nada again"
lon = -123.300258
lat = 46.485279

qry = glue_sql("INSERT INTO location_coordinates ",
               "(location_id, horizontal_accuracy, comment_text, gid, geom, created_by) ",
               "VALUES (?, ?, ?, ",
               "nextval('location_coordinates_gid_seq'::regclass), ",
               "ST_GeomFromText('POINT(? ?)', 2927), {created_by}) ",
               .con = con)

# Checkout a connection
con = poolCheckout(pool)
query = DBI::dbExecute(con, qry)
DBI::dbBind(query, list(location_id, horizontal_accuracy, comment_text, lon, lat, created_by))
DBI::dbExecute(query)
poolReturn(con)


#===================================================================
# Test without send_statement...not working...just using above code
#===================================================================

# Create insert values for geometry table
horizontal_accuracy = 15L
comment_text = "Nada Nada"
lon = -123.300254
lat = 46.485277

# Checkout a connection
con = poolCheckout(pool)
insert_loc_result = dbSendStatement(
  con, glue("INSERT INTO location_coordinates ",
                "(location_id, horizontal_accuracy, comment_text, gid, geom, created_by) ",
                "VALUES (?, ?, ?, ",
                "nextval('location_coordinates_gid_seq'::regclass), ",
                "ST_GeomFromText('POINT(? ?)', 2927), ?)")
dbBind(insert_loc_result, list(location_id, horizontal_accuracy, comment_text, lon, lat, created_by))
dbGetRowsAffected(insert_loc_result)
dbClearResult(insert_loc_result)
poolReturn(con)














# Checkout a connection
con = poolCheckout(pool)
insert_loc_result = dbSendStatement(
  con, glue_sql("INSERT INTO location_coordinates (",
                "location_id, ",
                "horizontal_accuracy, ",
                "comment_text, ",
                "gid, ",
                "geom, ",
                "created_by) ",
                "VALUES (",
                "?, ?, ?, ?, ?, ?)"))
dbBind(insert_loc_result, list(location_id, horizonal_accuracy,
                               comment_text, gid, geom, created_by))
dbGetRowsAffected(insert_loc_result)
dbClearResult(insert_loc_result)
poolReturn(con)

# Create data to insert to location_coordinates table
geom = st_point(c(-123.300146, 46.485270)) %>%
  st_sfc(., crs = 4326) %>%
  st_transform(., 2927)

# Try sf and odbc
coords = tibble(location_id = location_id,
                geom = geom,
                created_by = created_by)

# Write to temp then select insert
con = poolCheckout(pool)
st_write(obj = coords, dsn = con, layer = "location_coordinates_temp", overwrite = TRUE)

# Use select into query to get data into location_coordinates
qry = glue::glue("insert into location_coordinates ",
                 "(location_id, geom, created_by) ",
                 "select cast(location_id as uuid), geom, created_by FROM location_coordinates_temp")

# Insert select to DB
DBI::dbExecute(con, qry)

# Drop temp
dbExecute(con, "drop table location_coordinates_temp")

# Return con to pool
poolReturn(con)

#==============================================================================================================
# TEST ON REAL DB, using data in temp table
#==============================================================================================================

# Define dsns
salmon_db = "local_spawn"

# Set up dsn connection
chk_pool = pool::dbPool(drv = odbc::odbc(), timezone = "UTC", dsn = salmon_db)

# Use select into query to get data into location_coordinates: ERROR: duplicate key value violates unique constraint "location_coordinates_gid_key";
qry = glue::glue("insert into location_coordinates ",
                 "(location_id, horizontal_accuracy, geom, created_by) ",
                 "select cast(location_id as uuid), horizontal_accuracy, geom, created_by FROM location_coordinates_temp")

# Insert select to DB
chk_con = poolCheckout(chk_pool)
DBI::dbExecute(chk_con, qry)

# Return con to pool
poolReturn(chk_con)

#==============================================================================================================
# TEST ON REAL DB, using data in temp table....gid needed to be defined in temp table....redo so it is loaded
#==============================================================================================================

# Define dsns
salmon_db = "local_spawn"

# Set up dsn connection
chk_pool = pool::dbPool(drv = odbc::odbc(), timezone = "UTC", dsn = salmon_db)

# Use select into query to get data into location_coordinates: ERROR: duplicate key value violates unique constraint "location_coordinates_gid_key";
qry = glue::glue("insert into location_coordinates ",
                 "(location_id, horizontal_accuracy, gid, geom, created_by) ",
                 "select cast(location_id as uuid), horizontal_accuracy, gid, geom, created_by FROM location_coordinates_temp")

# Insert select to DB
chk_con = poolCheckout(chk_pool)
DBI::dbExecute(chk_con, qry)

# Return con to pool
poolReturn(chk_con)

#==========================================================================
# Get centroid of waterbody to use in interactive redd_map
#==========================================================================

# Define dsns
salmon_db = "local_spawn"

# Set up dsn connection
pool = pool::dbPool(drv = odbc::odbc(), timezone = "UTC", dsn = salmon_db)

# Alder Creek 23.1185
waterbody_id = '3fd8a43f-505c-4e1b-9474-94046099af62'

# Stream centroid
get_stream_centroid = function(waterbody_id) {
  qry = glue("select DISTINCT waterbody_id, ",
             "ST_X(ST_Transform(ST_Centroid(geom), 4326)) as center_lon, ",
             "ST_Y(ST_Transform(ST_Centroid(geom), 4326)) as center_lat ",
             "from stream ",
             "where waterbody_id = '{waterbody_id}'")
  con = poolCheckout(pool)
  stream_centroid = DBI::dbGetQuery(con, qry)
  poolReturn(con)
  return(stream_centroid)
}

# Test
stream_centroid = get_stream_centroid(waterbody_id)

# Function to close pool
onStop(function() {
  poolClose(pool)
})
