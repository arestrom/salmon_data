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
location_name = "01-23-19-52"
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
# TEST ON REAL DB, using data in temp table
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










# Function to close pool
onStop(function() {
  poolClose(pool)
})
