#===============================================================================
# Verify queries work
#
# AS 2020-02-25
#===============================================================================

# Load libraries
library(odbc)
library(DBI)
library(RPostgres)
library(tibble)
library(sf)
library(glue)

#=====================================================================================
# Function to get user for database
pg_user <- function(user_label) {
  Sys.getenv(user_label)
}

# Function to get pw for database
pg_pw <- function(pwd_label) {
  Sys.getenv(pwd_label)
}

# Function to get pw for database
pg_host <- function(host_label) {
  Sys.getenv(host_label)
}

# Function to connect to postgres
pg_con_local = function(dbname, port = '5432') {
  con <- dbConnect(
    RPostgres::Postgres(),
    host = "localhost",
    dbname = dbname,
    user = pg_user("pg_user"),
    password = pg_pw("pg_pwd_local"),
    port = port)
  con
}

#================================================================
# Base testing...check difference between binary 2927, 4326
#================================================================

# Coordinates
latitude = 46.5567
longitude = -123.7764

# 4326
geom_4326 = st_point(c(longitude, latitude)) %>%
  st_sfc(., crs = 4326) %>%
  st_as_binary(., hex = TRUE)

# 2927
geom_2927 = st_point(c(longitude, latitude)) %>%
  st_sfc(., crs = 4326) %>%
  st_transform(., 2927) %>%
  st_as_binary(., hex = TRUE)

# Compare
geom_4326
geom_2927

#===============================================================
# Current method in salmon_data
#===============================================================

# Parameters
latitude = 46.5567
longitude = -123.7764
location_id = "72b6a260-5317-41d1-bcd0-ec0e9991c195"
horizontal_accuracy = 10L
comment_text = "Nothing"
created_by = Sys.getenv("USERNAME")

# Standard method
con = pg_con_local("location_test")
qry = glue_sql("INSERT INTO location_coordinates ",
               "(location_id, horizontal_accuracy, comment_text, geom, created_by) ",
               "VALUES ({location_id}, {horizontal_accuracy}, {comment_text}, ",
               "ST_Transform(ST_GeomFromText('POINT({longitude} {latitude})', 4326), 2927), ",
               "{created_by}) ",
               .con = con)
DBI::dbExecute(con, qry)
DBI::dbDisconnect(con)

#===============================================================
# New insert method from chehalis_data...This works.
#===============================================================

# Parameters
latitude = 46.5567
longitude = -123.7764
location_id = "85856d58-a136-494c-9448-f86b79f9083f"
location_coordinates_id = remisc::get_uuid(1L)
horizontal_accuracy = 6L
comment_text = "Nothing"
created_by = Sys.getenv("USERNAME")
mod_dt = lubridate::with_tz(Sys.time(), "UTC")
mod_by = Sys.getenv("USERNAME")
# Run update....This works
con = pg_con_local("location_test")
#con = dbConnect(odbc::odbc(), dsn = "location_test_db")
insert_lc_result = dbSendStatement(
  con, glue_sql("INSERT INTO location_coordinates (",
                "location_coordinates_id, ",
                "location_id, ",
                "horizontal_accuracy, ",
                "geom, ",
                "created_by) ",
                "VALUES (",
                "$1, $2, $3, ",
                "ST_Transform(ST_GeomFromText('POINT({longitude} {latitude})', 4326), 2927), ",
                "$4)"))
dbBind(insert_lc_result, list(location_coordinates_id, location_id,
                              horizontal_accuracy, created_by))
dbGetRowsAffected(insert_lc_result)
dbClearResult(insert_lc_result)
DBI::dbDisconnect(con)


#===============================================================
# New update method from chehalis_data....Works
#===============================================================

# Parameters
latitude = 46.5567
longitude = -123.7764
location_id = "72b6a260-5317-41d1-bcd0-ec0e9991c195"
horizontal_accuracy = 6L
comment_text = "Nothing"
created_by = Sys.getenv("USERNAME")
mod_dt = lubridate::with_tz(Sys.time(), "UTC")
mod_by = Sys.getenv("USERNAME")
geom = st_point(c(longitude, latitude)) %>%
  st_sfc(., crs = 4326) %>%
  st_transform(., 2927) %>%
  st_as_binary(., hex = TRUE)

# Run update....This works
con = pg_con_local("location_test")
#con = dbConnect(odbc::odbc(), dsn = "location_test_db")
update_lc_result = dbSendStatement(
  con, glue_sql("UPDATE location_coordinates SET ",
                "horizontal_accuracy = $1, ",
                "geom = ST_Transform(ST_GeomFromText('POINT({longitude} {latitude})', 4326), 2927), ",
                "modified_datetime = $2, ",
                "modified_by = $3 ",
                "where location_id = $4"))
dbBind(update_lc_result, list(horizontal_accuracy,
                              mod_dt, mod_by, location_id))
dbGetRowsAffected(update_lc_result)
dbClearResult(update_lc_result)
DBI::dbDisconnect(con)






















