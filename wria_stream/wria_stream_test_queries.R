#===============================================================================
# Verify queries work
#
# AS 2020-02-25
#===============================================================================

# Load libraries
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

# Get wrias
get_wrias = function() {
  qry = glue("select distinct wr.wria_code || ' ' || wr.wria_description as wria_name ",
             "from wria_lut as wr ",
             "order by wria_name")
  con = pg_con_local(dbname = "spawning_ground")
  wria_list = DBI::dbGetQuery(con, qry) %>%
    pull(wria_name)
  dbDisconnect(con)
  return(wria_list)
}

# Test
strt = Sys.time()
(wrias = get_wrias())
nd = Sys.time(); nd - strt

# # Get streams Method 1, filter in dplyr....works
# chosen_wria = wrias[[1]]
# get_streams_one = function(chosen_wria) {
#   qry = glue("select distinct wb.waterbody_id, wb.waterbody_name as stream_name, ",
#              "wb.waterbody_name, wb.latitude_longitude_id as llid, ",
#              "wb.stream_catalog_code as cat_code, wr.wria_id, st.stream_id, ",
#              "wr.wria_code || ' ' || wr.wria_description as wria_name, st.geom as geometry ",
#              "from waterbody_lut as wb ",
#              "inner join stream as st on wb.waterbody_id = st.waterbody_id ",
#              "inner join location as loc on wb.waterbody_id = loc.waterbody_id ",
#              "inner join wria_lut as wr on loc.wria_id = wr.wria_id ",
#              "order by stream_name")
#   con = dbConnect(RSQLite::SQLite(), dbname = 'data/sg_lite.sqlite')
#   streams_st = sf::st_read(con, query = qry)
#   dbDisconnect(con)
#   streams_st = streams_st %>%
#     filter(wria_name %in% chosen_wria)
#   return(streams_st)
# }
#
# # Test
# strt = Sys.time()
# streams = get_streams_one(chosen_wria)
# nd = Sys.time(); nd - strt

# # Get streams
# chosen_wria = substr(wrias[[23]], 1, 2)
# get_streams = function(chosen_wria) {
#   qry = glue("select distinct wb.waterbody_id, wb.waterbody_name as stream_name, ",
#              "wb.waterbody_name, wb.latitude_longitude_id as llid, ",
#              "wb.stream_catalog_code as cat_code, wr.wria_id, st.stream_id, ",
#              "wr.wria_code || ' ' || wr.wria_description as wria_name, st.geom as geometry ",
#              "from waterbody_lut as wb ",
#              "inner join stream as st on wb.waterbody_id = st.waterbody_id ",
#              "left join location as loc on wb.waterbody_id = loc.waterbody_id ",
#              "inner join wria_lut as wr on loc.wria_id = wr.wria_id ",
#              "where wr.wria_code = '{chosen_wria}' ",
#              "order by stream_name")
#   con = pg_con_local(dbname = "spawning_ground")
#   streams_st = sf::st_read(con, query = qry, crs = 2927)
#   dbDisconnect(con)
#   return(streams_st)
# }
#
# # Test
# strt = Sys.time()
# streams = get_streams(chosen_wria)
# nd = Sys.time(); nd - strt

# Get streams....Method 3, filter in sql using spatial join....much faster, from 1.42 seconds to 0.49 seconds
chosen_wria = substr(wrias[[23]], 1, 2)
get_streams = function(chosen_wria) {
  qry = glue("select distinct wb.waterbody_id, wb.waterbody_name as stream_name, ",
             "wb.waterbody_name, wb.latitude_longitude_id as llid, ",
             "wb.stream_catalog_code as cat_code, wr.wria_id, st.stream_id, ",
             "wr.wria_code || ' ' || wr.wria_description as wria_name, st.geom as geometry ",
             "from waterbody_lut as wb ",
             "inner join stream as st on wb.waterbody_id = st.waterbody_id ",
             "inner join wria_lut as wr on st_intersects(st.geom, wr.geom) ",
             "where wr.wria_code = '{chosen_wria}' ",
             "order by stream_name")
  con = pg_con_local(dbname = "spawning_ground")
  streams_st = sf::st_read(con, query = qry, crs = 2927)
  dbDisconnect(con)
  return(streams_st)
}

# Test
strt = Sys.time()
streams = get_streams(chosen_wria)
nd = Sys.time(); nd - strt


# Get years
waterbody_id = streams$waterbody_id[[1]]
get_data_years = function(waterbody_id) {
  qry = glue("select distinct strftime('%Y', datetime(s.survey_datetime, 'localtime')) as data_year ",
             "from survey as s ",
             "inner join location as up_loc on s.upper_end_point_id = up_loc.location_id ",
             "inner join location as lo_loc on s.lower_end_point_id = lo_loc.location_id ",
             "where up_loc.waterbody_id = '{waterbody_id}' ",
             "or lo_loc.waterbody_id = '{waterbody_id}' ",
             "order by data_year desc")
  con = dbConnect(RSQLite::SQLite(), dbname = 'data/sg_lite.sqlite')
  year_list = DBI::dbGetQuery(con, qry) %>%
    pull(data_year)
  dbDisconnect(con)
  return(year_list)
}

# Test
strt = Sys.time()
years = get_data_years(waterbody_id)
nd = Sys.time(); nd - strt

# Pull out Absher Creeek
wb_id = streams %>%
  filter(stream_name == "Absher Creek (LB)") %>%
  pull(waterbody_id)

# Get the id
waterbody_id = wb_id

# Stream centroid query
get_stream_bounds = function(waterbody_id) {
  qry = glue("select DISTINCT st.waterbody_id, ",
             "st.geom as geometry ",
             "from stream as st ",
             "where st.waterbody_id = '{waterbody_id}'")
  # con = poolCheckout(pool)
  con = dbConnect(RSQLite::SQLite(), dbname = 'data/sg_lite.sqlite')
  stream_bounds = sf::st_read(con, query = qry, crs = 2927)
  # poolReturn(con)
  dbDisconnect(con)
  stream_bounds = stream_bounds %>%
    st_transform(., 4326) %>%
    st_cast(., "POINT", warn = FALSE) %>%
    mutate(lat = as.numeric(st_coordinates(geometry)[,2])) %>%
    mutate(lon = as.numeric(st_coordinates(geometry)[,1])) %>%
    st_drop_geometry() %>%
    mutate(min_lat = min(lat),
           min_lon = min(lon),
           max_lat = max(lat),
           max_lon = max(lon)) %>%
    select(waterbody_id, min_lat, min_lon, max_lat, max_lon) %>%
    distinct()
  return(stream_bounds)
}
