#==============================================================
# Application to edit data for individual beaches in shellfish
# database
#
# Notes:
#  1. Update portion now works with pool.
#  2. Very strange error using the pool and dbplyr query for
#     get_beaches(). I had to wrap output with as.data.frame().
#     I did not get this with the original standard odbc query.
#     Must be something weird with dbplyr and pool. Each row
#     of the datatable held all rows.
#  3. dbplyr does not recognize geometry columns. Need standard
#     text query. Can use odbc package and pull out lat lons.
#
# ToDo:
#  1. Add animation to buttons as in dt_editor example.
#  2. Add validate and need functions to eliminate crashes
#  3. Make sure users are set up with permissions and dsn's.
#  4. For a real application need to verify deletions are allowed.
#
# AS 2019-05-15
#==============================================================

# Load libraries
library(shiny)
library(shinythemes)
library(shinyjs)
library(odbc)
library(glue)
library(tibble)
library(DBI)
library(pool)
library(dplyr)
library(DT)
library(tibble)
library(bsplus)
library(leaflet)

# Keep connections pane from opening
options("connectionObserver" = NULL)

# Read .rds data
wria_list = readRDS("www/wria_list.rds")

# Define globals ================================================================

# Define dsns
salmon_db = "local_spawn"

# Set up dsn connection
pool = pool::dbPool(drv = odbc::odbc(), timezone = "UTC", dsn = salmon_db)

# Define functions =============================================================

# Function to close pool
onStop(function() {
  poolClose(pool)
})

get_streams = function(pool, chosen_wria) {
  qry = glue("select distinct wb.waterbody_id, wb.waterbody_display_name as stream_name, ",
             "wb.waterbody_name, wb.latitude_longitude_id as llid, ",
             "wb.stream_catalog_code as cat_code, ",
             "wr.wria_code || ' ' || wr.wria_description as wria_name, st.geom as geometry ",
             "from waterbody_lut as wb ",
             "inner join stream as st on wb.waterbody_id = st.waterbody_id ",
             "inner join wria_lut as wr on st_intersects(st.geom, wr.geom)")
  streams_st = pool %>%
    st_read(query = qry) %>%
    filter(wria_name %in% chosen_wria)
  return(streams_st)
}

# Pull out values for drop-down
wria_list = wria_st %>%
  select(wria_code, wria_name) %>%
  st_drop_geometry() %>%
  arrange(as.integer(wria_code)) %>%
  mutate(wria_name = paste0(wria_code, " ", wria_name)) %>%
  select(wria_name)

# Define function to get all trips
get_trips = function() {
  # Get trip-level data
  trips = pool %>% tbl("survey") %>%
    select(survey_id, start_location_id, end_location_id, data_review_status_id,
           trip_start_time = start_datetime, trip_end_time = end_datetime,
           trip_comment = comment_text, trip_created_dt = created_datetime,
           trip_created_by = created_by, trip_modified_dt = modified_datetime,
           trip_modified_by = modified_by)
  # Join trip-level lut values
  trip_locations = pool %>%  tbl("point_location") %>%
    select(point_location_id, location_type_id, location_name, location_description)
  location_type_lut = pool %>% tbl("location_type_lut") %>%
    select(location_type_id, location_type_description)
  trip_locations = trip_locations %>%
    left_join(location_type_lut, by = "location_type_id") %>%
    filter(location_type_description == "Survey location")
  start_locations = trip_locations %>%
    select(start_location_id = point_location_id, start_name = location_name,
           start_description = location_description)
  end_locations = trip_locations %>%
    select(end_location_id = point_location_id, end_name = location_name,
           end_description = location_description)
  # Join data_review values
  data_review_lut = pool %>% tbl("data_review_status_lut") %>%
    select(data_review_status_id, data_review_status_description)
  trips = trips %>%
    left_join(start_locations, by = "start_location_id") %>%
    left_join(end_locations, by = "end_location_id") %>%
    left_join(data_review_lut, by = "data_review_status_id") %>%
    collect()
  return(trips)
}

# # Get trips
# trips = get_trips()

#=========================================================================
# Cant do locations inside database, geometry column is lost
#=========================================================================

# Function to get trip locations
get_trip_locs = function(db_dsn = "local_myco") {
  qry = glue::glue("select point_location_id, ",
                   "st_y(geog::geometry) as lat, ",
                   "st_x(geog::geometry) as lon ",
                   "from point_location as pl ",
                   "inner join location_type_lut as lt ",
                   "on pl.location_type_id = lt.location_type_id ",
                   "where location_type_description = 'Survey location'")

  db_con = dbConnect(drv = odbc::odbc(), dsn = db_dsn, timezone = "UTC")
  trip_locs = DBI::dbGetQuery(db_con, qry)
  dbDisconnect(db_con)
  return(trip_locs)
}

#=======================================
# Need to wrap below
#=======================================

# Create function
trips_locations = function(db_dsn = "local_myco") {
  # Get trips
  trips = get_trips()
  # Get trip_locations
  trip_locations = get_trip_locs()
  # Separate out to enable joins
  start_locations = trip_locations %>%
    select(start_location_id = point_location_id,
           start_lat = lat, start_lon = lon)

  end_locations = trip_locations %>%
    select(end_location_id = point_location_id,
           end_lat = lat, end_lon = lon)

  # Add coordinates to trips
  trips = trips %>%
    left_join(start_locations, by = "start_location_id") %>%
    left_join(end_locations, by = "end_location_id")
  return(trips)
}

# # Run to test....all good
# trips_with_locations = trips_locations()

#===================================================
# Add companions
#===================================================

# Wrap as function
get_companions = function() {
  # Get companions and create single column of names
  companions = pool %>% tbl("companions") %>%
    mutate(companion_name = paste0(first_name, " ", last_name)) %>%
    select(companions_id, companion_name)

  # Get survey_ids
  survey_companions = pool %>% tbl("survey_companions") %>%
    left_join(companions, by = "companions_id") %>%
    select(survey_id, companion_name) %>%
    arrange(companion_name) %>%
    collect()
  # Spread not possible inside database
  survey_companions = survey_companions %>%
    group_by(survey_id) %>%
    mutate(nseq = row_number()) %>%
    spread(nseq, companion_name, fill = "") %>%
    ungroup()

  # Need new line because ncol changes
  survey_companions = survey_companions %>%
    unite(companion_names, 2:ncol(survey_companions), sep = "  ") %>%
    mutate(companion_names = trimws(companion_names)) %>%
    mutate(companion_names = gsub("  ", "; ", companion_names))
  return(survey_companions)
}

# # Run to test....all good.
# companions = get_companions()
# trips_with_locations = trips_locations()
# trips_with_companions = trips_with_locations %>%
#   left_join(companions, by = "survey_id")























#========================================================
# Insert callback
#========================================================

# Define the insert callback
beach_insert = function(new_values) {
  # Get insert values
  tide_station = new_values$tide_station
  if ( tide_station == "Seattle" ) {
    tide_station_location_id = "74c2ccff-2a7d-47e0-99f5-0c4c8d96ca5c"
  } else if ( tide_station == "Port Townsend") {
    tide_station_location_id = "2ae45919-d13b-4cf6-bdcb-c0e5305d843f"
  } else {
    stop("No tide station was selected")
  }
  beach_name = new_values$beach_name
  if (is.na(beach_name) | beach_name == "") { beach_name = NA }
  beach_desc = new_values$beach_desc
  if (is.na(beach_desc) | beach_desc == "" | beach_desc == "NA") { beach_desc = NA }
  low_corr_min = new_values$low_corr_min
  low_corr_ft = new_values$low_corr_ft
  high_corr_min = new_values$high_corr_min
  high_corr_ft = new_values$high_corr_ft
  create_dt = lubridate::with_tz(Sys.time(), "UTC")
  create_by = Sys.getenv("USERNAME")
  # Checkout a connection
  con = poolCheckout(pool)
  insert_res = dbSendStatement(
    con, glue_sql("INSERT INTO beach (",
                  "tide_station_location_id, ",
                  "local_beach_name, ",
                  "beach_description, ",
                  "low_tide_correction_minutes, ",
                  "low_tide_correction_feet, ",
                  "high_tide_correction_minutes, ",
                  "high_tide_correction_feet, ",
                  "created_datetime, ",
                  "created_by) ",
                  "VALUES (",
                  "?, ?, ?, ?, ?, ?, ?, ?, ?)"))
  dbBind(insert_res, list(tide_station_location_id, beach_name, beach_desc,
                          low_corr_min, low_corr_ft, high_corr_min,
                          high_corr_ft, create_dt, create_by))
  dbGetRowsAffected(insert_res)
  dbClearResult(insert_res)
  poolReturn(con)
}

#========================================================
# Edit update callback
#========================================================

# Define update callback
beach_update = function(edit_values) {
  # Pull out data
  tide_station = edit_values$tide_station
  if (is.na(tide_station) | tide_station == "") {
    tide_station_location_id = NA
  } else if ( tide_station == "Seattle" ) {
    tide_station_location_id = "74c2ccff-2a7d-47e0-99f5-0c4c8d96ca5c"
  } else if ( tide_station == "Port Townsend" ) {
    tide_station_location_id = "2ae45919-d13b-4cf6-bdcb-c0e5305d843f"
  }
  beach_name = edit_values$beach_name
  if (is.na(beach_name) | beach_name == "") { beach_name = NA }
  beach_desc = edit_values$beach_desc
  if (is.na(beach_desc) | beach_desc == "" | beach_desc == "NA") { beach_desc = NA }
  low_corr_min = edit_values$low_corr_min
  low_corr_ft = edit_values$low_corr_ft
  high_corr_min = edit_values$high_corr_min
  high_corr_ft = edit_values$high_corr_ft
  mod_dt = lubridate::with_tz(Sys.time(), "UTC")
  mod_by = Sys.getenv("USERNAME")
  bch_id = edit_values$beach_id
  # Checkout a connection
  con = poolCheckout(pool)
  update_res = dbSendStatement(
    con, glue_sql("UPDATE beach SET ",
                  "tide_station_location_id = ?, ",
                  "local_beach_name = ?, ",
                  "beach_description = ?, ",
                  "low_tide_correction_minutes = ?, ",
                  "low_tide_correction_feet = ?, ",
                  "high_tide_correction_minutes = ?, ",
                  "high_tide_correction_feet = ?, ",
                  "modified_datetime = ?, ",
                  "modified_by = ? ",
                  "where beach_id = ?"))
  dbBind(update_res, list(tide_station_location_id, beach_name, beach_desc,
                          low_corr_min, low_corr_ft, high_corr_min,
                          high_corr_ft, mod_dt, mod_by, bch_id))
  dbGetRowsAffected(update_res)
  dbClearResult(update_res)
  poolReturn(con)
}

#========================================================
# Delete callback
#========================================================

# Define delete callback
beach_delete = function(delete_values) {
  bch_id = delete_values$beach_id
  con = poolCheckout(pool)
  delete_res = dbSendStatement(
    con, glue_sql("DELETE FROM beach WHERE beach_id = ?"))
  dbBind(delete_res, list(bch_id))
  dbGetRowsAffected(delete_res)
  dbClearResult(delete_res)
  poolReturn(con)
}


