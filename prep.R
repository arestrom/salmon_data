#================================================================
# Prep data for salmon_data shiny interface
#
# AS 2019-06-11
#================================================================

# Clear workspace
rm(list = ls(all.names = TRUE))

library(DBI)
library(odbc)
library(pool)
library(dplyr)
library(sf)
library(glue)
library(rmapshaper)

# Set options
#options(digits=14)

# Keep connections pane from opening
options("connectionObserver" = NULL)

#=============================================================================
# Get wria data..including geometry (for map)
#=============================================================================

# Query to get wria data
qry = glue("select wria_code, wria_description as wria_name, geom as geometry ",
           "from wria_lut")

# Run the query
db_con = dbConnect(odbc::odbc(), timezone = "UTC", dsn = "local_spawn")
wria_st = st_read(db_con, query = qry)
dbDisconnect(db_con)

# Check size of object
object.size(wria_st)

# Pull out values for drop-down
wria_list = wria_st %>%
  select(wria_code, wria_name) %>%
  st_drop_geometry() %>%
  arrange(as.integer(wria_code)) %>%
  mutate(wria_name = paste0(wria_code, " ", wria_name)) %>%
  select(wria_name)

# # Output wria_list to rds
# saveRDS(wria_list, "www/wria_list.rds")

# Create wa_beaches with tide corrections and stations
wria_polys = wria_st %>%
  st_transform(4326) %>%
  ms_simplify() %>%
  mutate(wria_name = paste0(wria_code, " ", wria_name)) %>%
  select(wria_name, geometry)

# Check size of object
object.size(wria_polys)

# # Output wria_polys to rds
# saveRDS(wria_polys, "www/wria_polys.rds")

# Test wria
chosen_wria = "23 Upper Chehalis"

# Validate function to pull out wria centroid coords
zoom_wria = wria_polys %>%
  filter(wria_name == chosen_wria) %>%
  st_transform(2927) %>%
  mutate(zoom_pt = st_transform(st_centroid(geometry), 4326)) %>%
  mutate(lat = st_coordinates(zoom_pt)[[2]]) %>%
  mutate(lon = st_coordinates(zoom_pt)[[1]]) %>%
  st_drop_geometry() %>%
  select(wria_name, lat, lon)

# Test of streams function to be stored in global.R

# Define dsns
salmon_db = "local_spawn"
# Set up dsn connection
pool = pool::dbPool(drv = odbc::odbc(), timezone = "UTC", dsn = salmon_db)

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

stream_data = get_streams(pool, chosen_wria)
any(duplicated(stream_data$waterbody_id))

# Get streams in wria
wria_streams = get_streams(pool, chosen_wria) %>%
  mutate(stream_label = if_else(is.na(stream_name) & !is.na(waterbody_name),
                                waterbody_name, stream_name)) %>%
  mutate(stream_label = paste0(stream_name, ": ", llid)) %>%
  mutate(waterbody_id = tolower(waterbody_id)) %>%
  st_transform(4326) %>%
  select(waterbody_id, stream_label, geometry)

# Pull out stream_list from wria_streams
stream_list = stream_data %>%
  st_drop_geometry() %>%
  mutate(stream_label = if_else(is.na(stream_name) & !is.na(waterbody_name),
                                waterbody_name, stream_name)) %>%
  mutate(stream_label = paste0(stream_name, " (LLID: ", llid, ")")) %>%
  select(stream_label) %>%
  distinct()


# Function to close pool
onStop(function() {
  poolClose(pool)
})

# Define function to get all beaches
get_surveys = function(chosen_stream) {
  beaches = pool %>% tbl("survey") %>%
    select(survey_id, survey_date = survey_datetime, data_source_id, data_source_unit_id,
           survey_method_id, data_review_status_id,
           low_corr_min = low_tide_correction_minutes, low_corr_ft = low_tide_correction_feet,
           high_corr_min = high_tide_correction_minutes, high_corr_ft = high_tide_correction_feet,
           created_dt = created_datetime, created_by, modified_dt = modified_datetime,
           modified_by) %>%
    arrange(beach_name) %>%
    collect()
  beaches = beaches %>%
    mutate(tide_station = if_else(tide_station_id == "74c2ccff-2a7d-47e0-99f5-0c4c8d96ca5c",
                                  "Seattle", "Port Townsend")) %>%
    select(beach_id, tide_station, beach_name, beach_desc, low_corr_min, low_corr_ft,
           high_corr_min, high_corr_ft, created_dt, created_by, modified_dt, modified_by)
  beaches = as.data.frame(beaches)
  return(beaches)
}






