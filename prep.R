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
library(lubridate)

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

get_surveys = function(pool, waterbody_id, survey_year) {
  qry = glue("select s.survey_id, s.survey_datetime as survey_date, ds.data_source_name, ",
             "du.data_source_unit_name as data_source_unit, ",
             "sm.survey_method_description as survey_method, ",
             "dr.data_review_status_description as data_review_status, ",
             "plu.river_mile_measure as upper_rm, ",
             "pll.river_mile_measure as lower_rm, ",
             "plu.point_location_id as upper_location_id, ",
             "pll.point_location_id as lower_location_id, ",
             "sct.completion_status_description as completion_status, ",
             "ics.incomplete_survey_description as incomplete_type, ",
             "s.survey_start_datetime as start_time, ",
             "s.survey_end_datetime as end_time, ",
             "s.observer_last_name as observer, ",
             "s.data_submitter_last_name as submitter, ",
             "s.created_datetime as created_date, ",
             "s.created_by, s.modified_datetime as modified_date, ",
             "s.modified_by ",
             "from survey as s ",
             "inner join data_source_lut as ds on s.data_source_id = ds.data_source_id ",
             "inner join data_source_unit_lut as du on s.data_source_unit_id = du.data_source_unit_id ",
             "inner join survey_method_lut as sm on s.survey_method_id = sm.survey_method_id ",
             "inner join data_review_status_lut as dr on s.data_review_status_id = dr.data_review_status_id ",
             "inner join point_location as plu on s.upper_end_point_id = plu.point_location_id ",
             "inner join point_location as pll on s.lower_end_point_id = pll.point_location_id ",
             "left join survey_completion_status_lut as sct on s.survey_completion_status_id = sct.survey_completion_status_id ",
             "inner join incomplete_survey_type_lut as ics on s.incomplete_survey_type_id = ics.incomplete_survey_type_id ",
             "where date_part('year', survey_datetime) = {survey_year} ",
             "and (plu.waterbody_id = '{waterbody}' or pll.waterbody_id = '{waterbody}')")
  surveys = DBI::dbGetQuery(pool, qry)
  surveys = surveys %>%
    mutate(survey_id = tolower(survey_id)) %>%
    mutate(upper_location_id = tolower(upper_location_id)) %>%
    mutate(lower_location_id = tolower(lower_location_id)) %>%
    mutate(survey_date = with_tz(survey_date, tzone = "America/Los_Angeles")) %>%
    mutate(start_time = with_tz(start_time, tzone = "America/Los_Angeles")) %>%
    mutate(end_time = with_tz(end_time, tzone = "America/Los_Angeles")) %>%
    mutate(created_date = with_tz(created_date, tzone = "America/Los_Angeles")) %>%
    mutate(modified_date = with_tz(modified_date, tzone = "America/Los_Angeles")) %>%
    arrange(survey_date, start_time)
  return(surveys)
}

# Get surveys
waterbody_id = stream_data %>%
  st_drop_geometry() %>%
  filter(stream_name == "NF Newaukum R (23.0887)") %>%
  mutate(waterbody_id = tolower(waterbody_id)) %>%
  select(waterbody_id) %>%
  distinct() %>%
  pull(waterbody_id)



survey_year = 2017L

surveys = get_surveys(pool, waterbody_id, survey_year)




