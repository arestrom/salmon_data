#======================================================================================
# Test of location upload strategies
#
# Notes:
#  1. Huge change in first query by not looking for where fish_location_id was null
#     in fish_encounter table. No left join to fish_encounter resulted in mind-blowing
#     speed-up.
#  2. May want to include set of two year values in query one to filter down possible
#     number of locations for some streams. No need to filter by RMs or waterbody_id
#     anymore if we are selecting by location_id.
#
# AS 2019-08-22
#======================================================================================

library(RPostgres)
library(DBI)
library(remisc)
library(sf)
library(glue)
library(tibble)
library(pool)

# Keep connections pane from opening
options("connectionObserver" = NULL)

# Function to get user for database
pg_user <- function(user_label) {
  Sys.getenv(user_label)
}

# Function to get pw for database
pg_pw <- function(pwd_label) {
  Sys.getenv(pwd_label)
}

# Switch to RPostgres....works, but need to change placeholders in separate branch...then do PR.
pool = pool::dbPool(RPostgres::Postgres(), dbname = "spawning_ground", host = "localhost",
                    port = "5432", user = pg_user("pg_user"), password = pg_pw("pg_pwd_local"))

# Set data for query
# Absher Cr
waterbody_id = '05a031e6-62b7-411f-9e3c-b5d6efbda33b'
# Alder Cr
# waterbody_id = '3fd8a43f-505c-4e1b-9474-94046099af62'
up_rm = 0.50
#up_rm = "null"
lo_rm = 0.00
#lo_rm = "null"
survey_date = "2019-12-04"
# survey_date = "null"
# # Chinook
# species_id = "e42aa0fc-c591-4fab-8481-55b0df38dcb1"
# Coho
species_id = "a0f5b3af-fa07-449c-9f02-14c5368ab304"
#created_by = Sys.getenv("USERNAME")

# Two part query
get_fish_locations = function(waterbody_id, up_rm, lo_rm, survey_date, species_id) {
  # Define query for new fish locations...no attached surveys yet...finds new entries or orphan entries
  qry_one = glue("select floc.location_id as fish_location_id, ",
                 "floc.location_name as fish_name, ",
                 "lc.location_coordinates_id, ",
                 "st_x(st_transform(lc.geom, 4326)) as longitude, ",
                 "st_y(st_transform(lc.geom, 4326)) as latitude, ",
                 "lc.horizontal_accuracy as horiz_accuracy, ",
                 "sc.channel_type_description as channel_type, ",
                 "lo.orientation_type_description as orientation_type, ",
                 "floc.location_description, ",
                 "floc.created_datetime as created_date, floc.created_by, ",
                 "floc.modified_datetime as modified_date, floc.modified_by ",
                 "from location as floc ",
                 "inner join location_type_lut as lt on floc.location_type_id = lt.location_type_id ",
                 "left join location_coordinates as lc on floc.location_id = lc.location_id ",
                 "inner join stream_channel_type_lut as sc on floc.stream_channel_type_id = sc.stream_channel_type_id ",
                 "inner join location_orientation_type_lut as lo on floc.location_orientation_type_id = lo.location_orientation_type_id ",
                 "where lt.location_type_description = 'Fish encounter' ",
                 "and floc.waterbody_id = '{waterbody_id}'")
  # Checkout connection
  con = poolCheckout(pool)
  print("fish-one")
  strt = Sys.time()
  fish_loc_one = DBI::dbGetQuery(con, qry_one)
  nd = Sys.time()
  print(nd - strt)
  # Pull out location_ids for second query
  loc_ids = paste0(paste0("'", unique(fish_loc_one$fish_location_id), "'"), collapse = ", ")
  # Define query for fish locations already tied to surveys
  qry_two = glue("select s.survey_datetime as survey_date, se.species_id, ",
                 "uploc.river_mile_measure as up_rm, loloc.river_mile_measure as lo_rm, ",
                 "floc.location_id as fish_location_id, ",
                 "floc.location_name as fish_name, ",
                 "fs.fish_status_description as fish_status, ",
                 "lc.location_coordinates_id, ",
                 "st_x(st_transform(lc.geom, 4326)) as longitude, ",
                 "st_y(st_transform(lc.geom, 4326)) as latitude, ",
                 "lc.horizontal_accuracy as horiz_accuracy, ",
                 "sc.channel_type_description as channel_type, ",
                 "lo.orientation_type_description as orientation_type, ",
                 "floc.location_description, ",
                 "floc.created_datetime as created_date, floc.created_by, ",
                 "floc.modified_datetime as modified_date, floc.modified_by ",
                 "from survey as s ",
                 "inner join survey_event as se on s.survey_id = se.survey_id ",
                 "inner join location as uploc on s.upper_end_point_id = uploc.location_id ",
                 "inner join location as loloc on s.lower_end_point_id = loloc.location_id ",
                 "inner join fish_encounter as fd on se.survey_event_id = fd.survey_event_id ",
                 "inner join fish_status_lut as fs on fd.fish_status_id = fs.fish_status_id ",
                 "inner join location as floc on fd.fish_location_id = floc.location_id ",
                 "left join location_coordinates as lc on floc.location_id = lc.location_id ",
                 "left join stream_channel_type_lut as sc on floc.stream_channel_type_id = sc.stream_channel_type_id ",
                 "left join location_orientation_type_lut as lo on floc.location_orientation_type_id = lo.location_orientation_type_id ",
                 "where fd.fish_location_id is not null ",
                 "and loloc.waterbody_id = '{waterbody_id}' ",
                 "and uploc.river_mile_measure <= {up_rm} ",
                 "and loloc.river_mile_measure >= {lo_rm} ",
                 "and se.species_id = '{species_id}' ",
                 "and s.survey_datetime < '{survey_date}'::date + interval '1 day' ",
                 "and s.survey_datetime >= '{survey_date}'::date - interval '3 months' ",
                 "and not fs.fish_status_description in ('Live')")
  print("fish-two")
  strt = Sys.time()
  fish_loc_two = DBI::dbGetQuery(con, qry_two)
  nd = Sys.time()
  print(nd - strt)
  poolReturn(con)
  fish_locations = bind_rows(fish_loc_one, fish_loc_two) %>%
    mutate(latitude = round(latitude, 7)) %>%
    mutate(longitude = round(longitude, 7)) %>%
    mutate(survey_date = with_tz(survey_date, tzone = "America/Los_Angeles")) %>%
    mutate(survey_dt = format(survey_date, "%m/%d/%Y")) %>%
    mutate(created_date = with_tz(created_date, tzone = "America/Los_Angeles")) %>%
    mutate(created_dt = format(created_date, "%m/%d/%Y %H:%M")) %>%
    mutate(modified_date = with_tz(modified_date, tzone = "America/Los_Angeles")) %>%
    mutate(modified_dt = format(modified_date, "%m/%d/%Y %H:%M")) %>%
    select(fish_location_id, location_coordinates_id,
           survey_date, survey_dt, fish_name, fish_status,
           latitude, longitude, horiz_accuracy, channel_type,
           orientation_type, location_description, created_date,
           created_dt, created_by, modified_date, modified_dt,
           modified_by) %>%
    distinct() %>%
    arrange(created_date)
  return(fish_locations)
}

# Run
fish_locs = get_fish_locations(waterbody_id, up_rm, lo_rm, survey_date, species_id)





