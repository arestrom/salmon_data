#==============================================================
# Application to edit data for spawning_ground database
#
# Notes:
#  1. Update portion now works with pool.
#  2. Very strange error using the pool and dbplyr query for
#     get_beaches(). I had to wrap output with as.data.frame().
#     I did not get this with the original standard odbc query.
#     Must be something weird with dbplyr and pool. Each row
#     of the datatable held all rows.
#  3. dbplyr does not recognize geometry columns. Need standard
#     text query. Can use odbc package + sf and pull out lat lons.
#  4. For posible use with bs_accordion:
#     https://stackoverflow.com/questions/53642157/shiny-how-to-detect-which-accordion-elements-is-selected/53649246
#  5. For automating right sidebar:
#     https://community.rstudio.com/t/automatic-rightsidebar-popup-when-menuitem-is-clicked-in-shinydashboardplus/16574
#
# ToDo:
#  1. Add animation to buttons as in dt_editor example.
#  2. Add validate and need functions to eliminate crashes
#  3. Make sure users are set up with permissions and dsn's.
#  4. Need to verify deletions are allowed.
#  5. Allow map modal to be resizable and draggable.
#     Try the shinyjqui package:
#     https://github.com/nanxstats/awesome-shiny-extensions
#  6. Need screens to allow entry and edit of RMs and encounters
#     Start with RMs. Use MapEdit.
#  7. Set data_source order using number of surveys in category.
#     Can do a query of data to arrange by n, then name.
#  8. Need to add survey_method to survey CRUD screens.
#
# AS 2019-05-15
#==============================================================

# Load libraries
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyTime)
library(bsplus)
# library(shinyjs)
library(odbc)
library(glue)
library(tibble)
library(DBI)
library(pool)
library(dplyr)
library(DT)
library(tibble)
library(leaflet)
library(sf)
library(lubridate)

# Keep connections pane from opening
options("connectionObserver" = NULL)

# Read .rds data
wria_list = readRDS("www/wria_list.rds")
wria_polys = readRDS("www/wria_polys.rds")

# Read content definitions of data-entry screens
source("survey_content.R")
source("dash_header.R")
source("dash_rightsidebar.R")
source("dash_leftsidebar.R")

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

# Function for bsplus modal
map_modal = bs_modal (
  id = "map_modal",
  title = NULL,
  body = leafletOutput("stream_map", height = "700px"),
  size = "large",
  footer = NULL
)

get_streams = function(pool, chosen_wria) {
  qry = glue("select distinct wb.waterbody_id, wb.waterbody_display_name as stream_name, ",
             "wb.waterbody_name, wb.latitude_longitude_id as llid, ",
             "wb.stream_catalog_code as cat_code, ",
             "wr.wria_code || ' ' || wr.wria_description as wria_name, st.geom as geometry ",
             "from waterbody_lut as wb ",
             "inner join stream as st on wb.waterbody_id = st.waterbody_id ",
             "inner join wria_lut as wr on st_intersects(st.geom, wr.geom)")
  streams_st = pool %>%
    sf::st_read(query = qry) %>%
    filter(wria_name %in% chosen_wria)
  return(streams_st)
}

get_end_points = function(pool, waterbody_id) {
  qry = glue("select distinct pt.point_location_id, pt.river_mile_measure as river_mile, ",
             "pt.location_description as rm_desc ",
             "from point_location as pt ",
             "inner join location_type_lut as lt on pt.location_type_id = lt.location_type_id ",
             "where location_type_description in ('Reach boundary point', 'Section break point') ",
             "and waterbody_id = '{waterbody_id}'")
  end_points = DBI::dbGetQuery(pool, qry) %>%
    mutate(point_location_id = tolower(point_location_id)) %>%
    arrange(river_mile) %>%
    mutate(rm_label = if_else(is.na(rm_desc), as.character(river_mile),
                              paste0(river_mile, " ", rm_desc))) %>%
    select(point_location_id, rm_label)
  return(end_points)
}

# Function to get header data...just use multiselect for year
get_surveys = function(pool, waterbody_id, survey_years) {
  qry = glue("select s.survey_id, s.survey_datetime as survey_date, data_source_code,
             ds.data_source_name, du.data_source_unit_name as data_source_unit, ",
             "sm.survey_method_code as survey_method, ",
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
             "where date_part('year', survey_datetime) in ({survey_years}) ",
             "and (plu.waterbody_id = '{waterbody_id}' or pll.waterbody_id = '{waterbody_id}')")
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

#==========================================================================
# Get generic lut input values...data_source, etc.
#==========================================================================

# Data source
get_data_source = function(pool) {
  qry = glue("select data_source_id, data_source_code ",
             "from data_source_lut ",
             "where obsolete_datetime is null")
  data_source = DBI::dbGetQuery(pool, qry) %>%
    mutate(data_source_id = tolower(data_source_id)) %>%
    arrange(data_source_code) %>%
    select(data_source_id, data_source_code)
  return(data_source)
}

# Survey method
get_survey_method = function(pool) {
  qry = glue("select survey_method_id, survey_method_code as survey_method ",
             "from survey_method_lut ",
             "where obsolete_datetime is null")
  survey_method_list = DBI::dbGetQuery(pool, qry) %>%
    mutate(survey_method_id = tolower(survey_method_id)) %>%
    arrange(survey_method) %>%
    select(survey_method_id, survey_method)
  return(survey_method_list)
}

# Data review
get_data_review = function(pool) {
  qry = glue("select data_review_status_id, data_review_status_description as data_review ",
             "from data_review_status_lut ",
             "where obsolete_datetime is null")
  data_review_list = DBI::dbGetQuery(pool, qry) %>%
    mutate(data_review_status_id = tolower(data_review_status_id)) %>%
    arrange(data_review) %>%
    select(data_review_status_id, data_review)
  return(data_review_list)
}

# Completion status
get_completion_status = function(pool) {
  qry = glue("select survey_completion_status_id, completion_status_description as completion ",
             "from survey_completion_status_lut ",
             "where obsolete_datetime is null")
  completion_list = DBI::dbGetQuery(pool, qry) %>%
    mutate(survey_completion_status_id = tolower(survey_completion_status_id)) %>%
    arrange(completion) %>%
    select(survey_completion_status_id, completion)
  return(completion_list)
}

#==========================================================================
# Validate survey create operations
#==========================================================================

# Check for existing surveys prior to survey insert operation
dup_survey = function(new_survey_vals, existing_survey_vals) {
  new_survey_vals = new_survey_vals %>%
    select(survey_dt, survey_method, up_rm, lo_rm, observer, data_source)
  matching_rows = new_survey_vals %>%
    inner_join(existing_survey_vals,
               by = c("survey_dt", "survey_method", "up_rm", "lo_rm",
                      "observer", "data_source"))
  if (nrow(matching_rows) > 0 ) {
    dup_flag = TRUE
  } else {
    dup_flag = FALSE
  }
  return(dup_flag)
}

#========================================================
# Insert callback
#========================================================

# Define the insert callback
survey_insert = function(new_values) {
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


