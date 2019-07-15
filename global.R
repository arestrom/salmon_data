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
#  6. For DT updates: https://stackoverflow.com/questions/56879672/how-to-replacedata-in-dt-rendered-in-r-shiny-using-the-datatable-function
#     https://dev.to/awwsmm/reactive-datatables-in-r-with-persistent-filters-l26
#  7. Can not use reactives for pulling data for use in DTs
#     They do not fire properly to update tables. Just use
#     query functions from global directly. I tested by just
#     putting a reactive between functions in beach_data and
#     got failures right away.
#  8. Do not use rownames = FALSE in DT. Data will not reload
#     when using replaceData() function.
#
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
#     Start with RMs. Use MapEdit. Trigger using RM == "add".
#  7. Set data_source order using number of surveys in category.
#     Can do a query of data to arrange by n, then name.
#  8. Need to use just one function to get survey data....but
#     also add and include the other fields needed for display
#     in time and date slots of DT. DONE !!
#  9.
#
# AS 2019-07-15
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
source("survey_comment_content.R")
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
  qry = glue("select distinct loc.location_id, loc.river_mile_measure as river_mile, ",
             "loc.location_description as rm_desc ",
             "from location as loc ",
             "inner join location_type_lut as lt on loc.location_type_id = lt.location_type_id ",
             "where location_type_description in ('Reach boundary point', 'Section break point') ",
             "and waterbody_id = '{waterbody_id}'")
  end_points = DBI::dbGetQuery(pool, qry) %>%
    mutate(location_id = tolower(location_id)) %>%
    arrange(river_mile) %>%
    mutate(rm_label = if_else(is.na(rm_desc), as.character(river_mile),
                              paste0(river_mile, " ", rm_desc))) %>%
    select(location_id, rm_label)
  return(end_points)
}

# Function to get header data...use multiselect for year
get_surveys = function(pool, waterbody_id, survey_years) {
  qry = glue("select s.survey_id, s.survey_datetime as survey_date,  ",
             "ds.data_source_code, du.data_source_unit_name as data_unit, ",
             "sm.survey_method_code as survey_method, ",
             "dr.data_review_status_description as data_review, ",
             "locu.river_mile_measure as upper_rm, ",
             "locl.river_mile_measure as lower_rm, ",
             "locu.location_id as upper_location_id, ",
             "locl.location_id as lower_location_id, ",
             "sct.completion_status_description as completion, ",
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
             "inner join location as locu on s.upper_end_point_id = locu.location_id ",
             "inner join location as locl on s.lower_end_point_id = locl.location_id ",
             "left join survey_completion_status_lut as sct on s.survey_completion_status_id = sct.survey_completion_status_id ",
             "inner join incomplete_survey_type_lut as ics on s.incomplete_survey_type_id = ics.incomplete_survey_type_id ",
             "where date_part('year', survey_datetime) in ({survey_years}) ",
             "and (locu.waterbody_id = '{waterbody_id}' or locl.waterbody_id = '{waterbody_id}')")
  surveys = DBI::dbGetQuery(pool, qry)
  surveys = surveys %>%
    mutate(survey_id = tolower(survey_id)) %>%
    mutate(upper_location_id = tolower(upper_location_id)) %>%
    mutate(lower_location_id = tolower(lower_location_id)) %>%
    mutate(survey_date = with_tz(survey_date, tzone = "America/Los_Angeles")) %>%
    mutate(survey_date_dt = format(survey_date, "%m/%d/%Y")) %>%
    mutate(start_time = with_tz(start_time, tzone = "America/Los_Angeles")) %>%
    mutate(start_time_dt = format(start_time, "%H:%M")) %>%
    mutate(end_time = with_tz(end_time, tzone = "America/Los_Angeles")) %>%
    mutate(end_time_dt = format(end_time, "%H:%M")) %>%
    mutate(created_date = with_tz(created_date, tzone = "America/Los_Angeles")) %>%
    mutate(created_dt = format(created_date, "%m/%d/%Y %H:%M")) %>%
    mutate(modified_date = with_tz(modified_date, tzone = "America/Los_Angeles")) %>%
    mutate(modified_dt = format(modified_date, "%m/%d/%Y %H:%M")) %>%
    mutate(survey_date = as.Date(survey_date)) %>%
    select(survey_id, survey_date, survey_date_dt, survey_method, up_rm = upper_rm,
           lo_rm = lower_rm, start_time, start_time_dt, end_time, end_time_dt,
           observer, submitter, data_source = data_source_code, data_unit,
           data_review, completion, created_date, created_dt,
           created_by, modified_date, modified_dt, modified_by) %>%
    arrange(survey_date, start_time, end_time, created_date)
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

# Area surveyed
get_area_surveyed = function(pool) {
  qry = glue("select area_surveyed_id, area_surveyed ",
             "from area_surveyed_lut ",
             "where obsolete_datetime is null")
  area_surveyed_list = DBI::dbGetQuery(pool, qry) %>%
    mutate(area_sureveyed_id = tolower(area_surveyed_id)) %>%
    arrange(area_surveyed) %>%
    select(area_surveyed_id, area_surveyed)
  return(area_surveyed_list)
}

# Abundance
get_abundance_condition = function(pool) {
  qry = glue("select fish_abundance_condition_id, fish_abundance_condition as abundance_condition ",
             "from fish_abundance_condition_lut ",
             "where obsolete_datetime is null")
  fish_abundance_list = DBI::dbGetQuery(pool, qry) %>%
    mutate(fish_abundance_condition_id = tolower(fish_abundance_condition_id)) %>%
    arrange(abundance_condition) %>%
    select(fish_abundance_condition_id, abundance_condition)
  return(fish_abundance_list)
}

# Stream condition
get_stream_condition = function(pool) {
  qry = glue("select stream_condition_id, stream_condition ",
             "from stream_condition_lut ",
             "where obsolete_datetime is null")
  stream_condition_list = DBI::dbGetQuery(pool, qry) %>%
    mutate(stream_condition_id = tolower(stream_condition_id)) %>%
    arrange(stream_condition) %>%
    select(stream_condition_id, stream_condition)
  return(stream_condition_list)
}

# Stream condition
get_stream_flow = function(pool) {
  qry = glue("select stream_flow_type_id, flow_type_short_description as stream_flow ",
             "from stream_flow_type_lut ",
             "where obsolete_datetime is null")
  stream_flow_list = DBI::dbGetQuery(pool, qry) %>%
    mutate(stream_flow_type_id = tolower(stream_flow_type_id)) %>%
    arrange(stream_flow) %>%
    select(stream_flow_type_id, stream_flow)
  return(stream_flow_list)
}

# Count condition
get_count_condition = function(pool) {
  qry = glue("select survey_count_condition_id, survey_count_condition as count_condition ",
             "from survey_count_condition_lut ",
             "where obsolete_datetime is null")
  count_condition_list = DBI::dbGetQuery(pool, qry) %>%
    mutate(survey_count_condition_id = tolower(survey_count_condition_id)) %>%
    arrange(count_condition) %>%
    select(survey_count_condition_id, count_condition)
  return(count_condition_list)
}

# Survey direction
get_survey_direction = function(pool) {
  qry = glue("select survey_direction_id, survey_direction_description as survey_direction ",
             "from survey_direction_lut ",
             "where obsolete_datetime is null")
  survey_direction_list = DBI::dbGetQuery(pool, qry) %>%
    mutate(survey_direction_id = tolower(survey_direction_id)) %>%
    arrange(survey_direction) %>%
    select(survey_direction_id, survey_direction)
  return(survey_direction_list)
}

# Survey timing
get_survey_timing = function(pool) {
  qry = glue("select survey_timing_id, survey_timing ",
             "from survey_timing_lut ",
             "where obsolete_datetime is null")
  survey_timing_list = DBI::dbGetQuery(pool, qry) %>%
    mutate(survey_timing_id = tolower(survey_timing_id)) %>%
    arrange(survey_timing) %>%
    select(survey_timing_id, survey_timing)
  return(survey_timing_list)
}

# Visibility condition
get_visibility_condition = function(pool) {
  qry = glue("select visibility_condition_id, visibility_condition ",
             "from visibility_condition_lut ",
             "where obsolete_datetime is null")
  visibility_condition_list = DBI::dbGetQuery(pool, qry) %>%
    mutate(visibility_condition_id = tolower(visibility_condition_id)) %>%
    arrange(visibility_condition) %>%
    select(visibility_condition_id, visibility_condition)
  return(visibility_condition_list)
}

# Visibility type
get_visibility_type = function(pool) {
  qry = glue("select visibility_type_id, visibility_type_short_description as visibility_type ",
             "from visibility_type_lut ",
             "where obsolete_datetime is null")
  visibility_type_list = DBI::dbGetQuery(pool, qry) %>%
    mutate(visibility_type_id = tolower(visibility_type_id)) %>%
    arrange(visibility_type) %>%
    select(visibility_type_id, visibility_type)
  return(visibility_type_list)
}

# Weather type
get_weather_type = function(pool) {
  qry = glue("select weather_type_id, weather_type_description as weather_type ",
             "from weather_type_lut ",
             "where obsolete_datetime is null")
  weather_type_list = DBI::dbGetQuery(pool, qry) %>%
    mutate(weather_type_id = tolower(weather_type_id)) %>%
    arrange(weather_type) %>%
    select(weather_type_id, weather_type)
  return(weather_type_list)
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
  new_values = new_values %>%
    mutate(incomplete_survey_type_id = "cde5d9fb-bb33-47c6-9018-177cd65d15f5") %>%   # Not applicable
    mutate(data_source_unit_id = "e2d51ceb-398c-49cb-9aa5-d20a839e9ad9")             # Not applicable
  # Checkout a connection
  con = poolCheckout(pool)
  insert_result = dbSendStatement(
    con, glue_sql("INSERT INTO survey (",
                  "survey_datetime, ",
                  "data_source_id, ",
                  "data_source_unit_id, ",
                  "survey_method_id, ",
                  "data_review_status_id, ",
                  "upper_end_point_id, ",
                  "lower_end_point_id, ",
                  "survey_completion_status_id, ",
                  "incomplete_survey_type_id, ",
                  "survey_start_datetime, ",
                  "survey_end_datetime, ",
                  "observer_last_name, ",
                  "data_submitter_last_name, ",
                  "created_by) ",
                  "VALUES (",
                  "?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"))
  dbBind(insert_result, list(new_values$survey_dt,
                             new_values$data_source_id,
                             new_values$data_source_unit_id,
                             new_values$survey_method_id,
                             new_values$data_review_status_id,
                             new_values$upper_end_point_id,
                             new_values$lower_end_point_id,
                             new_values$survey_completion_status_id,
                             new_values$incomplete_survey_type_id,
                             new_values$survey_start_datetime,
                             new_values$survey_end_datetime,
                             new_values$observer_last_name,
                             new_values$data_submitter_last_name,
                             new_values$created_by))
  dbGetRowsAffected(insert_result)
  dbClearResult(insert_result)
  poolReturn(con)
}

#========================================================
# Edit update callback
#========================================================

# Define update callback
survey_update = function(edit_values) {
  # Pull out data
  survey_datetime = edit_values$survey_datetime
  data_source_id = edit_values$data_source_id
  survey_method_id = edit_values$survey_method_id
  data_review_status_id = edit_values$data_review_status_id
  upper_end_point_id = edit_values$upper_end_point_id
  lower_end_point_id = edit_values$lower_end_point_id
  survey_completion_status_id = edit_values$survey_completion_status_id
  survey_start_datetime = edit_values$survey_start_datetime
  survey_end_datetime = edit_values$survey_end_datetime
  observer_last_name = edit_values$observer
  data_submitter_last_name = edit_values$submitter
  mod_dt = lubridate::with_tz(Sys.time(), "UTC")
  mod_by = Sys.getenv("USERNAME")
  survey_id = edit_values$survey_id
  # Checkout a connection
  con = poolCheckout(pool)
  update_result = dbSendStatement(
    con, glue_sql("UPDATE survey SET ",
                  "survey_datetime = ?, ",
                  "data_source_id = ?, ",
                  "survey_method_id = ?, ",
                  "data_review_status_id = ?, ",
                  "upper_end_point_id = ?, ",
                  "lower_end_point_id = ?, ",
                  "survey_completion_status_id = ?, ",
                  "survey_start_datetime = ?, ",
                  "survey_end_datetime = ?, ",
                  "observer_last_name = ?, ",
                  "data_submitter_last_name = ?, ",
                  "modified_datetime = ?, ",
                  "modified_by = ? ",
                  "where survey_id = ?"))
  dbBind(update_result, list(survey_datetime, data_source_id, survey_method_id,
                             data_review_status_id, upper_end_point_id,
                             lower_end_point_id, survey_completion_status_id,
                             survey_start_datetime, survey_end_datetime,
                             observer_last_name, data_submitter_last_name,
                             mod_dt, mod_by, survey_id))
  dbGetRowsAffected(update_result)
  dbClearResult(update_result)
  poolReturn(con)
}

#========================================================
# Delete callback
#========================================================

# Define delete callback
survey_delete = function(delete_values) {
  survey_id = delete_values$survey_id
  con = poolCheckout(pool)
  delete_result = dbSendStatement(
    con, glue_sql("DELETE FROM survey WHERE survey_id = ?"))
  dbBind(delete_result, list(survey_id))
  dbGetRowsAffected(delete_result)
  dbClearResult(delete_result)
  poolReturn(con)
}


