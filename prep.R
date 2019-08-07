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
# Check code template for modals
#=============================================================================

# chk_dat = reactive({
#   new_survey_event_vals = survey_event_edit() %>%
#     select(species, survey_design, cwt_detect_method, run, run_year,
#            pct_fish_seen, species_comment)
#   old_survey_event_vals = selected_survey_event_data() %>%
#     select(species, survey_design, cwt_detect_method, run, run_year,
#            pct_fish_seen, species_comment)
#   all_survey_event_vals = get_survey_event(pool, selected_survey_data()$survey_id) %>%
#     select(species, survey_design, cwt_detect_method, run, run_year,
#            pct_fish_seen, species_comment)
#   print("new_vals")
#   print(new_survey_event_vals)
#   print("old_vals")
#   print(old_survey_event_vals)
#   print("all_vals")
#   print(all_survey_event_vals)
# })
#
# observeEvent(input$survey_event_edit, {
#   output$chk_vals = renderText(unlist(names(chk_dat()$new_survey_event_vals)))
# })

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

# Function to get header data...use multiselect for year
get_surveys = function(pool, waterbody_id, survey_years) {
  qry = glue("select s.survey_id, s.survey_datetime as survey_date,  ",
             "ds.data_source_code, du.data_source_unit_name as data_unit, ",
             "sm.survey_method_code as survey_method, ",
             "dr.data_review_status_description as data_review, ",
             "plu.river_mile_measure as upper_rm, ",
             "pll.river_mile_measure as lower_rm, ",
             "plu.point_location_id as upper_location_id, ",
             "pll.point_location_id as lower_location_id, ",
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
    mutate(survey_date_dt = format(survey_date, "%m/%d/%Y")) %>%
    mutate(start_time = with_tz(start_time, tzone = "America/Los_Angeles")) %>%
    mutate(start_time_dt = format(start_time, "%H:%M")) %>%
    mutate(end_time = with_tz(end_time, tzone = "America/Los_Angeles")) %>%
    mutate(end_time_dt = format(end_time, "%H:%M")) %>%
    mutate(created_date = with_tz(created_date, tzone = "America/Los_Angeles")) %>%
    mutate(created_dt = format(created_date, "%m/%d/%Y %H:%M")) %>%
    mutate(modified_date = with_tz(modified_date, tzone = "America/Los_Angeles")) %>%
    mutate(modified_dt = format(modified_dt, "%m/%d/%Y %H:%M")) %>%
    mutate(survey_date = as.Date(survey_date)) %>%
    select(survey_id, survey_date, survey_date_dt, survey_method, up_rm = upper_rm,
           lo_rm = lower_rm, start_time, start_time_dt, end_time, end_time_dt,
           observer, submitter, data_source = data_source_code, data_unit,
           data_review, completion, created_date, created_dt,
           created_by, modified_date, modified_dt, modified_by) %>%
    arrange(survey_date, start_time, end_time, created_date)
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

end_points = get_end_points(pool, waterbody_id)

# Later can filter by n-surveys to set priority
get_data_source = function(pool) {
  qry = glue("select data_source_id, data_source_name as data_source ",
             "from data_source_lut")
  data_source = DBI::dbGetQuery(pool, qry) %>%
    mutate(data_source_id = tolower(data_source_id)) %>%
    arrange(data_source) %>%
    select(data_source_id, data_source)
  return(data_source)
}

# Check for existing surveys prior to insert operation
dup_survey = function(new_vals, old_vals) {
  matching_rows = new_vals %>%
    inner_join(old_vals, by = c("survey_dt", "observer"))
  if (nrow(matching_rows) > 0 ) {
    dup_flag = TRUE
  } else {
    dup_flag = FALSE
  }
  return(dup_flag)
}

#=====================================================================================================
# Survey insert code
#=====================================================================================================

# Mimic what comes out of survey_create() reactive
new_values = tibble(survey_dt = as.POSIXct("2019-06-02", tz = "America/Los_Angeles"),

                    up_rm = input$upper_rm_select,
                    lo_rm = input$lower_rm_select,
                    start_time = format(input$start_time_select, "%H:%M"),
                    end_time = format(input$end_time_select, "%H:%M"),
                    observer = input$observer_input,
                    submitter = input$submitter_input,
                    data_source = input$data_source_select,
                    data_review = input$data_review_select,
                    completion = input$completion_select,
                    created_dt = lubridate::with_tz(Sys.time(), "UTC"),
                    created_by = Sys.getenv("USERNAME"))

# Define the insert callback
survey_insert = function(new_values) {
  survey_dt = new_values$survey_dt
  # Format start times
  if (substr(new_values$start_time, 12, 13) == "00" ) {
    new_values$start_time = with_tz(as.POSIXct(NA), tzone = "UTC")
  } else {
    new_values$start_time = as.POSIXct(paste0(format(survey_dt, " ", start_time)), tz = "America/Los_Angeles")
    new_values$start_time = with_tz(new_values$start_time, tzone = "UTC")
  }
  # Format end times
  if (substr(new_values$end_time, 12, 13) == "00" ) {
    new_values$end_time = with_tz(as.POSIXct(NA), tzone = "UTC")
  } else {
    new_values$end_time = as.POSIXct(paste0(format(survey_dt, " ", end_time)), tz = "America/Los_Angeles")
    new_values$end_time = with_tz(new_values$end_time, tzone = "UTC")
  }
  # Format remaining values
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
  insert_result = dbSendStatement(
    con, glue_sql("INSERT INTO survey (",
                  "survey_datetime, ",
                  "data_source_id, ",
                  "beach_description, ",
                  "low_tide_correction_minutes, ",
                  "low_tide_correction_feet, ",
                  "high_tide_correction_minutes, ",
                  "high_tide_correction_feet, ",
                  "created_datetime, ",
                  "created_by) ",
                  "VALUES (",
                  "?, ?, ?, ?, ?, ?, ?, ?, ?)"))
  dbBind(insert_result, list(tide_station_location_id, beach_name, beach_desc,
                             low_corr_min, low_corr_ft, high_corr_min,
                             high_corr_ft, create_dt, create_by))
  dbGetRowsAffected(insert_result)
  dbClearResult(insert_result)
  poolReturn(con)
}

# Survey_comment query
# Function to get header data...use multiselect for year
get_survey_comment = function(pool, survey_id) {
  qry = glue("select sc.survey_comment_id, ars.area_surveyed, ",
             "fa.fish_abundance_condition as abundance_condition, ",
             "stc.stream_condition, sf.flow_type_short_description as stream_flow, ",
             "cc.survey_count_condition as count_condition, ",
             "sd.survey_direction_description as survey_direction, ",
             "st.survey_timing, vc.visibility_condition, ",
             "vt.visibility_type_short_description as visibility_type, ",
             "wt.weather_type_description as weather_type, ",
             "sc.comment_text as survey_comment, ",
             "sc.created_datetime as created_date, ",
             "sc.created_by, sc.modified_datetime as modified_date, ",
             "sc.modified_by ",
             "from survey_comment as sc ",
             "left join area_surveyed_lut as ars on sc.area_surveyed_id = ars.area_surveyed_id ",
             "left join fish_abundance_condition_lut as fa on sc.fish_abundance_condition_id = fa.fish_abundance_condition_id ",
             "left join stream_condition_lut as stc on sc.stream_condition_id = stc.stream_condition_id ",
             "left join stream_flow_type_lut as sf on sc.stream_flow_type_id = sf.stream_flow_type_id ",
             "left join survey_count_condition_lut as cc on sc.survey_count_condition_id = cc.survey_count_condition_id ",
             "left join survey_direction_lut as sd on sc.survey_direction_id = sd.survey_direction_id ",
             "left join survey_timing_lut as st on sc.survey_timing_id = st.survey_timing_id ",
             "left join visibility_condition_lut as vc on sc.visibility_condition_id = vc.visibility_condition_id ",
             "left join visibility_type_lut as vt on sc.visibility_type_id = vt.visibility_type_id ",
             "left join weather_type_lut as wt on sc.weather_type_id = wt.weather_type_id ",
             "where sc.survey_id = '{survey_id}'")
  survey_comments = DBI::dbGetQuery(pool, qry)
  survey_comments = survey_comments %>%
    mutate(survey_comment_id = tolower(survey_comment_id)) %>%
    mutate(created_date = with_tz(created_date, tzone = "America/Los_Angeles")) %>%
    mutate(created_dt = format(created_date, "%m/%d/%Y %H:%M")) %>%
    mutate(modified_date = with_tz(modified_date, tzone = "America/Los_Angeles")) %>%
    mutate(modified_dt = format(modified_date, "%m/%d/%Y %H:%M")) %>%
    select(survey_comment_id, area_surveyed, abundance_condition, stream_condition,
           stream_flow, count_condition, survey_direction, survey_timing,
           visibility_condition, visibility_type, weather_type, survey_comment,
           created_date, created_dt, created_by, modified_date, modified_dt,
           modified_by) %>%
    arrange(created_date)
  return(survey_comments)
}

# Test
survey_id = "60ee72dc-f48f-4f7b-b9be-4dd0517c7bf6"    # Arbitary

survey_comments = get_survey_comment(pool, survey_id = survey_id)

# Identify survey dependencies prior to delete....do the same for survey_event
get_survey_dependencies = function(survey_id) {
  qry = glue("select count(fb.fish_barrier_id) as fish_barrier, ",
             "count(fc.fish_capture_id) as fish_capture, ",
             "count(fs.fish_species_presence_id) as fish_species_presence, ",
             "count(ms.mobile_survey_form_id) as mobile_survey_form, ",
             "count(ob.other_observation_id) as other_observation, ",
             "count(sc.survey_comment_id) as survey_comment, ",
             "count(se.survey_event_id) as survey_event, ",
             "count(si.survey_intent_id) as survey_intent, ",
             "count(wm.waterbody_measurement_id) as waterbody_measurement ",
             "from survey as s ",
             "left join fish_barrier as fb on s.survey_id = fb.survey_id ",
             "left join fish_capture as fc on s.survey_id = fc.survey_id ",
             "left join fish_species_presence as fs on s.survey_id = fs.survey_id ",
             "left join mobile_survey_form as ms on s.survey_id = ms.survey_id ",
             "left join other_observation as ob on s.survey_id = ob.survey_id ",
             "left join survey_comment as sc on s.survey_id = sc.survey_id ",
             "left join survey_event as se on s.survey_id = se.survey_id ",
             "left join survey_intent as si on s.survey_id = si.survey_id ",
             "left join waterbody_measurement as wm on s.survey_id = si.survey_id ",
             "where s.survey_id = '{survey_id}'")
  con = poolCheckout(pool)
  survey_dependents = DBI::dbGetQuery(pool, qry)
  has_entries = function(x) any(x > 0L)
  survey_dependents = survey_dependents %>%
    select_if(has_entries)
  return(survey_dependents)
}

# Test
survey_id = "11d2e816-2d44-454d-8a55-3aad7ec83775"

survey_dependents = get_survey_dependencies(survey_id)
has_entries = function(x) any(x > 0L)
survey_dependents = survey_dependents %>%
  select_if(has_entries)

# Main waterbody measurements query
get_waterbody_meas = function(pool, survey_id) {
  qry = glue("select wb.waterbody_measurement_id, wc.clarity_type_short_description as clarity_type, ",
             "wb.water_clarity_meter as clarity_meter, wb.stream_flow_measurement_cfs as flow_cfs, ",
             "s.survey_datetime as survey_date, wb.start_water_temperature_celsius as start_temperature, ",
             "wb.start_water_temperature_datetime as start_tmp_time, ",
             "wb.end_water_temperature_celsius as end_temperature, ",
             "wb.end_water_temperature_datetime as end_tmp_time, ",
             "wb.waterbody_ph as water_ph, wb.created_datetime as created_date, ",
             "wb.created_by, wb.modified_datetime as modified_date, wb.modified_by ",
             "from waterbody_measurement as wb ",
             "inner join survey as s on wb.survey_id = s.survey_id ",
             "left join water_clarity_type_lut as wc on wb.water_clarity_type_id = wc.water_clarity_type_id ",
             "where wb.survey_id = '{survey_id}'")
  waterbody_measure = DBI::dbGetQuery(pool, qry)
  waterbody_measure = waterbody_measure %>%
    mutate(waterbody_measurement_id = tolower(waterbody_measurement_id)) %>%
    mutate(survey_date = with_tz(survey_date, tzone = "America/Los_Angeles")) %>%
    mutate(start_tmp_time = with_tz(start_tmp_time, tzone = "America/Los_Angeles")) %>%
    mutate(start_tmp_dt = format(start_tmp_time, "%H:%M")) %>%
    mutate(end_tmp_time = with_tz(end_tmp_time, tzone = "America/Los_Angeles")) %>%
    mutate(end_tmp_dt = format(end_tmp_time, "%H:%M")) %>%
    mutate(created_date = with_tz(created_date, tzone = "America/Los_Angeles")) %>%
    mutate(created_dt = format(created_date, "%m/%d/%Y %H:%M")) %>%
    mutate(modified_date = with_tz(modified_date, tzone = "America/Los_Angeles")) %>%
    mutate(modified_dt = format(modified_date, "%m/%d/%Y %H:%M")) %>%
    select(waterbody_measurement_id, clarity_type, clarity_meter, flow_cfs, survey_date,
           start_temperature, start_tmp_time, start_tmp_dt, end_temperature,
           end_tmp_time, end_tmp_dt, water_ph, created_date, created_dt,
           created_by, modified_date, modified_dt, modified_by) %>%
    arrange(created_date)
  return(waterbody_measure)
}

chk_meas = get_waterbody_meas(pool, survey_id = 'a046d04d-aad0-48a5-b9d9-fc86dd241ece')





