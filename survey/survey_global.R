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

