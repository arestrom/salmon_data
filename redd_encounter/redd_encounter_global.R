
# Main redd_encounter query
get_redd_encounter = function(pool, survey_event_id) {
  qry = glue("select rd.redd_encounter_id, rd.redd_encounter_datetime as redd_encounter_time, ",
             "rd.redd_count, rs.redd_status_short_description as redd_status, ",
             "loc.location_name as redd_name, rd.comment_text as redd_comment, ",
             "rd.created_datetime as created_date, rd.created_by, ",
             "rd.modified_datetime as modified_date, rd.modified_by ",
             "from redd_encounter as rd ",
             "inner join redd_status_lut as rs on rd.redd_status_id = rs.redd_status_id ",
             "left join location as loc on rd.redd_location_id = loc.location_id ",
             "where rd.survey_event_id = '{survey_event_id}'")
  redd_encounters = DBI::dbGetQuery(pool, qry)
  redd_encounters = redd_encounters %>%
    mutate(redd_encounter_id = tolower(redd_encounter_id)) %>%
    mutate(redd_encounter_time = with_tz(redd_encounter_time, tzone = "America/Los_Angeles")) %>%
    mutate(redd_encounter_dt = format(redd_encounter_time, "%H:%M")) %>%
    mutate(redd_status = if_else(redd_status == "Combined visible redds",
                                 "Visible new and old redds combined", redd_status)) %>%
    mutate(created_date = with_tz(created_date, tzone = "America/Los_Angeles")) %>%
    mutate(created_dt = format(created_date, "%m/%d/%Y %H:%M")) %>%
    mutate(modified_date = with_tz(modified_date, tzone = "America/Los_Angeles")) %>%
    mutate(modified_dt = format(modified_date, "%m/%d/%Y %H:%M")) %>%
    select(redd_encounter_id, redd_encounter_time, redd_encounter_dt, redd_count,
           redd_status, redd_name, redd_comment, created_date, created_dt,
           created_by, modified_date, modified_dt, modified_by) %>%
    arrange(created_date)
  return(redd_encounters)
}

#==========================================================================
# Get generic lut input values
#==========================================================================

# Redd status
get_redd_name = function(pool, survey_event_id) {
  qry = glue("select loc.location_id as redd_location_id, loc.location_name as redd_name ",
             "from redd_encounter as rd ",
             "inner join location as loc on rd.redd_location_id = loc.location_id ",
             "where rd.survey_event_id = '{survey_event_id}' and loc.location_name is not null")
  redd_name_list = DBI::dbGetQuery(pool, qry) %>%
    mutate(redd_location_id = tolower(redd_location_id)) %>%
    arrange(redd_name) %>%
    select(redd_location_id, redd_name)
  return(redd_name_list)
}

# Redd status
get_redd_status = function(pool) {
  qry = glue("select redd_status_id, redd_status_short_description as redd_status ",
             "from redd_status_lut ",
             "where obsolete_datetime is null")
  redd_status_list = DBI::dbGetQuery(pool, qry) %>%
    mutate(redd_status_id = tolower(redd_status_id)) %>%
    mutate(redd_status = if_else(redd_status == "Combined visible redds",
                                 "Visible new and old redds combined", redd_status)) %>%
    arrange(redd_status) %>%
    select(redd_status_id, redd_status)
  return(redd_status_list)
}

#========================================================
# Insert callback
#========================================================

# Define the insert callback
redd_encounter_insert = function(new_redd_encounter_values) {
  new_insert_values = new_redd_encounter_values
  # Pull out data
  survey_event_id = new_insert_values$survey_event_id
  redd_location_id = new_insert_values$redd_location_id
  redd_status_id = new_insert_values$redd_status_id
  redd_encounter_datetime = new_insert_values$redd_encounter_datetime
  redd_count = new_insert_values$redd_count
  comment_text = new_insert_values$comment_text
  if (is.na(comment_text) | comment_text == "") { comment_text = NA }
  created_by = new_insert_values$created_by
  # Checkout a connection
  con = poolCheckout(pool)
  insert_result = dbSendStatement(
    con, glue_sql("INSERT INTO redd_encounter (",
                  "survey_event_id, ",
                  "redd_location_id, ",
                  "redd_status_id, ",
                  "redd_encounter_datetime, ",
                  "redd_count, ",
                  "comment_text, ",
                  "created_by) ",
                  "VALUES (",
                  "?, ?, ?, ?, ?, ?, ?)"))
  dbBind(insert_result, list(survey_event_id, redd_location_id,
                             redd_status_id, redd_encounter_datetime,
                             redd_count, comment_text, created_by))
  dbGetRowsAffected(insert_result)
  dbClearResult(insert_result)
  poolReturn(con)
}

# #========================================================
# # Edit update callback
# #========================================================
#
# # Define update callback
# fish_encounter_update = function(fish_encounter_edit_values) {
#   edit_values = fish_encounter_edit_values
#   # Pull out data
#   fish_encounter_id = edit_values$fish_encounter_id
#   fish_status_id = edit_values$fish_status_id
#   sex_id =  edit_values$sex_id
#   maturity_id = edit_values$maturity_id
#   origin_id = edit_values$origin_id
#   cwt_detection_status_id = edit_values$cwt_detection_status_id
#   adipose_clip_status_id = edit_values$adipose_clip_status_id
#   fish_behavior_type_id = edit_values$fish_behavior_type_id
#   fish_encounter_datetime = edit_values$fish_encounter_time
#   fish_count = edit_values$fish_count
#   previously_counted_indicator = edit_values$previously_counted_indicator
#   if ( is.na(previously_counted_indicator) ) { previously_counted_indicator = FALSE }
#   mod_dt = lubridate::with_tz(Sys.time(), "UTC")
#   mod_by = Sys.getenv("USERNAME")
#   # Checkout a connection
#   con = poolCheckout(pool)
#   update_result = dbSendStatement(
#     con, glue_sql("UPDATE fish_encounter SET ",
#                   "fish_status_id = ?, ",
#                   "sex_id = ?, ",
#                   "maturity_id = ?, ",
#                   "origin_id = ?, ",
#                   "cwt_detection_status_id = ?, ",
#                   "adipose_clip_status_id = ?, ",
#                   "fish_behavior_type_id = ?, ",
#                   "fish_encounter_datetime = ?, ",
#                   "fish_count = ?, ",
#                   "previously_counted_indicator = ?, ",
#                   "modified_datetime = ?, ",
#                   "modified_by = ? ",
#                   "where fish_encounter_id = ?"))
#   dbBind(update_result, list(fish_status_id, sex_id, maturity_id, origin_id,
#                              cwt_detection_status_id, adipose_clip_status_id,
#                              fish_behavior_type_id, fish_encounter_datetime,
#                              fish_count, previously_counted_indicator,
#                              mod_dt, mod_by, fish_encounter_id))
#   dbGetRowsAffected(update_result)
#   dbClearResult(update_result)
#   poolReturn(con)
# }
#
#========================================================
# Identify redd encounter dependencies prior to delete
#========================================================

# Identify fish_encounter dependencies prior to delete
get_redd_encounter_dependencies = function(redd_encounter_id) {
  qry = glue("select ",
             "count(ir.individual_redd_id) as individual_redd, ",
             "count(rc.redd_confidence_id) as redd_confidence, ",
             "count(rs.redd_substrate_id) as redd_substrate ",
             "from redd_encounter as rd ",
             "left join individual_redd as ir on rd.redd_encounter_id = ir.redd_encounter_id ",
             "left join redd_confidence as rc on rd.redd_encounter_id = rc.redd_encounter_id ",
             "left join redd_substrate as rs on rd.redd_encounter_id = rs.redd_encounter_id ",
             "where rd.redd_encounter_id = '{redd_encounter_id}'")
  con = poolCheckout(pool)
  redd_encounter_dependents = DBI::dbGetQuery(pool, qry)
  has_entries = function(x) any(x > 0L)
  redd_encounter_dependents = redd_encounter_dependents %>%
    select_if(has_entries)
  return(redd_encounter_dependents)
}

#========================================================
# Delete callback
#========================================================

# Define delete callback
redd_encounter_delete = function(delete_values) {
  redd_encounter_id = delete_values$redd_encounter_id
  con = poolCheckout(pool)
  delete_result = dbSendStatement(
    con, glue_sql("DELETE FROM redd_encounter WHERE redd_encounter_id = ?"))
  dbBind(delete_result, list(redd_encounter_id))
  dbGetRowsAffected(delete_result)
  dbClearResult(delete_result)
  poolReturn(con)
}












