
# Main fish_encounter query
get_fish_encounter = function(pool, survey_event_id) {
  qry = glue("select fe.fish_encounter_id, fe.fish_encounter_datetime as fish_encounter_time, ",
             "fe.fish_count, fs.fish_status_description as fish_status, ",
             "sx.sex_description as sex, ma.maturity_short_description as maturity, ",
             "ori.origin_description as origin, cw.detection_status_description as cwt_status, ",
             "ad.adipose_clip_status_description as clip_status, ",
             "fb.behavior_short_description as fish_behavior, ",
             "fe.previously_counted_indicator as prev_counted, ",
             "fe.created_datetime as created_date, fe.created_by, ",
             "fe.modified_datetime as modified_date, fe.modified_by ",
             "from fish_encounter as fe ",
             "left join fish_status_lut as fs on fe.fish_status_id = fs.fish_status_id ",
             "left join sex_lut as sx on fe.sex_id = sx.sex_id ",
             "left join maturity_lut as ma on fe.maturity_id = ma.maturity_id ",
             "left join origin_lut as ori on fe.origin_id = ori.origin_id ",
             "left join cwt_detection_status_lut as cw on fe.cwt_detection_status_id = cw.cwt_detection_status_id ",
             "left join adipose_clip_status_lut as ad on fe.adipose_clip_status_id = ad.adipose_clip_status_id ",
             "left join fish_behavior_type_lut as fb on fe.fish_behavior_type_id = fb.fish_behavior_type_id ",
             "where fe.survey_event_id = '{survey_event_id}'")
  fish_encounters = DBI::dbGetQuery(pool, qry)
  fish_encounters = fish_encounters %>%
    mutate(fish_encounter_id = tolower(fish_encounter_id)) %>%
    mutate(fish_encounter_time = with_tz(fish_encounter_time, tzone = "America/Los_Angeles")) %>%
    mutate(fish_encounter_dt = format(fish_encounter_time, "%H:%M")) %>%
    mutate(prev_counted = if_else(prev_counted == "No", FALSE, TRUE)) %>%
    mutate(created_date = with_tz(created_date, tzone = "America/Los_Angeles")) %>%
    mutate(created_dt = format(created_date, "%m/%d/%Y %H:%M")) %>%
    mutate(modified_date = with_tz(modified_date, tzone = "America/Los_Angeles")) %>%
    mutate(modified_dt = format(modified_date, "%m/%d/%Y %H:%M")) %>%
    select(fish_encounter_id, fish_encounter_time, fish_encounter_dt, fish_count, fish_status,
           sex, maturity, origin, cwt_status, clip_status, fish_behavior, prev_counted, created_date,
           created_dt, created_by, modified_date, modified_dt, modified_by) %>%
    arrange(created_date)
  return(fish_encounters)
}

#==========================================================================
# Get generic lut input values
#==========================================================================

# Fish status
get_fish_status = function(pool) {
  qry = glue("select fish_status_id, fish_status_description as fish_status ",
             "from fish_status_lut ",
             "where obsolete_datetime is null")
  fish_status_list = DBI::dbGetQuery(pool, qry) %>%
    mutate(fish_status_id = tolower(fish_status_id)) %>%
    arrange(fish_status) %>%
    select(fish_status_id, fish_status)
  return(fish_status_list)
}

# Sex
get_sex = function(pool) {
  qry = glue("select sex_id, sex_description as sex ",
             "from sex_lut ",
             "where obsolete_datetime is null")
  sex_list = DBI::dbGetQuery(pool, qry) %>%
    mutate(sex_id = tolower(sex_id)) %>%
    arrange(sex) %>%
    select(sex_id, sex)
  return(sex_list)
}

# Maturity
get_maturity = function(pool) {
  qry = glue("select maturity_id, maturity_short_description as maturity ",
             "from maturity_lut ",
             "where obsolete_datetime is null")
  maturity_list = DBI::dbGetQuery(pool, qry) %>%
    mutate(maturity_id = tolower(maturity_id)) %>%
    arrange(maturity) %>%
    select(maturity_id, maturity)
  return(maturity_list)
}

# Origin
get_origin = function(pool) {
  qry = glue("select origin_id, origin_description as origin ",
             "from origin_lut ",
             "where obsolete_datetime is null")
  origin_list = DBI::dbGetQuery(pool, qry) %>%
    mutate(origin_id = tolower(origin_id)) %>%
    arrange(origin) %>%
    select(origin_id, origin)
  return(origin_list)
}

# CWT status
get_cwt_status = function(pool) {
  qry = glue("select cwt_detection_status_id, detection_status_description as cwt_status ",
             "from cwt_detection_status_lut ",
             "where obsolete_datetime is null")
  cwt_status_list = DBI::dbGetQuery(pool, qry) %>%
    mutate(cwt_detection_status_id = tolower(cwt_detection_status_id)) %>%
    mutate(cwt_status = if_else(cwt_status == "Coded-wire tag undetermined, e.g., no head",
                                "Tag undetermined, e.g., no head", cwt_status)) %>%
    arrange(cwt_status) %>%
    select(cwt_detection_status_id, cwt_status)
  return(cwt_status_list)
}

# Clip status
get_clip_status = function(pool) {
  qry = glue("select adipose_clip_status_id, adipose_clip_status_description as clip_status ",
             "from adipose_clip_status_lut ",
             "where obsolete_datetime is null")
  clip_status_list = DBI::dbGetQuery(pool, qry) %>%
    mutate(adipose_clip_status_id = tolower(adipose_clip_status_id)) %>%
    mutate(clip_status = if_else(clip_status == "Adipose fin checked but undetermined, e.g., too deteriorated",
                                 "Checked but UD, e.g., decayed", clip_status)) %>%
    arrange(clip_status) %>%
    select(adipose_clip_status_id, clip_status)
  return(clip_status_list)
}

# Fish behavior
get_fish_behavior = function(pool) {
  qry = glue("select fish_behavior_type_id, behavior_short_description as fish_behavior ",
             "from fish_behavior_type_lut ",
             "where obsolete_datetime is null")
  fish_behavior_list = DBI::dbGetQuery(pool, qry) %>%
    mutate(fish_behavior_type_id = tolower(fish_behavior_type_id)) %>%
    arrange(fish_behavior) %>%
    select(fish_behavior_type_id, fish_behavior)
  return(fish_behavior_list)
}

# #==========================================================================
# # Validate survey_intent create operations
# #==========================================================================
#
# # Check for existing duplicate survey_intent prior to survey_intent insert operation
# dup_survey_event = function(new_survey_event_vals, existing_survey_event_vals) {
#   new_survey_event_vals = new_survey_event_vals %>%
#     select(species, survey_design, run, run_year)
#   matching_rows = new_survey_event_vals %>%
#     inner_join(existing_survey_event_vals,
#                by = c("species", "survey_design", "run", "run_year"))
#   if (nrow(matching_rows) > 0 ) {
#     dup_flag = TRUE
#   } else {
#     dup_flag = FALSE
#   }
#   return(dup_flag)
# }
#
# #========================================================
# # Insert callback
# #========================================================
#
# # Define the insert callback
# survey_event_insert = function(new_event_values) {
#   new_event_values = new_event_values
#   # Pull out data
#   survey_id = new_event_values$survey_id
#   species_id = new_event_values$species_id
#   survey_design_type_id =  new_event_values$survey_design_type_id
#   cwt_detection_method_id = new_event_values$cwt_detection_method_id
#   run_id = new_event_values$run_id
#   run_year = new_event_values$run_year
#   estimated_percent_fish_seen = new_event_values$pct_fish_seen
#   comment_text = new_event_values$species_comment
#   if (is.na(comment_text) | comment_text == "") { comment_text = NA }
#   created_by = new_event_values$created_by
#   # Checkout a connection
#   con = poolCheckout(pool)
#   insert_result = dbSendStatement(
#     con, glue_sql("INSERT INTO survey_event (",
#                   "survey_id, ",
#                   "species_id, ",
#                   "survey_design_type_id, ",
#                   "cwt_detection_method_id, ",
#                   "run_id, ",
#                   "run_year, ",
#                   "estimated_percent_fish_seen, ",
#                   "comment_text, ",
#                   "created_by) ",
#                   "VALUES (",
#                   "?, ?, ?, ?, ?, ?, ?, ?, ?)"))
#   dbBind(insert_result, list(survey_id, species_id, survey_design_type_id,
#                              cwt_detection_method_id, run_id, run_year,
#                              estimated_percent_fish_seen, comment_text,
#                              created_by))
#   dbGetRowsAffected(insert_result)
#   dbClearResult(insert_result)
#   poolReturn(con)
# }
#
# #========================================================
# # Edit update callback
# #========================================================
#
# # Define update callback
# survey_event_update = function(survey_event_edit_values) {
#   edit_values = survey_event_edit_values
#   # Pull out data
#   survey_event_id = edit_values$survey_event_id
#   species_id = edit_values$species_id
#   survey_design_type_id =  edit_values$survey_design_type_id
#   cwt_detection_method_id = edit_values$cwt_detection_method_id
#   run_id = edit_values$run_id
#   run_year = edit_values$run_year
#   estimated_percent_fish_seen = edit_values$pct_fish_seen
#   comment_text = edit_values$species_comment
#   if (is.na(comment_text) | comment_text == "") { comment_text = NA }
#   mod_dt = lubridate::with_tz(Sys.time(), "UTC")
#   mod_by = Sys.getenv("USERNAME")
#   # Checkout a connection
#   con = poolCheckout(pool)
#   update_result = dbSendStatement(
#     con, glue_sql("UPDATE survey_event SET ",
#                   "species_id = ?, ",
#                   "survey_design_type_id = ?, ",
#                   "cwt_detection_method_id = ?, ",
#                   "run_id = ?, ",
#                   "run_year = ?, ",
#                   "estimated_percent_fish_seen = ?, ",
#                   "comment_text = ?, ",
#                   "modified_datetime = ?, ",
#                   "modified_by = ? ",
#                   "where survey_event_id = ?"))
#   dbBind(update_result, list(species_id, survey_design_type_id,
#                              cwt_detection_method_id, run_id, run_year,
#                              estimated_percent_fish_seen, comment_text,
#                              mod_dt, mod_by, survey_event_id))
#   dbGetRowsAffected(update_result)
#   dbClearResult(update_result)
#   poolReturn(con)
# }
#
# #========================================================
# # Identify species dependencies prior to delete
# #========================================================
#
# # Identify survey dependencies prior to delete....do the same for survey_event
# get_survey_event_dependencies = function(survey_event_id) {
#   qry = glue("select ",
#              "count(fe.fish_encounter_id) as fish_encounter, ",
#              "count(rd.redd_encounter_id) as redd_encounter ",
#              "from survey_event as se ",
#              "left join fish_encounter as fe on se.survey_event_id = fe.survey_event_id ",
#              "left join redd_encounter as rd on se.survey_event_id = rd.survey_event_id ",
#              "where se.survey_event_id = '{survey_event_id}'")
#   con = poolCheckout(pool)
#   survey_event_dependents = DBI::dbGetQuery(pool, qry)
#   has_entries = function(x) any(x > 0L)
#   survey_event_dependents = survey_event_dependents %>%
#     select_if(has_entries)
#   return(survey_event_dependents)
# }
#
# #========================================================
# # Delete callback
# #========================================================
#
# # Define delete callback
# survey_event_delete = function(delete_values) {
#   survey_event_id = delete_values$survey_event_id
#   con = poolCheckout(pool)
#   delete_result = dbSendStatement(
#     con, glue_sql("DELETE FROM survey_event WHERE survey_event_id = ?"))
#   dbBind(delete_result, list(survey_event_id))
#   dbGetRowsAffected(delete_result)
#   dbClearResult(delete_result)
#   poolReturn(con)
# }
#











