
# Main fish_encounter query
get_individual_fish = function(pool, fish_encounter_id) {
  qry = glue("select ind.individual_fish_id, ",
             "fc.fish_condition_short_description as fish_condition, ",
             "ft.trauma_type_short_description as fish_trauma, ",
             "gcn.gill_condition_type_description as gill_condition, ",
             "sc.spawn_condition_short_description as spawn_condition, ",
             "cr.cwt_result_type_short_description as cwt_result, ",
             "ac.european_age_code as age_code, gilbert_rich_age_code as gr, ",
             "ind.percent_eggs_retained as pct_eggs, ",
             "ind.eggs_retained_gram as eggs_gram, ",
             "ind.eggs_retained_number as eggs_number, ",
             "ind.fish_sample_number as fish_sample_num, ",
             "ind.scale_sample_card_number as scale_card_num, ",
             "ind.scale_sample_position_number as scale_position_num, ",
             "ind.cwt_snout_sample_number as snout_sample_num, ",
             "ind.cwt_tag_code, ind.genetic_sample_number as genetic_sample_num, ",
             "ind.otolith_sample_number as otolith_sample_num, ",
             "ind.comment_text as fish_comment, ",
             "ind.created_datetime as created_date, ind.created_by, ",
             "ind.modified_datetime as modified_date, ind.modified_by ",
             "from individual_fish as ind ",
             "left join fish_condition_type_lut as fc on ind.fish_condition_type_id = fc.fish_condition_type_id ",
             "left join fish_trauma_type_lut as ft on ind.fish_trauma_type_id = ft.fish_trauma_type_id ",
             "left join gill_condition_type_lut as gcn on ind.gill_condition_type_id = gcn.gill_condition_type_id ",
             "left join spawn_condition_type_lut as sc on ind.spawn_condition_type_id = sc.spawn_condition_type_id ",
             "left join cwt_result_type_lut as cr on ind.cwt_result_type_id = cr.cwt_result_type_id ",
             "left join age_code_lut as ac on ind.age_code_id = ac.age_code_id ",
             "where ind.fish_encounter_id = '{fish_encounter_id}'")
  individual_fish = DBI::dbGetQuery(pool, qry)
  individual_fish = individual_fish %>%
    mutate(individual_fish_id = tolower(individual_fish_id)) %>%
    mutate(age_code = if_else(!is.na(age_code), paste0("eu: ", age_code), age_code)) %>%
    mutate(age_code = if_else(!is.na(age_code) & !is.na(gr), paste0(age_code, "; gr: ", gr), age_code)) %>%
    mutate(created_date = with_tz(created_date, tzone = "America/Los_Angeles")) %>%
    mutate(created_dt = format(created_date, "%m/%d/%Y %H:%M")) %>%
    mutate(modified_date = with_tz(modified_date, tzone = "America/Los_Angeles")) %>%
    mutate(modified_dt = format(modified_date, "%m/%d/%Y %H:%M")) %>%
    select(individual_fish_id, fish_condition, fish_trauma, gill_condition, spawn_condition,
           fish_sample_num, scale_card_num, scale_position_num, age_code, snout_sample_num,
           cwt_tag_code, cwt_result, genetic_sample_num, otolith_sample_num, pct_eggs,
           eggs_gram, eggs_number, fish_comment, created_date, created_dt, created_by,
           modified_date, modified_dt, modified_by) %>%
    arrange(created_date)
  return(individual_fish)
}

#==========================================================================
# Get generic lut input values
#==========================================================================

# Fish condition
get_fish_condition = function(pool) {
  qry = glue("select fish_condition_type_id, fish_condition_short_description as fish_condition ",
             "from fish_condition_type_lut ",
             "where obsolete_datetime is null")
  fish_condition_list = DBI::dbGetQuery(pool, qry) %>%
    mutate(fish_condition_type_id = tolower(fish_condition_type_id)) %>%
    arrange(fish_condition) %>%
    select(fish_condition_type_id, fish_condition)
  return(fish_condition_list)
}

# Fish trauma
get_fish_trauma = function(pool) {
  qry = glue("select fish_trauma_type_id, trauma_type_short_description as fish_trauma ",
             "from fish_trauma_type_lut ",
             "where obsolete_datetime is null")
  fish_trauma_list = DBI::dbGetQuery(pool, qry) %>%
    mutate(fish_trauma_type_id = tolower(fish_trauma_type_id)) %>%
    arrange(fish_trauma) %>%
    select(fish_trauma_type_id, fish_trauma)
  return(fish_trauma_list)
}

# Gill conditon
get_gill_condition = function(pool) {
  qry = glue("select gill_condition_type_id, gill_condition_type_description as gill_condition ",
             "from gill_condition_type_lut ",
             "where obsolete_datetime is null")
  gill_condition_list = DBI::dbGetQuery(pool, qry) %>%
    mutate(gill_condition_type_id = tolower(gill_condition_type_id)) %>%
    arrange(gill_condition) %>%
    select(gill_condition_type_id, gill_condition)
  return(gill_condition_list)
}

# Spawn conditon
get_spawn_condition = function(pool) {
  qry = glue("select spawn_condition_type_id, spawn_condition_short_description as spawn_condition ",
             "from spawn_condition_type_lut ",
             "where obsolete_datetime is null")
  spawn_condition_list = DBI::dbGetQuery(pool, qry) %>%
    mutate(spawn_condition_type_id = tolower(spawn_condition_type_id)) %>%
    arrange(spawn_condition) %>%
    select(spawn_condition_type_id, spawn_condition)
  return(spawn_condition_list)
}

# Age code
get_age_code = function(pool) {
  qry = glue("select age_code_id, european_age_code as age_code, gilbert_rich_age_code as gr ",
             "from age_code_lut ",
             "where obsolete_datetime is null")
  age_code_list = DBI::dbGetQuery(pool, qry) %>%
    mutate(age_code_id = tolower(age_code_id)) %>%
    mutate(age_code = paste0("eu: ", age_code)) %>%
    mutate(age_code = if_else(!is.na(gr), paste0(age_code, "; gr: ", gr), age_code)) %>%
    arrange(age_code) %>%
    select(age_code_id, age_code)
  return(age_code_list)
}

# CWT result
get_cwt_result = function(pool) {
  qry = glue("select cwt_result_type_id, cwt_result_type_short_description as cwt_result ",
             "from cwt_result_type_lut ",
             "where obsolete_datetime is null")
  cwt_result_list = DBI::dbGetQuery(pool, qry) %>%
    mutate(cwt_result_type_id = tolower(cwt_result_type_id)) %>%
    arrange(cwt_result) %>%
    select(cwt_result_type_id, cwt_result)
  return(cwt_result_list)
}

#========================================================
# Insert callback.....NEED TO ADD eggs stuff....currently missing !!!!!!!!!!!!!!!!!!
#========================================================

# Define the insert callback
individual_fish_insert = function(new_individual_fish_values) {
  new_insert_values = new_individual_fish_values
  # Pull out data
  fish_encounter_id = new_insert_values$fish_encounter_id
  fish_condition_type_id = new_insert_values$fish_condition_type_id
  fish_trauma_type_id =  new_insert_values$fish_trauma_type_id
  gill_condition_type_id = new_insert_values$gill_condition_type_id
  spawn_condition_type_id = new_insert_values$spawn_condition_type_id
  cwt_result_type_id = new_insert_values$cwt_result_type_id
  age_code_id = new_insert_values$age_code_id
  fish_sample_number = new_insert_values$fish_sample_num
  scale_sample_card_number = new_insert_values$scale_card_num
  scale_sample_position_number = new_insert_values$scale_position_num
  cwt_snout_sample_number = new_insert_values$snout_sample_num
  cwt_tag_code = new_insert_values$cwt_tag_code
  genetic_sample_number = new_insert_values$genetic_sample_num
  otolith_sample_number = new_insert_values$otolith_sample_num
  comment_text = new_insert_values$fish_comment
  created_by = new_insert_values$created_by
  # Checkout a connection
  con = poolCheckout(pool)
  insert_result = dbSendStatement(
    con, glue_sql("INSERT INTO individual_fish (",
                  "fish_encounter_id, ",
                  "fish_condition_type_id, ",
                  "fish_trauma_type_id, ",
                  "gill_condition_type_id, ",
                  "spawn_condition_type_id, ",
                  "cwt_result_type_id, ",
                  "age_code_id, ",
                  "fish_sample_number, ",
                  "scale_sample_card_number, ",
                  "scale_sample_position_number, ",
                  "cwt_snout_sample_number, ",
                  "cwt_tag_code, ",
                  "genetic_sample_number, ",
                  "otolith_sample_number, ",
                  "comment_text, ",
                  "created_by) ",
                  "VALUES (",
                  "?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"))
  dbBind(insert_result, list(fish_encounter_id, fish_condition_type_id, fish_trauma_type_id,
                             gill_condition_type_id, spawn_condition_type_id, cwt_result_type_id,
                             age_code_id, fish_sample_number, scale_sample_card_number,
                             scale_sample_position_number, cwt_snout_sample_number, cwt_tag_code,
                             genetic_sample_number, otolith_sample_number, comment_text, created_by))
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
# #========================================================
# # Identify species dependencies prior to delete
# #========================================================
#
# # Identify fish_encounter dependencies prior to delete
# get_fish_encounter_dependencies = function(fish_encounter_id) {
#   qry = glue("select ",
#              "count(ind.individual_fish_id) as individual_fish, ",
#              "count(fc.fish_capture_event_id) as fish_capture_event, ",
#              "count(fm.fish_mark_id) as fish_mark ",
#              "from fish_encounter as fe ",
#              "left join individual_fish as ind on fe.fish_encounter_id = ind.fish_encounter_id ",
#              "left join fish_capture_event as fc on fe.fish_encounter_id = fc.fish_encounter_id ",
#              "left join fish_mark as fm on fe.fish_encounter_id = fm.fish_encounter_id ",
#              "where fe.fish_encounter_id = '{fish_encounter_id}'")
#   con = poolCheckout(pool)
#   fish_encounter_dependents = DBI::dbGetQuery(pool, qry)
#   has_entries = function(x) any(x > 0L)
#   fish_encounter_dependents = fish_encounter_dependents %>%
#     select_if(has_entries)
#   return(fish_encounter_dependents)
# }
#
# #========================================================
# # Delete callback
# #========================================================
#
# # Define delete callback
# fish_encounter_delete = function(delete_values) {
#   fish_encounter_id = delete_values$fish_encounter_id
#   con = poolCheckout(pool)
#   delete_result = dbSendStatement(
#     con, glue_sql("DELETE FROM fish_encounter WHERE fish_encounter_id = ?"))
#   dbBind(delete_result, list(fish_encounter_id))
#   dbGetRowsAffected(delete_result)
#   dbClearResult(delete_result)
#   poolReturn(con)
# }
#











