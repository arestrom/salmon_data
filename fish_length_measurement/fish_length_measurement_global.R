
# Main fish_encounter query
get_length_measurements = function(pool, individual_fish_id) {
  qry = glue("select flm.fish_length_measurement_id, ",
             "mt.length_type_description as length_type, ",
             "flm.length_measurement_centimeter as length_cm, ",
             "flm.created_datetime as created_date, flm.created_by, ",
             "flm.modified_datetime as modified_date, flm.modified_by ",
             "from fish_length_measurement as flm ",
             "inner join fish_length_measurement_type_lut as mt on flm.fish_length_measurement_type_id = mt.fish_length_measurement_type_id ",
             "where flm.individual_fish_id = '{individual_fish_id}'")
  length_measurements = DBI::dbGetQuery(pool, qry)
  length_measurements = length_measurements %>%
    mutate(fish_length_measurement_id = tolower(fish_length_measurement_id)) %>%
    mutate(created_date = with_tz(created_date, tzone = "America/Los_Angeles")) %>%
    mutate(created_dt = format(created_date, "%m/%d/%Y %H:%M")) %>%
    mutate(modified_date = with_tz(modified_date, tzone = "America/Los_Angeles")) %>%
    mutate(modified_dt = format(modified_date, "%m/%d/%Y %H:%M")) %>%
    select(fish_length_measurement_id, length_type, length_cm, created_date, created_dt, created_by,
           modified_date, modified_dt, modified_by) %>%
    arrange(created_date)
  return(length_measurements)
}

#==========================================================================
# Get generic lut input values
#==========================================================================

# Length type
get_length_type = function(pool) {
  qry = glue("select fish_length_measurement_type_id, length_type_description as length_type ",
             "from fish_length_measurement_type_lut ",
             "where obsolete_datetime is null")
  length_type_list = DBI::dbGetQuery(pool, qry) %>%
    mutate(fish_length_measurement_type_id = tolower(fish_length_measurement_type_id)) %>%
    arrange(length_type) %>%
    select(fish_length_measurement_type_id, length_type)
  return(length_type_list)
}

# #========================================================
# # Insert callback
# #========================================================
#
# # Define the insert callback
# individual_fish_insert = function(new_individual_fish_values) {
#   new_insert_values = new_individual_fish_values
#   # Pull out data
#   fish_encounter_id = new_insert_values$fish_encounter_id
#   fish_condition_type_id = new_insert_values$fish_condition_type_id
#   fish_trauma_type_id =  new_insert_values$fish_trauma_type_id
#   gill_condition_type_id = new_insert_values$gill_condition_type_id
#   spawn_condition_type_id = new_insert_values$spawn_condition_type_id
#   cwt_result_type_id = new_insert_values$cwt_result_type_id
#   age_code_id = new_insert_values$age_code_id
#   percent_eggs_retained = new_insert_values$pct_eggs
#   eggs_retained_gram = new_insert_values$eggs_gram
#   eggs_retained_number = new_insert_values$eggs_number
#   fish_sample_number = new_insert_values$fish_sample_num
#   scale_sample_card_number = new_insert_values$scale_card_num
#   scale_sample_position_number = new_insert_values$scale_position_num
#   cwt_snout_sample_number = new_insert_values$snout_sample_num
#   cwt_tag_code = new_insert_values$cwt_tag_code
#   genetic_sample_number = new_insert_values$genetic_sample_num
#   otolith_sample_number = new_insert_values$otolith_sample_num
#   comment_text = new_insert_values$fish_comment
#   if (is.na(fish_sample_number) | fish_sample_number == "") { fish_sample_number = NA }
#   if (is.na(scale_sample_card_number) | scale_sample_card_number == "") { scale_sample_card_number = NA }
#   if (is.na(scale_sample_position_number) | scale_sample_position_number == "") { scale_sample_position_number = NA }
#   if (is.na(cwt_snout_sample_number) | cwt_snout_sample_number == "") { cwt_snout_sample_number = NA }
#   if (is.na(cwt_tag_code) | cwt_tag_code == "") { cwt_tag_code = NA }
#   if (is.na(genetic_sample_number) | genetic_sample_number == "") { genetic_sample_number = NA }
#   if (is.na(otolith_sample_number) | otolith_sample_number == "") { otolith_sample_number = NA }
#   if (is.na(comment_text) | comment_text == "") { comment_text = NA }
#   created_by = new_insert_values$created_by
#   # Checkout a connection
#   con = poolCheckout(pool)
#   insert_result = dbSendStatement(
#     con, glue_sql("INSERT INTO individual_fish (",
#                   "fish_encounter_id, ",
#                   "fish_condition_type_id, ",
#                   "fish_trauma_type_id, ",
#                   "gill_condition_type_id, ",
#                   "spawn_condition_type_id, ",
#                   "cwt_result_type_id, ",
#                   "age_code_id, ",
#                   "percent_eggs_retained, ",
#                   "eggs_retained_gram, ",
#                   "eggs_retained_number, ",
#                   "fish_sample_number, ",
#                   "scale_sample_card_number, ",
#                   "scale_sample_position_number, ",
#                   "cwt_snout_sample_number, ",
#                   "cwt_tag_code, ",
#                   "genetic_sample_number, ",
#                   "otolith_sample_number, ",
#                   "comment_text, ",
#                   "created_by) ",
#                   "VALUES (",
#                   "?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"))
#   dbBind(insert_result, list(fish_encounter_id, fish_condition_type_id, fish_trauma_type_id,
#                              gill_condition_type_id, spawn_condition_type_id, cwt_result_type_id,
#                              age_code_id, percent_eggs_retained, eggs_retained_gram,
#                              eggs_retained_number, fish_sample_number, scale_sample_card_number,
#                              scale_sample_position_number, cwt_snout_sample_number, cwt_tag_code,
#                              genetic_sample_number, otolith_sample_number, comment_text, created_by))
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
# individual_fish_update = function(individual_fish_edit_values) {
#   edit_values = individual_fish_edit_values
#   # Pull out data
#   individual_fish_id = edit_values$individual_fish_id
#   fish_condition_type_id = edit_values$fish_condition_type_id
#   fish_trauma_type_id =  edit_values$fish_trauma_type_id
#   gill_condition_type_id = edit_values$gill_condition_type_id
#   spawn_condition_type_id = edit_values$spawn_condition_type_id
#   cwt_result_type_id = edit_values$cwt_result_type_id
#   age_code_id = edit_values$age_code_id
#   percent_eggs_retained = edit_values$pct_eggs
#   eggs_retained_gram = edit_values$eggs_gram
#   eggs_retained_number = edit_values$eggs_number
#   fish_sample_number = edit_values$fish_sample_num
#   scale_sample_card_number = edit_values$scale_card_num
#   scale_sample_position_number = edit_values$scale_position_num
#   cwt_snout_sample_number = edit_values$snout_sample_num
#   cwt_tag_code = edit_values$cwt_tag_code
#   genetic_sample_number = edit_values$genetic_sample_num
#   otolith_sample_number = edit_values$otolith_sample_num
#   comment_text = edit_values$fish_comment
#   if (is.na(fish_sample_number) | fish_sample_number == "") { fish_sample_number = NA }
#   if (is.na(scale_sample_card_number) | scale_sample_card_number == "") { scale_sample_card_number = NA }
#   if (is.na(scale_sample_position_number) | scale_sample_position_number == "") { scale_sample_position_number = NA }
#   if (is.na(cwt_snout_sample_number) | cwt_snout_sample_number == "") { cwt_snout_sample_number = NA }
#   if (is.na(cwt_tag_code) | cwt_tag_code == "") { cwt_tag_code = NA }
#   if (is.na(genetic_sample_number) | genetic_sample_number == "") { genetic_sample_number = NA }
#   if (is.na(otolith_sample_number) | otolith_sample_number == "") { otolith_sample_number = NA }
#   if (is.na(comment_text) | comment_text == "") { comment_text = NA }
#   mod_dt = lubridate::with_tz(Sys.time(), "UTC")
#   mod_by = Sys.getenv("USERNAME")
#   # Checkout a connection
#   con = poolCheckout(pool)
#   update_result = dbSendStatement(
#     con, glue_sql("UPDATE individual_fish SET ",
#                   "fish_condition_type_id = ?, ",
#                   "fish_trauma_type_id = ?, ",
#                   "gill_condition_type_id = ?, ",
#                   "spawn_condition_type_id = ?, ",
#                   "cwt_result_type_id = ?, ",
#                   "age_code_id = ?, ",
#                   "percent_eggs_retained = ?, ",
#                   "eggs_retained_gram = ?, ",
#                   "eggs_retained_number = ?, ",
#                   "fish_sample_number = ?, ",
#                   "scale_sample_card_number = ?, ",
#                   "scale_sample_position_number = ?, ",
#                   "cwt_snout_sample_number = ?, ",
#                   "cwt_tag_code = ?, ",
#                   "genetic_sample_number = ?, ",
#                   "otolith_sample_number = ?, ",
#                   "comment_text = ?, ",
#                   "modified_datetime = ?, ",
#                   "modified_by = ? ",
#                   "where individual_fish_id = ?"))
#   dbBind(update_result, list(fish_condition_type_id, fish_trauma_type_id,
#                              gill_condition_type_id, spawn_condition_type_id,
#                              cwt_result_type_id, age_code_id, percent_eggs_retained,
#                              eggs_retained_gram, eggs_retained_number,
#                              fish_sample_number, scale_sample_card_number,
#                              scale_sample_position_number, cwt_snout_sample_number,
#                              cwt_tag_code, genetic_sample_number,
#                              otolith_sample_number, comment_text,
#                              mod_dt, mod_by, individual_fish_id))
#   dbGetRowsAffected(update_result)
#   dbClearResult(update_result)
#   poolReturn(con)
# }
#
# #========================================================
# # Identify individual fish dependencies prior to delete
# #========================================================
#
# # Identify fish_encounter dependencies prior to delete
# get_individual_fish_dependencies = function(individual_fish_id) {
#   qry = glue("select ",
#              "count(fl.fish_length_measurement_id) as fish_length_measurement ",
#              "from fish_length_measurement as fl ",
#              "where fl.individual_fish_id = '{individual_fish_id}'")
#   con = poolCheckout(pool)
#   individual_fish_dependents = DBI::dbGetQuery(pool, qry)
#   has_entries = function(x) any(x > 0L)
#   individual_fish_dependents = individual_fish_dependents %>%
#     select_if(has_entries)
#   return(individual_fish_dependents)
# }
#
# #========================================================
# # Delete callback
# #========================================================
#
# # Define delete callback
# individual_fish_delete = function(delete_values) {
#   individual_fish_id = delete_values$individual_fish_id
#   con = poolCheckout(pool)
#   delete_result = dbSendStatement(
#     con, glue_sql("DELETE FROM individual_fish WHERE individual_fish_id = ?"))
#   dbBind(delete_result, list(individual_fish_id))
#   dbGetRowsAffected(delete_result)
#   dbClearResult(delete_result)
#   poolReturn(con)
# }
#











