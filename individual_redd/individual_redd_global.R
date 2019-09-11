
# Main individual_redd query
get_individual_redd = function(pool, redd_encounter_id) {
  qry = glue("select ir.individual_redd_id, rh.redd_shape_description as redd_shape, ",
             "rd.dewatered_type_description as dewatered_type, ",
             "ir.percent_redd_visible as pct_visible, ",
             "ir.redd_length_measure_meter as redd_length_m, ",
             "ir.redd_width_measure_meter as redd_width_m, ",
             "ir.redd_depth_measure_meter as redd_depth_m, ",
             "ir.tailspill_height_measure_meter as tailspill_height_m, ",
             "ir.percent_redd_superimposed as pct_superimposed, ",
             "ir.percent_redd_degraded as pct_degraded, ",
             "ir.superimposed_redd_name, ",
             "ir.comment_text as individual_redd_comment, ",
             "ir.created_datetime as created_date, ir.created_by, ",
             "ir.modified_datetime as modified_date, ir.modified_by ",
             "from individual_redd as ir ",
             "left join redd_shape_lut as rh on ir.redd_shape_id = rh.redd_shape_id ",
             "left join redd_dewatered_type_lut as rd on ir.redd_dewatered_type_id = rd.redd_dewatered_type_id ",
             "where ir.redd_encounter_id = '{redd_encounter_id}'")
  individual_redds = DBI::dbGetQuery(pool, qry)
  individual_redds = individual_redds %>%
    mutate(individual_redd_id = tolower(individual_redd_id)) %>%
    mutate(created_date = with_tz(created_date, tzone = "America/Los_Angeles")) %>%
    mutate(created_dt = format(created_date, "%m/%d/%Y %H:%M")) %>%
    mutate(modified_date = with_tz(modified_date, tzone = "America/Los_Angeles")) %>%
    mutate(modified_dt = format(modified_date, "%m/%d/%Y %H:%M")) %>%
    select(individual_redd_id, redd_shape, dewatered_type, pct_visible,
           redd_length_m, redd_width_m, redd_depth_m, tailspill_height_m,
           pct_superimposed, pct_degraded, superimposed_redd_name,
           individual_redd_comment, created_date, created_dt,
           created_by, modified_date, modified_dt, modified_by) %>%
    arrange(created_date)
  return(individual_redds)
}

#==========================================================================
# Get generic lut input values
#==========================================================================

# Redd shape
get_redd_shape = function(pool) {
  qry = glue("select redd_shape_id, redd_shape_description as redd_shape ",
             "from redd_shape_lut ",
             "where obsolete_datetime is null")
  redd_shape_list = DBI::dbGetQuery(pool, qry) %>%
    mutate(redd_shape_id = tolower(redd_shape_id)) %>%
    arrange(redd_shape) %>%
    select(redd_shape_id, redd_shape)
  return(redd_shape_list)
}

# Dewatered type
get_dewatered_type = function(pool) {
  qry = glue("select redd_dewatered_type_id, dewatered_type_description as dewatered_type ",
             "from redd_dewatered_type_lut ",
             "where obsolete_datetime is null")
  dewatered_type_list = DBI::dbGetQuery(pool, qry) %>%
    mutate(redd_dewatered_type_id = tolower(redd_dewatered_type_id)) %>%
    arrange(dewatered_type) %>%
    select(redd_dewatered_type_id, dewatered_type)
  return(dewatered_type_list)
}

#========================================================
# Insert callback
#========================================================

# Define the insert callback
individual_redd_insert = function(new_individual_redd_values) {
  new_insert_values = new_individual_redd_values
  # Pull out data
  redd_encounter_id = new_insert_values$redd_encounter_id
  redd_shape_id = new_insert_values$redd_shape_id
  redd_dewatered_type_id = new_insert_values$redd_dewatered_type_id
  percent_redd_visible = new_insert_values$pct_visible
  redd_length_measure_meter = new_insert_values$redd_length_m
  redd_width_measure_meter = new_insert_values$redd_width_m
  redd_depth_measure_meter = new_insert_values$redd_depth_m
  tailspill_height_measure_meter = new_insert_values$tailspill_height_m
  percent_redd_superimposed = new_insert_values$pct_superimposed
  percent_redd_degraded = new_insert_values$pct_degraded
  superimposed_redd_name = new_insert_values$superimposed_redd_name
  comment_text = new_insert_values$individual_redd_comment
  if (is.na(superimposed_redd_name) | superimposed_redd_name == "") { superimposed_redd_name = NA }
  if (is.na(comment_text) | comment_text == "") { comment_text = NA }
  created_by = new_insert_values$created_by
  # Checkout a connection
  con = poolCheckout(pool)
  insert_result = dbSendStatement(
    con, glue_sql("INSERT INTO individual_redd (",
                  "redd_encounter_id, ",
                  "redd_shape_id, ",
                  "redd_dewatered_type_id, ",
                  "percent_redd_visible, ",
                  "redd_length_measure_meter, ",
                  "redd_width_measure_meter, ",
                  "redd_depth_measure_meter, ",
                  "tailspill_height_measure_meter, ",
                  "percent_redd_superimposed, ",
                  "percent_redd_degraded, ",
                  "superimposed_redd_name, ",
                  "comment_text, ",
                  "created_by) ",
                  "VALUES (",
                  "?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"))
  dbBind(insert_result, list(redd_encounter_id, redd_shape_id, redd_dewatered_type_id,
                             percent_redd_visible, redd_length_measure_meter,
                             redd_width_measure_meter, redd_depth_measure_meter,
                             tailspill_height_measure_meter, percent_redd_superimposed,
                             percent_redd_degraded, superimposed_redd_name,
                             comment_text, created_by))
  dbGetRowsAffected(insert_result)
  dbClearResult(insert_result)
  poolReturn(con)
}

# #========================================================
# # Edit update callback
# #========================================================
#
# # Define update callback
# redd_encounter_update = function(redd_encounter_edit_values) {
#   edit_values = redd_encounter_edit_values
#   # Pull out data
#   redd_encounter_id = edit_values$redd_encounter_id
#   redd_location_id = edit_values$redd_location_id
#   redd_status_id = edit_values$redd_status_id
#   redd_encounter_datetime = edit_values$redd_encounter_time
#   redd_count = edit_values$redd_count
#   comment_text = edit_values$redd_comment
#   if (is.na(comment_text) | comment_text == "") { comment_text = NA }
#   mod_dt = lubridate::with_tz(Sys.time(), "UTC")
#   mod_by = Sys.getenv("USERNAME")
#   # Checkout a connection
#   con = poolCheckout(pool)
#   update_result = dbSendStatement(
#     con, glue_sql("UPDATE redd_encounter SET ",
#                   "redd_location_id = ?, ",
#                   "redd_status_id = ?, ",
#                   "redd_encounter_datetime = ?, ",
#                   "redd_count = ?, ",
#                   "comment_text = ?, ",
#                   "modified_datetime = ?, ",
#                   "modified_by = ? ",
#                   "where redd_encounter_id = ?"))
#   dbBind(update_result, list(redd_location_id, redd_status_id,
#                              redd_encounter_datetime, redd_count,
#                              comment_text, mod_dt, mod_by,
#                              redd_encounter_id))
#   dbGetRowsAffected(update_result)
#   dbClearResult(update_result)
#   poolReturn(con)
# }
#
# #========================================================
# # Identify redd encounter dependencies prior to delete
# #========================================================
#
# # Identify fish_encounter dependencies prior to delete
# get_redd_encounter_dependencies = function(redd_encounter_id) {
#   qry = glue("select ",
#              "count(ir.individual_redd_id) as individual_redd, ",
#              "count(rc.redd_confidence_id) as redd_confidence, ",
#              "count(rs.redd_substrate_id) as redd_substrate ",
#              "from redd_encounter as rd ",
#              "left join individual_redd as ir on rd.redd_encounter_id = ir.redd_encounter_id ",
#              "left join redd_confidence as rc on rd.redd_encounter_id = rc.redd_encounter_id ",
#              "left join redd_substrate as rs on rd.redd_encounter_id = rs.redd_encounter_id ",
#              "where rd.redd_encounter_id = '{redd_encounter_id}'")
#   con = poolCheckout(pool)
#   redd_encounter_dependents = DBI::dbGetQuery(pool, qry)
#   has_entries = function(x) any(x > 0L)
#   redd_encounter_dependents = redd_encounter_dependents %>%
#     select_if(has_entries)
#   return(redd_encounter_dependents)
# }
#
# #========================================================
# # Delete callback
# #========================================================
#
# # Define delete callback
# redd_encounter_delete = function(delete_values) {
#   redd_encounter_id = delete_values$redd_encounter_id
#   con = poolCheckout(pool)
#   delete_result = dbSendStatement(
#     con, glue_sql("DELETE FROM redd_encounter WHERE redd_encounter_id = ?"))
#   dbBind(delete_result, list(redd_encounter_id))
#   dbGetRowsAffected(delete_result)
#   dbClearResult(delete_result)
#   poolReturn(con)
# }
#











