
# Main waterbody measurements query
get_waterbody_meas = function(pool, survey_id) {
  qry = glue("select wb.waterbody_measurement_id, wc.clarity_type_short_description as clarity_type, ",
             "wb.water_clarity_meter as clarity_meter, wb.stream_flow_measurement_cfs as flow_cfs, ",
             "wb.start_water_temperature_datetime as start_temperature, ",
             "wb.start_water_temperature_datetime as start_tmp_time, ",
             "wb.end_water_temperature_datetime as end_temperature, ",
             "wb.end_water_temperature_datetime as end_tmp_time, ",
             "wb.waterbody_ph as water_ph, wb.created_datetime as created_date, ",
             "wb.created_by, wb.modified_datetime as modified_date, wb.modified_by ",
             "from waterbody_measurement as wb ",
             "left join water_clarity_type_lut as wc on wb.water_clarity_type_id = wc.water_clarity_type_id ",
             "left join count_type_lut as ct on si.count_type_id = ct.count_type_id ",
             "where si.survey_id = '{survey_id}'")
  survey_intents = DBI::dbGetQuery(pool, qry)
  survey_intents = survey_intents %>%
    mutate(survey_intent_id = tolower(survey_intent_id)) %>%
    mutate(created_date = with_tz(created_date, tzone = "America/Los_Angeles")) %>%
    mutate(created_dt = format(created_date, "%m/%d/%Y %H:%M")) %>%
    mutate(modified_date = with_tz(modified_date, tzone = "America/Los_Angeles")) %>%
    mutate(modified_dt = format(modified_date, "%m/%d/%Y %H:%M")) %>%
    select(survey_intent_id, species, count_type, created_date, created_dt,
           created_by, modified_date, modified_dt, modified_by) %>%
    arrange(created_date)
  return(survey_intents)
}

#==========================================================================
# Get generic lut input values
#==========================================================================

# Water clarity type
get_clarity_type = function(pool) {
  qry = glue("select water_clarity_type_id, clarity_type_short_description as clarity_type ",
             "from water_clarity_type_lut ",
             "where obsolete_datetime is null")
  clarity_type_list = DBI::dbGetQuery(pool, qry) %>%
    mutate(water_clarity_type_id = tolower(water_clarity_type_id)) %>%
    arrange(clarity_type) %>%
    select(water_clarity_type_id, clarity_type)
  return(clarity_type_list)
}
