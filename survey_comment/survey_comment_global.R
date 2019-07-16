
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
