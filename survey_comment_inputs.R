
output$area_surveyed_select = renderUI({
  area_surveyed_list = get_area_surveyed(pool)$area_surveyed
  area_surveyed_list = c("", area_surveyed_list)
  selectizeInput("area_surveyed_select", label = "area_surveyed",
                 choices = area_surveyed_list, selected = NULL,
                 width = "175px")
})

output$abundance_condition_select = renderUI({
  abundance_condition_list = get_abundance_condition(pool)$abundance_condition
  abundance_condition_list = c("", abundance_condition_list)
  selectizeInput("abundance_condition_select", label = "abundance_condition",
                 choices = abundance_condition_list, selected = NULL,
                 width = "125px")
})

output$stream_condition_select = renderUI({
  stream_condition_list = get_stream_condition(pool)$stream_condition
  stream_condition_list = c("", stream_condition_list)
  selectizeInput("stream_condition_select", label = "stream_condition",
                 choices = stream_condition_list, selected = NULL,
                 width = "200px")
})

output$stream_flow_select = renderUI({
  stream_flow_list = get_stream_flow(pool)$stream_flow
  stream_flow_list = c("", stream_flow_list)
  selectizeInput("stream_flow_select", label = "stream_flow",
                 choices = stream_flow_list, selected = NULL,
                 width = "125px")
})

output$count_condition_select = renderUI({
  count_condition_list = get_count_condition(pool)$count_condition
  count_condition_list = c("", count_condition_list)
  selectizeInput("count_condition_select", label = "count_condition",
                 choices = count_condition_list, selected = NULL,
                 width = "200px")
})

output$survey_direction_select = renderUI({
  survey_direction_list = get_survey_direction(pool)$survey_direction
  survey_direction_list = c("", survey_direction_list)
  selectizeInput("survey_direction_select", label = "survey_direction",
                 choices = survey_direction_list, selected = NULL,
                 width = "150px")
})

output$survey_timing_select = renderUI({
  survey_timing_list = get_survey_timing(pool)$survey_timing
  survey_timing_list = c("", survey_timing_list)
  selectizeInput("survey_timing_select", label = "survey_timing",
                 choices = survey_timing_list, selected = NULL,
                 width = "150px")
})

output$visibility_condition_select = renderUI({
  visibility_condition_list = get_visibility_condition(pool)$visibility_condition
  visibility_condition_list = c("", visibility_condition_list)
  selectizeInput("visibility_condition_select", label = "visibility_condition",
                 choices = visibility_condition_list, selected = NULL,
                 width = "150px")
})

output$visibility_type_select = renderUI({
  visibility_type_list = get_visibility_type(pool)$visibility_type
  visibility_type_list = c("", visibility_type_list)
  selectizeInput("visibility_type_select", label = "visibility_type",
                 choices = visibility_type_list, selected = NULL,
                 width = "150px")
})

output$weather_type_select = renderUI({
  weather_type_list = get_weather_type(pool)$weather_type
  weather_type_list = c("", weather_type_list)
  selectizeInput("weather_type_select", label = "weather_type",
                 choices = weather_type_list, selected = NULL,
                 width = "150px")
})
