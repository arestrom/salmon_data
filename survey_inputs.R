output$survey_method_select = renderUI({
  survey_method_list = get_survey_method(pool)$survey_method
  selectizeInput("survey_method_select", label = "survey_method",
                 choices = survey_method_list, selected = "Foot",
                 width = "100px")
})

output$data_source_select = renderUI({
  data_source_list = get_data_source(pool)$data_source_code
  selectizeInput("data_source_select", label = "data_source",
                 choices = data_source_list, selected = "WDFW",
                 width = "100px")
})

output$data_review_select = renderUI({
  data_review_list = get_data_review(pool)$data_review
  selectizeInput("data_review_select", label = "data_review",
                 choices = data_review_list, selected = data_review_list[1],
                 width = "115px")
})

output$completion_select = renderUI({
  completion_list = get_completion_status(pool)$completion
  selectizeInput("completion_select", label = "completed?",
                 choices = completion_list, selected = completion_list[1],
                 width = "150px")
})
