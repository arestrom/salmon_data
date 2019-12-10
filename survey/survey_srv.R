
# Server side inputs
output$survey_method_select = renderUI({
  survey_method_list = get_survey_method()$survey_method
  selectizeInput("survey_method_select", label = "survey_method",
                 choices = survey_method_list, selected = "Foot",
                 width = "115px")
})

output$data_source_select = renderUI({
  data_source_list = get_data_source()$data_source_code
  selectizeInput("data_source_select", label = "data_source",
                 choices = data_source_list, selected = "WDFW",
                 width = "100px")
})

output$data_review_select = renderUI({
  data_review_list = get_data_review()$data_review
  selectizeInput("data_review_select", label = "data_review",
                 choices = data_review_list, selected = data_review_list[1],
                 width = "115px")
})

output$completion_select = renderUI({
  completion_list = get_completion_status()$completion
  selectizeInput("completion_select", label = "completed?",
                 choices = completion_list, selected = completion_list[1],
                 width = "150px")
})

# Primary DT datatable for database
output$surveys = renderDT({
  req(input$year_select)
  req(input$stream_select)
  # req(!year_vals() == "No surveys")
  # req(input$tabs == "data_entry")
  # req(nchar(waterbody_id() == 36))
  survey_title = glue("Surveys for {input$stream_select} in {year_vals()}")
  survey_data = get_surveys(waterbody_id(), year_vals()) %>%
    mutate(start_time = start_time_dt, end_time = end_time_dt) %>%
    select(survey_dt = survey_date_dt, survey_method, up_rm,
           lo_rm, start_time, end_time, observer, submitter,
           data_source, data_review, completion, created_dt,
           created_by, modified_dt, modified_by)
  # Generate table
  datatable(survey_data,
            selection = list(mode = 'single'),
            extensions = 'Buttons',
            options = list(dom = 'Blftp',
                           pageLength = 5,
                           lengthMenu = c(1, 5, 10, 20, 40, 60, 100),
                           scrollX = T,
                           buttons = c('excel', 'print'),
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
                             "}")),
            caption = htmltools::tags$caption(
              style = 'caption-side: top; text-align: left; color: black; width: auto;',
              htmltools::em(htmltools::strong(survey_title))))
})

# Create surveys DT proxy object
survey_dt_proxy = dataTableProxy(outputId = "surveys")

# Set row selection to NULL if tab changes
# Update DB and reload DT
observeEvent(input$tabs, {
  if (input$tabs == "wria_stream") {
    selectRows(survey_dt_proxy, NULL)
  }
})

#========================================================
# Collect survey values from selected row for later use
#========================================================

# Create reactive to collect input values for update and delete actions
selected_survey_data = reactive({
  req(input$surveys_rows_selected)
  # req(input$year_select)
  # req(!year_vals() == "No surveys")
  # req(input$tabs == "data_entry")
  # req(!is.na(waterbody_id()))
  # selected_survey = NULL
  # req(nchar(waterbody_id() == 36))
  surveys = get_surveys(waterbody_id(), year_vals())
  survey_row = input$surveys_rows_selected
  selected_survey = tibble(survey_id = surveys$survey_id[survey_row],
                           survey_date = surveys$survey_date[survey_row],
                           survey_method = surveys$survey_method[survey_row],
                           up_rm = as.character(surveys$up_rm[survey_row]),
                           lo_rm = as.character(surveys$lo_rm[survey_row]),
                           start_time = surveys$start_time[survey_row],
                           end_time = surveys$end_time[survey_row],
                           observer = surveys$observer[survey_row],
                           submitter = surveys$submitter[survey_row],
                           data_source = surveys$data_source[survey_row],
                           data_review = surveys$data_review[survey_row],
                           completed = surveys$completion[survey_row],
                           created_date = surveys$created_date[survey_row],
                           created_by = surveys$created_by[survey_row],
                           modified_date = surveys$modified_date[survey_row],
                           modified_by = surveys$modified_by[survey_row])
  print("n rows in selected_survey_data")
  print(nrow(selected_survey))
  return(selected_survey)
})

#========================================================
# Update survey select inputs to values in selected row
#========================================================

# Update all survey input values to values in selected row
observeEvent(input$surveys_rows_selected, {
  req(input$surveys_rows_selected)
  ssdat = selected_survey_data()
  updateDateInput(session, "survey_date_input", value = ssdat$survey_date)
  updateSelectizeInput(session, "survey_method_select", selected = ssdat$survey_method)
  updateSelectizeInput(session, "upper_rm_select", selected = ssdat$up_rm)
  updateSelectizeInput(session, "lower_rm_select", selected = ssdat$lo_rm)
  updateTimeInput(session, "start_time_select", value = ssdat$start_time)
  updateTimeInput(session, "end_time_select", value = ssdat$end_time)
  updateTextInput(session, "observer_input", value = ssdat$observer)
  updateTextInput(session, "submitter_input", value = ssdat$submitter)
  updateSelectizeInput(session, "data_source_select", selected = ssdat$data_source)
  updateSelectizeInput(session, "data_review_select", selected = ssdat$data_review)
  updateSelectizeInput(session, "completion_select", selected = ssdat$completed)
})

#========================================================
# Insert operations: reactives, observers and modals
#========================================================

# Create reactive to collect input values for insert actions
survey_create = reactive({
  # Survey date
  survey_date_input = input$survey_date_input
  # Data source
  data_source_input = input$data_source_select
  if (data_source_input == "" ) {
    data_source_id = NA
  } else {
    data_source_vals = get_data_source()
    data_source_id = data_source_vals %>%
      filter(data_source_code == data_source_input) %>%
      pull(data_source_id)
  }
  # Survey method
  survey_method_input = input$survey_method_select
  if (survey_method_input == "" ) {
    survey_method_id = NA
  } else {
    survey_method_vals = get_survey_method()
    survey_method_id = survey_method_vals %>%
      filter(survey_method == survey_method_input) %>%
      pull(survey_method_id)
  }
  # Data review
  data_review_input = input$data_review_select
  if ( data_review_input == "" ) {
    data_review_status_id = NA
  } else {
    data_review_vals = get_data_review()
    data_review_status_id = data_review_vals %>%
      filter(data_review == data_review_input) %>%
      pull(data_review_status_id)
  }
  # RM values
  rm_vals = rm_list()
  up_rm_input = input$upper_rm_select
  if (up_rm_input == "" ) {
    upper_end_point_id = NA
  } else {
    upper_end_point_id = rm_vals %>%
      filter(rm_label == up_rm_input) %>%
      pull(location_id)
  }
  print("n upper end point")
  print(length(upper_end_point_id))
  print("vals upper end point")
  print(upper_end_point_id)
  lo_rm_input = input$lower_rm_select
  if ( lo_rm_input == "" ) {
    lower_end_point_id = NA
  } else {
    lower_end_point_id = rm_vals %>%
      filter(rm_label == lo_rm_input) %>%
      pull(location_id)
  }
  print("n lower end point")
  print(length(lower_end_point_id))
  print("vals lower end point")
  print(lower_end_point_id)
  # Time values
  start_time = format(input$start_time_select)
  if (nchar(start_time) < 16) { start_time = NA_character_ }
  start_time = as.POSIXct(start_time)
  end_time = format(input$end_time_select)
  if (nchar(end_time) < 16) { end_time = NA_character_ }
  end_time = as.POSIXct(end_time)
  # Survey completion
  completion_input = input$completion_select
  if (completion_input == "" ) {
    survey_completion_status_id = NA
  } else {
    completion_vals = get_completion_status()
    survey_completion_status_id = completion_vals %>%
      filter(completion == completion_input) %>%
      pull(survey_completion_status_id)
  }
  new_survey = tibble(survey_dt = survey_date_input,
                      data_source = data_source_input,
                      data_source_id = data_source_id,
                      survey_method = survey_method_input,
                      survey_method_id = survey_method_id,
                      data_review = data_review_input,
                      data_review_status_id = data_review_status_id,
                      up_rm = up_rm_input,
                      upper_end_point_id = upper_end_point_id,
                      lo_rm = lo_rm_input,
                      lower_end_point_id = lower_end_point_id,
                      start_time = start_time,
                      end_time = end_time,
                      observer = input$observer_input,
                      submitter = input$submitter_input,
                      completion = completion_input,
                      survey_completion_status_id = survey_completion_status_id,
                      created_dt = lubridate::with_tz(Sys.time(), "UTC"),
                      created_by = Sys.getenv("USERNAME"))
  new_survey = new_survey %>%
    mutate(observer = if_else(is.na(observer) | observer == "", NA_character_, observer)) %>%
    mutate(submitter = if_else(is.na(submitter) | submitter == "", NA_character_, submitter))
  print("n rows in survey_create()")
  print(nrow(new_survey))
  return(new_survey)
})

# Generate values to show in modal
output$survey_modal_insert_vals = renderDT({
  survey_modal_in_vals = survey_create() %>%
    mutate(start_time = format(start_time, "%H:%M")) %>%
    mutate(end_time = format(end_time, "%H:%M")) %>%
    select(survey_dt, survey_method, up_rm, lo_rm, start_time, end_time,
           observer, submitter, data_source, data_review, completion)
  # Generate table
  datatable(survey_modal_in_vals,
            rownames = FALSE,
            options = list(dom = 't',
                           scrollX = T,
                           ordering = FALSE,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
                             "}")))
})

observeEvent(input$survey_add, {
  new_survey_vals = survey_create()
  req(input$year_select)
  req(!input$year_select == "No surveys")
  req(input$tabs == "data_entry")
  existing_survey_vals = get_surveys(waterbody_id(), year_vals()) %>%
    mutate(up_rm = as.character(up_rm)) %>%
    mutate(lo_rm = as.character(lo_rm)) %>%
    select(survey_dt = survey_date, survey_method, up_rm, lo_rm,
           observer, data_source)
  dup_flag = dup_survey(new_survey_vals, existing_survey_vals)
  showModal(
    # Verify required fields have values
    tags$div(id = "survey_insert_modal",
             if ( is.na(new_survey_vals$survey_dt) |
                  is.na(new_survey_vals$survey_method) |
                  is.na(new_survey_vals$up_rm) |
                  is.na(new_survey_vals$lo_rm) |
                  is.na(new_survey_vals$observer) |
                  is.na(new_survey_vals$submitter) |
                  is.na(new_survey_vals$data_source) |
                  is.na(new_survey_vals$data_review) |
                  is.na(new_survey_vals$completion) ) {
               modalDialog (
                 size = "m",
                 title = "Warning",
                 paste0("All data fields are mandatory (except start and stop times)"),
                 easyClose = TRUE,
                 footer = NULL
               )
               # Verify survey is not already in database
             } else if ( dup_flag == TRUE ) {
               modalDialog (
                 size = "m",
                 title = "Warning",
                 paste0("Survey already exists. Please edit the date, RMs, observer, or source before proceeding." ),
                 easyClose = TRUE,
                 footer = NULL
               )
             } else {
               modalDialog (
                 size = 'l',
                 title = glue("Insert new survey to the database?"),
                 fluidPage (
                   DT::DTOutput("survey_modal_insert_vals"),
                   br(),
                   br(),
                   actionButton("insert_survey", "Insert survey")
                 ),
                 easyClose = TRUE,
                 footer = NULL
               )
             }
    ))
})

# Reactive to hold values that will actually be inserted
survey_insert_vals = reactive({
  new_values = survey_create() %>%
    mutate(survey_start_datetime = case_when(
      is.na(start_time) ~ as.POSIXct(NA),
      !is.na(start_time) ~ as.POSIXct(paste0(format(survey_dt), " ", format(start_time, "%H:%M")),
                                                      tz = "America/Los_Angeles"))) %>%
    mutate(survey_start_datetime = with_tz(survey_start_datetime, tzone = "UTC")) %>%
    mutate(survey_end_datetime = case_when(
      is.na(end_time) ~ as.POSIXct(NA),
      !is.na(end_time) ~ as.POSIXct(paste0(format(survey_dt), " ", format(end_time, "%H:%M")),
                                      tz = "America/Los_Angeles"))) %>%
    mutate(survey_end_datetime = with_tz(survey_end_datetime, tzone = "UTC")) %>%
    mutate(survey_dt = as.POSIXct(format(survey_dt), tz = "America/Los_Angeles")) %>%
    mutate(survey_dt = with_tz(survey_dt, tzone = "UTC")) %>%
    select(survey_dt, data_source_id, survey_method_id, data_review_status_id,
           upper_end_point_id, lower_end_point_id, survey_completion_status_id, survey_start_datetime,
           survey_end_datetime, observer_last_name = observer, data_submitter_last_name = submitter,
           created_by)
  return(new_values)
})

# Update DB and reload DT
observeEvent(input$insert_survey, {
  req(input$year_select)
  req(!input$year_select == "No surveys")
  req(input$tabs == "data_entry")
  survey_insert(survey_insert_vals())
  removeModal()
  post_insert_vals = get_surveys(waterbody_id(), year_vals()) %>%
    mutate(start_time = start_time_dt, end_time = end_time_dt) %>%
    select(survey_dt = survey_date_dt, survey_method, up_rm,
           lo_rm, start_time, end_time, observer, submitter,
           data_source, data_review, completion, created_dt,
           created_by, modified_dt, modified_by)
  replaceData(survey_dt_proxy, post_insert_vals)
})

#========================================================
# Edit operations: reactives, observers and modals
#========================================================

# Create reactive to collect input values for edit actions
survey_edit = reactive({
  # Data source
  data_source_vals = get_data_source()
  data_source_input = input$data_source_select
  data_source_id = data_source_vals %>%
    filter(data_source_code == data_source_input) %>%
    pull(data_source_id)
  # Survey method
  survey_method_vals = get_survey_method()
  survey_method_input = input$survey_method_select
  survey_method_id = survey_method_vals %>%
    filter(survey_method == survey_method_input) %>%
    pull(survey_method_id)
  # Data review
  data_review_vals = get_data_review()
  data_review_input = input$data_review_select
  data_review_status_id = data_review_vals %>%
    filter(data_review == data_review_input) %>%
    pull(data_review_status_id)
  # RM values
  rm_vals = rm_list()
  up_rm_input = input$upper_rm_select
  upper_end_point_id = rm_vals %>%
    filter(rm_label == up_rm_input) %>%
    pull(location_id)
  lo_rm_input = input$lower_rm_select
  lower_end_point_id = rm_vals %>%
    filter(rm_label == lo_rm_input) %>%
    pull(location_id)
  # Data source
  completion_vals = get_completion_status()
  completion_input = input$completion_select
  survey_completion_status_id = completion_vals %>%
    filter(completion == completion_input) %>%
    pull(survey_completion_status_id)
  edit_survey = tibble(survey_id = selected_survey_data()$survey_id,
                       survey_dt = input$survey_date_input,
                       data_source = data_source_input,
                       data_source_id = data_source_id,
                       survey_method = survey_method_input,
                       survey_method_id = survey_method_id,
                       data_review = data_review_input,
                       data_review_status_id = data_review_status_id,
                       up_rm = up_rm_input,
                       upper_end_point_id = upper_end_point_id,
                       lo_rm = lo_rm_input,
                       lower_end_point_id = lower_end_point_id,
                       start_time = format(input$start_time_select, "%H:%M"),
                       end_time = format(input$end_time_select, "%H:%M"),
                       observer = input$observer_input,
                       submitter = input$submitter_input,
                       completion = completion_input,
                       survey_completion_status_id = survey_completion_status_id,
                       modified_dt = lubridate::with_tz(Sys.time(), "UTC"),
                       modified_by = Sys.getenv("USERNAME"))
  edit_survey = edit_survey %>%
    mutate(survey_datetime = as.POSIXct(format(survey_dt), tz = "America/Los_Angeles")) %>%
    mutate(survey_datetime = lubridate::with_tz(survey_datetime, tzone = "UTC")) %>%
    mutate(start_time = if_else(is.na(start_time) | start_time == "", NA_character_, start_time)) %>%
    mutate(end_time = if_else(is.na(end_time) | end_time == "", NA_character_, end_time)) %>%
    mutate(survey_start_datetime = case_when(
      start_time %in% c("00:00", "") ~ as.POSIXct(NA),
      !start_time %in% c("00:00", "") ~ as.POSIXct(paste0(format(survey_dt), " ", start_time),
                                                   tz = "America/Los_Angeles"))) %>%
    mutate(survey_start_datetime = with_tz(survey_start_datetime, tzone = "UTC")) %>%
    mutate(survey_end_datetime = case_when(
      end_time %in% c("00:00", "") ~ as.POSIXct(NA),
      !end_time %in% c("00:00", "") ~ as.POSIXct(paste0(format(survey_dt), " ", end_time),
                                                 tz = "America/Los_Angeles"))) %>%
    mutate(survey_end_datetime = with_tz(survey_end_datetime, tzone = "UTC")) %>%
    mutate(observer = if_else(is.na(observer) | observer == "", NA_character_, observer)) %>%
    mutate(submitter = if_else(is.na(submitter) | submitter == "", NA_character_, submitter))
  return(edit_survey)
})

# Generate values to show in modal
output$survey_modal_update_vals = renderDT({
  survey_modal_up_vals = survey_edit() %>%
    select(survey_dt, survey_method, up_rm, lo_rm, start_time, end_time,
           observer, submitter, data_source, data_review, completion)
  # Generate table
  datatable(survey_modal_up_vals,
            rownames = FALSE,
            options = list(dom = 't',
                           scrollX = T,
                           ordering = FALSE,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
                             "}")))
})

observeEvent(input$survey_edit, {
  old_vals = selected_survey_data() %>%
    mutate(start_time = format(start_time, "%H:%M")) %>%
    mutate(end_time = format(end_time, "%H:%M")) %>%
    mutate(start_time = if_else(is.na(start_time), "00:00", start_time)) %>%
    mutate(end_time = if_else(is.na(end_time), "00:00", end_time)) %>%
    select(survey_dt = survey_date, survey_method, up_rm, lo_rm, start_time,
           end_time, observer, submitter, data_source, data_review,
           completion = completed)
  new_vals = survey_edit() %>%
    select(survey_dt, survey_method, up_rm, lo_rm, start_time, end_time,
           observer, submitter, data_source, data_review, completion)
  showModal(
    tags$div(id = "survey_update_modal",
             if ( !length(input$surveys_rows_selected) == 1 ) {
               modalDialog (
                 size = "m",
                 title = "Warning",
                 paste("Please select a row to edit!"),
                 easyClose = TRUE,
                 footer = NULL
               )
             } else if ( isTRUE(all_equal(old_vals, new_vals)) ) {
               modalDialog (
                 size = "m",
                 title = "Warning",
                 paste("Please change at least one value!"),
                 easyClose = TRUE,
                 footer = NULL
               )
             } else {
               modalDialog (
                 size = 'l',
                 title = "Update data for survey to these new values?",
                 fluidPage (
                   DT::DTOutput("survey_modal_update_vals"),
                   br(),
                   br(),
                   actionButton("save_edits","Save changes")
                 ),
                 easyClose = TRUE,
                 footer = NULL
               )
             }
    ))
})

# Update DB and reload DT
observeEvent(input$save_edits, {
  req(input$year_select)
  req(!input$year_select == "No surveys")
  req(input$tabs == "data_entry")
  survey_update(survey_edit())
  removeModal()
  post_edit_vals = get_surveys(waterbody_id(), year_vals()) %>%
    mutate(start_time = start_time_dt, end_time = end_time_dt) %>%
    select(survey_dt = survey_date_dt, survey_method, up_rm,
           lo_rm, start_time, end_time, observer, submitter,
           data_source, data_review, completion, created_dt,
           created_by, modified_dt, modified_by)
  replaceData(survey_dt_proxy, post_edit_vals)
})

#========================================================
# Delete operations: reactives, observers and modals
#========================================================

# Generate values to show in modal
output$survey_modal_delete_vals = renderDT({
  req(input$year_select)
  req(!input$year_select == "No surveys")
  req(input$tabs == "data_entry")
  survey_modal_del_id = selected_survey_data()$survey_id
  survey_modal_del_vals = get_surveys(waterbody_id(), year_vals()) %>%
    filter(survey_id == survey_modal_del_id) %>%
    mutate(start_time = start_time_dt, end_time = end_time_dt) %>%
    select(survey_dt = survey_date_dt, survey_method, up_rm,
           lo_rm, start_time, end_time, observer, submitter,
           data_source, data_review, completion, created_dt,
           created_by, modified_dt, modified_by)
  # Generate table
  datatable(survey_modal_del_vals,
            rownames = FALSE,
            options = list(dom = 't',
                           scrollX = T,
                           ordering = FALSE,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
                             "}")))
})

observeEvent(input$survey_delete, {
  survey_id = selected_survey_data()$survey_id
  survey_dependencies = get_survey_dependencies(survey_id)
  table_names = paste0(names(survey_dependencies), collapse = ", ")
  showModal(
    tags$div(id = "survey_delete_modal",
             if ( length(survey_id) == 0 ) {
               modalDialog (
                 size = "m",
                 title = "Warning",
                 paste("Please select a row to delete!" ),
                 easyClose = TRUE,
                 footer = NULL
               )
             } else if ( ncol(survey_dependencies) > 0L ) {
               modalDialog (
                 size = "m",
                 title = "Warning",
                 glue("Please delete associated survey data from the following tables first: {table_names}"),
                 easyClose = TRUE,
                 footer = NULL
               )
             } else {
               modalDialog (
                 size = 'l',
                 title = "Are you sure you want to delete this survey from the database?",
                 fluidPage (
                   DT::DTOutput("survey_modal_delete_vals"),
                   br(),
                   br(),
                   actionButton("delete_survey", "Delete survey")
                 ),
                 easyClose = TRUE,
                 footer = NULL
               )
             }
    ))
})

# Update DB and reload DT
observeEvent(input$delete_survey, {
  req(input$year_select)
  req(!input$year_select == "No surveys")
  req(input$tabs == "data_entry")
  survey_delete(selected_survey_data())
  removeModal()
  surveys_after_delete = get_surveys(waterbody_id(), year_vals()) %>%
    mutate(start_time = start_time_dt, end_time = end_time_dt) %>%
    select(survey_dt = survey_date_dt, survey_method, up_rm,
           lo_rm, start_time, end_time, observer, submitter,
           data_source, data_review, completion, created_dt,
           created_by, modified_dt, modified_by)
  replaceData(survey_dt_proxy, surveys_after_delete)
})
