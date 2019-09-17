#========================================================
# Generate lut select ui's
#========================================================

output$redd_status_select = renderUI({
  redd_status_list = get_redd_status(pool)$redd_status
  redd_status_list = c("", redd_status_list)
  selectizeInput("redd_status_select", label = "redd_status",
                 choices = redd_status_list, selected = NULL,
                 width = "250px")
})

output$redd_name_select = renderUI({
  req(input$survey_events_rows_selected)
  survey_event_id = selected_survey_event_data()$survey_event_id
  redd_name_list = get_redd_name(pool, survey_event_id)$redd_name
  redd_name_list = c("", redd_name_list)
  selectizeInput("redd_name_select", label = "redd_name",
                 choices = redd_name_list, selected = NULL,
                 width = "200px")
})

#========================================================
# Primary datatable for redd_encounters
#========================================================

# Primary DT datatable for survey_intent
output$redd_encounters = renderDT({
  redd_encounter_title = glue("{selected_survey_event_data()$species} redd data for {input$stream_select} on ",
                              "{selected_survey_data()$survey_date} from river mile {selected_survey_data()$up_rm} ",
                              "to {selected_survey_data()$lo_rm}")
  redd_encounter_data = get_redd_encounter(pool, selected_survey_event_data()$survey_event_id) %>%
    select(redd_encounter_dt, redd_status, redd_count, redd_name, redd_comment,
           created_dt, created_by, modified_dt, modified_by)

  # Generate table
  datatable(redd_encounter_data,
            selection = list(mode = 'single'),
            options = list(dom = 'ltp',
                           pageLength = 5,
                           lengthMenu = c(1, 5, 10, 20),
                           scrollX = T,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
                             "}")),
            caption = htmltools::tags$caption(
              style = 'caption-side: top; text-align: left; color: black; width: auto;',
              htmltools::em(htmltools::strong(redd_encounter_title))))
})

# Create surveys DT proxy object
redd_encounter_dt_proxy = dataTableProxy(outputId = "redd_encounters")

#========================================================
# Collect encounter values from selected row for later use
#========================================================

# Create reactive to collect input values for update and delete actions
selected_redd_encounter_data = reactive({
  req(input$redd_encounters_rows_selected)
  redd_encounter_data = get_redd_encounter(pool, selected_survey_event_data()$survey_event_id)
  redd_encounter_row = input$redd_encounters_rows_selected
  selected_redd_encounter = tibble(redd_encounter_id = redd_encounter_data$redd_encounter_id[redd_encounter_row],
                                   redd_encounter_time = redd_encounter_data$redd_encounter_time[redd_encounter_row],
                                   redd_status = redd_encounter_data$redd_status[redd_encounter_row],
                                   redd_count = redd_encounter_data$redd_count[redd_encounter_row],
                                   redd_name = redd_encounter_data$redd_name[redd_encounter_row],
                                   redd_comment = redd_encounter_data$redd_comment[redd_encounter_row],
                                   created_date = redd_encounter_data$created_date[redd_encounter_row],
                                   created_by = redd_encounter_data$created_by[redd_encounter_row],
                                   modified_date = redd_encounter_data$modified_date[redd_encounter_row],
                                   modified_by = redd_encounter_data$modified_by[redd_encounter_row])
  return(selected_redd_encounter)
})

#========================================================
# Update event select inputs to values in selected row
#========================================================

# Update all input values to values in selected row
observeEvent(input$redd_encounters_rows_selected, {
  sredat = selected_redd_encounter_data()
  updateTimeInput(session, "redd_encounter_time_select", value = sredat$redd_encounter_time)
  updateSelectizeInput(session, "redd_status_select", selected = sredat$redd_status)
  updateNumericInput(session, "redd_count_input", value = sredat$redd_count)
  updateSelectizeInput(session, "redd_name_select", selected = sredat$redd_name)
  updateTextAreaInput(session, "redd_comment_input", value = sredat$redd_comment)
})

#========================================================
# Insert operations: reactives, observers and modals
#========================================================

# Create reactive to collect input values for insert actions
redd_encounter_create = reactive({
  # Survey date
  survey_date = selected_survey_data()$survey_date
  # Survey_event_id
  survey_event_id_input = selected_survey_event_data()$survey_event_id
  # Redd encounter time
  redd_encounter_dt = format(input$redd_encounter_time_select)
  if (nchar(redd_encounter_dt) < 16) { redd_encounter_dt = NA_character_ }
  redd_encounter_dt = as.POSIXct(redd_encounter_dt)
  # Redd status
  redd_status_input = input$redd_status_select
  if ( redd_status_input == "" ) {
    redd_status_id = NA
  } else {
    redd_status_vals = get_redd_status(pool)
    redd_status_id = redd_status_vals %>%
      filter(redd_status == redd_status_input) %>%
      pull(redd_status_id)
  }
  # Redd name, location_id
  redd_name_input = input$redd_name_select
  if ( redd_name_input == "" ) {
    redd_location_id = NA
  } else {
    redd_name_vals = get_redd_name(pool, survey_event_id_input)
    redd_location_id = redd_name_vals %>%
      filter(redd_name == redd_name_input) %>%
      pull(redd_location_id)
  }
  new_redd_encounter = tibble(survey_event_id = survey_event_id_input,
                              survey_date = survey_date,
                              # Need to create full datetime values below modal
                              redd_encounter_dt = redd_encounter_dt,
                              redd_status = redd_status_input,
                              redd_status_id = redd_status_id,
                              redd_count = input$redd_count_input,
                              redd_name = redd_name_input,
                              redd_location_id = redd_location_id,
                              redd_comment = input$redd_comment_input,
                              created_dt = lubridate::with_tz(Sys.time(), "UTC"),
                              created_by = Sys.getenv("USERNAME"))
  return(new_redd_encounter)
})

# Generate values to show in modal
output$redd_encounter_modal_insert_vals = renderDT({
  redd_encounter_modal_in_vals = redd_encounter_create() %>%
    mutate(redd_encounter_dt = format(redd_encounter_dt, "%H:%M")) %>%
    select(redd_encounter_dt, redd_status, redd_count,
           redd_name, redd_comment)
  # Generate table
  datatable(redd_encounter_modal_in_vals,
            rownames = FALSE,
            options = list(dom = 't',
                           scrollX = T,
                           ordering = FALSE,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
                             "}")))
})

# Modal for new redd encounters. No need for dup flag, multiple rows possible
observeEvent(input$redd_enc_add, {
  new_redd_encounter_vals = redd_encounter_create()
  showModal(
    # Verify required fields have data...none can be blank
    tags$div(id = "redd_encounter_insert_modal",
             if ( is.na(new_redd_encounter_vals$redd_count) |
                  is.na(new_redd_encounter_vals$redd_status_id) ) {
               modalDialog (
                 size = "m",
                 title = "Warning",
                 paste0("Values are required for redd_status and redd_count"),
                 easyClose = TRUE,
                 footer = NULL
               )
               # Write to DB
             } else {
               modalDialog (
                 size = 'l',
                 title = glue("Insert new redd data to the database?"),
                 fluidPage (
                   DT::DTOutput("redd_encounter_modal_insert_vals"),
                   br(),
                   br(),
                   actionButton("insert_redd_encounter", "Insert redd data")
                 ),
                 easyClose = TRUE,
                 footer = NULL
               )
             }
    ))
})

# Reactive to hold values actually inserted
redd_encounter_insert_vals = reactive({
  new_redd_enc_values = redd_encounter_create() %>%
    mutate(redd_encounter_datetime = case_when(
      is.na(redd_encounter_dt) ~ as.POSIXct(NA),
      !is.na(redd_encounter_dt) ~ as.POSIXct(paste0(format(survey_date), " ",
                                                    format(redd_encounter_dt, "%H:%M")),
                                             tz = "America/Los_Angeles"))) %>%
    select(survey_event_id, redd_location_id, redd_status_id, redd_encounter_datetime,
           redd_count, comment_text = redd_comment, created_by)
  return(new_redd_enc_values)
})

# Update DB and reload DT
observeEvent(input$insert_redd_encounter, {
  redd_encounter_insert(redd_encounter_insert_vals())
  removeModal()
  post_redd_encounter_insert_vals = get_redd_encounter(pool, selected_survey_event_data()$survey_event_id) %>%
    select(redd_encounter_dt, redd_status, redd_count, redd_name, redd_comment,
           created_dt, created_by, modified_dt, modified_by)
  replaceData(redd_encounter_dt_proxy, post_redd_encounter_insert_vals)
})

# Update DB and reload DT
observeEvent(input$insert_redd_location, {
  post_redd_location_insert_encounter_vals = get_redd_encounter(pool, selected_survey_event_data()$survey_event_id) %>%
    select(redd_encounter_dt, redd_status, redd_count, redd_name, redd_comment,
           created_dt, created_by, modified_dt, modified_by)
  replaceData(redd_encounter_dt_proxy, post_redd_location_insert_encounter_vals)
}, priority = -1)

#========================================================
# Edit operations: reactives, observers and modals
#========================================================

# Create reactive to collect input values for insert actions
redd_encounter_edit = reactive({
  # Survey date
  survey_date = selected_survey_data()$survey_date
  # Survey_event_id
  survey_event_id_input = selected_survey_event_data()$survey_event_id
  # Redd encounter time
  redd_encounter_dt = format(input$redd_encounter_time_select)
  if (nchar(redd_encounter_dt) < 16) { redd_encounter_dt = NA_character_ }
  redd_encounter_dt = as.POSIXct(redd_encounter_dt)
  # Redd status
  redd_status_input = input$redd_status_select
  if ( redd_status_input == "" ) {
    redd_status_id = NA
  } else {
    redd_status_vals = get_redd_status(pool)
    redd_status_id = redd_status_vals %>%
      filter(redd_status == redd_status_input) %>%
      pull(redd_status_id)
  }
  # Redd name, location_id
  redd_name_input = input$redd_name_select
  if ( redd_name_input == "" ) {
    redd_location_id = NA
  } else {
    redd_name_vals = get_redd_name(pool, survey_event_id_input)
    redd_location_id = redd_name_vals %>%
      filter(redd_name == redd_name_input) %>%
      pull(redd_location_id)
  }
  edit_redd_encounter = tibble(redd_encounter_id = selected_redd_encounter_data()$redd_encounter_id,
                               # Need to create full datetime values below modal
                               survey_date = survey_date,
                               redd_encounter_dt = redd_encounter_dt,
                               redd_status = redd_status_input,
                               redd_status_id = redd_status_id,
                               redd_count = input$redd_count_input,
                               redd_name = redd_name_input,
                               redd_location_id = redd_location_id,
                               redd_comment = input$redd_comment_input,
                               modified_dt = lubridate::with_tz(Sys.time(), "UTC"),
                               modified_by = Sys.getenv("USERNAME"))
  edit_redd_encounter = edit_redd_encounter %>%
    mutate(redd_encounter_time = case_when(
      is.na(redd_encounter_dt) ~ as.POSIXct(NA),
      !is.na(redd_encounter_dt) ~ as.POSIXct(paste0(format(survey_date), " ", format(redd_encounter_dt, "%H:%M")),
                                        tz = "America/Los_Angeles"))) %>%
    mutate(redd_encounter_time = with_tz(redd_encounter_time, tzone = "UTC"))
  return(edit_redd_encounter)
})

# Generate values to show in modal
output$redd_encounter_modal_update_vals = renderDT({
  redd_encounter_modal_up_vals = redd_encounter_edit() %>%
    mutate(redd_encounter_dt = format(redd_encounter_dt, "%H:%M")) %>%
    select(redd_encounter_dt, redd_status, redd_count,
           redd_name, redd_comment)
  # Generate table
  datatable(redd_encounter_modal_up_vals,
            rownames = FALSE,
            options = list(dom = 't',
                           scrollX = T,
                           ordering = FALSE,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
                             "}")))
})

# Edit modal
observeEvent(input$redd_enc_edit, {
  old_redd_encounter_vals = selected_redd_encounter_data() %>%
    mutate(redd_encounter_dt = format(redd_encounter_time, "%H:%M")) %>%
    select(redd_encounter_dt, redd_status, redd_count,
           redd_name, redd_comment)
  old_redd_encounter_vals[] = lapply(old_redd_encounter_vals, remisc::set_na)
  new_redd_encounter_vals = redd_encounter_edit() %>%
    mutate(redd_count = as.integer(redd_count)) %>%
    mutate(redd_encounter_dt = format(redd_encounter_dt, "%H:%M")) %>%
    select(redd_encounter_dt, redd_status, redd_count,
           redd_name, redd_comment)
  new_redd_encounter_vals[] = lapply(new_redd_encounter_vals, remisc::set_na)
  showModal(
    tags$div(id = "redd_encounter_update_modal",
             if ( !length(input$redd_encounters_rows_selected) == 1 ) {
               modalDialog (
                 size = "m",
                 title = "Warning",
                 paste("Please select a row to edit!"),
                 easyClose = TRUE,
                 footer = NULL
               )
             } else if ( isTRUE(all_equal(old_redd_encounter_vals, new_redd_encounter_vals)) ) {
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
                 title = "Update redd data to these new values?",
                 fluidPage (
                   DT::DTOutput("redd_encounter_modal_update_vals"),
                   br(),
                   br(),
                   actionButton("save_redd_enc_edits", "Save changes")
                 ),
                 easyClose = TRUE,
                 footer = NULL
               )
             }
    ))
})

# Update DB and reload DT
observeEvent(input$save_redd_enc_edits, {
  redd_encounter_update(redd_encounter_edit())
  removeModal()
  post_redd_encounter_edit_vals = get_redd_encounter(pool, selected_survey_event_data()$survey_event_id) %>%
    select(redd_encounter_dt, redd_status, redd_count, redd_name, redd_comment,
           created_dt, created_by, modified_dt, modified_by)
  replaceData(redd_encounter_dt_proxy, post_redd_encounter_edit_vals)
})

#========================================================
# Delete operations: reactives, observers and modals
#========================================================

# Generate values to show in modal
output$redd_encounter_modal_delete_vals = renderDT({
  redd_encounter_modal_del_id = selected_redd_encounter_data()$redd_encounter_id
  redd_encounter_modal_del_vals = get_redd_encounter(pool, selected_survey_event_data()$survey_event_id) %>%
    filter(redd_encounter_id == redd_encounter_modal_del_id) %>%
    select(redd_encounter_dt, redd_status, redd_count, redd_name, redd_comment)
  # Generate table
  datatable(redd_encounter_modal_del_vals,
            rownames = FALSE,
            options = list(dom = 't',
                           scrollX = T,
                           ordering = FALSE,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
                             "}")))
})

# Reactive to hold dependencies
redd_encounter_dependencies = reactive({
  redd_encounter_id = selected_redd_encounter_data()$redd_encounter_id
  redd_enc_dep = get_redd_encounter_dependencies(redd_encounter_id)
  print(redd_enc_dep)
  return(redd_enc_dep)
})

observeEvent(input$redd_enc_delete, {
  redd_encounter_id = selected_redd_encounter_data()$redd_encounter_id
  redd_enc_dependencies = redd_encounter_dependencies()
  table_names = paste0(paste0("'", names(redd_enc_dependencies), "'"), collapse = ", ")
  showModal(
    tags$div(id = "redd_encounter_delete_modal",
             if ( ncol(redd_enc_dependencies) > 0L ) {
               modalDialog (
                 size = "m",
                 title = "Warning",
                 glue("Please delete associated redd data from the following tables first: {table_names}"),
                 easyClose = TRUE,
                 footer = NULL
               )
             } else {
               modalDialog (
                 size = 'l',
                 title = "Are you sure you want to delete this redd data from the database?",
                 fluidPage (
                   DT::DTOutput("redd_encounter_modal_delete_vals"),
                   br(),
                   br(),
                   actionButton("delete_redd_encounter", "Delete redd data")
                 ),
                 easyClose = TRUE,
                 footer = NULL
               )
             }
    ))
})

# Update DB and reload DT
observeEvent(input$delete_redd_encounter, {
  redd_encounter_delete(selected_redd_encounter_data())
  removeModal()
  redd_encounters_after_delete = get_redd_encounter(pool, selected_survey_event_data()$survey_event_id) %>%
    select(redd_encounter_dt, redd_status, redd_count, redd_name, redd_comment,
           created_dt, created_by, modified_dt, modified_by)
  replaceData(redd_encounter_dt_proxy, redd_encounters_after_delete)
})

# Reload DT
observeEvent(input$delete_redd_location, {
  redd_encounters_after_location_delete = get_redd_encounter(pool, selected_survey_event_data()$survey_event_id) %>%
    select(redd_encounter_dt, redd_status, redd_count, redd_name, redd_comment,
           created_dt, created_by, modified_dt, modified_by)
  replaceData(redd_encounter_dt_proxy, redd_encounters_after_location_delete)
}, priority = -1)

