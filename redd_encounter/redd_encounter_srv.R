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

# #========================================================
# # Edit operations: reactives, observers and modals
# #========================================================
#
# # Create reactive to collect input values for insert actions
# fish_encounter_edit = reactive({
#   # Survey date
#   survey_date = selected_survey_data()$survey_date
#   # # Location....NA for now
#   # fish_location_input = NA
#   # Fish encounter time
#   fish_encounter_dt = format(input$fish_encounter_time_select)
#   if (nchar(fish_encounter_dt) < 16) { fish_encounter_dt = NA_character_ }
#   fish_encounter_dt = as.POSIXct(fish_encounter_dt)
#   # Fish status
#   fish_status_input = input$fish_status_select
#   if ( fish_status_input == "" ) {
#     fish_status_id = NA
#   } else {
#     fish_status_vals = get_fish_status(pool)
#     fish_status_id = fish_status_vals %>%
#       filter(fish_status == fish_status_input) %>%
#       pull(fish_status_id)
#   }
#   # Sex
#   sex_input = input$sex_select
#   if ( sex_input == "" ) {
#     sex_id = NA
#   } else {
#     sex_vals = get_sex(pool)
#     sex_id = sex_vals %>%
#       filter(sex == sex_input) %>%
#       pull(sex_id)
#   }
#   # Maturity
#   maturity_input = input$maturity_select
#   if ( maturity_input == "" ) {
#     maturity_id = NA
#   } else {
#     maturity_vals = get_maturity(pool)
#     maturity_id = maturity_vals %>%
#       filter(maturity == maturity_input) %>%
#       pull(maturity_id)
#   }
#   # Origin
#   origin_input = input$origin_select
#   if ( origin_input == "" ) {
#     origin_id = NA
#   } else {
#     origin_vals = get_origin(pool)
#     origin_id = origin_vals %>%
#       filter(origin == origin_input) %>%
#       pull(origin_id)
#   }
#   # CWT status
#   cwt_status_input = input$cwt_status_select
#   if ( cwt_status_input == "" ) {
#     cwt_status_id = NA
#   } else {
#     cwt_status_vals = get_cwt_status(pool)
#     cwt_detection_status_id = cwt_status_vals %>%
#       filter(cwt_status == cwt_status_input) %>%
#       pull(cwt_detection_status_id)
#   }
#   # Clip status
#   clip_status_input = input$clip_status_select
#   if ( clip_status_input == "" ) {
#     clip_status_id = NA
#   } else {
#     clip_status_vals = get_clip_status(pool)
#     adipose_clip_status_id = clip_status_vals %>%
#       filter(clip_status == clip_status_input) %>%
#       pull(adipose_clip_status_id)
#   }
#   # Fish behavior
#   fish_behavior_input = input$fish_behavior_select
#   if ( fish_behavior_input == "" ) {
#     fish_behavior_id = NA
#   } else {
#     fish_behavior_vals = get_fish_behavior(pool)
#     fish_behavior_type_id = fish_behavior_vals %>%
#       filter(fish_behavior == fish_behavior_input) %>%
#       pull(fish_behavior_type_id)
#   }
#   edit_fish_encounter = tibble(fish_encounter_id = selected_fish_encounter_data()$fish_encounter_id,
#                                #fish_location_id = fish_location_input,
#                                # Need to create full datetime values below modal
#                                fish_encounter_dt = fish_encounter_dt,
#                                fish_count = input$fish_count_input,
#                                fish_status = fish_status_input,
#                                fish_status_id = fish_status_id,
#                                sex = sex_input,
#                                sex_id = sex_id,
#                                maturity = maturity_input,
#                                maturity_id = maturity_id,
#                                origin = origin_input,
#                                origin_id = origin_id,
#                                cwt_status = cwt_status_input,
#                                cwt_detection_status_id = cwt_detection_status_id,
#                                clip_status = clip_status_input,
#                                adipose_clip_status_id = adipose_clip_status_id,
#                                fish_behavior = fish_behavior_input,
#                                fish_behavior_type_id = fish_behavior_type_id,
#                                # Need to create prev_counted boolean below
#                                prev_counted = input$prev_counted_select,
#                                modified_dt = lubridate::with_tz(Sys.time(), "UTC"),
#                                modified_by = Sys.getenv("USERNAME"))
#   edit_fish_encounter = edit_fish_encounter %>%
#     mutate(fish_encounter_time = case_when(
#       is.na(fish_encounter_dt) ~ as.POSIXct(NA),
#       !is.na(fish_encounter_dt) ~ as.POSIXct(paste0(format(survey_date), " ", format(fish_encounter_dt, "%H:%M")),
#                                         tz = "America/Los_Angeles"))) %>%
#     mutate(fish_encounter_time = with_tz(fish_encounter_time, tzone = "UTC")) %>%
#     mutate(previously_counted_indicator = if_else(prev_counted == "No", FALSE, TRUE))
#   return(edit_fish_encounter)
# })
#
# # Generate values to show in modal
# output$fish_encounter_modal_update_vals = renderDT({
#   fish_encounter_modal_up_vals = fish_encounter_edit() %>%
#     mutate(fish_encounter_dt = format(fish_encounter_dt, "%H:%M")) %>%
#     select(fish_encounter_dt, fish_count, fish_status, sex, maturity, origin, cwt_status,
#            clip_status, fish_behavior, prev_counted)
#   # Generate table
#   datatable(fish_encounter_modal_up_vals,
#             rownames = FALSE,
#             options = list(dom = 't',
#                            scrollX = T,
#                            ordering = FALSE,
#                            initComplete = JS(
#                              "function(settings, json) {",
#                              "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
#                              "}")))
# })
#
# # output$chk_edit = renderText({
# #   old_length_measurement_vals = selected_length_measurement_data() %>%
# #     select(length_type, length_cm) %>%
# #     mutate(length_cm = as.numeric(length_cm))
# #   new_length_measurement_vals = length_measurement_edit() %>%
# #     select(length_type, length_cm) %>%
# #     mutate(length_cm = as.numeric(length_cm))
# #   print(old_length_measurement_vals)
# #   print(new_length_measurement_vals)
# #   return(unlist(old_length_measurement_vals))
# # })
#
# # Edit modal
# observeEvent(input$fish_enc_edit, {
#   old_fish_encounter_vals = selected_fish_encounter_data() %>%
#     mutate(fish_encounter_dt = format(fish_encounter_time, "%H:%M")) %>%
#     select(fish_encounter_dt, fish_count, fish_status, sex, maturity, origin, cwt_status,
#            clip_status, fish_behavior, prev_counted)
#   new_fish_encounter_vals = fish_encounter_edit() %>%
#     mutate(fish_count = as.integer(fish_count)) %>%
#     mutate(fish_encounter_dt = format(fish_encounter_dt, "%H:%M")) %>%
#     select(fish_encounter_dt, fish_count, fish_status, sex, maturity, origin, cwt_status,
#            clip_status, fish_behavior, prev_counted)
#   showModal(
#     tags$div(id = "fish_encounter_update_modal",
#              if ( !length(input$fish_encounters_rows_selected) == 1 ) {
#                modalDialog (
#                  size = "m",
#                  title = "Warning",
#                  paste("Please select a row to edit!"),
#                  easyClose = TRUE,
#                  footer = NULL
#                )
#              } else if ( isTRUE(all_equal(old_fish_encounter_vals, new_fish_encounter_vals)) ) {
#                modalDialog (
#                  size = "m",
#                  title = "Warning",
#                  paste("Please change at least one value!"),
#                  easyClose = TRUE,
#                  footer = NULL
#                )
#              } else {
#                modalDialog (
#                  size = 'l',
#                  title = "Update fish data to these new values?",
#                  fluidPage (
#                    DT::DTOutput("fish_encounter_modal_update_vals"),
#                    br(),
#                    br(),
#                    actionButton("save_fish_enc_edits","Save changes")
#                  ),
#                  easyClose = TRUE,
#                  footer = NULL
#                )
#              }
#     ))
# })
#
# # Update DB and reload DT
# observeEvent(input$save_fish_enc_edits, {
#   fish_encounter_update(fish_encounter_edit())
#   removeModal()
#   post_fish_encounter_edit_vals = get_fish_encounter(pool, selected_survey_event_data()$survey_event_id) %>%
#     select(fish_encounter_dt, fish_count, fish_status, sex, maturity, origin,
#            cwt_status, clip_status, fish_behavior, prev_counted, created_dt,
#            created_by, modified_dt, modified_by)
#   replaceData(fish_encounter_dt_proxy, post_fish_encounter_edit_vals)
# })
#
# #========================================================
# # Delete operations: reactives, observers and modals
# #========================================================
#
# # Generate values to show in modal
# output$fish_encounter_modal_delete_vals = renderDT({
#   fish_encounter_modal_del_id = selected_fish_encounter_data()$fish_encounter_id
#   fish_encounter_modal_del_vals = get_fish_encounter(pool, selected_survey_event_data()$survey_event_id) %>%
#     filter(fish_encounter_id == fish_encounter_modal_del_id) %>%
#     select(fish_encounter_dt, fish_count, fish_status, sex, maturity, origin, cwt_status,
#            clip_status, fish_behavior, prev_counted)
#   # Generate table
#   datatable(fish_encounter_modal_del_vals,
#             rownames = FALSE,
#             options = list(dom = 't',
#                            scrollX = T,
#                            ordering = FALSE,
#                            initComplete = JS(
#                              "function(settings, json) {",
#                              "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
#                              "}")))
# })
#
# observeEvent(input$fish_enc_delete, {
#   fish_encounter_id = selected_fish_encounter_data()$fish_encounter_id
#   fish_encounter_dependencies = get_fish_encounter_dependencies(fish_encounter_id)
#   table_names = paste0(names(fish_encounter_dependencies), collapse = ", ")
#   showModal(
#     tags$div(id = "fish_encounter_delete_modal",
#              if ( ncol(fish_encounter_dependencies) > 0L ) {
#                modalDialog (
#                  size = "m",
#                  title = "Warning",
#                  glue("Please delete associated fish data from the following tables first: {table_names}"),
#                  easyClose = TRUE,
#                  footer = NULL
#                )
#              } else {
#                modalDialog (
#                  size = 'l',
#                  title = "Are you sure you want to delete this fish data from the database?",
#                  fluidPage (
#                    DT::DTOutput("fish_encounter_modal_delete_vals"),
#                    br(),
#                    br(),
#                    actionButton("delete_fish_encounter", "Delete fish data")
#                  ),
#                  easyClose = TRUE,
#                  footer = NULL
#                )
#              }
#     ))
# })
#
# # Update DB and reload DT
# observeEvent(input$delete_fish_encounter, {
#   fish_encounter_delete(selected_fish_encounter_data())
#   removeModal()
#   fish_encounters_after_delete = get_fish_encounter(pool, selected_survey_event_data()$survey_event_id) %>%
#     select(fish_encounter_dt, fish_count, fish_status, sex, maturity, origin,
#            cwt_status, clip_status, fish_behavior, prev_counted, created_dt,
#            created_by, modified_dt, modified_by)
#   replaceData(fish_encounter_dt_proxy, fish_encounters_after_delete)
# })
