
#========================================================
# Generate lut select ui's
#========================================================

output$channel_type_select = renderUI({
  channel_type_list = get_channel_type(pool)$channel_type
  channel_type_list = c("", channel_type_list)
  selectizeInput("channel_type_select", label = "channel_type",
                 choices = channel_type_list, selected = NULL,
                 width = "250px")
})

output$orientation_type_select = renderUI({
  orientation_type_list = get_orientation_type(pool)$orientation_type
  orientation_type_list = c("", orientation_type_list)
  selectizeInput("orientation_type_select", label = "orientation_type",
                 choices = orientation_type_list, selected = NULL,
                 width = "275px")
})

#========================================================
# Primary datatable for redd_locations
#========================================================

# Primary DT datatable for survey_intent
output$redd_locations = renderDT({
  redd_location_title = glue("{selected_survey_event_data()$species} redd locations for {input$stream_select} on ",
                             "{selected_survey_data()$survey_date} from river mile {selected_survey_data()$up_rm} ",
                             "to {selected_survey_data()$lo_rm}")
  redd_location_data = get_redd_location(pool, selected_redd_encounter_data()$redd_encounter_id) %>%
    select(redd_name, channel_type, orientation_type, latitude,
           longitude, horiz_accuracy, location_description,
           created_dt, created_by, modified_dt, modified_by)

  # Generate table
  datatable(redd_location_data,
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
              htmltools::em(htmltools::strong(redd_location_title))))
})

# Create surveys DT proxy object
redd_location_dt_proxy = dataTableProxy(outputId = "redd_locations")

#========================================================
# Collect location values from selected row for later use
#========================================================

# Create reactive to collect input values for update and delete actions
selected_redd_location_data = reactive({
  req(input$redd_locations_rows_selected)
  redd_location_data = get_redd_location(pool, selected_redd_encounter_data()$redd_encounter_id)
  redd_location_row = input$redd_locations_rows_selected
  selected_redd_location = tibble(redd_location_id = redd_location_data$redd_location_id[redd_location_row],
                                  location_coordinates_id = redd_location_data$location_coordinates_id[redd_location_row],
                                  redd_name = redd_location_data$redd_name[redd_location_row],
                                  channel_type = redd_location_data$channel_type[redd_location_row],
                                  orientation_type = redd_location_data$orientation_type[redd_location_row],
                                  latitude = redd_location_data$latitude[redd_location_row],
                                  longitude = redd_location_data$longitude[redd_location_row],
                                  horiz_accuracy = redd_location_data$horiz_accuracy[redd_location_row],
                                  location_description = redd_location_data$location_description[redd_location_row],
                                  created_date = redd_location_data$created_date[redd_location_row],
                                  created_by = redd_location_data$created_by[redd_location_row],
                                  modified_date = redd_location_data$modified_date[redd_location_row],
                                  modified_by = redd_location_data$modified_by[redd_location_row])
  return(selected_redd_location)
})

#========================================================
# Update inputs to values in selected row
#========================================================

# Update all input values to values in selected row
observeEvent(input$redd_locations_rows_selected, {
  srldat = selected_redd_location_data()
  updateTextInput(session, "redd_name_input", value = srldat$redd_name)
  updateSelectizeInput(session, "channel_type_select", selected = srldat$channel_type)
  updateSelectizeInput(session, "orientation_type_select", selected = srldat$orientation_type)
  updateNumericInput(session, "latitude_input", value = srldat$latitude)
  updateNumericInput(session, "longitude_input", value = srldat$longitude)
  updateNumericInput(session, "horiz_accuracy_input", value = srldat$horiz_accuracy)
  updateTextAreaInput(session, "location_description_input", value = srldat$redd_comment)
})

#========================================================
# Update inputs to values of selected map location
#========================================================

# NEED TO FILL IN HERE !!!!!!!!!!!!!!!!
# Make point moveable

#========================================================
# Insert operations: reactives, observers and modals
#========================================================

# Create reactive to collect input values for insert actions
redd_location_create = reactive({
  # Redd_encounter_id
  redd_encounter_id_input = selected_redd_encounter_data()$redd_encounter_id
  # Channel type
  channel_type_input = input$channel_type_select
  if ( channel_type_input == "" ) {
    stream_channel_type_id = NA
  } else {
    channel_type_vals = get_channel_type(pool)
    stream_channel_type_id = channel_type_vals %>%
      filter(channel_type == channel_type_input) %>%
      pull(stream_channel_type_id)
  }
  # Orienntation type
  orientation_type_input = input$orientation_type_select
  if ( orientation_type_input == "" ) {
    location_orientation_type_id = NA
  } else {
    orientation_type_vals = get_orientation_type(pool)
    location_orientation_type_id = orientation_type_vals %>%
      filter(orientation_type == orientation_type_input) %>%
      pull(location_orientation_type_id)
  }
  new_redd_location = tibble(redd_encounter_id = redd_encounter_id_input,
                             redd_name = input$redd_name_input,
                             channel_type = channel_type_input,
                             stream_channel_type_id = stream_channel_type_id,
                             orientation_type = orientation_type_input,
                             location_orientation_type_id = location_orientation_type_id,
                             latitude = input$latitude_input,
                             longitude = input$longitude_input,
                             horiz_accuracy = input$horiz_accuracy_input,
                             location_description = input$location_description_input,
                             created_dt = lubridate::with_tz(Sys.time(), "UTC"),
                             created_by = Sys.getenv("USERNAME"))
  return(new_redd_location)
})

# Generate values to show in modal
output$redd_location_modal_insert_vals = renderDT({
  redd_location_modal_in_vals = redd_location_create() %>%
    select(redd_name, channel_type, orientation_type, latitude,
           longitude, horiz_accuracy, location_description)
  # Generate table
  datatable(redd_location_modal_in_vals,
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
observeEvent(input$redd_loc_add, {
  new_redd_location_vals = redd_location_create()
  showModal(
    # Verify required fields have data...none can be blank
    tags$div(id = "redd_location_insert_modal",
             if ( is.na(new_redd_location_vals$redd_name) |
                  new_redd_location_vals$redd_name == "" |
                  is.na(new_redd_location_vals$channel_type) |
                  is.na(new_redd_location_vals$orientation_type) |
                  is.na(new_redd_location_vals$latitude) |
                  is.na(new_redd_location_vals$longitude) ) {
               modalDialog (
                 size = "m",
                 title = "Warning",
                 paste0("Values are required in all but the last two fields"),
                 easyClose = TRUE,
                 footer = NULL
               )
               # Write to DB
             } else {
               modalDialog (
                 size = 'l',
                 title = glue("Insert new redd location data to the database?"),
                 fluidPage (
                   DT::DTOutput("redd_location_modal_insert_vals"),
                   br(),
                   br(),
                   actionButton("insert_redd_location", "Insert location")
                 ),
                 easyClose = TRUE,
                 footer = NULL
               )
             }
    ))
})

# Reactive to pull out wria_id
wria_id = reactive({
  req(input$wria_select)
  get_streams(pool, chosen_wria = input$wria_select) %>%
    st_drop_geometry() %>%
    mutate(wria_id = tolower(wria_id)) %>%
    select(wria_id) %>%
    distinct() %>%
    pull(wria_id)
})

# Reactive to hold values actually inserted
redd_location_insert_vals = reactive({
  new_redd_loc_values = redd_location_create() %>%
    mutate(waterbody_id = waterbody_id()) %>%
    mutate(wria_id = wria_id()) %>%
    mutate(location_type_id = "d5edb1c0-f645-4e82-92af-26f5637b2de0") %>%     # Redd encounter
    select(redd_encounter_id, waterbody_id, wria_id, location_type_id,
           stream_channel_type_id, location_orientation_type_id,
           redd_name, location_description, latitude, longitude,
           horiz_accuracy, created_by)
  print(new_redd_loc_values)
  return(new_redd_loc_values)
})

# Update DB and reload DT
observeEvent(input$insert_redd_location, {
  redd_location_insert(redd_location_insert_vals())
  removeModal()
  post_redd_location_insert_vals = get_redd_location(pool, selected_redd_encounter_data()$redd_encounter_id) %>%
    select(redd_name, channel_type, orientation_type, latitude,
           longitude, horiz_accuracy, location_description,
           created_dt, created_by, modified_dt, modified_by)
  replaceData(redd_location_dt_proxy, post_redd_location_insert_vals)
})

# #========================================================
# # Edit operations: reactives, observers and modals
# #========================================================
#
# # Create reactive to collect input values for insert actions
# redd_encounter_edit = reactive({
#   # Survey date
#   survey_date = selected_survey_data()$survey_date
#   # Survey_event_id
#   survey_event_id_input = selected_survey_event_data()$survey_event_id
#   # Redd encounter time
#   redd_encounter_dt = format(input$redd_encounter_time_select)
#   if (nchar(redd_encounter_dt) < 16) { redd_encounter_dt = NA_character_ }
#   redd_encounter_dt = as.POSIXct(redd_encounter_dt)
#   # Redd status
#   redd_status_input = input$redd_status_select
#   if ( redd_status_input == "" ) {
#     redd_status_id = NA
#   } else {
#     redd_status_vals = get_redd_status(pool)
#     redd_status_id = redd_status_vals %>%
#       filter(redd_status == redd_status_input) %>%
#       pull(redd_status_id)
#   }
#   # Redd name, location_id
#   redd_name_input = input$redd_name_select
#   if ( redd_name_input == "" ) {
#     redd_location_id = NA
#   } else {
#     redd_name_vals = get_redd_name(pool, survey_event_id_input)
#     redd_location_id = redd_name_vals %>%
#       filter(redd_name == redd_name_input) %>%
#       pull(redd_location_id)
#   }
#   edit_redd_encounter = tibble(redd_encounter_id = selected_redd_encounter_data()$redd_encounter_id,
#                                # Need to create full datetime values below modal
#                                survey_date = survey_date,
#                                redd_encounter_dt = redd_encounter_dt,
#                                redd_status = redd_status_input,
#                                redd_status_id = redd_status_id,
#                                redd_count = input$redd_count_input,
#                                redd_name = redd_name_input,
#                                redd_location_id = redd_location_id,
#                                redd_comment = input$redd_comment_input,
#                                modified_dt = lubridate::with_tz(Sys.time(), "UTC"),
#                                modified_by = Sys.getenv("USERNAME"))
#   edit_redd_encounter = edit_redd_encounter %>%
#     mutate(redd_encounter_time = case_when(
#       is.na(redd_encounter_dt) ~ as.POSIXct(NA),
#       !is.na(redd_encounter_dt) ~ as.POSIXct(paste0(format(survey_date), " ", format(redd_encounter_dt, "%H:%M")),
#                                         tz = "America/Los_Angeles"))) %>%
#     mutate(redd_encounter_time = with_tz(redd_encounter_time, tzone = "UTC"))
#   return(edit_redd_encounter)
# })
#
# # Generate values to show in modal
# output$redd_encounter_modal_update_vals = renderDT({
#   redd_encounter_modal_up_vals = redd_encounter_edit() %>%
#     mutate(redd_encounter_dt = format(redd_encounter_dt, "%H:%M")) %>%
#     select(redd_encounter_dt, redd_status, redd_count,
#            redd_name, redd_comment)
#   # Generate table
#   datatable(redd_encounter_modal_up_vals,
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
# # output$chk_redd_edit = renderText({
# #   old_redd_encounter_vals = selected_redd_encounter_data() %>%
# #     mutate(redd_encounter_dt = format(redd_encounter_time, "%H:%M")) %>%
# #     select(redd_encounter_dt, redd_status, redd_count,
# #            redd_name, redd_comment)
# #   old_redd_encounter_vals[] = lapply(old_redd_encounter_vals, remisc::set_na)
# #   new_redd_encounter_vals = redd_encounter_edit() %>%
# #     mutate(redd_count = as.integer(redd_count)) %>%
# #     mutate(redd_encounter_dt = format(redd_encounter_dt, "%H:%M")) %>%
# #     select(redd_encounter_dt, redd_status, redd_count,
# #            redd_name, redd_comment)
# #   new_redd_encounter_vals[] = lapply(new_redd_encounter_vals, remisc::set_na)
# #   print(old_redd_encounter_vals)
# #   print(new_redd_encounter_vals)
# #   return(unlist(old_redd_encounter_vals))
# # })
#
# # Edit modal
# observeEvent(input$redd_enc_edit, {
#   old_redd_encounter_vals = selected_redd_encounter_data() %>%
#     mutate(redd_encounter_dt = format(redd_encounter_time, "%H:%M")) %>%
#     select(redd_encounter_dt, redd_status, redd_count,
#            redd_name, redd_comment)
#   old_redd_encounter_vals[] = lapply(old_redd_encounter_vals, remisc::set_na)
#   new_redd_encounter_vals = redd_encounter_edit() %>%
#     mutate(redd_count = as.integer(redd_count)) %>%
#     mutate(redd_encounter_dt = format(redd_encounter_dt, "%H:%M")) %>%
#     select(redd_encounter_dt, redd_status, redd_count,
#            redd_name, redd_comment)
#   new_redd_encounter_vals[] = lapply(new_redd_encounter_vals, remisc::set_na)
#   showModal(
#     tags$div(id = "redd_encounter_update_modal",
#              if ( !length(input$redd_encounters_rows_selected) == 1 ) {
#                modalDialog (
#                  size = "m",
#                  title = "Warning",
#                  paste("Please select a row to edit!"),
#                  easyClose = TRUE,
#                  footer = NULL
#                )
#              } else if ( isTRUE(all_equal(old_redd_encounter_vals, new_redd_encounter_vals)) ) {
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
#                  title = "Update redd data to these new values?",
#                  fluidPage (
#                    DT::DTOutput("redd_encounter_modal_update_vals"),
#                    br(),
#                    br(),
#                    actionButton("save_redd_enc_edits", "Save changes")
#                  ),
#                  easyClose = TRUE,
#                  footer = NULL
#                )
#              }
#     ))
# })
#
# # Update DB and reload DT
# observeEvent(input$save_redd_enc_edits, {
#   redd_encounter_update(redd_encounter_edit())
#   removeModal()
#   post_redd_encounter_edit_vals = get_redd_encounter(pool, selected_survey_event_data()$survey_event_id) %>%
#     select(redd_encounter_dt, redd_status, redd_count, redd_name, redd_comment,
#            created_dt, created_by, modified_dt, modified_by)
#   replaceData(redd_encounter_dt_proxy, post_redd_encounter_edit_vals)
# })
#
# #========================================================
# # Delete operations: reactives, observers and modals
# #========================================================
#
# # Generate values to show in modal
# output$redd_encounter_modal_delete_vals = renderDT({
#   redd_encounter_modal_del_id = selected_redd_encounter_data()$redd_encounter_id
#   redd_encounter_modal_del_vals = get_redd_encounter(pool, selected_survey_event_data()$survey_event_id) %>%
#     filter(redd_encounter_id == redd_encounter_modal_del_id) %>%
#     select(redd_encounter_dt, redd_status, redd_count, redd_name, redd_comment)
#   # Generate table
#   datatable(redd_encounter_modal_del_vals,
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
# observeEvent(input$redd_enc_delete, {
#   redd_encounter_id = selected_redd_encounter_data()$redd_encounter_id
#   redd_encounter_dependencies = get_redd_encounter_dependencies(redd_encounter_id)
#   table_names = paste0(names(redd_encounter_dependencies), collapse = ", ")
#   showModal(
#     tags$div(id = "redd_encounter_delete_modal",
#              if ( ncol(redd_encounter_dependencies) > 0L ) {
#                modalDialog (
#                  size = "m",
#                  title = "Warning",
#                  glue("Please delete associated redd data from the following tables first: {table_names}"),
#                  easyClose = TRUE,
#                  footer = NULL
#                )
#              } else {
#                modalDialog (
#                  size = 'l',
#                  title = "Are you sure you want to delete this redd data from the database?",
#                  fluidPage (
#                    DT::DTOutput("redd_encounter_modal_delete_vals"),
#                    br(),
#                    br(),
#                    actionButton("delete_redd_encounter", "Delete redd data")
#                  ),
#                  easyClose = TRUE,
#                  footer = NULL
#                )
#              }
#     ))
# })
#
# # Update DB and reload DT
# observeEvent(input$delete_redd_encounter, {
#   redd_encounter_delete(selected_redd_encounter_data())
#   removeModal()
#   redd_encounters_after_delete = get_redd_encounter(pool, selected_survey_event_data()$survey_event_id) %>%
#     select(redd_encounter_dt, redd_status, redd_count, redd_name, redd_comment,
#            created_dt, created_by, modified_dt, modified_by)
#   replaceData(redd_encounter_dt_proxy, redd_encounters_after_delete)
# })
