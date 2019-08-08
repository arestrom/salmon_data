#========================================================
# Generate lut select ui's
#========================================================

output$fish_status_select = renderUI({
  fish_status_list = get_fish_status(pool)$fish_status
  fish_status_list = c("", fish_status_list)
  selectizeInput("fish_status_select", label = "fish_status",
                 choices = fish_status_list, selected = NULL,
                 width = "75px")
})

output$sex_select = renderUI({
  sex_list = get_sex(pool)$sex
  sex_list = c("", sex_list)
  selectizeInput("sex_select", label = "fish_sex",
                 choices = sex_list, selected = NULL,
                 width = "115px")
})

output$maturity_select = renderUI({
  maturity_list = get_maturity(pool)$maturity
  maturity_list = c("", maturity_list)
  selectizeInput("maturity_select", label = "maturity",
                 choices = maturity_list, selected = NULL,
                 width = "115px")
})

output$origin_select = renderUI({
  origin_list = get_origin(pool)$origin
  origin_list = c("", origin_list)
  selectizeInput("origin_select", label = "origin",
                 choices = origin_list, selected = NULL,
                 width = "100px")
})

output$cwt_status_select = renderUI({
  cwt_status_list = get_cwt_status(pool)$cwt_status
  cwt_status_list = c("", cwt_status_list)
  selectizeInput("cwt_status_select", label = "cwt_status",
                 choices = cwt_status_list, selected = NULL,
                 width = "210px")
})

output$clip_status_select = renderUI({
  clip_status_list = get_clip_status(pool)$clip_status
  clip_status_list = c("", clip_status_list)
  selectizeInput("clip_status_select", label = "clip_status",
                 choices = clip_status_list, selected = NULL,
                 width = "210px")
})

output$fish_behavior_select = renderUI({
  fish_behavior_list = get_fish_behavior(pool)$fish_behavior
  fish_behavior_list = c("", fish_behavior_list)
  selectizeInput("fish_behavior_select", label = "fish_behavior",
                 choices = fish_behavior_list, selected = NULL,
                 width = "115px")
})

#========================================================
# Primary datatable for fish_encounters
#========================================================

# Primary DT datatable for survey_intent
output$fish_encounters = renderDT({
  fish_encounter_title = glue("{selected_survey_event_data()$species} data for {input$stream_select} on ",
                              "{selected_survey_data()$survey_date} from river mile {selected_survey_data()$up_rm} ",
                              "to {selected_survey_data()$lo_rm}")
  fish_encounter_data = get_fish_encounter(pool, selected_survey_event_data()$survey_event_id) %>%
    select(fish_encounter_time, fish_count, fish_status, sex, maturity, origin, cwt_status,
           clip_status, fish_behavior, prev_counted, created_dt, created_by, modified_dt,
           modified_by)

  # Generate table
  datatable(fish_encounter_data,
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
              htmltools::em(htmltools::strong(fish_encounter_title))))
})

# Create surveys DT proxy object
fish_encounter_dt_proxy = dataTableProxy(outputId = "fish_encounters")

#========================================================
# Collect encounter values from selected row for later use
#========================================================

# Create reactive to collect input values for update and delete actions
selected_fish_encounter_data = reactive({
  req(input$fish_encounters_rows_selected)
  fish_encounter_data = get_fish_encounter(pool, selected_survey_event_data()$survey_event_id)
  fish_encounter_row = input$fish_encounters_rows_selected
  selected_fish_encounter = tibble(fish_encounter_id = fish_encounter_data$fish_encounter_id[fish_encounter_row],
                                   fish_encounter_time = fish_encounter_data$fish_encounter_time[fish_encounter_row],
                                   fish_count = fish_encounter_data$fish_count[fish_encounter_row],
                                   fish_status = fish_encounter_data$fish_status[fish_encounter_row],
                                   sex = fish_encounter_data$sex[fish_encounter_row],
                                   maturity = fish_encounter_data$maturity[fish_encounter_row],
                                   origin = fish_encounter_data$origin[fish_encounter_row],
                                   cwt_status = fish_encounter_data$cwt_status[fish_encounter_row],
                                   clip_status = fish_encounter_data$clip_status[fish_encounter_row],
                                   fish_behavior = fish_encounter_data$fish_behavior[fish_encounter_row],
                                   prev_counted = fish_encounter_data$prev_counted[fish_encounter_row],
                                   created_date = fish_encounter_data$created_date[fish_encounter_row],
                                   created_by = fish_encounter_data$created_by[fish_encounter_row],
                                   modified_date = fish_encounter_data$modified_date[fish_encounter_row],
                                   modified_by = fish_encounter_data$modified_by[fish_encounter_row])
  return(selected_fish_encounter)
})

#========================================================
# Update event select inputs to values in selected row
#========================================================

# Update all survey input values to values in selected row
observeEvent(input$fish_encounters_rows_selected, {
  sfedat = selected_fish_encounter_data()
  updateTimeInput(session, "fish_encounter_time_select", value = sfedat$fish_encounter_time)
  updateNumericInput(session, "fish_count_input", value = sfedat$fish_count)
  updateSelectizeInput(session, "fish_status_select", selected = sfedat$fish_status)
  updateSelectizeInput(session, "sex_select", selected = sfedat$sex)
  updateSelectizeInput(session, "maturity_select", selected = sfedat$maturity)
  updateSelectizeInput(session, "origin_select", selected = sfedat$origin)
  updateSelectizeInput(session, "cwt_status_select", selected = sfedat$cwt_status)
  updateSelectizeInput(session, "clip_status_select", selected = sfedat$clip_status)
  updateSelectizeInput(session, "fish_behavior_select", selected = sfedat$fish_behavior)
  updateSelectizeInput(session, "prev_counted_select", selected = sfedat$prev_counted)
})

#========================================================
# Insert operations: reactives, observers and modals
#========================================================

# Create reactive to collect input values for insert actions
survey_event_create = reactive({
  # Survey_id
  survey_id_input = selected_survey_data()$survey_id
  # Species
  event_species_input = input$event_species_select
  if (event_species_input == "" ) {
    species_id = NA_character_
  } else {
    event_species_vals = get_event_species(pool)
    species_id = event_species_vals %>%
      filter(species == event_species_input) %>%
      pull(species_id)
  }
  # Survey design
  survey_design_input = input$survey_design_select
  if ( survey_design_input == "" ) {
    survey_design_type_id = NA
  } else {
    survey_design_vals = get_survey_design(pool)
    survey_design_type_id = survey_design_vals %>%
      filter(survey_design == survey_design_input) %>%
      pull(survey_design_type_id)
  }
  # CWT detect method
  cwt_detect_method_input = input$cwt_detect_method_select
  if ( cwt_detect_method_input == "" ) {
    cwt_detection_method_id = NA
  } else {
    cwt_detect_method_vals = get_cwt_detect_method(pool)
    cwt_detection_method_id = cwt_detect_method_vals %>%
      filter(cwt_detect_method == cwt_detect_method_input) %>%
      pull(cwt_detection_method_id)
  }
  # Run
  run_input = input$run_select
  if ( run_input == "" ) {
    run_id = NA
  } else {
    run_vals = get_run(pool)
    run_id = run_vals %>%
      filter(run == run_input) %>%
      pull(run_id)
  }
  new_survey_event = tibble(survey_id = survey_id_input,
                            species = event_species_input,
                            species_id = species_id,
                            survey_design = survey_design_input,
                            survey_design_type_id = survey_design_type_id,
                            cwt_detect_method = cwt_detect_method_input,
                            cwt_detection_method_id = cwt_detection_method_id,
                            run = run_input,
                            run_id = run_id,
                            run_year = input$run_year_select,
                            pct_fish_seen = input$pct_fish_seen_input,
                            species_comment = input$se_comment_input,
                            created_dt = lubridate::with_tz(Sys.time(), "UTC"),
                            created_by = Sys.getenv("USERNAME"))
  new_survey_event = new_survey_event %>%
    mutate(run_year = if_else(is.na(run_year) | run_year == "", NA_character_, run_year)) %>%
    mutate(run_year = as.integer(run_year)) %>%
    mutate(pct_fish_seen = as.integer(pct_fish_seen)) %>%
    mutate(species_comment = if_else(is.na(species_comment) | species_comment == "", NA_character_, species_comment))
  return(new_survey_event)
})

# Generate values to show in modal
output$survey_event_modal_insert_vals = renderDT({
  survey_event_modal_in_vals = survey_event_create() %>%
    select(species, survey_design, cwt_detect_method, run, run_year,
           pct_fish_seen, species_comment)
  # Generate table
  datatable(survey_event_modal_in_vals,
            rownames = FALSE,
            options = list(dom = 't',
                           scrollX = T,
                           ordering = FALSE,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
                             "}")))
})

# Modal for new intents. Need a dup flag, multiple rows possible
observeEvent(input$survey_event_add, {
  new_survey_event_vals = survey_event_create()
  existing_survey_event_vals = get_survey_event(pool, selected_survey_data()$survey_id) %>%
    select(species, survey_design, run, run_year)
  dup_event_flag = dup_survey_event(new_survey_event_vals, existing_survey_event_vals)
  showModal(
    # Verify all fields have data...none can be blank
    tags$div(id = "survey_event_insert_modal",
             if ( is.na(new_survey_event_vals$species_id) |
                  is.na(new_survey_event_vals$survey_design_type_id) |
                  is.na(new_survey_event_vals$cwt_detection_method_id) |
                  is.na(new_survey_event_vals$run_id) |
                  is.na(new_survey_event_vals$run_year) ) {
               modalDialog (
                 size = "m",
                 title = "Warning",
                 paste0("Values are required in all fields from species, through run_year"),
                 easyClose = TRUE,
                 footer = NULL
               )
             } else if ( dup_event_flag == TRUE ) {
               modalDialog (
                 size = "m",
                 title = "Warning",
                 paste0("Species data already exists. Please edit either species, survey_design, run, or run_year before proceeding."),
                 easyClose = TRUE,
                 footer = NULL
               )
               # Write to DB
             } else {
               modalDialog (
                 size = 'l',
                 title = glue("Insert new species data to the database?"),
                 fluidPage (
                   DT::DTOutput("survey_event_modal_insert_vals"),
                   br(),
                   br(),
                   actionButton("insert_survey_event", "Insert species data")
                 ),
                 easyClose = TRUE,
                 footer = NULL
               )
             }
    ))
})

# Reactive to hold values actually inserted
survey_event_insert_vals = reactive({
  new_event_values = survey_event_create() %>%
    select(survey_id, species_id, survey_design_type_id,
           cwt_detection_method_id, run_id, run_year,
           pct_fish_seen, species_comment, created_by)
  return(new_event_values)
})

# Update DB and reload DT
observeEvent(input$insert_survey_event, {
  survey_event_insert(survey_event_insert_vals())
  removeModal()
  post_event_insert_vals = get_survey_event(pool, selected_survey_data()$survey_id) %>%
    select(species, survey_design, cwt_detect_method, run, run_year, pct_fish_seen,
           species_comment, created_dt, created_by, modified_dt, modified_by)
  replaceData(survey_event_dt_proxy, post_event_insert_vals)
})

# #========================================================
# # Edit operations: reactives, observers and modals
# #========================================================
#
# # Create reactive to collect input values for insert actions
# survey_event_edit = reactive({
#   # Species
#   event_species_input = input$event_species_select
#   if (event_species_input == "" ) {
#     species_id = NA_character_
#   } else {
#     event_species_vals = get_event_species(pool)
#     species_id = event_species_vals %>%
#       filter(species == event_species_input) %>%
#       pull(species_id)
#   }
#   # Survey design
#   survey_design_input = input$survey_design_select
#   if ( survey_design_input == "" ) {
#     survey_design_type_id = NA
#   } else {
#     survey_design_vals = get_survey_design(pool)
#     survey_design_type_id = survey_design_vals %>%
#       filter(survey_design == survey_design_input) %>%
#       pull(survey_design_type_id)
#   }
#   # CWT detect method
#   cwt_detect_method_input = input$cwt_detect_method_select
#   if ( cwt_detect_method_input == "" ) {
#     cwt_detection_method_id = NA
#   } else {
#     cwt_detect_method_vals = get_cwt_detect_method(pool)
#     cwt_detection_method_id = cwt_detect_method_vals %>%
#       filter(cwt_detect_method == cwt_detect_method_input) %>%
#       pull(cwt_detection_method_id)
#   }
#   # Run
#   run_input = input$run_select
#   if ( run_input == "" ) {
#     run_id = NA
#   } else {
#     run_vals = get_run(pool)
#     run_id = run_vals %>%
#       filter(run == run_input) %>%
#       pull(run_id)
#   }
#   edit_survey_event = tibble(survey_event_id = selected_survey_event_data()$survey_event_id,
#                              species = event_species_input,
#                              species_id = species_id,
#                              survey_design = survey_design_input,
#                              survey_design_type_id = survey_design_type_id,
#                              cwt_detect_method = cwt_detect_method_input,
#                              cwt_detection_method_id = cwt_detection_method_id,
#                              run = run_input,
#                              run_id = run_id,
#                              run_year = input$run_year_select,
#                              pct_fish_seen = input$pct_fish_seen_input,
#                              species_comment = input$se_comment_input,
#                              modified_dt = lubridate::with_tz(Sys.time(), "UTC"),
#                              modified_by = Sys.getenv("USERNAME"))
#   edit_survey_event = edit_survey_event %>%
#     mutate(run_year = if_else(is.na(run_year) | run_year == "", NA_character_, run_year)) %>%
#     mutate(run_year = as.integer(run_year)) %>%
#     mutate(pct_fish_seen = as.integer(pct_fish_seen)) %>%
#     mutate(species_comment = if_else(is.na(species_comment) | species_comment == "", NA_character_, species_comment))
#   return(edit_survey_event)
# })
#
# # Generate values to show in modal
# output$survey_event_modal_update_vals = renderDT({
#   survey_event_modal_up_vals = survey_event_edit() %>%
#     select(species, survey_design, cwt_detect_method, run, run_year,
#            pct_fish_seen, species_comment)
#   # Generate table
#   datatable(survey_event_modal_up_vals,
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
# observeEvent(input$survey_event_edit, {
#   new_survey_event_vals = survey_event_edit() %>%
#     select(species, survey_design, cwt_detect_method, run, run_year,
#            pct_fish_seen, species_comment)
#   old_survey_event_vals = selected_survey_event_data() %>%
#     select(species, survey_design, cwt_detect_method, run, run_year,
#            pct_fish_seen, species_comment)
#   all_survey_event_vals = get_survey_event(pool, selected_survey_data()$survey_id) %>%
#     select(species, survey_design, cwt_detect_method, run, run_year,
#            pct_fish_seen, species_comment)
#   dup_event_flag = dup_survey_event(new_survey_event_vals, all_survey_event_vals)
#   showModal(
#     tags$div(id = "survey_event_update_modal",
#              if ( !length(input$survey_events_rows_selected) == 1 ) {
#                modalDialog (
#                  size = "m",
#                  title = "Warning",
#                  paste("Please select a row to edit!"),
#                  easyClose = TRUE,
#                  footer = NULL
#                )
#              } else if ( isTRUE(all_equal(old_survey_event_vals, new_survey_event_vals)) ) {
#                modalDialog (
#                  size = "m",
#                  title = "Warning",
#                  paste("Please change at least one value!"),
#                  easyClose = TRUE,
#                  footer = NULL
#                )
#              } else if ( dup_event_flag == TRUE ) {
#                modalDialog (
#                  size = "m",
#                  title = "Warning",
#                  paste0("Species data already exists. Please edit either species, survey_design, run, or run_year before proceeding." ),
#                  easyClose = TRUE,
#                  footer = NULL
#                )
#              } else {
#                modalDialog (
#                  size = 'l',
#                  title = "Update species data to these new values?",
#                  fluidPage (
#                    DT::DTOutput("survey_event_modal_update_vals"),
#                    br(),
#                    br(),
#                    actionButton("save_survey_event_edits","Save changes")
#                  ),
#                  easyClose = TRUE,
#                  footer = NULL
#                )
#              }
#     ))
# })
#
# # Update DB and reload DT
# observeEvent(input$save_survey_event_edits, {
#   survey_event_update(survey_event_edit())
#   removeModal()
#   post_survey_event_edit_vals = get_survey_event(pool, selected_survey_data()$survey_id) %>%
#     select(species, survey_design, cwt_detect_method, run, run_year, pct_fish_seen,
#            species_comment, created_dt, created_by, modified_dt, modified_by)
#   replaceData(survey_event_dt_proxy, post_survey_event_edit_vals)
# })
#
# #========================================================
# # Delete operations: reactives, observers and modals
# #========================================================
#
# # Generate values to show in modal
# output$survey_event_modal_delete_vals = renderDT({
#   survey_event_modal_del_id = selected_survey_event_data()$survey_event_id
#   survey_event_modal_del_vals = get_survey_event(pool, selected_survey_data()$survey_id) %>%
#     filter(survey_event_id == survey_event_modal_del_id) %>%
#     select(species, survey_design, cwt_detect_method, run, run_year,
#            pct_fish_seen, species_comment)
#   # Generate table
#   datatable(survey_event_modal_del_vals,
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
# observeEvent(input$survey_event_delete, {
#   survey_event_id = selected_survey_event_data()$survey_event_id
#   survey_event_dependencies = get_survey_event_dependencies(survey_event_id)
#   table_names = paste0(names(survey_event_dependencies), collapse = ", ")
#   showModal(
#     tags$div(id = "survey_event_delete_modal",
#              if ( ncol(survey_event_dependencies) > 0L ) {
#                modalDialog (
#                  size = "m",
#                  title = "Warning",
#                  glue("Please delete associated species data from the following tables first: {table_names}"),
#                  easyClose = TRUE,
#                  footer = NULL
#                )
#              } else {
#                modalDialog (
#                  size = 'l',
#                  title = "Are you sure you want to delete this species data from the database?",
#                  fluidPage (
#                    DT::DTOutput("survey_event_modal_delete_vals"),
#                    br(),
#                    br(),
#                    actionButton("delete_survey_event", "Delete species")
#                  ),
#                  easyClose = TRUE,
#                  footer = NULL
#                )
#              }
#     ))
# })
#
# # Update DB and reload DT
# observeEvent(input$delete_survey_event, {
#   survey_event_delete(selected_survey_event_data())
#   removeModal()
#   survey_events_after_delete = get_survey_event(pool, selected_survey_data()$survey_id) %>%
#     select(species, survey_design, cwt_detect_method, run, run_year, pct_fish_seen,
#            species_comment, created_dt, created_by, modified_dt, modified_by)
#   replaceData(survey_event_dt_proxy, survey_events_after_delete)
# })
