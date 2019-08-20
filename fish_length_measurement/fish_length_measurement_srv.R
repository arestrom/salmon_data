#========================================================
# Generate lut select ui's
#========================================================

output$length_type_select = renderUI({
  length_type_list = get_length_type(pool)$length_type
  length_type_list = c("", length_type_list)
  selectizeInput("length_type_select", label = "length_type",
                 choices = length_type_list, selected = "Fork length",
                 width = "250px")
})

#========================================================
# Primary datatable for length measurements
#========================================================

# Primary DT datatable for survey_intent
output$length_measurements = renderDT({
  length_measurements_title = glue("{selected_survey_event_data()$species} data for {input$stream_select} on ",
                               "{selected_survey_data()$survey_date} from river mile {selected_survey_data()$up_rm} ",
                               "to {selected_survey_data()$lo_rm}")
  length_measurement_data = get_length_measurements(pool, selected_individual_fish_data()$individual_fish_id) %>%
    select(length_type, length_cm, created_dt, created_by, modified_dt, modified_by)

  # Generate table
  datatable(length_measurement_data,
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
              htmltools::em(htmltools::strong(length_measurements_title))))
})

# Create surveys DT proxy object
length_measurement_dt_proxy = dataTableProxy(outputId = "length_measurements")

#========================================================
# Collect measurement values from selected row for later use
#========================================================

# Create reactive to collect input values for update and delete actions
selected_length_measurement_data = reactive({
  req(input$length_measurements_rows_selected)
  lengh_measurement_data = get_length_measurements(pool, selected_individual_fish_data()$individual_fish_id)
  length_measurement_row = input$length_measurements_rows_selected
  selected_length_measurement = tibble(fish_length_mesaurement_id = length_measurement_data$individual_fish_id[length_measurement_row],
                                       length_type = length_measurement_data$length_type[length_measurement_row],
                                       length_cm = length_measurement_data$length_cm[length_measurement_row],
                                       created_date = length_measurement_data$created_date[length_measurement_row],
                                       created_by = length_measurement_data$created_by[length_measurement_row],
                                       modified_date = length_measurement_data$modified_date[length_measurement_row],
                                       modified_by = length_measurement_data$modified_by[length_measurement_row])
  return(selected_length_measurement)
})

#========================================================
# Update event select inputs to values in selected row
#========================================================

# Update all survey input values to values in selected row
observeEvent(input$length_measurements_rows_selected, {
  slmdat = selected_length_measurement_data()
  updateSelectizeInput(session, "length_type_select", selected = sindat$fish_condition)
  updateNumericInput(session, "length_cm_input", value = sindat$pct_eggs)
})

#========================================================
# Insert operations: reactives, observers and modals
#========================================================

# Create reactive to collect input values for insert actions
length_measurement_create = reactive({
  # individual_fish_id
  individual_fish_id_input = selected_individual_fish_data()$individual_fish_id
  # length_type
  length_type_input = input$length_type_select
  if ( length_type_input == "" ) {
    fish_length_measurement_type_id = NA
  } else {
    fish_length_vals = get_length_type(pool)
    fish_length_measurement_type_id = fish_length_vals %>%
      filter(length_type == length_type_input) %>%
      pull(fish_length_measurement_type_id)
  }
  new_length_measurement = tibble(individual_fish_id = individual_fish_id_input,
                                  length_type = length_type_input,
                                  fish_length_measurement_type_id = fish_length_measurement_type_id,
                                  length_cm = input$length_cm_input,
                                  created_dt = lubridate::with_tz(Sys.time(), "UTC"),
                                  created_by = Sys.getenv("USERNAME"))
  return(new_length_measurement)
})

# Generate values to show in modal
output$lengh_measurement_modal_insert_vals = renderDT({
  length_measurement_modal_in_vals = length_measurement_create() %>%
    select(length_type, length_cm)
  # Generate table
  datatable(length_measurement_modal_in_vals,
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
observeEvent(input$fish_meas_add, {
  new_length_measurement_vals = length_measurement_create()
  showModal(
    # Verify required fields have data...none can be blank
    tags$div(id = "length_measurement_insert_modal",
             if ( is.na(new_length_measurement_vals$fish_length_measurement_type_id) |
                  is.na(new_length_measurement_vals$length_cm) ) {
               modalDialog (
                 size = "m",
                 title = "Warning",
                 paste0("No fields can be blank, please add both length_type and length_cm"),
                 easyClose = TRUE,
                 footer = NULL
               )
               # Write to DB
             } else if ( dup_length_flag == TRUE ) {
               modalDialog (
                 size = "m",
                 title = "Warning",
                 paste0("Length type already exists. Please edit the length_type before proceeding."),
                 easyClose = TRUE,
                 footer = NULL
               )
               # Write to DB
             } else {
               modalDialog (
                 size = 'l',
                 title = glue("Insert new length data to the database?"),
                 fluidPage (
                   DT::DTOutput("lengh_measurement_modal_insert_vals"),
                   br(),
                   br(),
                   actionButton("insert_length_measurements", "Insert length data")
                 ),
                 easyClose = TRUE,
                 footer = NULL
               )
             }
    ))
})


# STOPPED HERE !!!!!!!!!!!!!!!!!!!!!!!!!!   Need dup_length_flag in global



# Reactive to hold values actually inserted
individual_fish_insert_vals = reactive({
  new_ind_fish_values = individual_fish_create() %>%
    select(fish_encounter_id, fish_condition_type_id, fish_trauma_type_id, gill_condition_type_id,
           spawn_condition_type_id, cwt_result_type_id, age_code_id, pct_eggs, eggs_gram, eggs_number,
           fish_sample_num, scale_card_num, scale_position_num, snout_sample_num, cwt_tag_code,
           genetic_sample_num, otolith_sample_num, fish_comment, created_by)
  return(new_ind_fish_values)
})

# # Update DB and reload DT
# observeEvent(input$insert_individual_fish, {
#   individual_fish_insert(individual_fish_insert_vals())
#   removeModal()
#   post_individual_fish_insert_vals = get_individual_fish(pool, selected_fish_encounter_data()$fish_encounter_id) %>%
#     select(fish_condition, fish_trauma, gill_condition, spawn_condition, fish_sample_num, scale_card_num,
#            scale_position_num, age_code, snout_sample_num, cwt_tag_code, cwt_result, genetic_sample_num,
#            otolith_sample_num, pct_eggs, eggs_gram, eggs_number, fish_comment, created_dt, created_by,
#            modified_dt, modified_by)
#   replaceData(individual_fish_dt_proxy, post_individual_fish_insert_vals)
# })
#
# #========================================================
# # Edit operations: reactives, observers and modals
# #========================================================
#
# # Create reactive to collect input values for insert actions
# individual_fish_edit = reactive({
#   # Fish condition
#   fish_condition_input = input$fish_condition_select
#   if ( fish_condition_input == "" ) {
#     fish_condition_type_id = NA
#   } else {
#     fish_condition_vals = get_fish_condition(pool)
#     fish_condition_type_id = fish_condition_vals %>%
#       filter(fish_condition == fish_condition_input) %>%
#       pull(fish_condition_type_id)
#   }
#   # Fish trauma
#   fish_trauma_input = input$fish_trauma_select
#   if ( fish_trauma_input == "" ) {
#     fish_trauma_type_id = NA
#   } else {
#     fish_trauma_vals = get_fish_trauma(pool)
#     fish_trauma_type_id = fish_trauma_vals %>%
#       filter(fish_trauma == fish_trauma_input) %>%
#       pull(fish_trauma_type_id)
#   }
#   # Gill condition
#   gill_condition_input = input$gill_condition_select
#   if ( gill_condition_input == "" ) {
#     gill_condition_type_id = NA
#   } else {
#     gill_condition_vals = get_gill_condition(pool)
#     gill_condition_type_id = gill_condition_vals %>%
#       filter(gill_condition == gill_condition_input) %>%
#       pull(gill_condition_type_id)
#   }
#   # Spawn condition
#   spawn_condition_input = input$spawn_condition_select
#   if ( spawn_condition_input == "" ) {
#     spawn_condition_type_id = NA
#   } else {
#     spawn_condition_vals = get_spawn_condition(pool)
#     spawn_condition_type_id = spawn_condition_vals %>%
#       filter(spawn_condition == spawn_condition_input) %>%
#       pull(spawn_condition_type_id)
#   }
#   # Age code
#   age_code_input = input$age_code_select
#   if ( age_code_input == "" ) {
#     age_code_id = NA
#   } else {
#     age_code_vals = get_age_code(pool)
#     age_code_id = age_code_vals %>%
#       filter(age_code == age_code_input) %>%
#       pull(age_code_id)
#   }
#   # CWT result
#   cwt_result_input = input$cwt_result_select
#   if ( cwt_result_input == "" ) {
#     cwt_result_type_id = NA
#   } else {
#     cwt_result_vals = get_cwt_result(pool)
#     cwt_result_type_id = cwt_result_vals %>%
#       filter(cwt_result == cwt_result_input) %>%
#       pull(cwt_result_type_id)
#   }
#   edit_individual_fish = tibble(individual_fish_id = selected_individual_fish_data()$individual_fish_id,
#                                 fish_condition = fish_condition_input,
#                                 fish_condition_type_id = fish_condition_type_id,
#                                 fish_trauma = fish_trauma_input,
#                                 fish_trauma_type_id = fish_trauma_type_id,
#                                 gill_condition = gill_condition_input,
#                                 gill_condition_type_id = gill_condition_type_id,
#                                 spawn_condition = spawn_condition_input,
#                                 spawn_condition_type_id = spawn_condition_type_id,
#                                 fish_sample_num = input$fish_sample_num_input,
#                                 scale_card_num = input$scale_card_num_input,
#                                 scale_position_num = input$scale_position_num_input,
#                                 age_code = age_code_input,
#                                 age_code_id = age_code_id,
#                                 snout_sample_num = input$snout_sample_num_input,
#                                 cwt_tag_code = input$cwt_tag_code_input,
#                                 cwt_result = cwt_result_input,
#                                 cwt_result_type_id = cwt_result_type_id,
#                                 genetic_sample_num = input$genetic_sample_num_input,
#                                 otolith_sample_num = input$otolith_sample_num_input,
#                                 pct_eggs = input$pct_eggs_input,
#                                 eggs_gram = input$eggs_gram_input,
#                                 eggs_number = input$eggs_number_input,
#                                 fish_comment = input$ind_fish_comment_input,
#                                 created_dt = lubridate::with_tz(Sys.time(), "UTC"),
#                                 created_by = Sys.getenv("USERNAME"))
#   edit_individual_fish = edit_individual_fish %>%
#     mutate(fish_sample_num = if_else(is.na(fish_sample_num) | fish_sample_num == "", NA_character_, fish_sample_num)) %>%
#     mutate(scale_card_num = if_else(is.na(scale_card_num) | scale_card_num == "", NA_character_, scale_card_num)) %>%
#     mutate(scale_position_num = if_else(is.na(scale_position_num) | scale_position_num == "", NA_character_, scale_position_num)) %>%
#     mutate(snout_sample_num = if_else(is.na(snout_sample_num) | snout_sample_num == "", NA_character_, snout_sample_num)) %>%
#     mutate(cwt_tag_code = if_else(is.na(cwt_tag_code) | cwt_tag_code == "", NA_character_, cwt_tag_code)) %>%
#     mutate(genetic_sample_num = if_else(is.na(genetic_sample_num) | genetic_sample_num == "", NA_character_, genetic_sample_num)) %>%
#     mutate(otolith_sample_num = if_else(is.na(otolith_sample_num) | otolith_sample_num == "", NA_character_, otolith_sample_num)) %>%
#     mutate(fish_comment = if_else(is.na(fish_comment) | fish_comment == "", NA_character_, fish_comment))
#   return(edit_individual_fish)
# })
#
# # Generate values to show in modal
# output$individual_fish_modal_update_vals = renderDT({
#   individual_fish_modal_up_vals = individual_fish_edit() %>%
#     select(fish_condition, fish_trauma, gill_condition, spawn_condition, fish_sample_num, scale_card_num,
#            scale_position_num, age_code, snout_sample_num, cwt_tag_code, cwt_result, genetic_sample_num,
#            otolith_sample_num, pct_eggs, eggs_gram, eggs_number, fish_comment)
#   # Generate table
#   datatable(individual_fish_modal_up_vals,
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
# #   old_individual_fish_vals = selected_individual_fish_data() %>%
# #     select(fish_condition, fish_trauma, gill_condition, spawn_condition, fish_sample_num, scale_card_num,
# #            scale_position_num, age_code, snout_sample_num, cwt_tag_code, cwt_result, genetic_sample_num,
# #            otolith_sample_num, pct_eggs, eggs_gram, eggs_number, fish_comment)
# #   old_individual_fish_vals[] = lapply(old_individual_fish_vals, remisc::set_empty)
# #   old_individual_fish_vals = old_individual_fish_vals %>%
# #     mutate(pct_eggs = as.integer(pct_eggs)) %>%
# #     mutate(eggs_gram = as.numeric(eggs_gram)) %>%
# #     mutate(eggs_number = as.integer(eggs_number))
# #   new_individual_fish_vals = individual_fish_edit() %>%
# #     select(fish_condition, fish_trauma, gill_condition, spawn_condition, fish_sample_num, scale_card_num,
# #            scale_position_num, age_code, snout_sample_num, cwt_tag_code, cwt_result, genetic_sample_num,
# #            otolith_sample_num, pct_eggs, eggs_gram, eggs_number, fish_comment)
# #   new_individual_fish_vals[] = lapply(new_individual_fish_vals, remisc::set_empty)
# #   new_individual_fish_vals = new_individual_fish_vals %>%
# #     mutate(pct_eggs = as.integer(pct_eggs)) %>%
# #     mutate(eggs_gram = as.numeric(eggs_gram)) %>%
# #     mutate(eggs_number = as.integer(eggs_number))
# #   print(old_individual_fish_vals)
# #   print(new_individual_fish_vals)
# #   return(unlist(old_individual_fish_vals))
# # })
#
# observeEvent(input$ind_fish_edit, {
#   old_individual_fish_vals = selected_individual_fish_data() %>%
#     select(fish_condition, fish_trauma, gill_condition, spawn_condition, fish_sample_num, scale_card_num,
#            scale_position_num, age_code, snout_sample_num, cwt_tag_code, cwt_result, genetic_sample_num,
#            otolith_sample_num, pct_eggs, eggs_gram, eggs_number, fish_comment)
#   old_individual_fish_vals[] = lapply(old_individual_fish_vals, remisc::set_empty)
#   old_individual_fish_vals = old_individual_fish_vals %>%
#     mutate(pct_eggs = as.integer(pct_eggs)) %>%
#     mutate(eggs_gram = as.numeric(eggs_gram)) %>%
#     mutate(eggs_number = as.integer(eggs_number))
#   new_individual_fish_vals = individual_fish_edit() %>%
#     select(fish_condition, fish_trauma, gill_condition, spawn_condition, fish_sample_num, scale_card_num,
#            scale_position_num, age_code, snout_sample_num, cwt_tag_code, cwt_result, genetic_sample_num,
#            otolith_sample_num, pct_eggs, eggs_gram, eggs_number, fish_comment)
#   new_individual_fish_vals[] = lapply(new_individual_fish_vals, remisc::set_empty)
#   new_individual_fish_vals = new_individual_fish_vals %>%
#     mutate(pct_eggs = as.integer(pct_eggs)) %>%
#     mutate(eggs_gram = as.numeric(eggs_gram)) %>%
#     mutate(eggs_number = as.integer(eggs_number))
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
#              } else if ( isTRUE(all_equal(old_individual_fish_vals, new_individual_fish_vals)) ) {
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
#                  title = "Update individual fish data to these new values?",
#                  fluidPage (
#                    DT::DTOutput("individual_fish_modal_update_vals"),
#                    br(),
#                    br(),
#                    actionButton("save_ind_fish_edits","Save changes")
#                  ),
#                  easyClose = TRUE,
#                  footer = NULL
#                )
#              }
#     ))
# })
#
# # Update DB and reload DT
# observeEvent(input$save_ind_fish_edits, {
#   individual_fish_update(individual_fish_edit())
#   removeModal()
#   post_individual_fish_edit_vals = get_individual_fish(pool, selected_fish_encounter_data()$fish_encounter_id) %>%
#     select(fish_condition, fish_trauma, gill_condition, spawn_condition, fish_sample_num, scale_card_num,
#            scale_position_num, age_code, snout_sample_num, cwt_tag_code, cwt_result, genetic_sample_num,
#            otolith_sample_num, pct_eggs, eggs_gram, eggs_number, fish_comment, created_dt, created_by,
#            modified_dt, modified_by)
#   replaceData(individual_fish_dt_proxy, post_individual_fish_edit_vals)
# })
#
# #========================================================
# # Delete operations: reactives, observers and modals
# #========================================================
#
# # Generate values to show in modal
# output$individual_fish_modal_delete_vals = renderDT({
#   individual_fish_modal_del_id = selected_individual_fish_data()$individual_fish_id
#   individual_fish_modal_del_vals = get_individual_fish(pool, selected_fish_encounter_data()$fish_encounter_id) %>%
#     filter(individual_fish_id == individual_fish_modal_del_id) %>%
#     select(fish_condition, fish_trauma, gill_condition, spawn_condition, fish_sample_num, scale_card_num,
#            scale_position_num, age_code, snout_sample_num, cwt_tag_code, cwt_result, genetic_sample_num,
#            otolith_sample_num, pct_eggs, eggs_gram, eggs_number, fish_comment)
#   # Generate table
#   datatable(individual_fish_modal_del_vals,
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
# observeEvent(input$ind_fish_delete, {
#   individual_fish_id = selected_individual_fish_data()$individual_fish_id
#   individual_fish_dependencies = get_individual_fish_dependencies(individual_fish_id)
#   table_names = paste0(names(individual_fish_dependencies), collapse = ", ")
#   showModal(
#     tags$div(id = "individual_fish_delete_modal",
#              if ( ncol(individual_fish_dependencies) > 0L ) {
#                modalDialog (
#                  size = "m",
#                  title = "Warning",
#                  glue("Please delete associated individual fish data from the following tables first: {table_names}"),
#                  easyClose = TRUE,
#                  footer = NULL
#                )
#              } else {
#                modalDialog (
#                  size = 'l',
#                  title = "Are you sure you want to delete this set of individual fish data from the database?",
#                  fluidPage (
#                    DT::DTOutput("individual_fish_modal_delete_vals"),
#                    br(),
#                    br(),
#                    actionButton("delete_individual_fish", "Delete data")
#                  ),
#                  easyClose = TRUE,
#                  footer = NULL
#                )
#              }
#     ))
# })
#
# # Update DB and reload DT
# observeEvent(input$delete_individual_fish, {
#   individual_fish_delete(selected_individual_fish_data())
#   removeModal()
#   individual_fish_after_delete = get_individual_fish(pool, selected_fish_encounter_data()$fish_encounter_id) %>%
#     select(fish_condition, fish_trauma, gill_condition, spawn_condition, fish_sample_num, scale_card_num,
#            scale_position_num, age_code, snout_sample_num, cwt_tag_code, cwt_result, genetic_sample_num,
#            otolith_sample_num, pct_eggs, eggs_gram, eggs_number, fish_comment, created_dt, created_by,
#            modified_dt, modified_by)
#   replaceData(individual_fish_dt_proxy, individual_fish_after_delete)
# })
