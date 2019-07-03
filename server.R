
# Create the Shiny server
server = function(input, output, session) {

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

  # Get streams in wria
  wria_streams = reactive({
    req(input$wria_select)
    get_streams(pool, chosen_wria = input$wria_select) %>%
      mutate(stream_label = if_else(is.na(stream_name) & !is.na(waterbody_name),
                                    waterbody_name, stream_name)) %>%
      mutate(stream_label = paste0(stream_name, ": ", llid)) %>%
      mutate(waterbody_id = tolower(waterbody_id)) %>%
      st_transform(4326) %>%
      select(waterbody_id, stream_label, geometry)
  })

  selected_wria = reactive({
    req(input$wria_select)
    zoom_wria = wria_polys %>%
      filter(wria_name == input$wria_select) %>%
      st_transform(2927) %>%
      mutate(zoom_pt = st_transform(st_centroid(geometry), 4326)) %>%
      mutate(lat = st_coordinates(zoom_pt)[[2]]) %>%
      mutate(lon = st_coordinates(zoom_pt)[[1]]) %>%
      st_drop_geometry() %>%
      select(wria_name, lat, lon)
    return(zoom_wria)
  })

  # Pull out stream_list from wria_streams
  stream_list = reactive({
    stream_data = wria_streams() %>%
      st_drop_geometry() %>%
      select(stream_label) %>%
      arrange(stream_label) %>%
      distinct()
    return(stream_data$stream_label)
  })

  # Output leaflet bidn map
  output$stream_map <- renderLeaflet({
    m = leaflet() %>%
      setView(lng = selected_wria()$lon[1],
              lat = selected_wria()$lat[1],
              zoom = 9) %>%
      addPolylines(data = wria_streams(),
                   group = "Streams",
                   weight = 3,
                   color = "#0000e6",
                   label = ~stream_label,
                   layerId = ~stream_label,
                   labelOptions = labelOptions(noHide = FALSE)) %>%
      addProviderTiles("Esri.WorldImagery", group = "Esri World Imagery") %>%
      addProviderTiles("OpenTopoMap", group = "Open Topo Map") %>%
      addLayersControl(position = 'bottomright',
                       baseGroups = c("Esri World Imagery", "Open Topo Map"),
                       overlayGroups = c("Streams"),
                       options = layersControlOptions(collapsed = TRUE))
    m
  })

  observeEvent(input$show_map_stream, {
    showModal(
      tags$div(id = "map_modal",
                 modalDialog (
                   size = 'l',
                   title = NULL,
                   fluidPage (
                     leafletOutput("stream_map", height = "700px")
                   ),
                   easyClose = TRUE,
                   footer = NULL
                 )
      )
    )
  })

  # selected stream reactive value
  selected_stream <- reactiveValues(map_stream = NULL)

  # Observer to record drop-down selection
  observeEvent(input$stream_select, {
    selected_stream$map_stream = input$stream_select
  })

  # Observer to record polylines map click
  observeEvent(input$stream_map_shape_click, {
    selected_stream$map_stream = input$stream_map_shape_click$id[[1]]
  })

  # Update Stream input...use of req here means select stays null
  observe({
    updated_stream = selected_stream$map_stream[[1]]
    # Update
    updateSelectizeInput(session, "stream_select",
                         choices = stream_list(),
                         selected = updated_stream)
  })

  # Filter to selected stream
  waterbody_id = reactive({
    req(input$stream_select)
    stream_data = wria_streams() %>%
      st_drop_geometry() %>%
      filter(stream_label == input$stream_select) %>%
      select(waterbody_id) %>%
      distinct() %>%
      pull(waterbody_id)
  })

  # Get list of river mile end_points for waterbody_id
  rm_list = reactive({
    wb_id = waterbody_id()
    stream_rms = get_end_points(pool, waterbody_id())
    return(stream_rms)
  })

  # Update upper_rm select...use of req here means select stays null
  observe({
    rm_list()
    updated_rm_list = rm_list()$rm_label
    updated_rm_list = c(updated_rm_list, "add")
    # Update upper rm
    updateSelectizeInput(session, "upper_rm_select",
                         choices = updated_rm_list,
                         selected = updated_rm_list[1])
    # Update lower rm
    updateSelectizeInput(session, "lower_rm_select",
                         choices = updated_rm_list,
                         selected = updated_rm_list[1])
  })

  # Get surveys for dt
  dt_surveys = reactive({
    req(input$year_select)
    year_select = as.integer(input$year_select)
    validate(
      need(min(year_select) >= 1930L & max(year_select) <= as.integer(format(Sys.Date(), "%Y")) + 1L,
             glue("Error: Please use the right sidebar to select a stream and a valid survey year ",
                  "(between 1930 and {as.integer(format(Sys.Date(), '%Y')) + 1L})"))
    )
    year_select = paste0(year_select, collapse = ", ")
    surveys = get_surveys(pool, waterbody_id(), survey_years = year_select) %>%
      mutate(survey_date = as.Date(survey_date)) %>%
      select(survey_id, survey_dt = survey_date, survey_method, up_rm = upper_rm,
             lo_rm = lower_rm, start_time, end_time, observer, submitter,
             data_source = data_source_code, data_review = data_review_status,
             completion = completion_status, created_dt = created_date,
             created_by, modified_dt = modified_date, modified_by)
  })

  # Primary DT datatable for database
  output$surveys = renderDT({
    survey_title = glue("Surveys for {input$stream_select}")
    surveys = dt_surveys() %>%
      mutate(survey_dt = format(survey_dt, "%m/%d/%Y")) %>%
      mutate(start_time = format(start_time, "%H:%M")) %>%
      mutate(end_time = format(end_time, "%H:%M")) %>%
      mutate(created_dt = format(created_dt, "%m/%d/%Y %H:%M")) %>%
      mutate(modified_dt = format(modified_dt, "%m/%d/%Y %H:%M")) %>%
      select(survey_dt, survey_method, up_rm, lo_rm, start_time, end_time,
             observer, submitter, data_source, data_review, completion,
             created_dt, created_by, modified_dt, modified_by)
    # Generate table
    datatable(surveys,
              selection = list(mode = 'single'),
              rownames = FALSE,
              extensions = 'Buttons',
              options = list(dom = 'Blftp',
                             pageLength = 5,
                             lengthMenu = c(5, 10, 20, 40, 60, 100, 500),
                             scrollX = T,
                             buttons = c('excel', 'print'),
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
                               "}")),
              caption = htmltools::tags$caption(
                style = 'caption-side: top; text-align: left; color: black; width: auto;',
                'Table 1: ', htmltools::em(htmltools::strong(survey_title))))
  })

  # Create beaches DT proxy object
  survey_dt_proxy = dataTableProxy("surveys")

  #========================================================
  # Update select inputs to values in selected row
  #========================================================

  # Update all input values to values in selected row
  observeEvent(input$surveys_rows_selected, {
    req(input$surveys_rows_selected)
    surveys = dt_surveys()
    survey_row = input$surveys_rows_selected
    survey_dt = surveys$survey_dt[survey_row]
    survey_method = surveys$survey_method[survey_row]
    up_rm = as.character(surveys$up_rm[survey_row])
    lo_rm = as.character(surveys$lo_rm[survey_row])
    start_time = surveys$start_time[survey_row]
    end_time = surveys$end_time[survey_row]
    observer = surveys$observer[survey_row]
    submitter = surveys$submitter[survey_row]
    data_source = surveys$data_source[survey_row]
    data_review = surveys$data_review[survey_row]
    completed = surveys$completion[survey_row]
    updateDateInput(session, "survey_date_input", value = survey_dt)
    updateSelectizeInput(session, "survey_method_select", selected = survey_method)
    updateSelectizeInput(session, "upper_rm_select", selected = up_rm)
    updateSelectizeInput(session, "lower_rm_select", selected = lo_rm)
    updateTimeInput(session, "start_time_select", value = start_time)
    updateTimeInput(session, "end_time_select", value = end_time)
    updateTextInput(session, "observer_input", value = observer)
    updateTextInput(session, "submitter_input", value = submitter)
    updateSelectizeInput(session, "data_source_select", selected = data_source)
    updateSelectizeInput(session, "data_review_select", selected = data_review)
    updateSelectizeInput(session, "completion_select", selected = completed)
  })

  #========================================================
  # Collect survey values from selected row for later use
  #========================================================

  # Create reactive to collect input values for update and delete actions
  survey_selected_data = reactive({
    surveys = dt_surveys()
    survey_row = input$surveys_rows_selected
    existing_surveys = tibble(survey_id = surveys$survey_id[survey_row],
                              survey_dt = surveys$survey_dt[survey_row],
                              survey_method = surveys$survey_method[survey_row],
                              up_rm = as.character(surveys$up_rm[survey_row]),
                              lo_rm = as.character(surveys$lo_rm[survey_row]),
                              start_time = surveys$start_time[survey_row],
                              end_time = surveys$end_time[survey_row],
                              observer = surveys$observer[survey_row],
                              submitter = surveys$submitter[survey_row],
                              data_source = surveys$data_source[survey_row],
                              data_review = surveys$data_review[survey_row],
                              completed = surveys$completion[survey_row])
    return(existing_surveys)
  })

  #========================================================
  # Insert operations: reactives, observers and modals
  #========================================================

  # # Create reactive to collect input values for insert actions
  # survey_create = reactive({
  #   new_survey = tibble(survey_dt = input$survey_date_input,
  #                       up_rm = input$upper_rm_select,
  #                       lo_rm = input$lower_rm_select,
  #                       start_time = as.POSIXct(input$start_time_select),
  #                       end_time = as.POSIXct(input$end_time_select),
  #                       observer = input$observer_input,
  #                       submitter = input$submitter_input,
  #                       data_source = input$data_source_select,
  #                       data_review = input$data_review_select,
  #                       completion = input$completion_select,
  #                       created_dt = lubridate::with_tz(Sys.time(), "UTC"),
  #                       created_by = Sys.getenv("USERNAME"))
  #   # print(new_survey$start_time)
  #   # print(new_survey$end_time)
  #   # print(class(new_survey$start_time))
  #   # print(class(new_survey$end_time))
  #   new_survey = new_survey %>%
  #     mutate(start_time = if_else(is.na(start_time), as.POSIXct(NA), start_time)) %>%
  #     mutate(end_time = if_else(is.na(end_time), as.POSIXct(NA), end_time)) %>%
  #     mutate(observer = if_else(is.na(observer) | observer == "", NA_character_, observer)) %>%
  #     mutate(submitter = if_else(is.na(submitter) | submitter == "", NA_character_, submitter))
  #   return(new_survey)
  # })

  # Create reactive to collect input values for insert actions
  survey_create = reactive({
    # Data source
    data_source_vals = get_data_source(pool)
    data_source_input = input$data_source_select
    data_source_id = data_source_vals %>%
      filter(data_source_code == data_source_input) %>%
      pull(data_source_id)
    # Survey method
    survey_method_vals = get_survey_method(pool)
    survey_method_input = input$survey_method_select
    survey_method_id = survey_method_vals %>%
      filter(survey_method == survey_method_input) %>%
      pull(survey_method_id)
    # Data review
    data_review_vals = get_data_review(pool)
    data_review_input = input$data_review_select
    data_review_status_id = data_review_vals %>%
      filter(data_review == data_review_input) %>%
      pull(data_review_status_id)
    # RM values
    rm_vals = rm_list()
    up_rm_input = input$upper_rm_select
    upper_end_point_id = rm_vals %>%
      filter(rm_label == up_rm_input) %>%
      pull(point_location_id)
    lo_rm_input = input$lower_rm_select
    lower_end_point_id = rm_vals %>%
      filter(rm_label == lo_rm_input) %>%
      pull(point_location_id)
    # Data source
    completion_vals = get_completion_status(pool)
    completion_input = input$completion_select
    survey_completion_status_id = completion_vals %>%
      filter(completion == completion_input) %>%
      pull(survey_completion_status_id)
    new_survey = tibble(survey_dt = input$survey_date_input,
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
                        created_dt = lubridate::with_tz(Sys.time(), "UTC"),
                        created_by = Sys.getenv("USERNAME"))
    new_survey = new_survey %>%
      mutate(start_time = if_else(is.na(start_time) | start_time == "", NA_character_, start_time)) %>%
      mutate(end_time = if_else(is.na(end_time) | end_time == "", NA_character_, end_time)) %>%
      mutate(observer = if_else(is.na(observer) | observer == "", NA_character_, observer)) %>%
      mutate(submitter = if_else(is.na(submitter) | submitter == "", NA_character_, submitter))
    print(new_survey)
    return(new_survey)
  })

  # Generate values to show in modal
  output$survey_modal_insert_vals = renderDT({
    survey_modal_in_vals = survey_create() %>%
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
    existing_survey_vals = dt_surveys() %>%
      mutate(up_rm = as.character(up_rm)) %>%
      mutate(lo_rm = as.character(lo_rm)) %>%
      select(survey_dt, survey_method, up_rm, lo_rm, observer, data_source)
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
                   size = "s",
                   title = "Warning",
                   paste0("All data fields are mandatory (except start and stop times)"),
                   easyClose = TRUE,
                   footer = NULL
                 )
                 # Verify survey is not already in database
               } else if ( dup_flag == TRUE ) {
                 modalDialog (
                   size = "s",
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

  # Update DB and reload DT
  observeEvent(input$insert_survey, {
    new_values = survey_create() %>%
      mutate(survey_start_datetime = case_when(
        substr(start_time, 12, 13) == "00" ~ as.POSIXct(NA),
        substr(start_time, 12, 13) != "00" ~ as.POSIXct(paste0(format(survey_dt), " ", start_time), tz = "America/Los_Angeles"))) %>%
      mutate(survey_start_datetime = with_tz(survey_start_datetime, tzone = "UTC")) %>%
      mutate(survey_end_datetime = case_when(
        substr(end_time, 12, 13) == "00" ~ as.POSIXct(NA),
        substr(end_time, 12, 13) != "00" ~ as.POSIXct(paste0(format(survey_dt), " ", end_time), tz = "America/Los_Angeles"))) %>%
      mutate(survey_end_datetime = with_tz(survey_end_datetime, tzone = "UTC")) %>%
      mutate(survey_dt = as.POSIXct(survey_dt)) %>%
      select(survey_datetime = survey_dt, data_source_id, survey_method_id, data_review_status_id,
             upper_end_point_id, lower_end_point_id, survey_completion_status_id, survey_start_datetime,
             survey_end_datetime, observer_last_name = observer, data_submitter_last_name = submitter,
             created_by)
    #print(new_values)
    survey_insert(new_values)
    removeModal()
    # STOPPED HERE.....the surveys stuff below does not work....need to copy beach_data app
    replaceData(survey_dt_proxy,
                surveys = dt_surveys() %>%
                  mutate(survey_dt = format(survey_dt, "%m/%d/%Y")) %>%
                  mutate(start_time = format(start_time, "%H:%M")) %>%
                  mutate(end_time = format(end_time, "%H:%M")) %>%
                  mutate(created_dt = format(created_dt, "%m/%d/%Y %H:%M")) %>%
                  mutate(modified_dt = format(modified_dt, "%m/%d/%Y %H:%M")) %>%
                  select(survey_dt, survey_method, up_rm, lo_rm, start_time, end_time,
                         observer, submitter, data_source, data_review, completion,
                         created_dt, created_by, modified_dt, modified_by))
  })

  # #========================================================
  # # Edit operations: reactives, observers and modals
  # #========================================================
  #
  # # Create reactive to collect input values for update, or delete actions
  # # See if I can get rid of this later....no need for req here now
  # beach_inputs = reactive({
  #   beaches = get_beaches()
  #   beach_row = input$beaches_rows_selected
  #   bch_sel = tibble(beach_id = beaches$beach_id[beach_row],
  #                    tide_station = input$station_input,
  #                    beach_name = input$beach_name_input,
  #                    beach_desc = input$beach_desc_input,
  #                    low_corr_min = input$low_min_input,
  #                    low_corr_ft = input$low_ft_input,
  #                    high_corr_min = input$high_min_input,
  #                    high_corr_ft = input$high_ft_input,
  #                    created_dt = beaches$created_dt[beach_row],
  #                    created_by = beaches$created_by[beach_row])
  #   bch_sel = bch_sel %>%
  #     mutate(beach_desc = if_else(is.na(beach_desc) | beach_desc == "", NA_character_, beach_desc)) %>%
  #     mutate(low_corr_min = as.integer(low_corr_min)) %>%
  #     mutate(low_corr_ft = as.numeric(low_corr_ft)) %>%
  #     mutate(high_corr_min = as.integer(high_corr_min)) %>%
  #     mutate(high_corr_ft = as.numeric(high_corr_ft))
  #   return(bch_sel)
  # })
  #
  # # Generate values to show in modal
  # output$modal_update_vals = renderDT({
  #   modal_up_vals = beach_inputs() %>%
  #     select(tide_station, beach_name, beach_desc, low_corr_min,
  #            low_corr_ft, high_corr_min, high_corr_ft)
  #   # Generate table
  #   datatable(modal_up_vals,
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
  # observeEvent(input$beaches_edit, {
  #   old_vals = beach_selected_data()
  #   new_vals = beach_inputs() %>%
  #     select(-c(created_dt, created_by))
  #   showModal(
  #     tags$div(id = "update_modal",
  #              if ( !length(input$beaches_rows_selected) == 1 ) {
  #                modalDialog (
  #                  size = "s",
  #                  title = "Warning",
  #                  paste("Please select a row to edit!"),
  #                  easyClose = TRUE,
  #                  footer = NULL
  #                )
  #              } else if ( isTRUE(all_equal(old_vals, new_vals)) ) {
  #                modalDialog (
  #                  size = "s",
  #                  title = "Warning",
  #                  paste("Please change at least one value!"),
  #                  easyClose = TRUE,
  #                  footer = NULL
  #                )
  #              } else {
  #                modalDialog (
  #                  size = 'l',
  #                  title = glue("Update {input$beach_name_input} tide data to these new values?"),
  #                  fluidPage (
  #                    DT::DTOutput("modal_update_vals"),
  #                    br(),
  #                    br(),
  #                    actionButton("save_edits","Save changes")
  #                  ),
  #                  easyClose = TRUE,
  #                  footer = NULL
  #                )
  #              }
  #     ))
  # })
  #
  # # Update DB and reload DT
  # observeEvent(input$save_edits, {
  #   beach_update(beach_inputs())
  #   removeModal()
  #   replaceData(dt_proxy, get_beaches()[,2:12])
  # })
  #
  # #========================================================
  # # Delete operations: reactives, observers and modals
  # #========================================================
  #
  # # Generate values to show in modal
  # output$modal_delete_vals = renderDT({
  #   modal_del_vals = beach_inputs() %>%
  #     select(tide_station, beach_name, beach_desc, low_corr_min,
  #            low_corr_ft, high_corr_min, high_corr_ft)
  #
  #   # Generate table
  #   datatable(modal_del_vals,
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
  # observeEvent(input$beaches_delete, {
  #   beach_id = beach_selected_data()$beach_id
  #   showModal(
  #     tags$div(id = "delete_modal",
  #              if ( length(beach_id) == 0 ) {
  #                modalDialog (
  #                  size = "s",
  #                  title = "Warning",
  #                  paste("Please select a row to delete!" ),
  #                  easyClose = TRUE,
  #                  footer = NULL
  #                )
  #              } else {
  #                modalDialog (
  #                  size = 'l',
  #                  title = glue("Are you sure you want to delete {input$beach_name_input} from the database?"),
  #                  fluidPage (
  #                    DT::DTOutput("modal_delete_vals"),
  #                    br(),
  #                    br(),
  #                    actionButton("delete_beach","Delete beach")
  #                  ),
  #                  easyClose = TRUE,
  #                  footer = NULL
  #                )
  #              }
  #     ))
  # })
  #
  # # Update DB and reload DT
  # observeEvent(input$delete_beach, {
  #   beach_delete(beach_selected_data())
  #   removeModal()
  #   replaceData(dt_proxy, get_beaches()[,2:12])
  # })

  # # close the R session when Chrome closes
  # session$onSessionEnded(function() {
  #   stopApp()
  #   q("no")
  # })

}
