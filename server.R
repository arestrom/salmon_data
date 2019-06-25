# Create the Shiny server
server = function(input, output, session) {

  # observe({
  #   if (input$left_sidebar %in% c("data_entry", "map_edit")) {
  #     shinyjs::addClass(selector = "aside.control-sidebar", class = "control-sidebar-open")
  #   } else {
  #     shinyjs::removeClass(selector = "aside.control-sidebar", class = "control-sidebar-open")
  #   }
  # })

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
    surveys = get_surveys(pool, waterbody_id(), year_select) %>%
      mutate(survey_date = as.Date(survey_date)) %>%
      select(survey_id, survey_dt = survey_date, up_rm = upper_rm,
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
      select(survey_dt, up_rm, lo_rm, start_time, end_time, observer,
             submitter, data_source, data_review, completion, created_dt,
             created_by, modified_dt, modified_by)
    # Generate table
    datatable(surveys,
              selection = list(mode = 'single'),
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
                style = 'caption-side: top; text-align: left; color: black;',
                'Table 1: ', htmltools::em(htmltools::strong(survey_title))))
  })

  # Create beaches DT proxy object
  dt_proxy = dataTableProxy("surveys")

  # #========================================================
  # # Update select inputs to values in selected row
  # #========================================================
  #
  # # Update all input values to values in selected row
  # observeEvent(input$beaches_rows_selected, {
  #   req(input$beaches_rows_selected)
  #   beaches = get_beaches()
  #   beach_row = input$beaches_rows_selected
  #   tide_station = beaches$tide_station[beach_row]
  #   beach_name = beaches$beach_name[beach_row]
  #   beach_desc = beaches$beach_desc[beach_row]
  #   low_corr_min = beaches$low_corr_min[beach_row]
  #   low_corr_ft = beaches$low_corr_ft[beach_row]
  #   high_corr_min = beaches$high_corr_min[beach_row]
  #   high_corr_ft = beaches$high_corr_ft[beach_row]
  #   updateSelectizeInput(session, "station_input", selected = tide_station)
  #   updateTextInput(session, "beach_name_input", value = beach_name)
  #   updateTextInput(session, "beach_desc_input", value = beach_desc)
  #   updateNumericInput(session, "low_min_input", value = low_corr_min)
  #   updateNumericInput(session, "low_ft_input", value = low_corr_ft)
  #   updateNumericInput(session, "high_min_input", value = high_corr_min)
  #   updateNumericInput(session, "high_ft_input", value = high_corr_ft)
  # })
  #
  # #========================================================
  # # Collect values from selected row for later use
  # #========================================================
  #
  # # Create reactive to collect input values for update and delete actions
  # beach_selected_data = reactive({
  #   beaches = get_beaches()
  #   beach_row = input$beaches_rows_selected
  #   old_data = tibble(beach_id = beaches$beach_id[beach_row],
  #                     tide_station = beaches$tide_station[beach_row],
  #                     beach_name = beaches$beach_name[beach_row],
  #                     beach_desc = beaches$beach_desc[beach_row],
  #                     low_corr_min = beaches$low_corr_min[beach_row],
  #                     low_corr_ft = beaches$low_corr_ft[beach_row],
  #                     high_corr_min = beaches$high_corr_min[beach_row],
  #                     high_corr_ft = beaches$high_corr_ft[beach_row])
  #   return(old_data)
  # })
  #
  # #========================================================
  # # Insert operations: reactives, observers and modals
  # #========================================================
  #
  # # Create reactive to collect input values for insert actions
  # beach_create = reactive({
  #   beaches = get_beaches()
  #   new_bch = tibble(tide_station = input$station_input,
  #                    beach_name = input$beach_name_input,
  #                    beach_desc = input$beach_desc_input,
  #                    low_corr_min = input$low_min_input,
  #                    low_corr_ft = input$low_ft_input,
  #                    high_corr_min = input$high_min_input,
  #                    high_corr_ft = input$high_ft_input,
  #                    created_dt = lubridate::with_tz(Sys.time(), "UTC"),
  #                    created_by = Sys.getenv("USERNAME"))
  #   new_bch = new_bch %>%
  #     mutate(tide_station = if_else(is.na(tide_station) | tide_station == "", NA_character_, tide_station)) %>%
  #     mutate(beach_name = if_else(is.na(beach_name) | beach_name == "", NA_character_, beach_name)) %>%
  #     mutate(beach_desc = if_else(is.na(beach_desc) | beach_desc == "", NA_character_, beach_desc)) %>%
  #     mutate(low_corr_min = as.integer(low_corr_min)) %>%
  #     mutate(low_corr_ft = as.numeric(low_corr_ft)) %>%
  #     mutate(high_corr_min = as.integer(high_corr_min)) %>%
  #     mutate(high_corr_ft = as.numeric(high_corr_ft))
  #   return(new_bch)
  # })
  #
  # # Generate values to show in modal
  # output$modal_insert_vals = renderDT({
  #   modal_in_vals = beach_create() %>%
  #     select(tide_station, beach_name, beach_desc, low_corr_min,
  #            low_corr_ft, high_corr_min, high_corr_ft)
  #   # Generate table
  #   datatable(modal_in_vals,
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
  # observeEvent(input$beaches_add, {
  #   new_vals = beach_create()
  #   showModal(
  #     tags$div(id = "insert_modal",
  #              if ( is.na(new_vals$tide_station) ) {
  #                modalDialog (
  #                  size = "s",
  #                  title = "Warning",
  #                  paste0("Please select a tide_station"),
  #                  easyClose = TRUE,
  #                  footer = NULL
  #                )
  #              } else if ( is.na(new_vals$beach_name) ) {
  #                modalDialog (
  #                  size = "s",
  #                  title = "Warning",
  #                  paste0("Please enter a new beach name"),
  #                  easyClose = TRUE,
  #                  footer = NULL
  #                )
  #              } else if ( is.na(new_vals$low_corr_min) ) {
  #                modalDialog (
  #                  size = "s",
  #                  title = "Warning",
  #                  paste0("Please enter a value for low_corr_min"),
  #                  easyClose = TRUE,
  #                  footer = NULL
  #                )
  #              } else if ( new_vals$beach_name %in% get_beaches()$beach_name ) {
  #                modalDialog (
  #                  size = "s",
  #                  title = "Warning",
  #                  paste0("Please enter a beach name that is not already in the database" ),
  #                  easyClose = TRUE,
  #                  footer = NULL
  #                )
  #              } else {
  #                modalDialog (
  #                  size = 'l',
  #                  title = glue("Insert {input$beach_name_input} to the database?"),
  #                  fluidPage (
  #                    DT::DTOutput("modal_insert_vals"),
  #                    br(),
  #                    br(),
  #                    actionButton("insert_beach","Insert beach")
  #                  ),
  #                  easyClose = TRUE,
  #                  footer = NULL
  #                )
  #              }
  #     ))
  # })
  #
  # # Update DB and reload DT
  # observeEvent(input$insert_beach, {
  #   beach_insert(beach_create())
  #   removeModal()
  #   replaceData(dt_proxy, get_beaches()[,2:12])
  # })
  #
  # # # Could also set priority and do:
  # # # Update DB
  # # observeEvent(input$insert_beach, {
  # #   beach_insert(beach_create())
  # #   removeModal()
  # # }, priority = 999)
  # #
  # # # Then reload DT
  # # observeEvent(input$insert_beach, {
  # #   replaceData(dt_proxy, get_beaches()[,2:12])
  # # }, priority = -999)
  #
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
