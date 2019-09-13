
#========================================================
# Generate lut select ui's
#========================================================

output$fish_channel_type_select = renderUI({
  channel_type_list = get_fish_channel_type(pool)$channel_type
  channel_type_list = c("", channel_type_list)
  selectizeInput("fish_channel_type_select", label = "channel_type",
                 choices = channel_type_list, selected = NULL,
                 width = "250px")
})

output$fish_orientation_type_select = renderUI({
  orientation_type_list = get_fish_orientation_type(pool)$orientation_type
  orientation_type_list = c("", orientation_type_list)
  selectizeInput("fish_orientation_type_select", label = "orientation_type",
                 choices = orientation_type_list, selected = NULL,
                 width = "275px")
})

#========================================================
# Primary datatable for fish_locations
#========================================================

# Primary DT datatable for survey_intent
output$fish_locations = renderDT({
  fish_location_title = glue("{selected_survey_event_data()$species} fish locations for {input$stream_select} on ",
                             "{selected_survey_data()$survey_date} from river mile {selected_survey_data()$up_rm} ",
                             "to {selected_survey_data()$lo_rm}")
  fish_location_data = get_fish_location(pool, selected_fish_encounter_data()$fish_encounter_id) %>%
    select(fish_name, channel_type, orientation_type, latitude,
           longitude, horiz_accuracy, location_description,
           created_dt, created_by, modified_dt, modified_by)

  # Generate table
  datatable(fish_location_data,
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
              htmltools::em(htmltools::strong(fish_location_title))))
})

# Create surveys DT proxy object
fish_location_dt_proxy = dataTableProxy(outputId = "fish_locations")

#========================================================
# Collect location values from selected row for later use
#========================================================

# Create reactive to collect input values for update and delete actions
selected_fish_location_data = reactive({
  req(input$fish_locations_rows_selected)
  fish_location_data = get_fish_location(pool, selected_fish_encounter_data()$fish_encounter_id)
  fish_location_row = input$fish_locations_rows_selected
  selected_fish_location = tibble(fish_location_id = fish_location_data$fish_location_id[fish_location_row],
                                  location_coordinates_id = fish_location_data$location_coordinates_id[fish_location_row],
                                  fish_name = fish_location_data$fish_name[fish_location_row],
                                  channel_type = fish_location_data$channel_type[fish_location_row],
                                  orientation_type = fish_location_data$orientation_type[fish_location_row],
                                  latitude = fish_location_data$latitude[fish_location_row],
                                  longitude = fish_location_data$longitude[fish_location_row],
                                  horiz_accuracy = fish_location_data$horiz_accuracy[fish_location_row],
                                  location_description = fish_location_data$location_description[fish_location_row],
                                  created_date = fish_location_data$created_date[fish_location_row],
                                  created_by = fish_location_data$created_by[fish_location_row],
                                  modified_date = fish_location_data$modified_date[fish_location_row],
                                  modified_by = fish_location_data$modified_by[fish_location_row])
  #print(selected_fish_location)
  return(selected_fish_location)
})

#========================================================
# Update inputs to values in selected row
#========================================================

# Update all input values to values in selected row
observeEvent(input$fish_locations_rows_selected, {
  sfldat = selected_fish_location_data()
  updateTextInput(session, "fish_name_input", value = sfldat$fish_name)
  updateSelectizeInput(session, "fish_channel_type_select", selected = sfldat$channel_type)
  updateSelectizeInput(session, "fish_orientation_type_select", selected = sfldat$orientation_type)
  updateNumericInput(session, "fish_latitude_input", value = sfldat$latitude)
  updateNumericInput(session, "fish_longitude_input", value = sfldat$longitude)
  updateNumericInput(session, "fish_horiz_accuracy_input", value = sfldat$horiz_accuracy)
  updateTextAreaInput(session, "fish_location_description_input", value = sfldat$location_description)
})

#================================================================
# Get either selected fish coordinates or default stream centroid
#================================================================

# Get centroid of stream for setting view of redd_map
selected_fish_coords = reactive({
  req(input$fish_encounters_rows_selected)
  # Get centroid of stream....always available if stream is selected
  center_lat = selected_stream_centroid()$center_lat
  center_lon = selected_stream_centroid()$center_lon
  # Get location_coordinates data should be nrow == 0 if no coordiinates present
  fish_coords = get_fish_coordinates(pool, selected_fish_encounter_data()$fish_encounter_id)
  if ( nrow(fish_coords) == 0 ) {
    fish_lat = center_lat
    fish_lon = center_lon
    fish_name = "need fish name"
  } else {
    fish_lat = fish_coords$latitude
    fish_lon = fish_coords$longitude
    fish_name = selected_fish_location_data()$fish_name
  }
  fish_coords = tibble(fish_encounter_id = selected_fish_encounter_data()$fish_encounter_id,
                       fish_name = fish_name,
                       fish_lat = fish_lat,
                       fish_lon = fish_lon)
  return(fish_coords)
})

# Output leaflet bidn map....could also use color to indicate species:
# See: https://rstudio.github.io/leaflet/markers.html
output$fish_map <- renderLeaflet({
  fish_loc_data = selected_fish_coords()
  fish_lat = fish_loc_data$fish_lat
  fish_lon = fish_loc_data$fish_lon
  fish_name = fish_loc_data$fish_name
  fish_encounter_id = fish_loc_data$fish_encounter_id
  m = leaflet() %>%
    setView(
      lng = selected_stream_centroid()$center_lon,
      lat = selected_stream_centroid()$center_lat,
      zoom = 14) %>%
    # Needed to enable draggable circle-markers
    addDrawToolbar(circleOptions = NA,
                   circleMarkerOptions = NA,
                   markerOptions = NA,
                   polygonOptions = NA,
                   rectangleOptions = NA,
                   polylineOptions = NA) %>%
    addPolylines(
      data = wria_streams(),
      group = "Streams",
      weight = 3,
      color = "#0000e6",
      label = ~stream_label,
      layerId = ~stream_label,
      labelOptions = labelOptions(noHide = FALSE)) %>%
    addCircleMarkers(
      lng = fish_lon,
      lat = fish_lat,
      layerId = fish_encounter_id,
      popup = fish_name,
      radius = 8,
      color = "red",
      fillOpacity = 0.5,
      stroke = FALSE,
      options = markerOptions(draggable = TRUE,
                              riseOnHover = TRUE)) %>%
    addProviderTiles("Esri.WorldImagery", group = "Esri World Imagery") %>%
    addProviderTiles("OpenTopoMap", group = "Open Topo Map") %>%
    addLayersControl(position = 'bottomright',
                     baseGroups = c("Esri World Imagery", "Open Topo Map"),
                     overlayGroups = c("Streams"),
                     options = layersControlOptions(collapsed = TRUE))
  m
})

# Create reactive to hold click data
fish_marker_data = reactive({
  req(input$fish_map_marker_click)
  fish_click_data = input$fish_map_marker_click
  fish_mark_dat = tibble(latitude = round(as.numeric(fish_click_data$lat), digits = 6),
                         longitude = round(as.numeric(fish_click_data$lng), digits = 6))
  return(fish_mark_dat)
})

# Get dataframe of updated locations
output$fish_coordinates = renderUI({
  if ( length(input$fish_map_marker_click) == 0L ) {
    HTML("Drag marker to edit location. Click on marker to set coordinates")
  } else {
    HTML(glue("Fish location: ", {fish_marker_data()$latitude}, ": ", {fish_marker_data()$longitude}))
  }
})

# Modal for new fish locations...add or edit a point...write coordinates to lat, lon
observeEvent(input$fish_loc_map, {
  showModal(
    # Verify required fields have data...none can be blank
    tags$div(id = "fish_location_map_modal",
             modalDialog (
               size = 'l',
               title = glue("Add or edit fish location"),
               fluidPage (
                 fluidRow(
                   column(width = 12,
                          leafletOutput("fish_map", height = 500),
                          br()
                   )
                 ),
                 fluidRow(
                   column(width = 2,
                          actionButton("capture_fish_loc", "Capture fish location")),
                   column(width = 10,
                          htmlOutput("fish_coordinates"))
                 )
               ),
               easyClose = TRUE,
               footer = NULL
             )
    )
  )
})

#======================================================================
# Update fish location coordinate inputs to coordinates selected on map
#======================================================================

# Update all input values to values in selected row
observeEvent(input$capture_fish_loc, {
  fish_coord_data = fish_marker_data()
  updateNumericInput(session, "fish_latitude_input", value = fish_coord_data$latitude)
  updateNumericInput(session, "fish_longitude_input", value = fish_coord_data$longitude)
  removeModal()
})

#========================================================
# Insert operations: reactives, observers and modals
#========================================================

# Disable "New" button if a row of coordinates already exists
observe({
  input$insert_fish_location
  input$delete_fish_location
  fish_loc_data = get_fish_location(pool, selected_fish_encounter_data()$fish_encounter_id)
  if (nrow(fish_loc_data) >= 1L) {
    shinyjs::disable("fish_loc_add")
  } else {
    shinyjs::enable("fish_loc_add")
  }
})

# Create reactive to collect input values for insert actions
fish_location_create = reactive({
  # fish_encounter_id
  fish_encounter_id_input = selected_fish_encounter_data()$fish_encounter_id
  # Channel type
  fish_channel_type_input = input$fish_channel_type_select
  if ( fish_channel_type_input == "" ) {
    stream_channel_type_id = NA
  } else {
    fish_channel_type_vals = get_fish_channel_type(pool)
    stream_channel_type_id = fish_channel_type_vals %>%
      filter(channel_type == fish_channel_type_input) %>%
      pull(stream_channel_type_id)
  }
  # Orientation type
  fish_orientation_type_input = input$fish_orientation_type_select
  if ( fish_orientation_type_input == "" ) {
    location_orientation_type_id = NA
  } else {
    fish_orientation_type_vals = get_fish_orientation_type(pool)
    location_orientation_type_id = fish_orientation_type_vals %>%
      filter(orientation_type == fish_orientation_type_input) %>%
      pull(location_orientation_type_id)
  }
  new_fish_location = tibble(fish_encounter_id = fish_encounter_id_input,
                             fish_name = input$fish_name_input,
                             channel_type = fish_channel_type_input,
                             stream_channel_type_id = stream_channel_type_id,
                             orientation_type = fish_orientation_type_input,
                             location_orientation_type_id = location_orientation_type_id,
                             latitude = input$fish_latitude_input,
                             longitude = input$fish_longitude_input,
                             horiz_accuracy = input$fish_horiz_accuracy_input,
                             location_description = input$fish_location_description_input,
                             created_dt = lubridate::with_tz(Sys.time(), "UTC"),
                             created_by = Sys.getenv("USERNAME"))
  return(new_fish_location)
})

# Generate values to show in modal
output$fish_location_modal_insert_vals = renderDT({
  fish_location_modal_in_vals = fish_location_create() %>%
    select(fish_name, channel_type, orientation_type, latitude,
           longitude, horiz_accuracy, location_description)
  # Generate table
  datatable(fish_location_modal_in_vals,
            rownames = FALSE,
            options = list(dom = 't',
                           scrollX = T,
                           ordering = FALSE,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
                             "}")))
})

# Modal for new redd locations
observeEvent(input$fish_loc_add, {
  new_fish_location_vals = fish_location_create()
  showModal(
    # Verify required fields have data...none can be blank
    tags$div(id = "fish_location_insert_modal",
             if ( is.na(new_fish_location_vals$stream_channel_type_id) |
                  is.na(new_fish_location_vals$location_orientation_type_id) |
                  is.na(new_fish_location_vals$latitude) |
                  is.na(new_fish_location_vals$longitude) ) {
               modalDialog (
                 size = "m",
                 title = "Warning",
                 paste0("Values are required for channel, orientation, and coordinates"),
                 easyClose = TRUE,
                 footer = NULL
               )
               # Write to DB
             } else {
               modalDialog (
                 size = 'l',
                 title = glue("Insert new fish location data to the database?"),
                 fluidPage (
                   DT::DTOutput("fish_location_modal_insert_vals"),
                   br(),
                   br(),
                   actionButton("insert_fish_location", "Insert location")
                 ),
                 easyClose = TRUE,
                 footer = NULL
               )
             }
    ))
})

# Reactive to hold values actually inserted
fish_location_insert_vals = reactive({
  new_fish_loc_values = fish_location_create() %>%
    mutate(waterbody_id = waterbody_id()) %>%
    mutate(wria_id = wria_id()) %>%
    mutate(location_type_id = "c8c4020f-36ac-46ec-b158-6d07a3812bc8") %>%     # fish encounter
    select(fish_encounter_id, waterbody_id, wria_id, location_type_id,
           stream_channel_type_id, location_orientation_type_id,
           fish_name, location_description, latitude, longitude,
           horiz_accuracy, created_by)
  return(new_fish_loc_values)
})

# Update DB and reload DT
observeEvent(input$insert_fish_location, {
  fish_location_insert(fish_location_insert_vals())
  removeModal()
  post_fish_location_insert_vals = get_fish_location(pool, selected_fish_encounter_data()$fish_encounter_id) %>%
    select(fish_name, channel_type, orientation_type, latitude,
           longitude, horiz_accuracy, location_description,
           created_dt, created_by, modified_dt, modified_by)
  replaceData(fish_location_dt_proxy, post_fish_location_insert_vals)
}, priority = 9999)

#========================================================
# Edit operations: reactives, observers and modals
#========================================================

# Create reactive to collect input values for insert actions
fish_location_edit = reactive({
  # Channel type
  fish_channel_type_input = input$fish_channel_type_select
  if ( fish_channel_type_input == "" ) {
    stream_channel_type_id = NA
  } else {
    fish_channel_type_vals = get_fish_channel_type(pool)
    stream_channel_type_id = fish_channel_type_vals %>%
      filter(channel_type == fish_channel_type_input) %>%
      pull(stream_channel_type_id)
  }
  # Orientation type
  fish_orientation_type_input = input$fish_orientation_type_select
  if ( fish_orientation_type_input == "" ) {
    location_orientation_type_id = NA
  } else {
    fish_orientation_type_vals = get_fish_orientation_type(pool)
    location_orientation_type_id = fish_orientation_type_vals %>%
      filter(orientation_type == fish_orientation_type_input) %>%
      pull(location_orientation_type_id)
  }
  edit_fish_location = tibble(fish_location_id = selected_fish_location_data()$fish_location_id,
                              fish_name = input$fish_name_input,
                              channel_type = fish_channel_type_input,
                              stream_channel_type_id = stream_channel_type_id,
                              orientation_type = fish_orientation_type_input,
                              location_orientation_type_id = location_orientation_type_id,
                              latitude = input$fish_latitude_input,
                              longitude = input$fish_longitude_input,
                              horiz_accuracy = input$fish_horiz_accuracy_input,
                              location_description = input$fish_location_description_input,
                              modified_dt = lubridate::with_tz(Sys.time(), "UTC"),
                              modified_by = Sys.getenv("USERNAME"))
  return(edit_fish_location)
})

# Generate values to show in modal
output$fish_location_modal_update_vals = renderDT({
  fish_location_modal_up_vals = fish_location_edit() %>%
    select(fish_name, channel_type, orientation_type, latitude,
           longitude, horiz_accuracy, location_description)
  # Generate table
  datatable(fish_location_modal_up_vals,
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
observeEvent(input$fish_loc_edit, {
  old_fish_location_vals = selected_fish_location_data() %>%
    mutate(latitude = round(latitude, 6)) %>%
    mutate(longitude = round(longitude, 6)) %>%
    select(fish_name, channel_type, orientation_type, latitude,
           longitude, horiz_accuracy, location_description)
  old_fish_location_vals[] = lapply(old_fish_location_vals, remisc::set_na)
  new_fish_location_vals = fish_location_edit() %>%
    mutate(horiz_accuracy = as.numeric(horiz_accuracy)) %>%
    mutate(latitude = round(latitude, 6)) %>%
    mutate(longitude = round(longitude, 6)) %>%
    select(fish_name, channel_type, orientation_type, latitude,
           longitude, horiz_accuracy, location_description)
  new_fish_location_vals[] = lapply(new_fish_location_vals, remisc::set_na)
  showModal(
    tags$div(id = "fish_location_update_modal",
             if ( !length(input$fish_locations_rows_selected) == 1 ) {
               modalDialog (
                 size = "m",
                 title = "Warning",
                 paste("Please select a row to edit!"),
                 easyClose = TRUE,
                 footer = NULL
               )
             } else if ( isTRUE(all_equal(old_fish_location_vals, new_fish_location_vals)) ) {
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
                 title = "Update fish location data to these new values?",
                 fluidPage (
                   DT::DTOutput("fish_location_modal_update_vals"),
                   br(),
                   br(),
                   actionButton("save_fish_loc_edits", "Save changes")
                 ),
                 easyClose = TRUE,
                 footer = NULL
               )
             }
    ))
})

# Update DB and reload DT
observeEvent(input$save_fish_loc_edits, {
  fish_location_update(fish_location_edit())
  removeModal()
  post_fish_location_edit_vals = get_fish_location(pool, selected_fish_encounter_data()$fish_encounter_id) %>%
    select(fish_name, channel_type, orientation_type, latitude,
           longitude, horiz_accuracy, location_description,
           created_dt, created_by, modified_dt, modified_by)
  replaceData(fish_location_dt_proxy, post_fish_location_edit_vals)
}, priority = 9999)

#========================================================
# Delete operations: reactives, observers and modals
#========================================================

# Generate values to show in modal
output$fish_location_modal_delete_vals = renderDT({
  fish_location_modal_del_id = selected_fish_location_data()$fish_location_id
  fish_location_modal_del_vals = get_fish_location(pool, selected_fish_encounter_data()$fish_encounter_id) %>%
    filter(fish_location_id == fish_location_modal_del_id) %>%
    select(fish_name, channel_type, orientation_type, latitude,
           longitude, horiz_accuracy, location_description)
  # Generate table
  datatable(fish_location_modal_del_vals,
            rownames = FALSE,
            options = list(dom = 't',
                           scrollX = T,
                           ordering = FALSE,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
                             "}")))
})

observeEvent(input$fish_loc_delete, {
  fish_location_id = selected_fish_location_data()$fish_location_id
  showModal(
    tags$div(id = "fish_location_delete_modal",
             if ( length(fish_location_id) == 0 ) {
               modalDialog (
                 size = "m",
                 title = "Warning",
                 glue("Please select a row to delete!"),
                 easyClose = TRUE,
                 footer = NULL
               )
             } else {
               modalDialog (
                 size = 'l',
                 title = "Are you sure you want to delete this fish location data from the database?",
                 fluidPage (
                   DT::DTOutput("fish_location_modal_delete_vals"),
                   br(),
                   br(),
                   actionButton("delete_fish_location", "Delete location data")
                 ),
                 easyClose = TRUE,
                 footer = NULL
               )
             }
    ))
})

# Update DB and reload DT
observeEvent(input$delete_fish_location, {
  fish_location_delete(selected_fish_location_data(), selected_fish_encounter_data())
  removeModal()
  fish_locations_after_delete = get_fish_location(pool, selected_fish_encounter_data()$fish_encounter_id) %>%
    select(fish_name, channel_type, orientation_type, latitude,
           longitude, horiz_accuracy, location_description,
           created_dt, created_by, modified_dt, modified_by)
  replaceData(fish_location_dt_proxy, fish_locations_after_delete)
}, priority = 9999)
