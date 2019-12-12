
#========================================================
# Generate lut select ui's
#========================================================

output$channel_type_select = renderUI({
  channel_type_list = get_channel_type()$channel_type
  channel_type_list = c("", channel_type_list)
  selectizeInput("channel_type_select", label = "channel_type",
                 choices = channel_type_list, selected = NULL,
                 width = "250px")
})

output$orientation_type_select = renderUI({
  orientation_type_list = get_orientation_type()$orientation_type
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
  req(input$tabs == "data_entry")
  req(input$surveys_rows_selected)
  req(input$survey_events_rows_selected)
  req(input$redd_encounters_rows_selected)
  req(!is.na(selected_redd_encounter_data()$redd_encounter_id))
  redd_location_title = glue("{selected_survey_event_data()$species} redd locations for {input$stream_select} on ",
                             "{selected_survey_data()$survey_date} from river mile {selected_survey_data()$up_rm} ",
                             "to {selected_survey_data()$lo_rm}")
  redd_location_data = get_redd_location(selected_redd_encounter_data()$redd_encounter_id) %>%
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
  req(input$tabs == "data_entry")
  req(input$surveys_rows_selected)
  req(input$survey_events_rows_selected)
  req(input$redd_encounters_rows_selected)
  req(input$redd_locations_rows_selected)
  req(!is.na(selected_redd_encounter_data()$redd_encounter_id))
  redd_location_data = get_redd_location(selected_redd_encounter_data()$redd_encounter_id)
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
  #print(selected_redd_location)
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
  updateTextAreaInput(session, "location_description_input", value = srldat$location_description)
})

#================================================================
# Get either selected redd coordinates or default stream centroid
#================================================================

# Get centroid of stream for setting view of redd_map
selected_redd_coords = reactive({
  req(input$tabs == "data_entry")
  req(input$surveys_rows_selected)
  req(input$survey_events_rows_selected)
  req(input$redd_encounters_rows_selected)
  req(!is.na(selected_redd_encounter_data()$redd_encounter_id))
  # Get centroid of stream....always available if stream is selected
  center_lat = selected_stream_centroid()$center_lat
  center_lon = selected_stream_centroid()$center_lon
  # Get location_coordinates data should be nrow == 0 if no coordiinates present
  redd_coords = get_redd_coordinates(selected_redd_encounter_data()$redd_encounter_id)
  if ( nrow(redd_coords) == 0 ) {
    redd_lat = center_lat
    redd_lon = center_lon
    redd_name = "need redd name"
  } else {
    redd_lat = redd_coords$latitude
    redd_lon = redd_coords$longitude
    redd_name = selected_redd_encounter_data()$redd_name
  }
  redd_coords = tibble(redd_encounter_id = selected_redd_encounter_data()$redd_encounter_id,
                       redd_name = redd_name,
                       redd_lat = redd_lat,
                       redd_lon = redd_lon)
  return(redd_coords)
})

# Output leaflet bidn map....could also use color to indicate species:
# See: https://rstudio.github.io/leaflet/markers.html
output$redd_map <- renderLeaflet({
  redd_loc_data = selected_redd_coords()
  redd_lat = redd_loc_data$redd_lat
  redd_lon = redd_loc_data$redd_lon
  redd_name = redd_loc_data$redd_name
  redd_encounter_id = redd_loc_data$redd_encounter_id
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
      lng = redd_lon,
      lat = redd_lat,
      layerId = redd_encounter_id,
      popup = redd_name,
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
marker_data = reactive({
  req(input$redd_map_marker_click)
  click_data = input$redd_map_marker_click
  mark_dat = tibble(latitude = round(as.numeric(click_data$lat), digits = 6),
                    longitude = round(as.numeric(click_data$lng), digits = 6))
  return(mark_dat)
})

# Get dataframe of updated locations
output$redd_coordinates = renderUI({
  input$redd_map_marker_click
  if ( length(input$redd_map_marker_click) == 0L ) {
  HTML("Drag marker to edit location. Click on marker to set coordinates")
} else {
  HTML(glue("Redd location: ", {marker_data()$latitude}, ": ", {marker_data()$longitude}))
}
})

# Modal for new redd locations...add or edit a point...write coordinates to lat, lon
observeEvent(input$redd_loc_map, {
  showModal(
    # Verify required fields have data...none can be blank
    tags$div(id = "redd_location_map_modal",
             modalDialog (
               size = 'l',
               title = glue("Add or edit redd location"),
               fluidPage (
                 fluidRow(
                   column(width = 12,
                          leafletOutput("redd_map", height = 500),
                          br()
                   )
                 ),
                 fluidRow(
                   column(width = 2,
                          actionButton("capture_redd_loc", "Capture redd location")),
                   column(width = 10,
                          htmlOutput("redd_coordinates"))
                 )
               ),
               easyClose = TRUE,
               footer = NULL
             )
    )
  )
})

#======================================================================
# Update redd location coordinate inputs to coordinates selected on map
#======================================================================

# Update all input values to values in selected row
observeEvent(input$capture_redd_loc, {
  coord_data = marker_data()
  updateNumericInput(session, "latitude_input", value = coord_data$latitude)
  updateNumericInput(session, "longitude_input", value = coord_data$longitude)
  removeModal()
})

#========================================================
# Insert operations: reactives, observers and modals
#========================================================

# Disable "New" button if a row of comments already exists
observe({
  req(!is.na(selected_redd_encounter_data()$redd_encounter_id))
  input$insert_redd_location
  redd_loc_data = get_redd_location(selected_redd_encounter_data()$redd_encounter_id)
  if (nrow(redd_loc_data) >= 1L) {
    shinyjs::disable("redd_loc_add")
  } else {
    shinyjs::enable("redd_loc_add")
  }
})

# Create reactive to collect input values for insert actions
redd_location_create = reactive({
  req(input$tabs == "data_entry")
  req(input$surveys_rows_selected)
  req(input$survey_events_rows_selected)
  req(input$redd_encounters_rows_selected)
  req(!is.na(selected_redd_encounter_data()$redd_encounter_id))
  # Redd_encounter_id
  redd_encounter_id_input = selected_redd_encounter_data()$redd_encounter_id
  # Channel type
  channel_type_input = input$channel_type_select
  if ( channel_type_input == "" ) {
    stream_channel_type_id = NA
  } else {
    channel_type_vals = get_channel_type()
    stream_channel_type_id = channel_type_vals %>%
      filter(channel_type == channel_type_input) %>%
      pull(stream_channel_type_id)
  }
  # Orientation type
  orientation_type_input = input$orientation_type_select
  if ( orientation_type_input == "" ) {
    location_orientation_type_id = NA
  } else {
    orientation_type_vals = get_orientation_type()
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

# Modal for new redd locations
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
  return(new_redd_loc_values)
})

# Update DB and reload DT
observeEvent(input$insert_redd_location, {
  redd_location_insert(redd_location_insert_vals())
  removeModal()
  post_redd_location_insert_vals = get_redd_location(selected_redd_encounter_data()$redd_encounter_id) %>%
    select(redd_name, channel_type, orientation_type, latitude,
           longitude, horiz_accuracy, location_description,
           created_dt, created_by, modified_dt, modified_by)
  replaceData(redd_location_dt_proxy, post_redd_location_insert_vals)
}, priority = 9999)

#========================================================
# Edit operations: reactives, observers and modals
#========================================================

# Create reactive to collect input values for insert actions
redd_location_edit = reactive({
  req(input$tabs == "data_entry")
  req(input$surveys_rows_selected)
  req(input$survey_events_rows_selected)
  req(input$redd_encounters_rows_selected)
  req(input$redd_locations_rows_selected)
  req(!is.na(selected_redd_location_data()$redd_location_id))
  # Channel type
  channel_type_input = input$channel_type_select
  if ( channel_type_input == "" ) {
    stream_channel_type_id = NA
  } else {
    channel_type_vals = get_channel_type()
    stream_channel_type_id = channel_type_vals %>%
      filter(channel_type == channel_type_input) %>%
      pull(stream_channel_type_id)
  }
  # Orientation type
  orientation_type_input = input$orientation_type_select
  if ( orientation_type_input == "" ) {
    location_orientation_type_id = NA
  } else {
    orientation_type_vals = get_orientation_type()
    location_orientation_type_id = orientation_type_vals %>%
      filter(orientation_type == orientation_type_input) %>%
      pull(location_orientation_type_id)
  }
  edit_redd_location = tibble(redd_location_id = selected_redd_location_data()$redd_location_id,
                              redd_name = input$redd_name_input,
                              channel_type = channel_type_input,
                              stream_channel_type_id = stream_channel_type_id,
                              orientation_type = orientation_type_input,
                              location_orientation_type_id = location_orientation_type_id,
                              latitude = input$latitude_input,
                              longitude = input$longitude_input,
                              horiz_accuracy = input$horiz_accuracy_input,
                              location_description = input$location_description_input,
                              modified_dt = lubridate::with_tz(Sys.time(), "UTC"),
                              modified_by = Sys.getenv("USERNAME"))
  return(edit_redd_location)
})

dependent_redd_location_surveys = reactive({
  redd_loc_id = selected_redd_location_data()$redd_location_id
  redd_loc_srv = get_redd_location_surveys(redd_loc_id)
  return(redd_loc_srv)
})

# Generate values to show check modal
output$redd_loc_surveys = renderDT({
  redd_loc_srv = dependent_redd_location_surveys()
  redd_location_warning = glue("WARNING: All previous redds, photo's, or observations linked to this ",
                               "redd location are shown below. Please verify that all data below ",
                               "should be updated to the new values!")
  # Generate table
  datatable(redd_loc_srv,
            rownames = FALSE,
            options = list(dom = 't',
                           scrollX = T,
                           ordering = FALSE,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
                             "}")),
            caption = htmltools::tags$caption(
              style = 'caption-side: top; text-align: left; color: blue; width: auto;',
              htmltools::em(htmltools::strong(redd_location_warning))))
})

# Generate values to show in modal
output$redd_location_modal_update_vals = renderDT({
  redd_location_modal_up_vals = redd_location_edit() %>%
    select(redd_name, channel_type, orientation_type, latitude,
           longitude, horiz_accuracy, location_description)
  # Generate table
  datatable(redd_location_modal_up_vals,
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
observeEvent(input$redd_loc_edit, {
  old_redd_location_vals = selected_redd_location_data() %>%
    mutate(latitude = round(latitude, 6)) %>%
    mutate(longitude = round(longitude, 6)) %>%
    select(redd_name, channel_type, orientation_type, latitude,
           longitude, horiz_accuracy, location_description)
  old_redd_location_vals[] = lapply(old_redd_location_vals, remisc::set_na)
  new_redd_location_vals = redd_location_edit() %>%
    mutate(horiz_accuracy = as.numeric(horiz_accuracy)) %>%
    mutate(latitude = round(latitude, 6)) %>%
    mutate(longitude = round(longitude, 6)) %>%
    select(redd_name, channel_type, orientation_type, latitude,
           longitude, horiz_accuracy, location_description)
  new_redd_location_vals[] = lapply(new_redd_location_vals, remisc::set_na)
  showModal(
    tags$div(id = "redd_location_update_modal",
             if ( !length(input$redd_locations_rows_selected) == 1 ) {
               modalDialog (
                 size = "m",
                 title = "Warning",
                 paste("Please select a row to edit!"),
                 easyClose = TRUE,
                 footer = NULL
               )
             } else if ( isTRUE(all_equal(old_redd_location_vals, new_redd_location_vals)) ) {
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
                 title = "Update redd location data to these new values?",
                 fluidPage (
                   DT::DTOutput("redd_location_modal_update_vals"),
                   br(),
                   DT::DTOutput("redd_loc_surveys"),
                   br(),
                   br(),
                   actionButton("save_redd_loc_edits", "Save changes")
                 ),
                 easyClose = TRUE,
                 footer = NULL
               )
             }
    ))
})

# Update DB and reload DT
observeEvent(input$save_redd_loc_edits, {
  redd_location_update(redd_location_edit())
  removeModal()
  post_redd_location_edit_vals = get_redd_location(selected_redd_encounter_data()$redd_encounter_id) %>%
    select(redd_name, channel_type, orientation_type, latitude,
           longitude, horiz_accuracy, location_description,
           created_dt, created_by, modified_dt, modified_by)
  replaceData(redd_location_dt_proxy, post_redd_location_edit_vals)
})

#========================================================
# Delete operations: reactives, observers and modals
#========================================================

# Generate values to show in modal
output$redd_location_modal_delete_vals = renderDT({
  redd_location_modal_del_id = selected_redd_location_data()$redd_location_id
  redd_location_modal_del_vals = get_redd_location(selected_redd_encounter_data()$redd_encounter_id) %>%
    filter(redd_location_id == redd_location_modal_del_id) %>%
    select(redd_name, channel_type, orientation_type, latitude,
           longitude, horiz_accuracy, location_description)
  # Generate table
  datatable(redd_location_modal_del_vals,
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
redd_location_dependencies = reactive({
  redd_location_id = selected_redd_location_data()$redd_location_id
  redd_loc_dep = get_redd_location_dependencies(redd_location_id)
  return(redd_loc_dep)
})

observeEvent(input$redd_loc_delete, {
  redd_location_id = selected_redd_location_data()$redd_location_id
  redd_loc_dependencies = redd_location_dependencies()
  table_names = paste0(paste0("'", names(redd_loc_dependencies), "'"), collapse = ", ")
  redd_nm = selected_redd_coords()$redd_name
  # Customize the delete message depending on if other entries are linked to redd_name
  if (ncol(redd_loc_dependencies) > 1L | redd_loc_dependencies$redd_encounter[1] > 1L) {
    redd_delete_msg = glue("Other entries in {table_names} are linked to redd_name: '{redd_nm}'. ",
                           "Only the link to the redd location will be deleted.")
  } else {
    redd_delete_msg = "Are you sure you want to delete this redd location data from the database?"
  }
  showModal(
    tags$div(id = "redd_location_delete_modal",
             if ( !length(input$redd_locations_rows_selected) == 1 ) {
               modalDialog (
                 size = "m",
                 title = "Warning",
                 paste("Please select a row to edit!"),
                 easyClose = TRUE,
                 footer = NULL
               )
             } else {
               modalDialog (
                 size = 'l',
                 title = redd_delete_msg,
                 fluidPage (
                   DT::DTOutput("redd_location_modal_delete_vals"),
                   br(),
                   br(),
                   actionButton("delete_redd_location", "Delete location data")
                 ),
                 easyClose = TRUE,
                 footer = NULL
               )
             }
    ))
})

# Update DB and reload DT
observeEvent(input$delete_redd_location, {
  redd_location_delete(redd_location_dependencies(),
                       selected_redd_location_data(),
                       selected_redd_encounter_data())
  removeModal()
  redd_locations_after_delete = get_redd_location(selected_redd_encounter_data()$redd_encounter_id) %>%
    select(redd_name, channel_type, orientation_type, latitude,
           longitude, horiz_accuracy, location_description,
           created_dt, created_by, modified_dt, modified_by)
  replaceData(redd_location_dt_proxy, redd_locations_after_delete)
}, priority = 9999)
