
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
  print(selected_redd_location)
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

#========================================================
# Update inputs to values of selected map location
#========================================================

# Get centroid of stream for setting view of redd_map
selected_stream_centroid = reactive({
  req(input$stream_select)
  stream_centroid_coords = get_stream_centroid(waterbody_id())
  return(stream_centroid_coords)
})

# # Output leaflet bidn map....could also use color to indicate species:
# # See: https://rstudio.github.io/leaflet/markers.html
# output$redd_map <- renderLeaflet({
#   m = leaflet() %>%
#     setView(
#       lng = selected_stream_centroid()$center_lon,
#       lat = selected_stream_centroid()$center_lat,
#       zoom = 14) %>%
#     addPolylines(
#       data = wria_streams(),
#       group = "Streams",
#       weight = 3,
#       color = "#0000e6",
#       label = ~stream_label,
#       layerId = ~stream_label,
#       labelOptions = labelOptions(noHide = FALSE)) %>%
#     addProviderTiles("Esri.WorldImagery", group = "Esri World Imagery") %>%
#     addProviderTiles("OpenTopoMap", group = "Open Topo Map") %>%
#     addLayersControl(position = 'bottomright',
#                      baseGroups = c("Esri World Imagery", "Open Topo Map"),
#                      overlayGroups = c("Streams"),
#                      options = layersControlOptions(collapsed = TRUE))
#   m
# })

# Output leaflet bidn map....could also use color to indicate species:
# See: https://rstudio.github.io/leaflet/markers.html
output$redd_map <- renderLeaflet({
  redd_loc_data = get_redd_location(pool, selected_redd_encounter_data()$redd_encounter_id)
  m = leaflet() %>%
    setView(
      lng = selected_stream_centroid()$center_lon,
      lat = selected_stream_centroid()$center_lat,
      zoom = 14) %>%
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
      lng = if_else(nrow(redd_loc_data) == 1L & !is.na(redd_loc_data$longitude),
                    redd_loc_data$longitude,
                    selected_stream_centroid()$center_lon),
      lat = if_else(nrow(redd_loc_data) == 1L & !is.na(redd_loc_data$latitude),
                    redd_loc_data$latitude,
                    selected_stream_centroid()$center_lat),
      # layerId = if_else(!is.na(selected_redd_location_data()$redd_location_id),
      #                   selected_redd_location_data()$redd_location_id, "none"),
      # popup = if_else(!is.na(selected_redd_location_data()$redd_name),
      #                 selected_redd_location_data()$redd_name, "need redd_name"),
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

# # Output leaflet bidn map....could also use color to indicate species:
# # See: https://rstudio.github.io/leaflet/markers.html
# output$redd_map <- renderLeaflet({
#   m = leaflet() %>%
#     setView(
#       lng = selected_stream_centroid()$center_lon,
#       lat = selected_stream_centroid()$center_lat,
#       zoom = 14) %>%
#     addPolylines(
#       data = wria_streams(),
#       group = "Streams",
#       weight = 3,
#       color = "#0000e6",
#       label = ~stream_label,
#       layerId = ~stream_label,
#       labelOptions = labelOptions(noHide = FALSE)) %>%
#     addCircleMarkers(
#       lng = selected_redd_location_data()$longitude,
#       lat = selected_redd_location_data()$latitude,
#       layerId = selected_redd_location_data()$redd_location_id,
#       popup = selected_redd_location_data()$redd_name,
#       radius = 8,
#       color = "red",
#       fillOpacity = 0.5,
#       stroke = FALSE,
#       options = markerOptions(draggable = TRUE,
#                               riseOnHover = TRUE)) %>%
#     addProviderTiles("Esri.WorldImagery", group = "Esri World Imagery") %>%
#     addProviderTiles("OpenTopoMap", group = "Open Topo Map") %>%
#     addLayersControl(position = 'bottomright',
#                      baseGroups = c("Esri World Imagery", "Open Topo Map"),
#                      overlayGroups = c("Streams"),
#                      options = layersControlOptions(collapsed = TRUE))
#   m
# })

# STOPPED HERE. Create table in modal to show updated marker location
# Allow to add new marker if none is present
# Wire in modal button to capture location and write to updated lat-lon inputs

# Steps in redd_map proxy:
# 1. Check if selected_redd_location data includes coordinates.
#    If so
#    if not just draw empty map
# 2. Check if redd_map_click has occurred...will only happen if
#    empty map was created...Then add circle marker

# # Draw circleMarker from existing data
# observeEvent(input$redd_loc_map, {
#   redd_loc = selected_redd_location_data() %>%
#     select(redd_location_id, redd_name,
#            lng = longitude, lat = latitude)
#   redd_map_proxy = leafletProxy("redd_map")
#   if ( !is.na(redd_loc$lat) &
#        !is.na(redd_loc$lng) ) {
#     redd_map_proxy %>%
#       addCircleMarkers(
#         lng = redd_loc$lng,
#         lat = redd_loc$lat,
#         layerId = redd_loc$redd_location_id,
#         popup = redd_loc$redd_name,
#         radius = 8,
#         color = "red",
#         fillOpacity = 0.5,
#         stroke = FALSE,
#         options = markerOptions(draggable = TRUE,
#                                 riseOnHover = TRUE))
#   }
# }, priority = 9999)
#
#
#

# # Observe mouse clicks in empty map to record location
# observeEvent(input$redd_map_click, {
#   # Capture coordinates from click
#   redd_click = input$redd_map_click
#   redd_lat = redd_click$lat
#   redd_lng <- redd_click$lng
#
#   ## Add the circleMarker to proxy redd_map if no redd present on map
#   if ()
#     leafletProxy('map') %>% # use the proxy to save computation
#     addCircles(lng=clng, lat=clat, group='circles',
#                weight=1, radius=100, color='black', fillColor='orange',
#                popup=address, fillOpacity=0.5, opacity=1)
# })

# Create reactive to hold click data
marker_data = reactive({
  req(input$redd_map_marker_click)
  click_data = input$redd_map_marker_click
  mark_dat = tibble(Latitude = round(as.numeric(click_data$lat), digits = 6),
                    Longitude = round(as.numeric(click_data$lng), digits = 6))
  return(mark_dat)
})

# Get dataframe of updated locations
output$redd_coordinates = renderText({
  if (nrow(marker_data()) > 0 ) {
    glue({marker_data()$Latitude}, ": ", {marker_data()$Longitude})
  } else {
    "Please click on a redd marker"
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
                   column(width = 3,
                          verbatimTextOutput("redd_coordinates"))
                 )
               ),
               easyClose = TRUE,
               footer = NULL
             )
    )
  )
})

#========================================================
# Insert operations: reactives, observers and modals
#========================================================

# Disable "New" button if a row of comments already exists
observe({
  input$insert_redd_location
  redd_loc_data = get_redd_location(pool, selected_redd_encounter_data()$redd_encounter_id)
  if (nrow(redd_loc_data) >= 1L) {
    shinyjs::disable("redd_loc_add")
  } else {
    shinyjs::enable("redd_loc_add")
  }
})

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
  # Orientation type
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
}, priority = 9999)

#========================================================
# Edit operations: reactives, observers and modals
#========================================================

# Create reactive to collect input values for insert actions
redd_location_edit = reactive({
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
  # Orientation type
  orientation_type_input = input$orientation_type_select
  if ( orientation_type_input == "" ) {
    location_orientation_type_id = NA
  } else {
    orientation_type_vals = get_orientation_type(pool)
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

# output$chk_loc_edit = renderText({
#   old_redd_location_vals = selected_redd_location_data() %>%
#     select(redd_name, channel_type, orientation_type, latitude,
#            longitude, horiz_accuracy, location_description)
#   old_redd_location_vals[] = lapply(old_redd_location_vals, remisc::set_na)
#   new_redd_location_vals = redd_location_edit() %>%
#     mutate(horiz_accuracy = as.numeric(horiz_accuracy)) %>%
#     select(redd_name, channel_type, orientation_type, latitude,
#            longitude, horiz_accuracy, location_description)
#   new_redd_location_vals[] = lapply(new_redd_location_vals, remisc::set_na)
#   print("old locations")
#   print(old_redd_location_vals)
#   print("new locations")
#   print(new_redd_location_vals)
#   return(unlist(old_redd_location_vals))
# })

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
  post_redd_location_edit_vals = get_redd_location(pool, selected_redd_encounter_data()$redd_encounter_id) %>%
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
  redd_location_modal_del_vals = get_redd_location(pool, selected_redd_encounter_data()$redd_encounter_id) %>%
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

observeEvent(input$redd_loc_delete, {
  redd_location_id = selected_redd_location_data()$redd_location_id
  showModal(
    tags$div(id = "redd_location_delete_modal",
             if ( length(redd_location_id) == 0 ) {
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
                 title = "Are you sure you want to delete this redd location data from the database?",
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
  redd_location_delete(selected_redd_location_data(), selected_redd_encounter_data())
  removeModal()
  redd_locations_after_delete = get_redd_location(pool, selected_redd_encounter_data()$redd_encounter_id) %>%
    select(redd_name, channel_type, orientation_type, latitude,
           longitude, horiz_accuracy, location_description,
           created_dt, created_by, modified_dt, modified_by)
  replaceData(redd_location_dt_proxy, redd_locations_after_delete)
}, priority = 9999)
