
#========================================================
# Generate lut select ui's
#========================================================

output$reach_point_type_select = renderUI({
  reach_point_type_list = get_location_type()$reach_point_type
  reach_point_type_list = c("", reach_point_type_list)
  selectizeInput("reach_point_type_select", label = "reach_point_type",
                 choices = reach_point_type_list, selected = NULL,
                 width = "175px")
})

# output$reach_point_channel_type_select = renderUI({
#   channel_type_list = get_channel_type()$channel_type
#   channel_type_list = c("", channel_type_list)
#   selectizeInput("reach_point_channel_type_select", label = "channel_type",
#                  choices = channel_type_list, selected = NULL,
#                  width = "250px")
# })
#
# output$reach_point_orientation_type_select = renderUI({
#   orientation_type_list = get_orientation_type()$orientation_type
#   orientation_type_list = c("", orientation_type_list)
#   selectizeInput("reach_point_orientation_type_select", label = "orientation_type",
#                  choices = orientation_type_list, selected = NULL,
#                  width = "275px")
# })

#========================================================
# Primary datatable for reach points
#========================================================

# Primary DT datatable for survey_intent
output$reach_points = renderDT({
  reach_point_title = glue("Reach points for {input$stream_select}")
  reach_point_data = get_reach_point(waterbody_id()) %>%
    select(river_mile, reach_point_code, reach_point_name, reach_point_type, #channel_type, orientation_type,
           latitude, longitude, horiz_accuracy, reach_point_description,
           created_dt, created_by, modified_dt, modified_by)

  # Generate table
  datatable(reach_point_data,
            selection = list(mode = 'single'),
            options = list(dom = 'ltp',
                           pageLength = 10,
                           lengthMenu = c(10, 25, 50),
                           scrollX = T,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
                             "}")),
            caption = htmltools::tags$caption(
              style = 'caption-side: top; text-align: left; color: black; width: auto;',
              htmltools::em(htmltools::strong(reach_point_title))))
})

# Create surveys DT proxy object
reach_point_dt_proxy = dataTableProxy(outputId = "reach_points")

#========================================================
# Collect location values from selected row for later use
#========================================================

# Create reactive to collect input values for update and delete actions
selected_reach_point_data = reactive({
  req(input$reach_points_rows_selected)
  reach_point_data = get_reach_point(waterbody_id())
  reach_point_row = input$reach_points_rows_selected
  selected_reach_point = tibble(location_id = reach_point_data$location_id[reach_point_row],
                                location_coordinates_id = reach_point_data$location_coordinates_id[reach_point_row],
                                river_mile = reach_point_data$river_mile[reach_point_row],
                                reach_point_type = reach_point_data$reach_point_type[reach_point_row],
                                reach_point_code = reach_point_data$reach_point_code[reach_point_row],
                                reach_point_name = reach_point_data$reach_point_name[reach_point_row],
                                # channel_type = reach_point_data$channel_type[reach_point_row],
                                # orientation_type = reach_point_data$orientation_type[reach_point_row],
                                latitude = reach_point_data$latitude[reach_point_row],
                                longitude = reach_point_data$longitude[reach_point_row],
                                horiz_accuracy = reach_point_data$horiz_accuracy[reach_point_row],
                                reach_point_description = reach_point_data$reach_point_description[reach_point_row],
                                created_date = reach_point_data$created_date[reach_point_row],
                                created_by = reach_point_data$created_by[reach_point_row],
                                modified_date = reach_point_data$modified_date[reach_point_row],
                                modified_by = reach_point_data$modified_by[reach_point_row])
  #print(selected_reach_point)
  return(selected_reach_point)
})

#========================================================
# Update inputs to values in selected row
#========================================================

# Update all input values to values in selected row
observeEvent(input$reach_points_rows_selected, {
  srpdat = selected_reach_point_data()
  updateNumericInput(session, "river_mile_input", value = srpdat$river_mile)
  updateSelectizeInput(session, "reach_point_type_select", selected = srpdat$reach_point_type)
  updateTextInput(session, "reach_point_code_input", value = srpdat$reach_point_code)
  updateTextInput(session, "reach_point_name_input", value = srpdat$reach_point_name)
  # updateSelectizeInput(session, "reach_point_channel_type_select", selected = srpdat$channel_type)
  # updateSelectizeInput(session, "reach_point_orientation_type_select", selected = srpdat$orientation_type)
  updateNumericInput(session, "reach_point_latitude_input", value = srpdat$latitude)
  updateNumericInput(session, "reach_point_longitude_input", value = srpdat$longitude)
  updateNumericInput(session, "reach_point_horiz_accuracy_input", value = srpdat$horiz_accuracy)
  updateTextAreaInput(session, "reach_point_description_input", value = srpdat$reach_point_description)
})

#================================================================
# Get either selected reach coordinates or default stream centroid
#================================================================

# Get centroid of stream for setting view of redd_map
selected_reach_point_coords = reactive({
  # Get centroid of stream....always available if stream is selected
  center_lat = selected_stream_centroid()$center_lat
  center_lon = selected_stream_centroid()$center_lon
  # Get reach coordinates from inputs if present
  if ( is.na(input$reach_point_latitude_input) | is.na(input$reach_point_longitude_input) ) {
    reach_point_lat = center_lat
    reach_point_lon = center_lon
    reach_point_name = "need reach name"
  } else {
    reach_point_lat = input$reach_point_latitude_input
    reach_point_lon = input$reach_point_longitude_input
    reach_point_name = input$reach_point_name_input
    if ( is.na(reach_point_name) | reach_point_name == "" ) {
      reach_point_name = "need reach name"
    }
  }
  reach_point_coords = tibble(reach_point_name = reach_point_name,
                              reach_point_lat = reach_point_lat,
                              reach_point_lon = reach_point_lon)
  return(reach_point_coords)
})

# Output leaflet bidn map....could also use color to indicate species:
# See: https://rstudio.github.io/leaflet/markers.html
output$reach_point_map <- renderLeaflet({
  reach_point_data = selected_reach_point_coords()
  reach_point_lat = reach_point_data$reach_point_lat
  reach_point_lon = reach_point_data$reach_point_lon
  reach_point_name = reach_point_data$reach_point_name
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
      lng = reach_point_lon,
      lat = reach_point_lat,
      #layerId = fish_encounter_id,
      popup = reach_point_name,
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
reach_point_marker_data = reactive({
  req(input$reach_point_map_marker_click)
  reach_point_click_data = input$reach_point_map_marker_click
  reach_point_dat = tibble(latitude = round(as.numeric(reach_point_click_data$lat), digits = 6),
                           longitude = round(as.numeric(reach_point_click_data$lng), digits = 6))
  return(reach_point_dat)
})

# Get dataframe of updated locations
output$reach_point_coordinates = renderUI({
  if ( length(input$reach_point_map_marker_click) == 0L ) {
    HTML("Drag marker to edit reach point. Click on marker to set coordinates")
  } else {
    HTML(glue("Reach point coordinates: ", { reach_point_marker_data()$latitude }, ": ", { reach_point_marker_data()$longitude }))
  }
})

# Modal for new reach points...add or edit a point...write coordinates to lat, lon
observeEvent(input$use_reach_point_map, {
  showModal(
    # Verify required fields have data...none can be blank
    tags$div(id = "reach_point_map_modal",
             modalDialog (
               size = 'l',
               title = glue("Add or edit a reach point"),
               fluidPage (
                 fluidRow(
                   column(width = 12,
                          leafletOutput("reach_point_map", height = 500),
                          br()
                   )
                 ),
                 fluidRow(
                   column(width = 2,
                          actionButton("capture_reach_point", "Capture reach point")),
                   column(width = 10,
                          htmlOutput("reach_point_coordinates"))
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
observeEvent(input$capture_reach_point, {
  reach_point_coord_data = reach_point_marker_data()
  updateNumericInput(session, "reach_point_latitude_input", value = reach_point_coord_data$latitude)
  updateNumericInput(session, "reach_point_longitude_input", value = reach_point_coord_data$longitude)
  removeModal()
})

#========================================================
# Insert operations: reactives, observers and modals
#========================================================

# Create reactive to collect input values for insert actions
reach_point_create = reactive({
  # Location type
  reach_point_type_input = input$reach_point_type_select
  if ( reach_point_type_input == "" ) {
    location_type_id = NA
  } else {
    reach_point_type_vals = get_location_type()
    location_type_id = reach_point_type_vals %>%
      filter(reach_point_type == reach_point_type_input) %>%
      pull(location_type_id)
  }
  new_reach_point = tibble(river_mile = input$river_mile_input,
                           reach_point_type = reach_point_type_input,
                           location_type_id = location_type_id,
                           reach_point_code = input$reach_point_code_input,
                           reach_point_name = input$reach_point_name_input,
                           latitude = input$reach_point_latitude_input,
                           longitude = input$reach_point_longitude_input,
                           horiz_accuracy = input$reach_point_horiz_accuracy_input,
                           reach_point_description = input$reach_point_description_input,
                           created_dt = lubridate::with_tz(Sys.time(), "UTC"),
                           created_by = Sys.getenv("USERNAME"))
  return(new_reach_point)
})

# Generate values to show in modal
output$reach_point_modal_insert_vals = renderDT({
  reach_point_modal_in_vals = reach_point_create() %>%
    select(river_mile, reach_point_code, reach_point_name, latitude,
           longitude, horiz_accuracy, reach_point_description)
  # Generate table
  datatable(reach_point_modal_in_vals,
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
observeEvent(input$reach_point_add, {
  new_reach_point_vals = reach_point_create()
  old_reach_point_vals = get_reach_point(waterbody_id())
  new_coords = paste0(new_reach_point_vals$latitude, ":", new_reach_point_vals$longitude)
  old_coords = paste0(old_reach_point_vals$latitude, ":", old_reach_point_vals$longitude)
  showModal(
    # Verify required fields have data...none can be blank
    tags$div(id = "reach_point_insert_modal",
             if ( is.na(new_reach_point_vals$river_mile) |
                  is.na(new_reach_point_vals$location_type_id) |
                  is.na(new_reach_point_vals$latitude) |
                  is.na(new_reach_point_vals$longitude) ) {
               modalDialog (
                 size = "m",
                 title = "Warning",
                 paste0("Values are required for river_mile, location_type, and coordinates"),
                 easyClose = TRUE,
                 footer = NULL
               )
               # Verify no river_miles are duplicated
             } else if ( new_reach_point_vals$river_mile %in% old_reach_point_vals$river_mile ) {
               modalDialog (
                 size = "m",
                 title = "Warning",
                 paste0("This river mile point has already been entered"),
                 easyClose = TRUE,
                 footer = NULL
               )
               # Verify no set of coordinates are duplicated
             } else if ( new_coords %in% old_coords ) {
               modalDialog (
                 size = "m",
                 title = "Warning",
                 paste0("This set of coordinates has already been entered"),
                 easyClose = TRUE,
                 footer = NULL
               )
             } else {
               # Write to DB
               modalDialog (
                 size = 'l',
                 title = glue("Insert new reach points to the database?"),
                 fluidPage (
                   DT::DTOutput("reach_point_modal_insert_vals"),
                   br(),
                   br(),
                   actionButton("insert_reach_point", "Insert reach point")
                 ),
                 easyClose = TRUE,
                 footer = NULL
               )
             }
    ))
})

# Reactive to hold values actually inserted
reach_point_insert_vals = reactive({
  new_reach_point_values = reach_point_create() %>%
    mutate(waterbody_id = waterbody_id()) %>%
    mutate(wria_id = wria_id()) %>%
    mutate(stream_channel_type_id = "713a39a5-8e95-4069-b078-066699c321d8") %>%           # No data
    mutate(location_orientation_type_id = "eb4652b7-5390-43d4-a98e-60ea54a1d518") %>%     # No data
    select(waterbody_id, wria_id, location_type_id, stream_channel_type_id,
           location_orientation_type_id, river_mile, reach_point_code,
           reach_point_name, reach_point_description, latitude,
           longitude, horiz_accuracy, created_by)
  return(new_reach_point_values)
})

# Update DB and reload DT
observeEvent(input$insert_reach_point, {
  reach_point_insert(reach_point_insert_vals())
  removeModal()
  post_reach_point_insert_vals = get_reach_point(waterbody_id()) %>%
    select(river_mile, reach_point_code, reach_point_name, reach_point_type, #channel_type, orientation_type,
           latitude, longitude, horiz_accuracy, reach_point_description,
           created_dt, created_by, modified_dt, modified_by)
  replaceData(reach_point_dt_proxy, post_reach_point_insert_vals)
}, priority = 9999)

#========================================================
# Edit operations: reactives, observers and modals
#========================================================

# # Create reactive to collect input values for insert actions
# fish_location_edit = reactive({
#   # Channel type
#   fish_channel_type_input = input$fish_channel_type_select
#   if ( fish_channel_type_input == "" ) {
#     stream_channel_type_id = NA
#   } else {
#     fish_channel_type_vals = get_fish_channel_type()
#     stream_channel_type_id = fish_channel_type_vals %>%
#       filter(channel_type == fish_channel_type_input) %>%
#       pull(stream_channel_type_id)
#   }
#   # Orientation type
#   fish_orientation_type_input = input$fish_orientation_type_select
#   if ( fish_orientation_type_input == "" ) {
#     location_orientation_type_id = NA
#   } else {
#     fish_orientation_type_vals = get_fish_orientation_type()
#     location_orientation_type_id = fish_orientation_type_vals %>%
#       filter(orientation_type == fish_orientation_type_input) %>%
#       pull(location_orientation_type_id)
#   }
#   edit_fish_location = tibble(fish_location_id = selected_fish_location_data()$fish_location_id,
#                               fish_name = input$fish_name_input,
#                               channel_type = fish_channel_type_input,
#                               stream_channel_type_id = stream_channel_type_id,
#                               orientation_type = fish_orientation_type_input,
#                               location_orientation_type_id = location_orientation_type_id,
#                               latitude = input$fish_latitude_input,
#                               longitude = input$fish_longitude_input,
#                               horiz_accuracy = input$fish_horiz_accuracy_input,
#                               location_description = input$fish_location_description_input,
#                               modified_dt = lubridate::with_tz(Sys.time(), "UTC"),
#                               modified_by = Sys.getenv("USERNAME"))
#   return(edit_fish_location)
# })

# dependent_fish_location_surveys = reactive({
#   fish_loc_id = selected_fish_location_data()$fish_location_id
#   fish_loc_srv = get_fish_location_surveys(fish_loc_id)
#   return(fish_loc_srv)
# })
#
# # Generate values to show check modal
# output$fish_loc_surveys = renderDT({
#   fish_loc_srv = dependent_fish_location_surveys()
#   fish_location_warning = glue("WARNING: All  carcasses, photo's, or observations linked to this ",
#                                "fish location are shown below. Please verify that all data below ",
#                                "should be updated to the new values!")
#   # Generate table
#   datatable(fish_loc_srv,
#             rownames = FALSE,
#             options = list(dom = 't',
#                            scrollX = T,
#                            ordering = FALSE,
#                            initComplete = JS(
#                              "function(settings, json) {",
#                              "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
#                              "}")),
#             caption = htmltools::tags$caption(
#               style = 'caption-side: top; text-align: left; color: blue; width: auto;',
#               htmltools::em(htmltools::strong(fish_location_warning))))
# })
#
# # Generate values to show in modal
# output$fish_location_modal_update_vals = renderDT({
#   fish_location_modal_up_vals = fish_location_edit() %>%
#     select(fish_name, channel_type, orientation_type, latitude,
#            longitude, horiz_accuracy, location_description)
#   # Generate table
#   datatable(fish_location_modal_up_vals,
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
# # Edit modal
# observeEvent(input$fish_loc_edit, {
#   old_fish_location_vals = selected_fish_location_data() %>%
#     mutate(latitude = round(latitude, 6)) %>%
#     mutate(longitude = round(longitude, 6)) %>%
#     select(fish_name, channel_type, orientation_type, latitude,
#            longitude, horiz_accuracy, location_description)
#   old_fish_location_vals[] = lapply(old_fish_location_vals, remisc::set_na)
#   new_fish_location_vals = fish_location_edit() %>%
#     mutate(horiz_accuracy = as.numeric(horiz_accuracy)) %>%
#     mutate(latitude = round(latitude, 6)) %>%
#     mutate(longitude = round(longitude, 6)) %>%
#     select(fish_name, channel_type, orientation_type, latitude,
#            longitude, horiz_accuracy, location_description)
#   new_fish_location_vals[] = lapply(new_fish_location_vals, remisc::set_na)
#   showModal(
#     tags$div(id = "fish_location_update_modal",
#              if ( !length(input$fish_locations_rows_selected) == 1 ) {
#                modalDialog (
#                  size = "m",
#                  title = "Warning",
#                  paste("Please select a row to edit!"),
#                  easyClose = TRUE,
#                  footer = NULL
#                )
#              } else if ( isTRUE(all_equal(old_fish_location_vals, new_fish_location_vals)) ) {
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
#                  title = "Update fish location data to these new values?",
#                  fluidPage (
#                    DT::DTOutput("fish_location_modal_update_vals"),
#                    br(),
#                    DT::DTOutput("fish_loc_surveys"),
#                    br(),
#                    br(),
#                    actionButton("save_fish_loc_edits", "Save changes")
#                  ),
#                  easyClose = TRUE,
#                  footer = NULL
#                )
#              }
#     ))
# })
#
# # Update DB and reload DT
# observeEvent(input$save_fish_loc_edits, {
#   fish_location_update(fish_location_edit())
#   removeModal()
#   post_fish_location_edit_vals = get_fish_location(selected_fish_encounter_data()$fish_encounter_id) %>%
#     select(fish_name, channel_type, orientation_type, latitude,
#            longitude, horiz_accuracy, location_description,
#            created_dt, created_by, modified_dt, modified_by)
#   replaceData(fish_location_dt_proxy, post_fish_location_edit_vals)
# }, priority = 9999)
#
#========================================================
# Delete operations: reactives, observers and modals
#========================================================

# Generate values to show in modal
output$reach_point_modal_delete_vals = renderDT({
  reach_point_modal_del_id = selected_reach_point_data()$location_id
  reach_point_modal_del_vals = get_reach_point(waterbody_id()) %>%
    filter(location_id == reach_point_modal_del_id) %>%
    select(river_mile, reach_point_code, reach_point_name, latitude,
           longitude, horiz_accuracy, reach_point_description)
  # Generate table
  datatable(reach_point_modal_del_vals,
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
reach_point_dependencies = reactive({
  location_id = selected_reach_point_data()$location_id
  reach_point_dep = get_reach_point_dependencies(location_id)
  print(reach_point_dep)
  return(reach_point_dep)
})

dependent_reach_point_surveys = reactive({
  location_id = selected_reach_point_data()$location_id
  reach_point_srv = get_reach_point_surveys(location_id)
  return(reach_point_srv)
})

# Generate values to show check modal
output$reach_point_delete_surveys = renderDT({
  reach_pt_srv = dependent_reach_point_surveys()
  reach_point_dt_msg = glue("All surveys below need to be reassigned to a different reach end point!")
  # Generate table
  datatable(reach_pt_srv,
            rownames = FALSE,
            options = list(dom = 'ltp',
                           pageLength = 10,
                           lengthMenu = c(10, 25, 50),
                           scrollX = T,
                           ordering = FALSE,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
                             "}")),
            caption = htmltools::tags$caption(
              style = 'caption-side: top; text-align: left; color: blue; width: auto;',
              htmltools::em(htmltools::strong(reach_point_dt_msg))))
})

observeEvent(input$reach_point_delete, {
  location_id = selected_reach_point_data()$location_id
  reach_pt_dependencies = reach_point_dependencies()
  table_names = paste0(paste0("'", names(reach_pt_dependencies), "'"), collapse = ", ")
  # Customize the delete message depending on if other entries are linked to location
  if ( ncol(reach_pt_dependencies) > 0L ) {
    reach_point_delete_msg = glue("Other entries in {table_names} have been assigned to this reach point. ",
                                  "All entries below must be reassigned to another river mile before the ",
                                  "point can be deleted.")
  } else {
    reach_point_delete_msg = "Are you sure you want to delete this reach_point from the database?"
  }
  showModal(
    tags$div(id = "reach_point_delete_modal",
             if ( ncol(reach_pt_dependencies) > 0L ) {
               modalDialog (
                 size = "m",
                 title = "Warning",
                 reach_point_delete_msg,
                 fluidPage(
                   br(),
                   DT::DTOutput(("reach_point_delete_surveys"))
                 ),
                 easyClose = TRUE,
                 footer = NULL
               )
             } else {
               modalDialog (
                 size = 'l',
                 title = reach_point_delete_msg,
                 fluidPage (
                   DT::DTOutput("reach_point_modal_delete_vals"),
                   br(),
                   br(),
                   actionButton("delete_reach_point", "Delete reach point")
                 ),
                 easyClose = TRUE,
                 footer = NULL
               )
             }
    ))
})

# Update DB and reload DT
observeEvent(input$delete_reach_point, {
  reach_point_delete(selected_reach_point_data())
  removeModal()
  reach_points_after_delete = get_reach_point(waterbody_id()) %>%
    select(river_mile, reach_point_code, reach_point_name, reach_point_type, #channel_type, orientation_type,
           latitude, longitude, horiz_accuracy, reach_point_description,
           created_dt, created_by, modified_dt, modified_by)
  replaceData(reach_point_dt_proxy, reach_points_after_delete)
}, priority = 9999)


