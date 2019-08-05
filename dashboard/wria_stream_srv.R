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

# # Display the map modal to select stream
# observeEvent(input$show_map_stream, {
#   showModal(
#     tags$div(id = "map_modal",
#              modalDialog (
#                size = 'l',
#                title = NULL,
#                fluidPage (
#                  leafletOutput("stream_map", height = "700px")
#                ),
#                easyClose = TRUE,
#                footer = NULL
#              )
#     )
#   )
# })

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

# Update Stream input...Don't use req. If you do, select will stay NULL
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

# Generate year values as a reactive
year_vals = reactive({
  req(input$year_select)
  year_vals = paste0(input$year_select, collapse = ", ")
  return(year_vals)
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
