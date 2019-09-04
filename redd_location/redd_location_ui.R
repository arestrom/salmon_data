#=============================================================
# HTML content definition of redd_encounter accordion
#=============================================================

# Define the survey data content
redd_location_ui = tags$div(
  actionButton(inputId = "redd_loc_add", label = "New", class = "new_button"),
  actionButton(inputId = "redd_loc_edit", label = "Edit", class = "edit_button"),
  actionButton(inputId = "redd_loc_delete", label = "Delete", class = "delete_button"),
  actionButton(inputId = "redd_loc_map", label = "Use map", class = "map_button"),
  br(),
  br(),
  textInput(inputId = "redd_name_input", label = "redd_name", width = "125px"),
  uiOutput("channel_type_select", inline = TRUE),
  uiOutput("orientation_type_select", inline = TRUE),
  numericInput(inputId = "latitude_input", label = "latitude", value = NULL,
               min = 45, max = 49, width = "125px"),
  numericInput(inputId = "longitude_input", label = "longitude", value = NULL,
               min = -124, max = -116, width = "125px"),
  numericInput(inputId = "horiz_accuracy_input", label = "horiz_accuracy", value = NULL,
               min = 0, width = "100px"),
  textAreaInput(inputId = "location_description_input", label = "location_description", value = "",
                width = "300px", resize = "both"),
  br(),
  br(),
  br(),
  DT::DTOutput("redd_locations")
)
