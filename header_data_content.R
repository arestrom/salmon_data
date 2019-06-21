#=========================================================
# HTML content definition of header accordian
#========================================================

# Define the header content
header_data_content = tags$div(
  actionButton(inputId = "header_add", label = "New", class = "new_button"),
  actionButton(inputId = "header_edit", label = "Edit", class = "edit_button"),
  actionButton(inputId = "header_delete", label = "Delete", class = "delete_button"),
  br(),
  br(),
  selectizeInput(inputId = "station_input", label = "tide_station",
                 choices = c("", "Seattle", "Port Townsend"),
                 width = "150px"),
  textInput(inputId = "beach_name_input", label = "beach_name", value = NA, width = "200px"),
  textInput(inputId = "beach_desc_input", label = "beach_desc", value = NA, width = "200px"),
  numericInput(inputId = "low_min_input", label = "low_corr_min", value = NA, width = "100px"),
  numericInput(inputId = "low_ft_input", label = "low_corr_ft", value = NA, width = "100px"),
  numericInput(inputId = "high_min_input", label = "high_corr_min", value = NA, width = "100px"),
  numericInput(inputId = "high_ft_input", label = "high_corr_ft", value = NA, width = "100px"),
  br(),
  br(),
  br(),
  DT::DTOutput("surveys")
)
