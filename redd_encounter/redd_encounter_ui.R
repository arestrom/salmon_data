#=============================================================
# HTML content definition of redd_encounter accordion
#=============================================================

# Define the survey data content
redd_encounter_ui = tags$div(
  actionButton(inputId = "redd_enc_add", label = "New", class = "new_button"),
  actionButton(inputId = "redd_enc_edit", label = "Edit", class = "edit_button"),
  actionButton(inputId = "redd_enc_delete", label = "Delete", class = "delete_button"),
  tippy("<i style='color:#1a5e86;padding-left:8px', class='fas fa-info-circle'></i>",
        tooltip = glue("Please enter the redd location data first. Then select the ",
                       "new row of location data and enter the redd count and redd ",
                       "status here. Latitude and longitude are optional.")),
  br(),
  br(),
  timeInput(inputId = "redd_encounter_time_select", "encounter_time", seconds = FALSE),
  uiOutput("redd_status_select", inline = TRUE),
  numericInput(inputId = "redd_count_input", label = "redd_count", value = 0,
               min = 0, step = 1, width = "75px"),
  textAreaInput(inputId = "redd_comment_input", label = "redd_comment", value = "",
                width = "300px", resize = "both"),
  br(),
  br(),
  br(),
  DT::DTOutput("redd_encounters")
)
