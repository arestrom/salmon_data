#=============================================================
# HTML content definition of redd_encounter accordion
#=============================================================

# Define the survey data content
individual_redd_ui = tags$div(
  actionButton(inputId = "ind_redd_add", label = "New", class = "new_button"),
  actionButton(inputId = "ind_redd_edit", label = "Edit", class = "edit_button"),
  actionButton(inputId = "ind_redd_delete", label = "Delete", class = "delete_button"),
  br(),
  br(),
  uiOutput("redd_shape_select", inline = TRUE),
  uiOutput("dewatered_type_select", inline = TRUE),
  numericInput(inputId = "pct_visible_input", label = "pct_visible", value = NA,
               min = 0, max = 100, step = 5, width = "90px"),
  numericInput(inputId = "redd_length_input", label = "redd_length_m", value = NA,
               min = 0, max = 50, step = 5, width = "100px"),
  numericInput(inputId = "redd_width_input", label = "redd_width_m", value = NA,
               min = 0, max = 50, step = 5, width = "100px"),
  numericInput(inputId = "redd_depth_input", label = "redd_depth_m", value = NA,
               min = 0, max = 10, step = 1, width = "100px"),
  numericInput(inputId = "tailspill_height_input", label = "tailspill_height_m", value = NA,
               min = 0, max = 10, step = 1, width = "125px"),
  numericInput(inputId = "pct_superimposed_input", label = "pct_superimposed", value = NA,
               min = 0, max = 100, step = 5, width = "125px"),
  numericInput(inputId = "pct_degraded_input", label = "pct_degraded", value = NA,
               min = 0, max = 100, step = 5, width = "100px"),
  textInput(inputId = "superimposed_redd_name_input", label = "superimposed_redd_name", width = "175px"),
  textAreaInput(inputId = "ind_redd_comment_input", label = "individual_redd_comment", value = "",
                width = "300px", resize = "both"),
  br(),
  br(),
  br(),
  DT::DTOutput("individual_redds"),
  verbatimTextOutput("chk_ind_redd_edit")
)
