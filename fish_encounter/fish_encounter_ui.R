#=============================================================
# HTML content definition of fish_encounter accordion
#=============================================================

# Define the survey data content
fish_encounter_ui = tags$div(
  actionButton(inputId = "fish_encounter_add", label = "New", class = "new_button"),
  actionButton(inputId = "fish_encounter_edit", label = "Edit", class = "edit_button"),
  actionButton(inputId = "fish_encounter_delete", label = "Delete", class = "delete_button"),
  br(),
  br(),
  timeInput(inputId = "fish_encounter_time_select", "encounter_time", seconds = FALSE),
  numericInput(inputId = "fish_count_input", label = "fish_count", value = 1,
               min = 0, step = 1, width = "75px"),
  uiOutput("fish_status_select", inline = TRUE),
  uiOutput("sex_select", inline = TRUE),
  uiOutput("maturity_select", inline = TRUE),
  uiOutput("origin_select", inline = TRUE),
  uiOutput("cwt_status_select", inline = TRUE),
  uiOutput("clip_status_select", inline = TRUE),
  uiOutput("fish_behavior_select", inline = TRUE),
  selectizeInput(inputId = "prev_counted_select", label = "prev_counted?",
                 choices = c("No", "Yes"), selected = "No", width = "90px"),
  br(),
  br(),
  br(),
  DT::DTOutput("fish_encounters")
  #verbatimTextOutput("chk_vals")
)