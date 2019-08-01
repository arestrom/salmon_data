#=========================================================
# HTML content definition of survey_intent accordian
#========================================================

# Define the survey data content
survey_intent_ui = tags$div(
  actionButton(inputId = "intent_add", label = "New", class = "new_button"),
  actionButton(inputId = "intent_edit", label = "Edit", class = "edit_button"),
  actionButton(inputId = "intent_delete", label = "Delete", class = "delete_button"),
  br(),
  br(),
  uiOutput("intent_species_select", inline = TRUE),
  uiOutput("intent_count_type_select", inline = TRUE),
  br(),
  br(),
  br(),
  DT::DTOutput("survey_intents")
)
