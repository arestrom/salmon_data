ui = dashboardPagePlus(
  shinyjs::useShinyjs(),
  header = dash_header,
  sidebar = dash_leftsidebar,
  rightsidebar = dash_rightsidebar,
  body = dashboardBody(
    includeCSS("www/salmon_data.css"),
    map_modal,
    fluidRow(
      br(),
      br(),
      boxPlus(
        title = "Survey",
        closable = FALSE,
        collapsible = TRUE,
        solidHeader = FALSE,
        collapsed = FALSE,
        survey_ui,
        width = 12,
        accordion(
          accordionItem(
            id = 1,
            title = "Survey comments",
            color = "purple",
            collapsed = TRUE,
            survey_comment_ui
          ),
          accordionItem(
            id = 2,
            title = "Survey intent",
            color = "purple",
            collapsed = TRUE,
            survey_intent_ui
          ),
          accordionItem(
            id = 3,
            title = "Waterbody measurements",
            color = "purple",
            collapsed = TRUE,
            "This is some text!"
          )
        )
      ),
      boxPlus(
        title = "Fish encounter",
        closable = FALSE,
        collapsible = TRUE,
        solidHeader = FALSE,
        collapsed = TRUE,
        #fish_encounter_content,
        width = 12,
        "This is some text"
      ),
      boxPlus(
        title = "Redd encounter",
        closable = FALSE,
        collapsible = TRUE,
        solidHeader = FALSE,
        collapsed = TRUE,
        #redd_encounter_content,
        width = 12,
        "This is some text"
      )
    )
  ),
  title = "Salmon data"
)
