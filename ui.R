ui = dashboardPagePlus(
  #shinyjs::useShinyjs(),
  header = dash_header,
  sidebar = dash_leftsidebar,
  rightsidebar = dash_rightsidebar,
  body = dashboardBody(
    includeCSS("www/salmon_data.css"),
    map_modal,
    fluidRow(
      boxPlus(
        title = "Header data",
        closable = FALSE,
        collapsible = TRUE,
        solidHeader = FALSE,
        width = 12,
        accordion(
          accordionItem(
            id = 1,
            title = "Survey",
            color = "purple",
            collapsed = FALSE,
            survey_content
          ),
          accordionItem(
            id = 2,
            title = "Survey Comments",
            color = "purple",
            collapsed = TRUE,
            "This is some text!"
          ),
          accordionItem(
            id = 3,
            title = "Species",
            color = "purple",
            collapsed = TRUE,
            "This is some text!"
          )
        )
      )
    )
  ),
  title = "Salmon data"
)
