ui = dashboardPagePlus(
  #shinyjs::useShinyjs(),
  header = dash_header,
  sidebar = dash_leftsidebar,
  rightsidebar = dash_rightsidebar,
  body = dashboardBody(
    includeCSS("www/salmon_data.css"),
    map_modal,
    fluidRow(
      box(
        title = "Enter, edit, or delete data",
        width = 12,
        accordion(
          accordionItem(
            id = 1,
            title = "Header data",
            color = "info",
            collapsed = TRUE,
            header_data_content
          ),
          accordionItem(
            id = 2,
            title = "Species data",
            color = "info",
            collapsed = FALSE,
            "This is some text!"
          ),
          accordionItem(
            id = 3,
            title = "Live data",
            color = "info",
            collapsed = FALSE,
            "This is some text!"
          )
        )
      )
    )
  ),
  title = "Salmon data"
)
