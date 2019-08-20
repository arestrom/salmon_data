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
            waterbody_meas_ui
          )
        )
      ),
      boxPlus(
        title = "Species data",
        closable = FALSE,
        collapsible = TRUE,
        solidHeader = FALSE,
        collapsed = TRUE,
        survey_event_ui,
        width = 12
      ),
      boxPlus(
        title = "Fish encounter",
        closable = FALSE,
        collapsible = TRUE,
        solidHeader = FALSE,
        collapsed = TRUE,
        fish_encounter_ui,
        width = 12,
        accordion(
          accordionItem(
            id = 4,
            title = "Fish location",
            color = "purple",
            collapsed = TRUE,
            "This is some text"
            #fish_location_ui
          ),
          accordionItem(
            id = 5,
            title = "Individual fish",
            color = "purple",
            collapsed = TRUE,
            individual_fish_ui
          ),
          accordionItem(
            id = 6,
            title = "Fish length measurement",
            color = "purple",
            collapsed = TRUE,
            fish_length_measurement_ui
          )
        )
      ),
      boxPlus(
        title = "Redd encounter",
        closable = FALSE,
        collapsible = TRUE,
        solidHeader = FALSE,
        collapsed = TRUE,
        redd_encounter_ui,
        width = 12,
        accordion(
          accordionItem(
            id = 7,
            title = "Redd location",
            color = "purple",
            collapsed = TRUE,
            "This is some text"
            #redd_location_ui
          ),
          accordionItem(
            id = 8,
            title = "Individual redd",
            color = "purple",
            collapsed = TRUE,
            "This is some text"
            #individual_redd_ui
          ),
          accordionItem(
            id = 9,
            title = "Redd substrate",
            color = "purple",
            collapsed = TRUE,
            "This is some text"
            #redd_substrate_ui
          )
        )
      )
    )
  ),
  title = "Salmon data"
)
