ui = dashboardPagePlus(
  shinyjs::useShinyjs(),
  header = dash_header,
  sidebar = dash_leftsidebar,
  body = dashboardBody(
    includeCSS("www/salmon_data.css"),
    tabItems(
      tabItem(tabName = "wria_stream",
              fluidRow(
                br(),
                br(),
                boxPlus(
                  title = "Select location and survey year(s)",
                  closable = FALSE,
                  collapsible = TRUE,
                  solidHeader = FALSE,
                  width = NULL,
                  collapsed = FALSE,
                  enable_sidebar = TRUE,
                  sidebar_width = 25,
                  sidebar_start_open = TRUE,
                  sidebar_content = tags$div(
                    div(id = "sthd_image", img(src = "steelhead.png", width = "60%")),
                    br(),
                    br(),
                    br(),
                    br(),
                    div(id = "wria_text", p("Select WRIA and Stream:")),
                    selectizeInput(inputId = "wria_select",
                                   label = NULL,
                                   choices = wria_list,
                                   selected = "23 Upper Chehalis",
                                   width = "100%"),
                    selectizeInput(inputId = "stream_select",
                                   label = NULL,
                                   choices = NULL,
                                   selected = NULL,
                                   width = "100%"),
                    br(),
                    br(),
                    div(id = "year_text", p("Select survey year(s):")),
                    selectizeInput(inputId = "year_select",
                                   label = NULL,
                                   multiple = TRUE,
                                   choices = seq(as.integer(format(Sys.Date(), "%Y")) + 1, 1930L),
                                   #selected = as.integer(format(Sys.Date(), "%Y")),
                                   selected = 2017,
                                   width = "100%")
                  ),
                  leafletOutput("stream_map", height = "800px")
                )
              )
      ),
      tabItem(tabName = "data_entry",
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
                      fish_location_ui
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
                      redd_location_ui
                    ),
                    accordionItem(
                      id = 8,
                      title = "Individual redd",
                      color = "purple",
                      collapsed = TRUE,
                      individual_redd_ui
                    ),
                    accordionItem(
                      id = 9,
                      title = "Redd substrate",
                      color = "purple",
                      collapsed = TRUE,
                      redd_substrate_ui
                    )
                  )
                )
              )
      ),
      tabItem(tabName = "add_reach_point",
              fluidRow(
                br(),
                br(),
                boxPlus(
                  title = "Reach points",
                  closable = FALSE,
                  collapsible = TRUE,
                  solidHeader = FALSE,
                  collapsed = FALSE,
                  reach_point_ui,
                  width = 12
                )
              )
      )
    )
  ),
  title = "Salmon data"
)
