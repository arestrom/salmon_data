
#=======================================================================================
# Experimental CRUD app for spawning_ground database operations
#=======================================================================================

# Define UI for application that draws a histogram
shinyUI(navbarPage(theme = shinytheme("sandstone"),
                   "Salmon data",
                   id = "tab_being_displayed",
                   tabPanel("Data entry",
                            useShinyjs(),
                            sidebarLayout(
                              div(id = "Sidebar", sidebarPanel(width = 3,
                                           #align = "center",
                                           div(id = "sthd_image", img(src = "steelhead.png", width = "80%")),
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
                                           actionButton(inputId = "show_map_stream",
                                                        label = "or use map",
                                                        width = "100%"),
                                           br(),
                                           br(),
                                           br(),
                                           div(id = "year_text", p("Select year and year type:")),
                                           selectizeInput(inputId = "year_select",
                                                          label = NULL,
                                                          choices = seq(as.integer(format(Sys.Date(), "%Y")), 1930L),
                                                          selected = as.integer(format(Sys.Date(), "%Y")),
                                                          width = "100%"),
                                           radioButtons(inputId = "year_type",
                                                        label = NULL,
                                                        choices = c("Run year" = "run_year",
                                                                    "Calendar year" = "cal_year"),
                                                        inline = TRUE, width = "100%"),
                                           br(),
                                           br(),
                                           br(),
                                           div(id = "show_header_input",
                                               bs_button(label = "Header data", button_type = "primary") %>%
                                                 bs_attach_collapse("header")),
                                           br(),
                                           br(),
                                           div(id = "show_species_input",
                                               bs_button(label = "Species data", button_type = "primary") %>%
                                                 bs_attach_collapse("beach")))
                              ),
                              mainPanel(width = 9,
                                        includeCSS("www/salmon_data.css"),
                                        bs_collapse(
                                          id = "header",
                                          content = tags$div(
                                            actionButton(inputId = "toggle_sidebar", label = "Toggle sidebar", class = "select_button"),
                                            actionButton(inputId = "header_add", label = "New", class = "new_button"),
                                            actionButton(inputId = "header_edit", label = "Edit", class = "edit_button"),
                                            actionButton(inputId = "header_delete", label = "Delete", class = "delete_button"),
                                            br(),
                                            br(),
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
                                        )
                              )
                            )
                   ),

                   tabPanel("Query",
                            fluidRow(
                              column(width = 3,
                                     img(src = "aerial.jpg", width = "85%")
                              ),
                              column(offset = 1,
                                     width = 8,
                                     img(src = "ocean_two.jpg", width = "60%")
                              )
                            )
                   ),

                   tabPanel("About",
                            fluidRow(
                              column(width = 3,
                                     img(src = "aerial.jpg", width = "85%"),
                                     br(),
                                     br(),
                                     tags$div(includeMarkdown("www/credits.Rmd"), id = "rmd_credits")
                              ),
                              column(offset = 1,
                                     width = 8,
                                     img(src = "ocean_two.jpg", width = "60%"),
                                     br(),
                                     tags$div(includeMarkdown("www/about.Rmd"), id = "rmd_about")
                              )
                            )
                   )
        ))
