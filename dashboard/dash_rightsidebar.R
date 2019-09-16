#==========================================================================
# ShinyDashboardPlus right sidebar
#==========================================================================

dash_rightsidebar = rightSidebar(
  width = 200,
  div(id = "sthd_image", img(src = "steelhead.png", width = "90%")),
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
  actionButton(inputId = "show_map_stream", label = "or use map",
               class = "map_button", width = "100%"),
  br(),
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
)
