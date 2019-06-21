#==========================================================================
# ShinyDashboardPlus left sidebar
#==========================================================================

dash_leftsidebar = dashboardSidebar(
  sidebarMenu(
    menuItem("Data entry", tabName = "data_entry", icon = icon("database")),
    menuItem("Map edit", tabName = "map_edit", icon = icon("map-marked-alt")),
    menuItem("Mobile upload", tabName = "mobile_upload", icon = icon("sync-alt")),
    menuItem("File upload", tabName = "file_upload", icon = icon("file-upload")),
    menuItem("Data query", tabName = "data_query", icon = icon("share-square")),
    menuItem("Reports", tabName = "reports", icon = icon("microscope")),
    menuItem("About", tabName = "about", icon = icon("info-circle"))
  )
)
