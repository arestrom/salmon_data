#==========================================================================
# ShinyDashboardPlus left sidebar
#==========================================================================

dash_leftsidebar = dashboardSidebar(
  width = 175,
  sidebarMenu(
    id = "left_sidebar",
    menuItem("Data entry", tabName = "data_entry", icon = icon("database")),
    menuItem("Add reach point", tabName = "add_reach_point", icon = icon("map-pin")),
    menuItem("Import from mobile", tabName = "mobile_import", icon = icon("sync-alt")),
    menuItem("Import from file", tabName = "file_import", icon = icon("file-upload")),
    menuItem("Data query", tabName = "data_query", icon = icon("share-square")),
    menuItem("Reports", tabName = "reports", icon = icon("microscope")),
    menuItem("Waterbody edit", tabName = "waterbody_edit", icon = icon("map-marked-alt")),
    menuItem("About", tabName = "about", icon = icon("info-circle"))
  )
)
