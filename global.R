#==============================================================
# Application to edit data for spawning_ground database
#
# Notes:
#  1. Update portion now works with pool.
#  2. Very strange error using the pool and dbplyr query for
#     get_beaches(). I had to wrap output with as.data.frame().
#     I did not get this with the original standard odbc query.
#     Must be something weird with dbplyr and pool. Each row
#     of the datatable held all rows.
#  3. dbplyr does not recognize geometry columns. Need standard
#     text query. Can use odbc package + sf and pull out lat lons.
#  4. For posible use with bs_accordion:
#     https://stackoverflow.com/questions/53642157/shiny-how-to-detect-which-accordion-elements-is-selected/53649246
#  5. For automating right sidebar:
#     https://community.rstudio.com/t/automatic-rightsidebar-popup-when-menuitem-is-clicked-in-shinydashboardplus/16574
#  6. For DT updates: https://stackoverflow.com/questions/56879672/how-to-replacedata-in-dt-rendered-in-r-shiny-using-the-datatable-function
#     https://dev.to/awwsmm/reactive-datatables-in-r-with-persistent-filters-l26
#  7. Can not use reactives for pulling data for use in DTs
#     They do not fire properly to update tables. Just use
#     query functions from global directly. I tested by just
#     putting a reactive between functions in beach_data and
#     got failures right away.
#  8. Do not use rownames = FALSE in DT. Data will not reload
#     when using replaceData() function.
#  9. For sourcing server code: https://r-bar.net/organize-r-shiny-list-source/
#                               https://shiny.rstudio.com/articles/scoping.html
#
#
# ToDo:
#  1. Add animation to buttons as in dt_editor example.
#  2. Add validate and need functions to eliminate crashes
#  3. Make sure users are set up with permissions and dsn's.
#  4. Need to verify deletions are allowed.
#  5. Allow map modal to be resizable and draggable.
#     Try the shinyjqui package:
#     https://github.com/nanxstats/awesome-shiny-extensions
#  6. Need screens to allow entry and edit of RMs and encounters
#     Start with RMs. Use MapEdit. Trigger using RM == "add".
#  7. Set data_source order using number of surveys in category.
#     Can do a query of data to arrange by n, then name.
#  8. Need to use just one function to get survey data....but
#     also add and include the other fields needed for display
#     in time and date slots of DT. DONE !!
#  9. Add modal screen to validate clarity type is chosen along
#     with clarity_meter
# 10. For survey, survey intent and waterbody meas, the modal for
#     no row selected does not fire. Only survey comment works.
#     Adding req to selected_survey_comment_data reactive kills
#     the modal response. But removing req from others causes
#     errors to occur in reactives below.
# 11. For fish_location...wait till I figure out redd_location
#     first, then go back and add fish_location screen. Maybe
#     use map_edit module.
# 12. Next steps....survey_event_update()...create reactive
#     to inspect comparison values for update module
#
# AS 2019-07-15
#==============================================================

# Load libraries
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyTime)
library(shinyjs)
library(bsplus)
library(odbc)
library(glue)
library(tibble)
library(DBI)
library(pool)
library(dplyr)
library(DT)
library(tibble)
library(leaflet)
library(sf)
library(lubridate)
library(remisc)
#library(reactlog)

# Keep connections pane from opening
options("connectionObserver" = NULL)
#options(shiny.reactlog = TRUE)
#reactlogShow()

# Read .rds data
wria_list = readRDS("www/wria_list.rds")
wria_polys = readRDS("www/wria_polys.rds")

# Read content definitions of data-entry screens
# Server code is sourced in server.R  !!!!!!
source("dashboard/dash_header.R")
source("dashboard/dash_leftsidebar.R")
source("dashboard/dash_rightsidebar.R")
source("dashboard/wria_stream_global.R")
source("survey/survey_ui.R")
source("survey/survey_global.R")
source("survey_comment/survey_comment_ui.R")
source("survey_comment/survey_comment_global.R")
source("survey_intent/survey_intent_ui.R")
source("survey_intent/survey_intent_global.R")
source("waterbody_meas/waterbody_meas_ui.R")
source("waterbody_meas/waterbody_meas_global.R")
source("survey_event/survey_event_ui.R")
source("survey_event/survey_event_global.R")
source("fish_encounter/fish_encounter_ui.R")
source("fish_encounter/fish_encounter_global.R")
source("individual_fish/individual_fish_ui.R")
source("individual_fish/individual_fish_global.R")

# Define globals ================================================================

# Define dsns
salmon_db = "local_spawn"

# Set up dsn connection
pool = pool::dbPool(drv = odbc::odbc(), timezone = "UTC", dsn = salmon_db)

# Define functions =============================================================

# Function to close pool
onStop(function() {
  poolClose(pool)
})


