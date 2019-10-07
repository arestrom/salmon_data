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
# 10. For wria_stream code to work...needed to use eventReactive(), Solved problem
#     with weird firing of querys and leaked pool.
#
#
# ToDo:
#  1. Add animation to buttons as in dt_editor example.
#  2. Add validate and need functions to eliminate crashes
#  3. Make sure users are set up with permissions and dsn's.
#  4. Allow map modal to be resizable and draggable.
#     Try the shinyjqui package:
#     https://github.com/nanxstats/awesome-shiny-extensions
#  5. Set data_source order using number of surveys in category.
#     Can do a query of data to arrange by n, then name.
#  6. Add modal screen to validate clarity type is chosen along
#     with clarity_meter
#  7. For survey, survey intent and waterbody meas, the modal for
#     no row selected does not fire. Only survey comment works.
#     Adding req to selected_survey_comment_data reactive kills
#     the modal response. But removing req from others causes
#     errors to occur in reactives below.
#  8. Need to dump and reload all lakes data...using new layer
#     Dale is creating for me. Look for intersecting polygons
#     before uploading and dump any duplicates. Dale's layer
#     is omitting the marshland.
#  9. Need to scan all existing stream geometry for overlapping
#     segments...then dump those and reset sequences. Need to
#     add code to look for overlapping segments in scripts to
#     upload all geometry. Join line segments to one line per llid.
# 10. Add input$delete observers to all shinyjs disable code
#     See example in redd_substrate_srv code.
# 11. Check that select inputs are ordered optimally. Use
#     example code in redd_substrate_global as example to
#     order by levels.
# 12. If tracking of individual_carcass over time is needed,
#     we can add fish_name_select to fish_encounter ui. Set
#     up similar to redd-tracking in redd_encounter/location.
# 13. Need to add code to edit modals to make sure all
#     required fields have values entered. See fish_location.
#     Or use validate...need?
# 24. Wrap all database operations in transactions before
#     porting to the fish.spawning_ground schema.
#     See examples in dbWithTransactions() DBI docs.
# 15. In survey code...add incomplete_survey_type and
#     data_source_unit lut values as selects
# 16. Add code to limit the number of possible length
#     measurements to the number of items in length_type
#     lut list.
# 17. Verify all screens...especially edit, do not remove
#     required values by backspacing and updating.
# 18. Enable preloader:
#     https://cran.r-project.org/web/packages/shinydashboardPlus/vignettes/css-preloader.html
# 19. Change select for year in rightsidebar to
#     shinywidgets pickerSelect multiple.
# 20. Need css for radius on textInput boxes
# 21. Get rid of extra channel and orientation lut function
#     for fish in fish_location_global.R. Can reuse
#     function from redd_location.
# 22. May want to edit reach_point_srv.R to allow
#     multiple edits to point on reach_point_map
#     without having to update either lat-lon inputs
#     or selecting a different row in DT. Could use
#     model for wria_stream code instead of modal.
# 23. For editing reach points...only allow editing coordinates
#     for one year previous. Otherwise send request to data manager.
#     Done....Can also suggest adding new point with one decimal
#     difference in river_mile. Then leave historical data alone.
# 24. Find css to narrow sidebar in header boxplus.
#
# AS 2019-10-04
#==============================================================

# Load libraries
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyTime)
library(shinyjs)
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
library(leaflet.extras)
#library(reactlog)

# Keep connections pane from opening
options("connectionObserver" = NULL)
#options(shiny.reactlog = TRUE)
#reactlogShow()

# Read .rds data
#wria_list = readRDS("www/wria_list.rds")
wria_polys = readRDS("www/wria_polys.rds")

# Read content definitions of data-entry screens
source("dashboard/dash_header.R")
source("dashboard/dash_leftsidebar.R")
source("wria_stream/wria_stream_ui.R")
source("wria_stream/wria_stream_global.R")
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
source("fish_length_measurement/fish_length_measurement_ui.R")
source("fish_length_measurement/fish_length_measurement_global.R")
source("redd_encounter/redd_encounter_ui.R")
source("redd_encounter/redd_encounter_global.R")
source("redd_location/redd_location_ui.R")
source("redd_location/redd_location_global.R")
source("individual_redd/individual_redd_ui.R")
source("individual_redd/individual_redd_global.R")
source("redd_substrate/redd_substrate_ui.R")
source("redd_substrate/redd_substrate_global.R")
source("fish_location/fish_location_ui.R")
source("fish_location/fish_location_global.R")
source("reach_point/reach_point_ui.R")
source("reach_point/reach_point_global.R")

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


