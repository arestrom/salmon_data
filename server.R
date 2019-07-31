#=========================================================
# bsplus ui elements
#=========================================================

# Create the Shiny server
server = function(input, output, session) {

  # WRIA-Stream code
  source("dashboard/wria_stream_srv.R", local = TRUE)

  # Survey code
  source("survey/survey_srv.R", local = TRUE)

  # Survey comment code
  source("survey_comment/survey_comment_srv.R", local = TRUE)

  # Survey intent
  source("survey_intent/survey_intent_srv.R", local = TRUE)

  # # close the R session when Chrome closes
  # session$onSessionEnded(function() {
  #   stopApp()
  #   q("no")
  # })

}
