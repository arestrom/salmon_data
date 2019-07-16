

# Create the Shiny server
server = function(input, output, session) {

  #=========================================================================
  # Source server-side inputs
  #=========================================================================

  # Survey_inputs
  source("survey_inputs.R", local = TRUE)

  # Survey_comment_inputs
  source("survey_comment_inputs.R", local = TRUE)

  #=========================================================================
  # Source WRIA-Stream stuff
  #=========================================================================

  # Survey_comment_inputs
  source("wria_stream.R", local = TRUE)

  #=========================================================================
  # Source survey level crud stuff
  #=========================================================================

  # Survey_comment_inputs
  source("survey_crud.R", local = TRUE)

  #=========================================================================
  # Source survey_comment level crud stuff
  #=========================================================================


  # # close the R session when Chrome closes
  # session$onSessionEnded(function() {
  #   stopApp()
  #   q("no")
  # })

}
