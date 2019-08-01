
output$intent_species_select = renderUI({
  species_list = get_species(pool)$species
  species_list = c("", species_list)
  selectizeInput("intent_species_select", label = "species",
                 choices = species_list, selected = NULL,
                 width = "150px")
})

output$intent_count_type_select = renderUI({
  count_type_list = get_count_type(pool)$count_type
  count_type_list = c("", count_type_list)
  selectizeInput("intent_count_type_select", label = "count_type",
                 choices = count_type_list, selected = NULL,
                 width = "100px")
})

# Primary DT datatable for survey_intent
output$survey_intents = renderDT({
  survey_intent_title = glue("Survey intent for {input$stream_select} on ",
                             "{selected_survey_data()$survey_date} from river mile {selected_survey_data()$up_rm} ",
                             "to {selected_survey_data()$lo_rm}")
  survey_intent_data = get_survey_intent(pool, selected_survey_data()$survey_id) %>%
    select(species, count_type, created_dt, created_by, modified_dt, modified_by)

  # Generate table
  datatable(survey_intent_data,
            selection = list(mode = 'single'),
            options = list(dom = 'ltp',
                           pageLength = 5,
                           lengthMenu = c(1, 5, 10, 20),
                           scrollX = T,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
                             "}")),
            caption = htmltools::tags$caption(
              style = 'caption-side: top; text-align: left; color: black; width: auto;',
              htmltools::em(htmltools::strong(survey_intent_title))))
})

# Create surveys DT proxy object
survey_intent_dt_proxy = dataTableProxy(outputId = "survey_intents")












# STOPPED HERE...JUST NEED TO WIRE IN REST OF Intent server code
