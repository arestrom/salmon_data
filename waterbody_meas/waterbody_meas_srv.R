
output$clarity_type_select = renderUI({
  clarity_type_list = get_clarity_type(pool)$clarity_type
  clarity_type_list = c("", clarity_type_list)
  selectizeInput("clarity_type_select", label = "clarity_type",
                 choices = clarity_type_list, selected = NULL,
                 width = "250px")
})

# Primary DT datatable for survey_intent
output$waterbody_measure = renderDT({
  waterbody_meas_title = glue("Water measurements for {input$stream_select} on ",
                              "{selected_survey_data()$survey_date} from river mile {selected_survey_data()$up_rm} ",
                              "to {selected_survey_data()$lo_rm}")
  waterbody_meas_data = get_waterbody_meas(pool, selected_survey_data()$survey_id) %>%
    select(clarity_type, clarity_meter, flow_cfs, start_temperature, start_tmp_time,
           end_temperature, end_tmp_time, water_ph, created_dt, created_by, modified_dt,
           modified_by)

  # Generate table
  datatable(waterbody_meas_data,
            selection = list(mode = 'single'),
            options = list(dom = 'tp',
                           scrollX = T,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#9eb3d6'});",
                             "}")),
            caption = htmltools::tags$caption(
              style = 'caption-side: top; text-align: left; color: black; width: auto;',
              htmltools::em(htmltools::strong(waterbody_meas_title))))
})

# Create surveys DT proxy object
waterbody_measure_dt_proxy = dataTableProxy(outputId = "waterbody_measure")

