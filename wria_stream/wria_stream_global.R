
get_streams = function(chosen_wria) {
  qry = glue("select distinct wb.waterbody_id, wb.waterbody_display_name as stream_name, ",
             "wb.waterbody_name, wb.latitude_longitude_id as llid, ",
             "wb.stream_catalog_code as cat_code, wr.wria_id, st.stream_id, ",
             "wr.wria_code || ' ' || wr.wria_description as wria_name, st.geom as geometry ",
             "from waterbody_lut as wb ",
             "inner join stream as st on wb.waterbody_id = st.waterbody_id ",
             "inner join wria_lut as wr on st_intersects(st.geom, wr.geom)")
  con = poolCheckout(pool)
  streams_st = sf::st_read(con, query = qry)
  poolReturn(con)
  streams_st = streams_st %>%
    filter(wria_name %in% chosen_wria)
  return(streams_st)
}

get_end_points = function(waterbody_id) {
  qry = glue("select distinct loc.location_id, loc.river_mile_measure as river_mile, ",
             "loc.location_description as rm_desc ",
             "from location as loc ",
             "inner join location_type_lut as lt on loc.location_type_id = lt.location_type_id ",
             "where location_type_description in ('Reach boundary point', 'Section break point') ",
             "and waterbody_id = '{waterbody_id}'")
  con = poolCheckout(pool)
  end_points = DBI::dbGetQuery(con, qry)
  poolReturn(con)
  end_points = end_points %>%
    mutate(location_id = tolower(location_id)) %>%
    arrange(river_mile) %>%
    mutate(rm_label = if_else(is.na(rm_desc), as.character(river_mile),
                              paste0(river_mile, " ", rm_desc))) %>%
    select(location_id, rm_label)
  return(end_points)
}
