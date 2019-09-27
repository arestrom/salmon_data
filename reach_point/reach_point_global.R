
# Main redd_encounter query
get_reach_point = function(waterbody_id) {
  qry = glue("select loc.location_id, ",
             "lc.location_coordinates_id, ",
             "loc.river_mile_measure as river_mile, ",
             "loc.location_code as reach_point_code, ",
             "loc.location_name as reach_point_name, ",
             "lt.location_type_description as reach_point_type, ",
             #"sc.channel_type_description as channel_type, ",
             #"lo.orientation_type_description as orientation_type, ",
             "st_x(st_transform(lc.geom, 4326)) as longitude, ",
             "st_y(st_transform(lc.geom, 4326)) as latitude, ",
             "lc.horizontal_accuracy as horiz_accuracy, ",
             "loc.location_description as reach_point_description, ",
             "loc.created_datetime as created_date, loc.created_by, ",
             "loc.modified_datetime as modified_date, loc.modified_by ",
             "from location as loc ",
             "left join location_type_lut as lt on loc.location_type_id = lt.location_type_id ",
             # "left join stream_channel_type_lut as sc on loc.stream_channel_type_id = sc.stream_channel_type_id ",
             # "left join location_orientation_type_lut as lo on loc.location_orientation_type_id = lo.location_orientation_type_id ",
             "left join location_coordinates as lc on loc.location_id = lc.location_id ",
             "where loc.waterbody_id = '{waterbody_id}' ",
             "and lt.location_type_description in ('Reach boundary point', 'Section break point')")
  con = poolCheckout(pool)
  reach_points = DBI::dbGetQuery(con, qry)
  poolReturn(con)
  reach_points = reach_points %>%
    mutate(location_id = tolower(location_id)) %>%
    mutate(location_coordinates_id = tolower(location_coordinates_id)) %>%
    mutate(river_mile = round(river_mile, 2)) %>%
    mutate(latitude = round(latitude, 6)) %>%
    mutate(longitude = round(longitude, 6)) %>%
    mutate(created_date = with_tz(created_date, tzone = "America/Los_Angeles")) %>%
    mutate(created_dt = format(created_date, "%m/%d/%Y %H:%M")) %>%
    mutate(modified_date = with_tz(modified_date, tzone = "America/Los_Angeles")) %>%
    mutate(modified_dt = format(modified_date, "%m/%d/%Y %H:%M")) %>%
    select(location_id, location_coordinates_id, reach_point_code,
           river_mile, reach_point_name, reach_point_type, #channel_type, orientation_type,
           latitude, longitude, horiz_accuracy, reach_point_description,
           created_date, created_dt, created_by, modified_date, modified_dt,
           modified_by) %>%
    arrange(created_date)
  return(reach_points)
}

#==========================================================================
# Get generic lut input values
#==========================================================================

# Location type....Some bios distinguish section breaks from reach end points
get_location_type = function() {
  qry = glue("select location_type_id, location_type_description as reach_point_type ",
             "from location_type_lut ",
             "where obsolete_datetime is null ",
             "and location_type_description in ('Reach boundary point', 'Section break point')")
  con = poolCheckout(pool)
  reach_point_type_list = DBI::dbGetQuery(con, qry) %>%
    mutate(location_type_id = tolower(location_type_id)) %>%
    arrange(reach_point_type) %>%
    select(location_type_id, reach_point_type)
  poolReturn(con)
  return(reach_point_type_list)
}

#========================================================
# Insert callback
#========================================================

# Define the insert callback
reach_point_insert = function(new_reach_point_values) {
  new_insert_values = new_reach_point_values
  # Generate location_id
  location_id = remisc::get_uuid(1L)
  created_by = new_insert_values$created_by
  # Pull out location_coordinates table data
  horizontal_accuracy = as.numeric(new_insert_values$horiz_accuracy)
  latitude = new_insert_values$latitude
  longitude = new_insert_values$longitude
  # Pull out location table data
  waterbody_id = new_insert_values$waterbody_id
  wria_id = new_insert_values$wria_id
  location_type_id = new_insert_values$location_type_id
  stream_channel_type_id = new_insert_values$stream_channel_type_id
  location_orientation_type_id = new_insert_values$location_orientation_type_id
  river_mile_measure = new_insert_values$river_mile
  location_code = new_insert_values$reach_point_code
  location_name = new_insert_values$reach_point_name
  location_description = new_insert_values$reach_point_description
  if (is.na(location_code) | location_code == "") { location_code = NA }
  if (is.na(location_name) | location_name == "") { location_name = NA }
  if (is.na(location_description) | location_description == "") { location_description = NA }
  # Insert to location table
  con = poolCheckout(pool)
  insert_rp_result = dbSendStatement(
    con, glue_sql("INSERT INTO location (",
                  "location_id, ",
                  "waterbody_id, ",
                  "wria_id, ",
                  "location_type_id, ",
                  "stream_channel_type_id, ",
                  "location_orientation_type_id, ",
                  "river_mile_measure, ",
                  "location_code, ",
                  "location_name, ",
                  "location_description, ",
                  "created_by) ",
                  "VALUES (",
                  "?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"))
  dbBind(insert_rp_result, list(location_id, waterbody_id, wria_id,
                                location_type_id, stream_channel_type_id,
                                location_orientation_type_id, river_mile_measure,
                                location_code, location_name,
                                location_description, created_by))
  dbGetRowsAffected(insert_rp_result)
  dbClearResult(insert_rp_result)
  # Insert coordinates to location_coordinates
  qry = glue_sql("INSERT INTO location_coordinates ",
                 "(location_id, horizontal_accuracy, geom, created_by) ",
                 "VALUES ({location_id}, {horizontal_accuracy}, ",
                 "ST_Transform(ST_GeomFromText('POINT({longitude} {latitude})', 4326), 2927), ",
                 "{created_by}) ",
                 .con = con)
  # Checkout a connection
  DBI::dbExecute(con, qry)
  poolReturn(con)
}

# #==============================================================
# # Identify fish location surveys prior to update or delete
# #==============================================================
#
# # Identify fish_encounter dependencies prior to delete
# get_fish_location_surveys = function(fish_location_id) {
#   qry = glue("select s.survey_datetime as survey_date, ",
#              "s.observer_last_name as observer, ",
#              "fe.fish_count, mt.media_type_code as media_type, ",
#              "ot.observation_type_name as other_observation_type ",
#              "from location as loc ",
#              "left join fish_encounter as fe on loc.location_id = fe.fish_location_id ",
#              "left join survey_event as se on fe.survey_event_id = se.survey_event_id ",
#              "left join survey as s on se.survey_id = s.survey_id ",
#              "left join media_location as ml on loc.location_id = ml.location_id ",
#              "left join media_type_lut as mt on ml.media_type_id = mt.media_type_id ",
#              "left join other_observation as oo on loc.location_id = oo.observation_location_id ",
#              "left join observation_type_lut as ot on oo.observation_type_id = ot.observation_type_id ",
#              "where loc.location_id = '{fish_location_id}'")
#   con = poolCheckout(pool)
#   fish_loc_surveys = DBI::dbGetQuery(con, qry)
#   poolReturn(con)
#   fish_loc_surveys = fish_loc_surveys %>%
#     mutate(survey_date = with_tz(survey_date, tzone = "America/Los_Angeles")) %>%
#     mutate(survey_date = format(survey_date, "%m/%d/%Y"))
#   return(fish_loc_surveys)
# }
#
# #========================================================
# # Edit location callback
# #========================================================
#
# # Define update callback
# fish_location_update = function(fish_location_edit_values) {
#   edit_values = fish_location_edit_values
#   # Pull out data for location table
#   location_id = edit_values$fish_location_id
#   stream_channel_type_id = edit_values$stream_channel_type_id
#   location_orientation_type_id = edit_values$location_orientation_type_id
#   location_name = edit_values$fish_name
#   location_description = edit_values$location_description
#   if (is.na(location_name) | location_name == "") { location_name = NA }
#   if (is.na(location_description) | location_description == "") { location_description = NA }
#   mod_dt = lubridate::with_tz(Sys.time(), "UTC")
#   mod_by = Sys.getenv("USERNAME")
#   # Pull out data for location_coordinates table
#   horizontal_accuracy = edit_values$horiz_accuracy
#   latitude = edit_values$latitude
#   longitude = edit_values$longitude
#   # Checkout a connection
#   con = poolCheckout(pool)
#   update_result = dbSendStatement(
#     con, glue_sql("UPDATE location SET ",
#                   "stream_channel_type_id = ?, ",
#                   "location_orientation_type_id = ?, ",
#                   "location_name = ?, ",
#                   "location_description = ?, ",
#                   "modified_datetime = ?, ",
#                   "modified_by = ? ",
#                   "where location_id = ?"))
#   dbBind(update_result, list(stream_channel_type_id,
#                              location_orientation_type_id,
#                              location_name, location_description,
#                              mod_dt, mod_by,
#                              location_id))
#   dbGetRowsAffected(update_result)
#   dbClearResult(update_result)
#   # Update coordinates to location_coordinates
#   qry = glue_sql("UPDATE location_coordinates ",
#                  "SET horizontal_accuracy = {horizontal_accuracy}, ",
#                  "geom = ST_Transform(ST_GeomFromText('POINT({longitude} {latitude})', 4326), 2927), ",
#                  "modified_datetime = {mod_dt}, modified_by = {mod_by} ",
#                  "WHERE location_id = {location_id} ",
#                  .con = con)
#   # Checkout a connection
#   DBI::dbExecute(con, qry)
#   poolReturn(con)
# }
#
# #========================================================
# # Identify fish location dependencies prior to delete
# #========================================================
#
# # Identify fish_encounter dependencies prior to delete
# get_fish_location_dependencies = function(fish_location_id) {
#   qry = glue("select ",
#              "count(fe.fish_encounter_id) as fish_encounter, ",
#              "count(ml.media_location_id) as media_location, ",
#              "count(oo.other_observation_id) as other_observation ",
#              "from location as loc ",
#              "left join fish_encounter as fe on loc.location_id = fe.fish_location_id ",
#              "left join media_location as ml on loc.location_id = ml.location_id ",
#              "left join other_observation as oo on loc.location_id = oo.observation_location_id ",
#              "where loc.location_id = '{fish_location_id}'")
#   con = poolCheckout(pool)
#   fish_location_dependents = DBI::dbGetQuery(con, qry)
#   poolReturn(con)
#   has_entries = function(x) any(x > 0L)
#   fish_location_dependents = fish_location_dependents %>%
#     select_if(has_entries)
#   return(fish_location_dependents)
# }
#
# #========================================================
# # Delete callback
# #========================================================
#
# # Define delete callback
# fish_location_delete = function(location_dependencies, delete_values, encounter_values) {
#   fish_location_id = delete_values$fish_location_id
#   fish_encounter_id = encounter_values$fish_encounter_id
#   if ( ncol(location_dependencies) > 1L | location_dependencies$fish_encounter[1] > 1L) {
#     con = poolCheckout(pool)
#     update_result = dbSendStatement(
#       con, glue_sql("UPDATE fish_encounter SET fish_location_id = NULL ",
#                     "WHERE fish_encounter_id = ?"))
#     dbBind(update_result, list(fish_encounter_id))
#     dbGetRowsAffected(update_result)
#     dbClearResult(update_result)
#     poolReturn(con)
#   } else {
#     con = poolCheckout(pool)
#     update_result = dbSendStatement(
#       con, glue_sql("UPDATE fish_encounter SET fish_location_id = NULL ",
#                     "WHERE fish_encounter_id = ?"))
#     dbBind(update_result, list(fish_encounter_id))
#     dbGetRowsAffected(update_result)
#     dbClearResult(update_result)
#     delete_result_one = dbSendStatement(
#       con, glue_sql("DELETE FROM location_coordinates WHERE location_id = ?"))
#     dbBind(delete_result_one, list(fish_location_id))
#     dbGetRowsAffected(delete_result_one)
#     dbClearResult(delete_result_one)
#     delete_result_two = dbSendStatement(
#       con, glue_sql("DELETE FROM location WHERE location_id = ?"))
#     dbBind(delete_result_two, list(fish_location_id))
#     dbGetRowsAffected(delete_result_two)
#     dbClearResult(delete_result_two)
#     poolReturn(con)
#   }
# }
