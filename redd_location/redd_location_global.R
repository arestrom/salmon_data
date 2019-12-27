
# Main redd_location query
# Need stream, RMs, species, survey_date, time-span (four months prior)
get_redd_locations = function(waterbody_id, up_rm, lo_rm, survey_date, species_id) {
  # Define query for new redd locations...no attached surveys yet...finds new entries or orphan entries
  qry = glue("select rloc.location_id as redd_location_id, ",
             "rloc.location_name as redd_name, ",
             "lc.location_coordinates_id, ",
             "st_x(st_transform(lc.geom, 4326)) as longitude, ",
             "st_y(st_transform(lc.geom, 4326)) as latitude, ",
             "lc.horizontal_accuracy as horiz_accuracy, ",
             "sc.channel_type_description as channel_type, ",
             "lo.orientation_type_description as orientation_type, ",
             "rloc.location_description, ",
             "rloc.created_datetime as created_date, rloc.created_by, ",
             "rloc.modified_datetime as modified_date, rloc.modified_by ",
             "from location as rloc ",
             "left join location_coordinates as lc on rloc.location_id = lc.location_id ",
             "left join stream_channel_type_lut as sc on rloc.stream_channel_type_id = sc.stream_channel_type_id ",
             "left join location_orientation_type_lut as lo on rloc.location_orientation_type_id = lo.location_orientation_type_id ",
             "left join redd_encounter as rd on rloc.location_id = rd.redd_location_id ",
             "left join location_type_lut as lt on rloc.location_type_id = lt.location_type_id ",
             "where rloc.waterbody_id = '{waterbody_id}' ",
             "and lt.location_type_description = 'Redd encounter' ",
             "and rd.redd_encounter_id is null")
  con = poolCheckout(pool)
  new_redd_locations = DBI::dbGetQuery(con, qry)
  poolReturn(con)
  # Define query for redd locations already tied to surveys
  qry = glue("select s.survey_datetime as survey_date, se.species_id, ",
             "uploc.river_mile_measure as up_rm, loloc.river_mile_measure as lo_rm, ",
             "rloc.location_id as redd_location_id, ",
             "rloc.location_name as redd_name, ",
             "rs.redd_status_short_description as redd_status, ",
             "lc.location_coordinates_id, ",
             "st_x(st_transform(lc.geom, 4326)) as longitude, ",
             "st_y(st_transform(lc.geom, 4326)) as latitude, ",
             "lc.horizontal_accuracy as horiz_accuracy, ",
             "sc.channel_type_description as channel_type, ",
             "lo.orientation_type_description as orientation_type, ",
             "rloc.location_description, ",
             "rloc.created_datetime as created_date, rloc.created_by, ",
             "rloc.modified_datetime as modified_date, rloc.modified_by ",
             "from survey as s ",
             "inner join survey_event as se on s.survey_id = se.survey_id ",
             "inner join location as uploc on s.upper_end_point_id = uploc.location_id ",
             "inner join location as loloc on s.lower_end_point_id = loloc.location_id ",
             "inner join redd_encounter as rd on se.survey_event_id = rd.survey_event_id ",
             "inner join redd_status_lut as rs on rd.redd_status_id = rs.redd_status_id ",
             "inner join location as rloc on rd.redd_location_id = rloc.location_id ",
             "left join location_coordinates as lc on rloc.location_id = lc.location_id ",
             "left join stream_channel_type_lut as sc on rloc.stream_channel_type_id = sc.stream_channel_type_id ",
             "left join location_orientation_type_lut as lo on rloc.location_orientation_type_id = lo.location_orientation_type_id ",
             "where loloc.waterbody_id = '{waterbody_id}' ",
             "and uploc.river_mile_measure <= {up_rm} ",
             "and loloc.river_mile_measure >= {lo_rm} ",
             "and se.species_id = '{species_id}' ",
             "and s.survey_datetime < '{survey_date}'::date + interval '1 day' ",
             "and s.survey_datetime >= '{survey_date}'::date - interval '4 months' ",
             "and not redd_status_short_description in ('Previous redd, not visible')")
  con = poolCheckout(pool)
  old_redd_locations = DBI::dbGetQuery(con, qry)
  poolReturn(con)
  redd_locations = bind_rows(new_redd_locations, old_redd_locations) %>%
    mutate(latitude = round(latitude, 7)) %>%
    mutate(longitude = round(longitude, 7)) %>%
    mutate(survey_date = with_tz(survey_date, tzone = "America/Los_Angeles")) %>%
    mutate(survey_dt = format(survey_date, "%m/%d/%Y")) %>%
    mutate(created_date = with_tz(created_date, tzone = "America/Los_Angeles")) %>%
    mutate(created_dt = format(created_date, "%m/%d/%Y %H:%M")) %>%
    mutate(modified_date = with_tz(modified_date, tzone = "America/Los_Angeles")) %>%
    mutate(modified_dt = format(modified_date, "%m/%d/%Y %H:%M")) %>%
    select(redd_location_id, location_coordinates_id,
           survey_date, survey_dt, redd_name, redd_status,
           latitude, longitude, horiz_accuracy, channel_type,
           orientation_type, location_description, created_date,
           created_dt, created_by, modified_date, modified_dt,
           modified_by) %>%
    arrange(created_date)
  return(redd_locations)
}

#==========================================================================
# Get just the redd_coordinates
#==========================================================================

# # Redd_coordinates query...for setting stream centroid when redd_encounter row is selected
# get_redd_coordinates = function(redd_encounter_id) {
#   qry = glue("select lc.location_id as redd_location_id, ",
#              "lc.location_coordinates_id, ",
#              "st_x(st_transform(lc.geom, 4326)) as longitude, ",
#              "st_y(st_transform(lc.geom, 4326)) as latitude, ",
#              "lc.horizontal_accuracy as horiz_accuracy, ",
#              "lc.created_datetime as created_date, lc.created_by, ",
#              "lc.modified_datetime as modified_date, lc.modified_by ",
#              "from redd_encounter as rd ",
#              "inner join location as loc on rd.redd_location_id = loc.location_id ",
#              "inner join location_coordinates as lc on loc.location_id = lc.location_id ",
#              "where rd.redd_encounter_id = '{redd_encounter_id}'")
#   con = poolCheckout(pool)
#   redd_coordinates = DBI::dbGetQuery(con, qry)
#   poolReturn(con)
#   redd_coordinates = redd_coordinates %>%
#     mutate(latitude = round(latitude, 6)) %>%
#     mutate(longitude = round(longitude, 6)) %>%
#     mutate(created_date = with_tz(created_date, tzone = "America/Los_Angeles")) %>%
#     mutate(created_dt = format(created_date, "%m/%d/%Y %H:%M")) %>%
#     mutate(modified_date = with_tz(modified_date, tzone = "America/Los_Angeles")) %>%
#     mutate(modified_dt = format(modified_date, "%m/%d/%Y %H:%M")) %>%
#     select(redd_location_id, location_coordinates_id,
#            latitude, longitude, horiz_accuracy, created_date,
#            created_dt, created_by, modified_date, modified_dt,
#            modified_by) %>%
#     arrange(created_date)
#   return(redd_coordinates)
# }

# Redd_coordinates query...for setting redd_marker when redd_location row is selected
get_redd_coordinates = function(redd_location_id) {
  qry = glue("select loc.location_id, lc.location_coordinates_id, ",
             "st_x(st_transform(lc.geom, 4326)) as longitude, ",
             "st_y(st_transform(lc.geom, 4326)) as latitude, ",
             "lc.horizontal_accuracy as horiz_accuracy, ",
             "lc.created_datetime as created_date, lc.created_by, ",
             "lc.modified_datetime as modified_date, lc.modified_by ",
             "from location as loc ",
             "inner join location_coordinates as lc on loc.location_id = lc.location_id ",
             "where loc.location_id = '{redd_location_id}'")
  con = poolCheckout(pool)
  redd_coordinates = DBI::dbGetQuery(con, qry)
  poolReturn(con)
  redd_coordinates = redd_coordinates %>%
    mutate(latitude = round(latitude, 7)) %>%
    mutate(longitude = round(longitude, 7)) %>%
    mutate(created_date = with_tz(created_date, tzone = "America/Los_Angeles")) %>%
    mutate(created_dt = format(created_date, "%m/%d/%Y %H:%M")) %>%
    mutate(modified_date = with_tz(modified_date, tzone = "America/Los_Angeles")) %>%
    mutate(modified_dt = format(modified_date, "%m/%d/%Y %H:%M")) %>%
    select(redd_location_id = location_id, location_coordinates_id,
           latitude, longitude, horiz_accuracy, created_date,
           created_dt, created_by, modified_date, modified_dt,
           modified_by) %>%
    arrange(created_date)
  return(redd_coordinates)
}

#==========================================================================
# Get generic lut input values
#==========================================================================

# Redd status
get_channel_type = function() {
  qry = glue("select stream_channel_type_id, channel_type_description as channel_type ",
             "from stream_channel_type_lut ",
             "where obsolete_datetime is null")
  con = poolCheckout(pool)
  channel_type_list = DBI::dbGetQuery(con, qry) %>%
    arrange(channel_type) %>%
    select(stream_channel_type_id, channel_type)
  poolReturn(con)
  return(channel_type_list)
}

# Redd status
get_orientation_type = function() {
  qry = glue("select location_orientation_type_id, orientation_type_description as orientation_type ",
             "from location_orientation_type_lut ",
             "where obsolete_datetime is null")
  con = poolCheckout(pool)
  orientation_type_list = DBI::dbGetQuery(con, qry) %>%
    arrange(orientation_type) %>%
    select(location_orientation_type_id, orientation_type)
  poolReturn(con)
  return(orientation_type_list)
}

#==========================================================================
# Get centroid of waterbody to use in interactive redd_map
#==========================================================================

get_stream_centroid = function(waterbody_id) {
  qry = glue("select DISTINCT waterbody_id, ",
             "ST_X(ST_Transform(ST_Centroid(geom), 4326)) as center_lon, ",
             "ST_Y(ST_Transform(ST_Centroid(geom), 4326)) as center_lat ",
             "from stream ",
             "where waterbody_id = '{waterbody_id}'")
  con = poolCheckout(pool)
  stream_centroid = DBI::dbGetQuery(con, qry)
  poolReturn(con)
  return(stream_centroid)
}

#========================================================
# Insert callback
#========================================================

# Define the insert callback
redd_location_insert = function(new_redd_location_values) {
  new_insert_values = new_redd_location_values
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
  location_name = new_insert_values$redd_name
  location_description = new_insert_values$location_description
  if (is.na(location_name) | location_name == "") { location_name = NA }
  if (is.na(location_description) | location_description == "") { location_description = NA }
  mod_dt = lubridate::with_tz(Sys.time(), "UTC")
  mod_by = Sys.getenv("USERNAME")
  # Insert to location table
  con = poolCheckout(pool)
  insert_loc_result = dbSendStatement(
    con, glue_sql("INSERT INTO location (",
                  "location_id, ",
                  "waterbody_id, ",
                  "wria_id, ",
                  "location_type_id, ",
                  "stream_channel_type_id, ",
                  "location_orientation_type_id, ",
                  "location_name, ",
                  "location_description, ",
                  "created_by) ",
                  "VALUES (",
                  "$1, $2, $3, $4, $5, $6, $7, $8, $9)"))
  dbBind(insert_loc_result, list(location_id, waterbody_id, wria_id,
                                 location_type_id, stream_channel_type_id,
                                 location_orientation_type_id, location_name,
                                 location_description, created_by))
  dbGetRowsAffected(insert_loc_result)
  dbClearResult(insert_loc_result)
  if (!is.na(latitude) & !is.na(longitude) ) {
    # Insert coordinates to location_coordinates
    qry = glue_sql("INSERT INTO location_coordinates ",
                   "(location_id, horizontal_accuracy, geom, created_by) ",
                   "VALUES ({location_id}, {horizontal_accuracy}, ",
                   "ST_Transform(ST_GeomFromText('POINT({longitude} {latitude})', 4326), 2927), ",
                   "{created_by}) ",
                   .con = con)
    # Checkout a connection
    DBI::dbExecute(con, qry)
  }
  poolReturn(con)
}

#==============================================================
# Identify redd location surveys prior to update or delete
#==============================================================

# Identify fish_encounter dependencies prior to delete
get_redd_location_surveys = function(redd_location_id) {
  qry = glue("select s.survey_datetime as survey_date, ",
             "s.observer_last_name as observer, ",
             "re.redd_count, mt.media_type_code as media_type, ",
             "ot.observation_type_name as other_observation_type ",
             "from location as loc ",
             "left join redd_encounter as re on loc.location_id = re.redd_location_id ",
             "left join survey_event as se on re.survey_event_id = se.survey_event_id ",
             "left join survey as s on se.survey_id = s.survey_id ",
             "left join media_location as ml on loc.location_id = ml.location_id ",
             "left join media_type_lut as mt on ml.media_type_id = mt.media_type_id ",
             "left join other_observation as oo on loc.location_id = oo.observation_location_id ",
             "left join observation_type_lut as ot on oo.observation_type_id = ot.observation_type_id ",
             "where loc.location_id = '{redd_location_id}'")
  con = poolCheckout(pool)
  redd_loc_surveys = DBI::dbGetQuery(con, qry)
  poolReturn(con)
  redd_loc_surveys = redd_loc_surveys %>%
    mutate(survey_date = with_tz(survey_date, tzone = "America/Los_Angeles")) %>%
    mutate(survey_date = format(survey_date, "%m/%d/%Y"))
  return(redd_loc_surveys)
}

#========================================================
# Edit location callback
#========================================================

# # Define update callback
# redd_location_update = function(redd_location_edit_values) {
#   edit_values = redd_location_edit_values
#   # Pull out data for location table
#   location_id = edit_values$redd_location_id
#   stream_channel_type_id = edit_values$stream_channel_type_id
#   location_orientation_type_id = edit_values$location_orientation_type_id
#   location_name = edit_values$redd_name
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
#                   "stream_channel_type_id = $1, ",
#                   "location_orientation_type_id = $2, ",
#                   "location_name = $3, ",
#                   "location_description = $4, ",
#                   "modified_datetime = $5, ",
#                   "modified_by = $6 ",
#                   "where location_id = $7"))
#   dbBind(update_result, list(stream_channel_type_id,
#                              location_orientation_type_id,
#                              location_name, location_description,
#                              mod_dt, mod_by,
#                              location_id))
#   dbGetRowsAffected(update_result)
#   dbClearResult(update_result)
#   # Update coordinates to location_coordinates if entry exists...otherwise insert new coords
#   if ( !is.na(latitude) & !is.na(longitude) ) {
#     qry = glue_sql("UPDATE location_coordinates ",
#                    "SET horizontal_accuracy = {horizontal_accuracy}, ",
#                    "geom = ST_Transform(ST_GeomFromText('POINT({longitude} {latitude})', 4326), 2927), ",
#                    "modified_datetime = {mod_dt}, modified_by = {mod_by} ",
#                    "WHERE location_id = {location_id} ",
#                    .con = con)
#     # Checkout a connection
#     DBI::dbExecute(con, qry)
#   }
#   poolReturn(con)
# }

# Define update callback
redd_location_update = function(redd_location_edit_values, selected_redd_data) {
  edit_values = redd_location_edit_values
  # Pull out data for location table
  location_id = edit_values$redd_location_id
  stream_channel_type_id = edit_values$stream_channel_type_id
  location_orientation_type_id = edit_values$location_orientation_type_id
  location_name = edit_values$redd_name
  location_description = edit_values$location_description
  if (is.na(location_name) | location_name == "") { location_name = NA }
  if (is.na(location_description) | location_description == "") { location_description = NA }
  mod_dt = lubridate::with_tz(Sys.time(), "UTC")
  mod_by = Sys.getenv("USERNAME")
  created_by = mod_by
  # Pull out data for location_coordinates table
  horizontal_accuracy = edit_values$horiz_accuracy
  latitude = edit_values$latitude
  longitude = edit_values$longitude
  # Checkout a connection
  con = poolCheckout(pool)
  update_result = dbSendStatement(
    con, glue_sql("UPDATE location SET ",
                  "stream_channel_type_id = $1, ",
                  "location_orientation_type_id = $2, ",
                  "location_name = $3, ",
                  "location_description = $4, ",
                  "modified_datetime = $5, ",
                  "modified_by = $6 ",
                  "where location_id = $7"))
  dbBind(update_result, list(stream_channel_type_id,
                             location_orientation_type_id,
                             location_name, location_description,
                             mod_dt, mod_by,
                             location_id))
  dbGetRowsAffected(update_result)
  dbClearResult(update_result)
  # Insert coordinates to location_coordinates if previous entry does not exist
  if ( is.na(selected_redd_data$latitude) & is.na(selected_redd_data$longitude) ) {
    if ( !is.na(latitude) & !is.na(longitude) ) {
      # Insert coordinates to location_coordinates
      qry = glue_sql("INSERT INTO location_coordinates ",
                     "(location_id, horizontal_accuracy, geom, created_by) ",
                     "VALUES ({location_id}, {horizontal_accuracy}, ",
                     "ST_Transform(ST_GeomFromText('POINT({longitude} {latitude})', 4326), 2927), ",
                     "{created_by}) ",
                     .con = con)
      # Checkout a connection
      DBI::dbExecute(con, qry)
    }
  # Otherwise update coordinates if previous entry does exist
  } else if (!is.na(selected_redd_data$latitude) & !is.na(selected_redd_data$longitude) ) {
    if ( !is.na(latitude) & !is.na(longitude) ) {
      qry = glue_sql("UPDATE location_coordinates ",
                     "SET horizontal_accuracy = {horizontal_accuracy}, ",
                     "geom = ST_Transform(ST_GeomFromText('POINT({longitude} {latitude})', 4326), 2927), ",
                     "modified_datetime = {mod_dt}, modified_by = {mod_by} ",
                     "WHERE location_id = {location_id} ",
                     .con = con)
      # Checkout a connection
      DBI::dbExecute(con, qry)
    }
  }
  poolReturn(con)
}

#======================================================================
# Identify redd encounter IDs tied to redd location about to be deleted
#======================================================================

# Identify fish_encounter dependencies prior to delete
get_redd_encounter_ids = function(redd_location_id) {
  qry = glue("select ",
             "re.redd_encounter_id ",
             "from redd_encounter as re ",
             "inner join location as loc on re.redd_location_id = loc.location_id ",
             "where loc.location_id = '{redd_location_id}'")
  con = poolCheckout(pool)
  redd_encounters = DBI::dbGetQuery(con, qry)
  poolReturn(con)
  return(redd_encounters)
}

#==============================================================
# Identify redd location dependencies prior to delete
#==============================================================

# Identify fish_encounter dependencies prior to delete
get_redd_location_dependencies = function(redd_location_id) {
  qry = glue("select ",
             "count(re.redd_encounter_id) as redd_encounter, ",
             "count(ml.media_location_id) as media_location, ",
             "count(oo.other_observation_id) as other_observation ",
             "from location as loc ",
             "left join redd_encounter as re on loc.location_id = re.redd_location_id ",
             "left join media_location as ml on loc.location_id = ml.location_id ",
             "left join other_observation as oo on loc.location_id = oo.observation_location_id ",
             "where loc.location_id = '{redd_location_id}'")
  con = poolCheckout(pool)
  redd_location_dependents = DBI::dbGetQuery(con, qry)
  poolReturn(con)
  has_entries = function(x) any(x > 0L)
  redd_location_dependents = redd_location_dependents %>%
    select_if(has_entries)
  return(redd_location_dependents)
}

#========================================================
# Delete callback
#========================================================

# # Define delete callback
# redd_location_delete = function(location_dependencies, delete_values, encounter_values) {
#   redd_location_id = delete_values$redd_location_id
#   redd_encounter_id = encounter_values$redd_encounter_id
#   if ( ncol(location_dependencies) > 1L | location_dependencies$redd_encounter[1] > 1L) {
#     con = poolCheckout(pool)
#     update_result = dbSendStatement(
#       con, glue_sql("UPDATE redd_encounter SET redd_location_id = NULL ",
#                     "WHERE redd_encounter_id = $1"))
#     dbBind(update_result, list(redd_encounter_id))
#     dbGetRowsAffected(update_result)
#     dbClearResult(update_result)
#     poolReturn(con)
#   } else {
#     con = poolCheckout(pool)
#     update_result = dbSendStatement(
#       con, glue_sql("UPDATE redd_encounter SET redd_location_id = NULL ",
#                     "WHERE redd_encounter_id = $1"))
#     dbBind(update_result, list(redd_encounter_id))
#     dbGetRowsAffected(update_result)
#     dbClearResult(update_result)
#     delete_result_one = dbSendStatement(
#       con, glue_sql("DELETE FROM location_coordinates WHERE location_id = $1"))
#     dbBind(delete_result_one, list(redd_location_id))
#     dbGetRowsAffected(delete_result_one)
#     dbClearResult(delete_result_one)
#     delete_result_two = dbSendStatement(
#       con, glue_sql("DELETE FROM location WHERE location_id = $1"))
#     dbBind(delete_result_two, list(redd_location_id))
#     dbGetRowsAffected(delete_result_two)
#     dbClearResult(delete_result_two)
#     poolReturn(con)
#   }
# }

# Define delete callback
redd_location_delete = function(location_dependencies, delete_values, redd_encounter_ids) {
  redd_location_id = delete_values$redd_location_id
  con = poolCheckout(pool)
  # Set redd_location_ids to null for cases where redd_encounters are tied to redd_location
  if ( !redd_encounter_ids == "''" ) {
    update_result = dbSendStatement(
      con, glue_sql("UPDATE redd_encounter SET redd_location_id = NULL ",
                    "WHERE redd_encounter_id in ($1)"))
    dbBind(update_result, list(redd_encounter_ids))
    dbGetRowsAffected(update_result)
    dbClearResult(update_result)
  }
  # Just delete otherwise
  delete_result_one = dbSendStatement(
    con, glue_sql("DELETE FROM location_coordinates WHERE location_id = $1"))
  dbBind(delete_result_one, list(redd_location_id))
  dbGetRowsAffected(delete_result_one)
  dbClearResult(delete_result_one)
  delete_result_two = dbSendStatement(
    con, glue_sql("DELETE FROM location WHERE location_id = $1"))
  dbBind(delete_result_two, list(redd_location_id))
  dbGetRowsAffected(delete_result_two)
  dbClearResult(delete_result_two)
  poolReturn(con)
}
