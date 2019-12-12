
# Main redd_encounter query
get_fish_location = function(fish_encounter_id) {
  qry = glue("select loc.location_id as fish_location_id, ",
             "lc.location_coordinates_id, ",
             "loc.location_name as fish_name, ",
             "st_x(st_transform(lc.geom, 4326)) as longitude, ",
             "st_y(st_transform(lc.geom, 4326)) as latitude, ",
             "lc.horizontal_accuracy as horiz_accuracy, ",
             "sc.channel_type_description as channel_type, ",
             "lo.orientation_type_description as orientation_type, ",
             "loc.location_description, ",
             "loc.created_datetime as created_date, loc.created_by, ",
             "loc.modified_datetime as modified_date, loc.modified_by ",
             "from fish_encounter as fe ",
             "inner join location as loc on fe.fish_location_id = loc.location_id ",
             "left join location_coordinates as lc on loc.location_id = lc.location_id ",
             "left join stream_channel_type_lut as sc on loc.stream_channel_type_id = sc.stream_channel_type_id ",
             "left join location_orientation_type_lut as lo on loc.location_orientation_type_id = lo.location_orientation_type_id ",
             "where fe.fish_encounter_id = '{fish_encounter_id}'")
  con = poolCheckout(pool)
  fish_locations = DBI::dbGetQuery(con, qry)
  poolReturn(con)
  fish_locations = fish_locations %>%
    mutate(latitude = round(latitude, 6)) %>%
    mutate(longitude = round(longitude, 6)) %>%
    mutate(created_date = with_tz(created_date, tzone = "America/Los_Angeles")) %>%
    mutate(created_dt = format(created_date, "%m/%d/%Y %H:%M")) %>%
    mutate(modified_date = with_tz(modified_date, tzone = "America/Los_Angeles")) %>%
    mutate(modified_dt = format(modified_date, "%m/%d/%Y %H:%M")) %>%
    select(fish_location_id, location_coordinates_id, fish_name,
           latitude, longitude, horiz_accuracy, channel_type,
           orientation_type, location_description, created_date,
           created_dt, created_by, modified_date, modified_dt,
           modified_by) %>%
    arrange(created_date)
  return(fish_locations)
}

#==========================================================================
# Get just the redd_coordinates
#==========================================================================

# Redd_coordinates query
get_fish_coordinates = function(fish_encounter_id) {
  qry = glue("select lc.location_id as fish_location_id, ",
             "lc.location_coordinates_id, ",
             "st_x(st_transform(lc.geom, 4326)) as longitude, ",
             "st_y(st_transform(lc.geom, 4326)) as latitude, ",
             "lc.horizontal_accuracy as horiz_accuracy, ",
             "lc.created_datetime as created_date, lc.created_by, ",
             "lc.modified_datetime as modified_date, lc.modified_by ",
             "from fish_encounter as fe ",
             "inner join location as loc on fe.fish_location_id = loc.location_id ",
             "inner join location_coordinates as lc on loc.location_id = lc.location_id ",
             "where fe.fish_encounter_id = '{fish_encounter_id}'")
  con = poolCheckout(pool)
  fish_coordinates = DBI::dbGetQuery(con, qry)
  poolReturn(con)
  fish_coordinates = fish_coordinates %>%
    mutate(latitude = round(latitude, 6)) %>%
    mutate(longitude = round(longitude, 6)) %>%
    mutate(created_date = with_tz(created_date, tzone = "America/Los_Angeles")) %>%
    mutate(created_dt = format(created_date, "%m/%d/%Y %H:%M")) %>%
    mutate(modified_date = with_tz(modified_date, tzone = "America/Los_Angeles")) %>%
    mutate(modified_dt = format(modified_date, "%m/%d/%Y %H:%M")) %>%
    select(fish_location_id, location_coordinates_id,
           latitude, longitude, horiz_accuracy, created_date,
           created_dt, created_by, modified_date, modified_dt,
           modified_by) %>%
    arrange(created_date)
  return(fish_coordinates)
}

#==========================================================================
# Get generic lut input values
#==========================================================================

# Redd status
get_fish_channel_type = function() {
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
get_fish_orientation_type = function() {
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
fish_location_insert = function(new_fish_location_values) {
  new_insert_values = new_fish_location_values
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
  location_name = new_insert_values$fish_name
  location_description = new_insert_values$location_description
  if (is.na(location_name) | location_name == "") { location_name = NA }
  if (is.na(location_description) | location_description == "") { location_description = NA }
  # Pull out fish_encounter table data
  fish_encounter_id = new_insert_values$fish_encounter_id
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
  # Insert coordinates to location_coordinates
  qry = glue_sql("INSERT INTO location_coordinates ",
                 "(location_id, horizontal_accuracy, geom, created_by) ",
                 "VALUES ({location_id}, {horizontal_accuracy}, ",
                 "ST_Transform(ST_GeomFromText('POINT({longitude} {latitude})', 4326), 2927), ",
                 "{created_by}) ",
                 .con = con)
  # Checkout a connection
  DBI::dbExecute(con, qry)
  # Update location_id in redd_encounter table
  qry = glue_sql("UPDATE fish_encounter SET fish_location_id = {location_id}, ",
                 "modified_datetime = {mod_dt}, modified_by = {mod_by} ",
                 "WHERE fish_encounter_id = {fish_encounter_id}", .con = con)
  # Checkout a connection
  DBI::dbExecute(con, qry)
  poolReturn(con)
}

#==============================================================
# Identify fish location surveys prior to update or delete
#==============================================================

# Identify fish_encounter dependencies prior to delete
get_fish_location_surveys = function(fish_location_id) {
  qry = glue("select s.survey_datetime as survey_date, ",
             "s.observer_last_name as observer, ",
             "fe.fish_count, mt.media_type_code as media_type, ",
             "ot.observation_type_name as other_observation_type ",
             "from location as loc ",
             "left join fish_encounter as fe on loc.location_id = fe.fish_location_id ",
             "left join survey_event as se on fe.survey_event_id = se.survey_event_id ",
             "left join survey as s on se.survey_id = s.survey_id ",
             "left join media_location as ml on loc.location_id = ml.location_id ",
             "left join media_type_lut as mt on ml.media_type_id = mt.media_type_id ",
             "left join other_observation as oo on loc.location_id = oo.observation_location_id ",
             "left join observation_type_lut as ot on oo.observation_type_id = ot.observation_type_id ",
             "where loc.location_id = '{fish_location_id}'")
  con = poolCheckout(pool)
  fish_loc_surveys = DBI::dbGetQuery(con, qry)
  poolReturn(con)
  fish_loc_surveys = fish_loc_surveys %>%
    mutate(survey_date = with_tz(survey_date, tzone = "America/Los_Angeles")) %>%
    mutate(survey_date = format(survey_date, "%m/%d/%Y"))
  return(fish_loc_surveys)
}

#========================================================
# Edit location callback
#========================================================

# Define update callback
fish_location_update = function(fish_location_edit_values) {
  edit_values = fish_location_edit_values
  # Pull out data for location table
  location_id = edit_values$fish_location_id
  stream_channel_type_id = edit_values$stream_channel_type_id
  location_orientation_type_id = edit_values$location_orientation_type_id
  location_name = edit_values$fish_name
  location_description = edit_values$location_description
  if (is.na(location_name) | location_name == "") { location_name = NA }
  if (is.na(location_description) | location_description == "") { location_description = NA }
  mod_dt = lubridate::with_tz(Sys.time(), "UTC")
  mod_by = Sys.getenv("USERNAME")
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
  # Update coordinates to location_coordinates
  qry = glue_sql("UPDATE location_coordinates ",
                 "SET horizontal_accuracy = {horizontal_accuracy}, ",
                 "geom = ST_Transform(ST_GeomFromText('POINT({longitude} {latitude})', 4326), 2927), ",
                 "modified_datetime = {mod_dt}, modified_by = {mod_by} ",
                 "WHERE location_id = {location_id} ",
                 .con = con)
  # Checkout a connection
  DBI::dbExecute(con, qry)
  poolReturn(con)
}

#========================================================
# Identify fish location dependencies prior to delete
#========================================================

# Identify fish_encounter dependencies prior to delete
get_fish_location_dependencies = function(fish_location_id) {
  qry = glue("select ",
             "count(fe.fish_encounter_id) as fish_encounter, ",
             "count(ml.media_location_id) as media_location, ",
             "count(oo.other_observation_id) as other_observation ",
             "from location as loc ",
             "left join fish_encounter as fe on loc.location_id = fe.fish_location_id ",
             "left join media_location as ml on loc.location_id = ml.location_id ",
             "left join other_observation as oo on loc.location_id = oo.observation_location_id ",
             "where loc.location_id = '{fish_location_id}'")
  con = poolCheckout(pool)
  fish_location_dependents = DBI::dbGetQuery(con, qry)
  poolReturn(con)
  has_entries = function(x) any(x > 0L)
  fish_location_dependents = fish_location_dependents %>%
    select_if(has_entries)
  return(fish_location_dependents)
}

#========================================================
# Delete callback
#========================================================

# Define delete callback
fish_location_delete = function(location_dependencies, delete_values, encounter_values) {
  fish_location_id = delete_values$fish_location_id
  fish_encounter_id = encounter_values$fish_encounter_id
  if ( ncol(location_dependencies) > 1L | location_dependencies$fish_encounter[1] > 1L) {
    con = poolCheckout(pool)
    update_result = dbSendStatement(
      con, glue_sql("UPDATE fish_encounter SET fish_location_id = NULL ",
                    "WHERE fish_encounter_id = $1"))
    dbBind(update_result, list(fish_encounter_id))
    dbGetRowsAffected(update_result)
    dbClearResult(update_result)
    poolReturn(con)
  } else {
    con = poolCheckout(pool)
    update_result = dbSendStatement(
      con, glue_sql("UPDATE fish_encounter SET fish_location_id = NULL ",
                    "WHERE fish_encounter_id = $1"))
    dbBind(update_result, list(fish_encounter_id))
    dbGetRowsAffected(update_result)
    dbClearResult(update_result)
    delete_result_one = dbSendStatement(
      con, glue_sql("DELETE FROM location_coordinates WHERE location_id = $1"))
    dbBind(delete_result_one, list(fish_location_id))
    dbGetRowsAffected(delete_result_one)
    dbClearResult(delete_result_one)
    delete_result_two = dbSendStatement(
      con, glue_sql("DELETE FROM location WHERE location_id = $1"))
    dbBind(delete_result_two, list(fish_location_id))
    dbGetRowsAffected(delete_result_two)
    dbClearResult(delete_result_two)
    poolReturn(con)
  }
}
