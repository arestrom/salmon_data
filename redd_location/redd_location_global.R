
# Main redd_encounter query
get_redd_location = function(pool, redd_encounter_id) {
  qry = glue("select loc.location_id as redd_location_id, ",
             "lc.location_coordinates_id, ",
             "loc.location_name as redd_name, ",
             "st_x(st_transform(lc.geom, 4326)) as longitude, ",
             "st_y(st_transform(lc.geom, 4326)) as latitude, ",
             "lc.horizontal_accuracy as horiz_accuracy, ",
             "sc.channel_type_description as channel_type, ",
             "lo.orientation_type_description as orientation_type, ",
             "loc.location_description, ",
             "loc.created_datetime as created_date, loc.created_by, ",
             "loc.modified_datetime as modified_date, loc.modified_by ",
             "from redd_encounter as rd ",
             "inner join location as loc on rd.redd_location_id = loc.location_id ",
             "left join location_coordinates as lc on loc.location_id = lc.location_id ",
             "left join stream_channel_type_lut as sc on loc.stream_channel_type_id = sc.stream_channel_type_id ",
             "left join location_orientation_type_lut as lo on loc.location_orientation_type_id = lo.location_orientation_type_id ",
             "where rd.redd_encounter_id = '{redd_encounter_id}'")
  redd_locations = DBI::dbGetQuery(pool, qry)
  redd_locations = redd_locations %>%
    mutate(redd_location_id = tolower(redd_location_id)) %>%
    mutate(location_coordinates_id = tolower(location_coordinates_id)) %>%
    mutate(latitude = round(latitude, 6)) %>%
    mutate(longitude = round(longitude, 6)) %>%
    mutate(created_date = with_tz(created_date, tzone = "America/Los_Angeles")) %>%
    mutate(created_dt = format(created_date, "%m/%d/%Y %H:%M")) %>%
    mutate(modified_date = with_tz(modified_date, tzone = "America/Los_Angeles")) %>%
    mutate(modified_dt = format(modified_date, "%m/%d/%Y %H:%M")) %>%
    select(redd_location_id, location_coordinates_id, redd_name,
           latitude, longitude, horiz_accuracy, channel_type,
           orientation_type, location_description, created_date,
           created_dt, created_by, modified_date, modified_dt,
           modified_by) %>%
    arrange(created_date)
  return(redd_locations)
}

#==========================================================================
# Get generic lut input values
#==========================================================================

# Redd status
get_channel_type = function(pool) {
  qry = glue("select stream_channel_type_id, channel_type_description as channel_type ",
             "from stream_channel_type_lut ",
             "where obsolete_datetime is null")
  channel_type_list = DBI::dbGetQuery(pool, qry) %>%
    mutate(stream_channel_type_id = tolower(stream_channel_type_id)) %>%
    arrange(channel_type) %>%
    select(stream_channel_type_id, channel_type)
  return(channel_type_list)
}

# Redd status
get_orientation_type = function(pool) {
  qry = glue("select location_orientation_type_id, orientation_type_description as orientation_type ",
             "from location_orientation_type_lut ",
             "where obsolete_datetime is null")
  orientation_type_list = DBI::dbGetQuery(pool, qry) %>%
    mutate(location_orientation_type_id = tolower(location_orientation_type_id)) %>%
    arrange(orientation_type) %>%
    select(location_orientation_type_id, orientation_type)
  return(orientation_type_list)
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
  # Pull out redd_encounter table data
  redd_encounter_id = new_insert_values$redd_encounter_id
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
                  "?, ?, ?, ?, ?, ?, ?, ?, ?)"))
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
  qry = glue_sql("UPDATE redd_encounter SET redd_location_id = {location_id}, ",
                 "modified_datetime = {mod_dt}, modified_by = {mod_by} ",
                 "WHERE redd_encounter_id = {redd_encounter_id}", .con = con)
  # Checkout a connection
  DBI::dbExecute(con, qry)
  poolReturn(con)
}

# # Define the insert callback
# redd_location_insert = function(new_redd_location_values) {
#   new_insert_values = new_redd_location_values
#   # Generate location_id
#   location_id = remisc::get_uuid(1L)
#   created_by = new_insert_values$created_by
#   # Pull out location_coordinates table data
#   horizontal_accuracy = as.numeric(new_insert_values$horiz_accuracy)
#   latitude = new_insert_values$latitude
#   longitude = new_insert_values$longitude
#   # Pull out location table data
#   waterbody_id = new_insert_values$waterbody_id
#   wria_id = new_insert_values$wria_id
#   location_type_id = new_insert_values$location_type_id
#   stream_channel_type_id = new_insert_values$stream_channel_type_id
#   location_orientation_type_id = new_insert_values$location_orientation_type_id
#   location_name = new_insert_values$redd_name
#   location_description = new_insert_values$location_description
#   if (is.na(location_name) | location_name == "") { location_name = NA }
#   if (is.na(location_description) | location_description == "") { location_description = NA }
#   # Pull out redd_encounter table data
#   redd_encounter_id = new_insert_values$redd_encounter_id
#   mod_dt = lubridate::with_tz(Sys.time(), "UTC")
#   mod_by = Sys.getenv("USERNAME")
#   # Insert to location table
#   con = poolCheckout(pool)
#   insert_loc_result = dbSendStatement(
#     con, glue_sql("INSERT INTO location (",
#                   "location_id, ",
#                   "waterbody_id, ",
#                   "wria_id, ",
#                   "location_type_id, ",
#                   "stream_channel_type_id, ",
#                   "location_orientation_type_id, ",
#                   "location_name, ",
#                   "location_description, ",
#                   "created_by) ",
#                   "VALUES (",
#                   "?, ?, ?, ?, ?, ?, ?, ?, ?)"))
#   dbBind(insert_loc_result, list(location_id, waterbody_id, wria_id,
#                                  location_type_id, stream_channel_type_id,
#                                  location_orientation_type_id, location_name,
#                                  location_description, created_by))
#   dbGetRowsAffected(insert_loc_result)
#   dbClearResult(insert_loc_result)
#   # Insert coordinates to location_coordinates
#   qry = glue_sql("INSERT INTO location_coordinates ",
#                  "(location_id, horizontal_accuracy, gid, geom, created_by) ",
#                  "VALUES ({location_id}, {horizontal_accuracy}, ",
#                  "nextval('location_coordinates_gid_seq'::regclass), ",
#                  "ST_Transform(ST_GeomFromText('POINT({longitude} {latitude})', 4326), 2927), ",
#                  "{created_by}) ",
#                  .con = con)
#   # Checkout a connection
#   DBI::dbExecute(con, qry)
#   # Update location_id in redd_encounter table
#   qry = glue_sql("UPDATE redd_encounter SET redd_location_id = {location_id}, ",
#                  "modified_datetime = {mod_dt}, modified_by = {mod_by} ",
#                  "WHERE redd_encounter_id = {redd_encounter_id}", .con = con)
#   # Checkout a connection
#   DBI::dbExecute(con, qry)
#   poolReturn(con)
# }

# #========================================================
# # Edit update callback
# #========================================================
#
# # Define update callback
# redd_encounter_update = function(redd_encounter_edit_values) {
#   edit_values = redd_encounter_edit_values
#   # Pull out data
#   redd_encounter_id = edit_values$redd_encounter_id
#   redd_location_id = edit_values$redd_location_id
#   redd_status_id = edit_values$redd_status_id
#   redd_encounter_datetime = edit_values$redd_encounter_time
#   redd_count = edit_values$redd_count
#   comment_text = edit_values$redd_comment
#   if (is.na(comment_text) | comment_text == "") { comment_text = NA }
#   mod_dt = lubridate::with_tz(Sys.time(), "UTC")
#   mod_by = Sys.getenv("USERNAME")
#   # Checkout a connection
#   con = poolCheckout(pool)
#   update_result = dbSendStatement(
#     con, glue_sql("UPDATE redd_encounter SET ",
#                   "redd_location_id = ?, ",
#                   "redd_status_id = ?, ",
#                   "redd_encounter_datetime = ?, ",
#                   "redd_count = ?, ",
#                   "comment_text = ?, ",
#                   "modified_datetime = ?, ",
#                   "modified_by = ? ",
#                   "where redd_encounter_id = ?"))
#   dbBind(update_result, list(redd_location_id, redd_status_id,
#                              redd_encounter_datetime, redd_count,
#                              comment_text, mod_dt, mod_by,
#                              redd_encounter_id))
#   dbGetRowsAffected(update_result)
#   dbClearResult(update_result)
#   poolReturn(con)
# }
#
#========================================================
# Delete callback
#========================================================

# Define delete callback
redd_location_delete = function(delete_values, encounter_values) {
  redd_location_id = delete_values$redd_location_id
  redd_encounter_id = encounter_values$redd_encounter_id
  con = poolCheckout(pool)
  update_result = dbSendStatement(
    con, glue_sql("UPDATE redd_encounter SET redd_location_id = NULL ",
                  "WHERE redd_encounter_id = ?"))
  dbBind(update_result, list(redd_encounter_id))
  dbGetRowsAffected(update_result)
  dbClearResult(update_result)
  delete_result_one = dbSendStatement(
    con, glue_sql("DELETE FROM location_coordinates WHERE location_id = ?"))
  dbBind(delete_result_one, list(redd_location_id))
  dbGetRowsAffected(delete_result_one)
  dbClearResult(delete_result_one)
  delete_result_two = dbSendStatement(
    con, glue_sql("DELETE FROM location WHERE location_id = ?"))
  dbBind(delete_result_two, list(redd_location_id))
  dbGetRowsAffected(delete_result_two)
  dbClearResult(delete_result_two)
  poolReturn(con)
}












