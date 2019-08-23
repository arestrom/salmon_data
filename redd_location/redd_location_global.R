
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
# Insert callback....need unique name for temp table !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
  geom = st_point(c(longitude, latitude)) %>%
    st_sfc(., crs = 4326) %>%
    st_transform(., 2927)
  # Get max_gid....did not work assuming default
  # Checkout a connection
  # con = poolCheckout(pool)
  # qry = "select max(gid) from location_coordinates"
  # max_gid = dbGetQuery(con, qry)
  # new_gid = max_gid$gid + 1L
  # Create dataframe for upload to location_coordinates using sf
  coords = tibble(location_id = location_id,
                  horizontal_accuracy = horizontal_accuracy,
                  geom = geom,
                  created_by = created_by)
  print("coords")
  print(coords)
  # Insert to location_coordinates table...need to create unique table name !!!!....use a ulid
  con = poolCheckout(pool)
  st_write(obj = coords, dsn = con, layer = "location_coordinates_temp", overwrite = TRUE)
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
  # Insert to location table
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
  # Use select into query to get data into location_coordinates
  qry = glue("insert into location_coordinates ",
             "(location_id, horizontal_accuracy, geom, created_by) ",
             "select cast(location_id as uuid), horizontal_accuracy, geom, created_by ",
             "FROM location_coordinates_temp")
  # Insert select to DB
  dbExecute(con, qry)
  # # Drop temp
  # dbExecute(con, "drop table location_coordinates_temp")
  poolReturn(con)
}

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
# #========================================================
# # Identify redd encounter dependencies prior to delete
# #========================================================
#
# # Identify fish_encounter dependencies prior to delete
# get_redd_encounter_dependencies = function(redd_encounter_id) {
#   qry = glue("select ",
#              "count(ir.individual_redd_id) as individual_redd, ",
#              "count(rc.redd_confidence_id) as redd_confidence, ",
#              "count(rs.redd_substrate_id) as redd_substrate ",
#              "from redd_encounter as rd ",
#              "left join individual_redd as ir on rd.redd_encounter_id = ir.redd_encounter_id ",
#              "left join redd_confidence as rc on rd.redd_encounter_id = rc.redd_encounter_id ",
#              "left join redd_substrate as rs on rd.redd_encounter_id = rs.redd_encounter_id ",
#              "where rd.redd_encounter_id = '{redd_encounter_id}'")
#   con = poolCheckout(pool)
#   redd_encounter_dependents = DBI::dbGetQuery(pool, qry)
#   has_entries = function(x) any(x > 0L)
#   redd_encounter_dependents = redd_encounter_dependents %>%
#     select_if(has_entries)
#   return(redd_encounter_dependents)
# }
#
# #========================================================
# # Delete callback
# #========================================================
#
# # Define delete callback
# redd_encounter_delete = function(delete_values) {
#   redd_encounter_id = delete_values$redd_encounter_id
#   con = poolCheckout(pool)
#   delete_result = dbSendStatement(
#     con, glue_sql("DELETE FROM redd_encounter WHERE redd_encounter_id = ?"))
#   dbBind(delete_result, list(redd_encounter_id))
#   dbGetRowsAffected(delete_result)
#   dbClearResult(delete_result)
#   poolReturn(con)
# }












