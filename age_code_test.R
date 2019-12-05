#=====================================================================================
# Testing to see how subscript characters can be read into R and shiny
#
# Notes:
#  1. Maybe just load as an Rdata file and leave it a that for now....?????
#     age code values are unlikely to change. And if more are needed they
#     will be requested. Or use another driver?
#
# AS 2018-06-05
#=====================================================================================

# Clear workspace
rm(list = ls(all.names = TRUE))

# Load libraries
library(odbc)
library(DBI)
library(pool)
library(RPostgreSQL)
library(dplyr)
library(stringi)
library(remisc)
library(tidyr)
library(sf)
library(openxlsx)
library(glue)
library(dplR)
library(lubridate)

#=====================================================================================
# Function to get user for database
pg_user <- function(user_label) {
  Sys.getenv(user_label)
}

# Function to get pw for database
pg_pw <- function(pwd_label) {
  Sys.getenv(pwd_label)
}

# Function to get pw for database
pg_host <- function(host_label) {
  Sys.getenv(host_label)
}

# Function to connect to postgres
pg_con_local = function(dbname, port = '5432') {
  con <- dbConnect(
    RPostgreSQL::PostgreSQL(),
    host = "localhost",
    dbname = dbname,
    user = pg_user("pg_user"),
    password = pg_pw("pg_pwd_local"),
    port = port)
  con
}

#=======================================================================================
# odbc package....no go
#=======================================================================================

# Define dsns
salmon_db = "local_spawn"

# Set up dsn connection
pool = pool::dbPool(drv = odbc::odbc(), timezone = "UTC", encoding = "UTF-8", dsn = salmon_db)

# Get the age_notation values
qry = glue("select age_code_id, european_age_code as european, gilbert_rich_age_code as gilbert_rich ",
           "from age_code_lut")
con = poolCheckout(pool)
age_code = dbGetQuery(con, qry)
poolReturn(con)

# Close pool
poolClose(pool)

#=======================================================================================
# RPostgrSQL package....works....needs conversion
#=======================================================================================

# Get the age_notation values
qry = glue("select age_code_id, european_age_code as european, gilbert_rich_age_code as gilbert_rich ",
           "from age_code_lut")

db_con = pg_con_local(dbname = "spawning_ground")
age_code = dbGetQuery(db_con, qry)
dbDisconnect(db_con)

# Set encoding to UTF-8
Encoding(age_code$gilbert_rich) = "UTF-8"

# # Testing for front-end
# pg_age = pg_age %>%
#   mutate(combo_code = paste0(european_age_code, " - ", gilbert_rich_age_code))
# pg_age$combo_code[[2]]

#=======================================================================================
# RPostgres package....works...no conversion needed
#=======================================================================================

# Get the age_notation values
qry = glue("select age_code_id, european_age_code as european, gilbert_rich_age_code as gilbert_rich ",
           "from age_code_lut")

# Read
pg_con = dbConnect(RPostgres::Postgres(), dbname = "spawning_ground", host = "localhost",
                   port = "5432", user = pg_user("pg_user"), password = pg_pw("pg_pwd_local"))
age_code = dbGetQuery(pg_con, qry)
dbDisconnect(pg_con)




























