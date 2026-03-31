# --------------------------------------------
# Script Name: Data retriever and databases
# Purpose: My class mainly focuses on exploring the Doubs River 
#          dataset. This script is to show how to get data from 
#          free public databases and save a database of SQlite 
#          or postgresql.

# Author:     Fanglin Liu
# Email:      flliu315@163.com
# Date:       2026-04-01
#
# --------------------------------------------
cat("\014") # Clears the console
rm(list = ls()) # Remove all variables

#####################################################
# 01- Get data from an URL or a repository
#####################################################

# A) download.file() or read_csv() from websites
# https://www.davidzeleny.net/anadat-r/doku.php/en:data:doubs

# Set the base URL for the datasets

base_url <- "https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/"

datasets <- c("DoubsSpe.csv","DoubsEnv.csv","DoubsSpa.csv")  # List of datasets 

# Download each dataset

for(dataset in datasets) {
  full_url <- paste0(base_url, dataset) # full URL of files
  dest_file <- file.path("data/data_db", dataset) # the destination
  download.file(full_url, destfile = dest_file, mode = "wb") # Download
  cat("Downloaded:", dataset, "\n") # Print a message for complete
}

# if getting an error, check DNS (sudo vim /etc/resolv.conf)

# B) get data from web APIs
# library(httr2)
# response <- request(base_url) |>
#   req_*() |>
#   req_perform()
# library(rgbif)

# C) using rdataretriever to download data from databases

# # Install rdataretriever in python environment 
# # https://github.com/ropensci/rdataretriever
# # https://rstudio.github.io/reticulate/
# 
# # install.packages('reticulate') # interface to Python
# library(reticulate) # run in virtual env!!!
# py_config()
# # $pip install retriever # Install retriever package
# # install.packages('rdataretriever') # install rdataretriever
# library(rdataretriever)
# get_updates() # Update the available datasets
# # List the datasets available via the Retriever
# datasets()
# 
# # install_csv('portal') # Install csv portal
# download('portal', 'data/data_db/portal') # [219 dataset]
# portal = fetch('portal')
# names(portal)
# head(portal$species)
# 
# plot <- read.csv("data/data_db/portal/3299474")
# species <- read.csv("data/data_db/portal/3299483")
# survey <- read.csv("data/data_db/portal/5603981")
# library(tidyverse)
# glimpse(survey)

# download('harvard-forest', 'data/data_db') # vector [162]
# unzip("data/data_db/hf110-01-gis.zip")
# 
# library(sf)
# sf <- st_read(unzip("data/data_db/hf110-01-gis.zip", #read .shp into R
#                     "Harvard_Forest_Properties_GIS_Layers/stands_1937.shp"))


#####################################################
# 02-Working on the SQLite with R
#####################################################
# A) Installing SQLite and DB Browser
# https://www.jianshu.com/p/54261f6105a0

# B) exporting data to a sqlite db using R code
# https://caltechlibrary.github.io/data-carpentry-R-ecology-lesson/05-r-and-databases.html

library(tidyverse) # for the read_csv()
SPE <- read_csv("data/data_db/DoubsSpe.csv")
ENV <- read_csv("data/data_db/DoubsEnv.csv")
SPA <- read_csv("data/data_db/DoubsSpa.csv")

# connecting or creating db with dplyr

library(dplyr) 
# create a database by src_sqlite()
my_db <- dplyr::src_sqlite("results/DOUBS.sqlite", 
                           create = TRUE)
my_db

# copying the data.frames into the empty database
copy_to(my_db, SPE, temporary = FALSE)
copy_to(my_db, ENV, temporary = FALSE)
copy_to(my_db, SPA, temporary = FALSE)
my_db

dbDisconnect(my_db$con)
my_db$con

# C) Connecting to RStudio using R code or rstdudio pane

# https://www.youtube.com/watch?v=id0GX4sXnyI
# https://www.youtube.com/watch?v=0euy9b3CjuY
# https://staff.washington.edu/phurvitz/r_sql/
# https://solutions.posit.co/connections/db/best-practices/drivers/

# // setup sqlite for rstudio's connections
# apt install unixodbc 
# apt install sqliteodbd

# // two files used to set up the DSN information
# vim /etc/odbcinst.ini
# vim /etc/odbc.ini

con <- DBI::dbConnect(RSQLite::SQLite(), # create a connect
                        "results/DOUBS.sqlite")
library(dbplyr)
dbplyr::src_dbi(con) # view the database

dbListTables(con)
dbListFields(con, "ENV")

# creating tables and inserting data by dbplyr

env <- dplyr::tbl(con, "ENV") # Querying table
head(env)
library(tidyverse)
env_clean <- env |>
  select(-...1) |>
  collect() # load into the R session
env_clean 

DBI::dbDisconnect(con)
con

#################################################
# 03-Working on PostgreSQL with R
#################################################
# ## A) Installing PostgreSQL and pgAdmin4 
# 
# # Install and configure postgresql by following
# # https://www.youtube.com/watch?v=OxIQ_xJ-yzI
# 
# # For ubuntu 22.04, install a default postgresql version by following the site 
# # https://www.rosehosting.com/blog/how-to-install-postgresql-on-ubuntu-22-04/
# # 
# # Verify the installation
# # $ dpkg --status postgresql
# # $ whereis postgresql
# # $ which psql # psql is an interactive PostgreSQL client
# # $ ll /usr/bin/psql
# # $ psql -V # check postgresql version
# 
# # Configure the postgresql
# # Including client authentication methods,connecting to 
# # PostgreSQL server, authenticating with users, etc. see
# # https://ubuntu.com/server/docs/databases-postgresql
# 
# # Create a database and enable PostGIS extension
# # https://staff.washington.edu/phurvitz/r_sql/
# 

# B) creating a database and some schemas with psql 

# // create the user for the database
# create user doubs with encrypted password 'doubs';

# // default high level privileges for this user
# alter default privileges grant all on schemas to doubs;
# alter default privileges grant all on tables to doubs;
# alter default privileges grant all on sequences to doubs;
# alter default privileges grant all on functions to doubs;

# // create a few schemas
# create schema DOUBS; --for doubsEnv, doubsSpe, doubsSpa
# create schema postgis;

# // extension
# create extension postgis with schema postgis;

# C) connecting R to a database of PostgreSQL

library(RPostgreSQL)
doubsdata <- DBI::dbConnect(RPostgreSQL::PostgreSQL(), # connect
                          dbname = 'doubs',
                          host = 'localhost',
                          port = 5432,
                          user = 'doubs',
                          password = 'doubs')

doubsdata
dbGetInfo(doubsdata)

# Connect to PostgreSQL as an ODBC Data Source

# library(odbc)
# //open ubuntu terminal to edit /etc/odbc.ini like this
# [doubsdata]
# Driver = CData ODBC Driver for PostgreSQL
# Description = My Description
# User = doubs
# Password = doubs
# Database = doubs
# Server = 127.0.0.1
# Port = 5432

# D) saving dataframes to a specific schemas

# reading the doubs data into R env

library(tidyverse) # for the read_csv()
SPE <- read_csv("data/data_db/DoubsSpe.csv")
ENV <- read_csv("data/data_db/DoubsEnv.csv")
SPA <- read_csv("data/data_db/DoubsSpa.csv")

?dbWriteTable # from RPostgreSQL package
dbWriteTable(conn = doubsdata,
             name = c("envspespa",  # schema
                      "doubs_env"), # dataframe
             value = ENV,
             row.names = FALSE,
             overwrite = TRUE)

dbWriteTable(conn = doubsdata,
             name = c("envspespa",  # schema
                      "doubs_spe"), # dataframe
             value = SPE,
             row.names = FALSE,
             overwrite = TRUE)

dbWriteTable(conn = doubsdata,
             name = c("envspespa",  # schema
                      "doubs_spa"), # dataframe
             value = SPA,
             row.names = FALSE,
             overwrite = TRUE)

dbGetQuery(doubsdata,# List tables in the schema
           "SELECT table_name FROM information_schema.tables
                   WHERE table_schema='envspespa'")

dbListFields(doubsdata, c("envspespa",
                     "doubs_env")) # List fields of the table

dbDisconnect(doubsdata)
dbGetInfo(doubsdata)
