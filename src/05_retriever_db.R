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

# A) using R packages to visit databases for data
# rgbif

library(rgbif)
name_backbone(name = "Lepus saxatilis") # obtaining a species key

key <- 2436775

dat <- occ_search( # searching and downloading data
  taxonKey = key,
  # country = "JP",        
  # year = "2000,2020",    
  hasCoordinate = TRUE,
  limit = 2000
)

head(dat$data) # viewing the data
nrow(dat$data)
ncol(dat$data)
colnames(dat$data)

library(ggplot2)
library(maps)

ggplot() +
  borders("world", colour = "gray70", fill = "gray90") +  # world map
  geom_point(data = dat$data,
             aes(x = decimalLongitude, y = decimalLatitude),
             color = "red", size = 1) +
  theme_minimal()

write.csv(dat$data, "data/lepus.csv", row.names = FALSE)
lepus_data <- read.csv("data/lepus.csv")

# using rdataretriever to download data from databases

# # Install rdataretriever in python environment 
# # https://github.com/ropensci/rdataretriever
# # https://rstudio.github.io/reticulate/
# 
# # install.packages('reticulate') # interface to Python
library(reticulate) # run in virtual env!!!
py_config()
# $pip install retriever
# reticulate::py_run_string("import sys; print(sys.executable)")
# reticulate::py_install("retriever", pip = TRUE)

# Install retriever package
# install.packages('rdataretriever') 

library(rdataretriever)
get_updates() # Update the available datasets
datasets() # List the datasets available via the Retriever
install_csv('portal') # Install csv portal, i.e. 219 dataset
download('portal', 'data/portal') # Download the portal dataset
portal = fetch('portal') # Install and load a dataset
names(portal)
head(portal$plot)

plot <- read.csv("data/portal/3299474")
species <- read.csv("data/portal/3299483")
main <- read.csv("data/portal/5603981")

library(tidyverse)
glimpse(main)

# download('harvard-forest', 'data/data_db') # vector [162]
# unzip("data/data_db/hf110-01-gis.zip")
# 
# library(sf)
# sf <- st_read(unzip("data/data_db/hf110-01-gis.zip", #read .shp into R
#                     "Harvard_Forest_Properties_GIS_Layers/stands_1937.shp"))

# B) download.file() or read_csv() from websites
# https://www.davidzeleny.net/anadat-r/doku.php/en:data:doubs

# Set the base URL for the datasets

base_url <- "https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/"

datasets <- c("DoubsSpe.csv","DoubsEnv.csv","DoubsSpa.csv")  # List of datasets 

# Download each dataset

for(dataset in datasets) {
  full_url <- paste0(base_url, dataset) # full URL of files
  dest_file <- file.path("data/", dataset) # the destination
  download.file(full_url, destfile = dest_file, mode = "wb") # Download
  cat("Downloaded:", dataset, "\n") # Print a message for complete
}

# if getting an error, check DNS (sudo vim /etc/resolv.conf)

# C) get data from web APIs

library(httr2)
response <- request("https://api.gbif.org/v1/occurrence/search") %>%
  req_url_query(
    scientificName = "Lepus saxatilis",
    hasCoordinate = TRUE,
    limit = 100
  ) %>%
  req_perform()


library(jsonlite)

lepus_data <- fromJSON(
  resp_body_string(response),
  flatten = TRUE
)

df <- lepus_data$results
df_key <- df[, c(
  "decimalLongitude",
  "decimalLatitude",
  "eventDate",
  "country",
  "basisOfRecord"
)]

str(df_key)

#####################################################
# 02- loading and saving data from the R environment
#####################################################

Env <- read.csv("data/DoubsEnv.csv")
Spe <- read.csv("data/DoubsSpe.csv")
write.csv(Env, "data/Env.csv", row.names = FALSE)
write.csv(Spe, "data/Spe.csv", row.names = FALSE)

saveRDS(Env, "data/Env.rds")
saveRDS(Spe, "data/Spe.rds")
Env <- readRDS("data/Env.rds")
Spe <- readRDS("data/Spe.rds")


#####################################################
# 03-Working on the SQLite with R
#####################################################
# 1) Installing SQLite and DB Browser
# to check if SQLite is installed, installing on Ubuntu
# by reference to https://www.jianshu.com/p/54261f6105a0

# sudo apt-get install sqlite3
# sudo apt-get install libsqlite3-dev 
# sudo apt-get install sqlitebrowser

# A) exporting data to a sqlite db by DB brower

# importing Env and Spe into a sqlite
# a. creating a sqlite such as doubs.sqlite
# b. File → Import → Table from CSV

# exporting Env and Spe from the sqlite
# a. open DB brower
# b. File → Export → Table(s) as CSV
# c. specifying the folder and file name

# B) exporting data to a sqlite db using R code
# https://caltechlibrary.github.io/data-carpentry-R-ecology-lesson/05-r-and-databases.html

library(tidyverse) # for the read_csv()
SPE <- read_csv("data/Spe.csv")
ENV <- read_csv("data/Env.csv")


# connecting or creating db with dplyr
library(DBI)
library(dplyr) 
# create a database by src_sqlite()
my_db <- dplyr::src_sqlite("data/DOUBS.sqlite", 
                           create = TRUE)
my_db

# copying the data.frames into the empty database
copy_to(my_db, SPE, temporary = FALSE)
copy_to(my_db, ENV, temporary = FALSE)
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
                        "data/DOUBS.sqlite")
library(dbplyr)
dbplyr::src_dbi(con) # view the database

dbListTables(con)
dbListFields(con, "ENV")

# creating tables and inserting data by dbplyr

env <- dplyr::tbl(con, "ENV") # Querying table
head(env)
library(tidyverse)
env_clean <- env |>
  select(-X) |>
  collect() # load into the R session
env_clean 

DBI::dbDisconnect(con)
con

#################################################
# 04-Working on PostgreSQL with R
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

# C) connecting to a database of PostgreSQL via DBI

library(RPostgreSQL)
doubsdata <- DBI::dbConnect(RPostgreSQL::PostgreSQL(), # connect
                          dbname = 'doubs',
                          host = 'localhost',
                          port = 5432,
                          user = 'doubs',
                          password = 'doubs')

doubsdata
dbGetInfo(doubsdata)

# reading data and saving them into postgresql
SPE <- read.csv("data/DoubsSpe.csv", row.names = 1)
ENV <- read.csv("data/DoubsEnv.csv", row.names = 1)
SPA <- read.csv("data/DoubsSpa.csv", row.names =1)

dbWriteTable(conn = doubsdata,
             name = "doubs_env", 
             value = ENV,
             row.names = FALSE,
             overwrite = TRUE)

dbWriteTable(conn = doubsdata,
             name = "doubs_spe",
             value = SPE,
             row.names = FALSE,
             overwrite = TRUE)

dbWriteTable(conn = doubsdata,
             name = "doubs_spa",
             value = SPA,
             row.names = FALSE,
             overwrite = TRUE)

dbListFields(doubsdata, "doubs_env") # List fields of the table

dbDisconnect(doubsdata)
dbGetInfo(doubsdata)

# Connect to PostgreSQL via Rstudio connection pane
# https://www.youtube.com/watch?v=0euy9b3CjuY&t=551s

# //open ubuntu terminal to edit /etc/odbc.ini like this
# [doubsdata]
# Driver = CData ODBC Driver for PostgreSQL
# Description = My Description
# User = doubs
# Password = xxxx
# Database = doubs
# Server = 127.0.0.1
# Port = 5432

# # saving doubs data into postgresql
# ?dbWriteTable # from RPostgreSQL package
# dbWriteTable(con, "doubs_env", ENV, overwrite = TRUE)
# dbWriteTable(con, "doubs_spe", SPE, overwrite = TRUE)
