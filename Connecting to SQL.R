# This script attempts to figure out how to connect between the Rstudio cloud and 
# a SQL database hosted on AWS using RPostgres



# Libraries ---------------------------------------------------------------
library(RPostgres)
library(tidyverse)
library(dbplyr)


# Functions ---------------------------------------------------------------


# Connecting ------------------------------------
con <- dbConnect(RPostgres::Postgres(),
                 user = "postgres",
                 password = rstudioapi::askForPassword("connection password"),
                 host = "snappabase.cvoo4ewh2y4x.us-west-1.rds.amazonaws.com",
                 port = 5432,
                 dbname = "Snappa Scoreboard"
                 )




###### Generate a table and work on merging it
copy_to(con, nycflights13::flights, "flights",
        temporary = F,
        indexes = list(
          c("year", "month", "day", 
            "carrier", "tailnum", "dest")
        )
)

flights_db <- tbl(con, "flights")
flights_old <- flights_db %>% collect()

# Proof of concept: Make a dataframe from a SQL select, join that with 
# the new data frame, then export the new dataframe as the new table
# on the server

new_data <- flights_old[1:10,]


flights_new <- flights_old %>% full_join(new_data)
# Still need to figure out what indexes do
copy_to(con, flights_new, 'flights',
        temporary = F,
        overwrite = T,
        indexes = list(
          c("year", "month", "day",
            "carrier", "tailnum", "dest")
        )
)














# Taken from the shiny site on RSQLite integration. Possibly not useful
library(RSQLite)
sqlitePath <- "/path/to/sqlite/database"
table <- "responses"

saveData <- function(data) {
  # Connect to the database
  db <- dbConnect(SQLite(), sqlitePath)
  # Construct the update query by looping over the data fields
  query <- sprintf(
    "INSERT INTO %s (%s) VALUES ('%s')",
    table, 
    paste(names(data), collapse = ", "),
    paste(data, collapse = "', '")
  )
  # Submit the update query and disconnect
  dbGetQuery(db, query)
  dbDisconnect(db)
}

loadData <- function() {
  # Connect to the database
  db <- dbConnect(SQLite(), sqlitePath)
  # Construct the fetching query
  query <- sprintf("SELECT * FROM %s", table)
  # Submit the fetch query and disconnect
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  data
}