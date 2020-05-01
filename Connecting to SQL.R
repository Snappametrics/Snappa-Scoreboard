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



## Use purrr to turn a preexisting data table into a vector of strings that I can
# insert into SQL

x <- tibble(a = c(1, 2, 3), 
            b = c("hello", "world", "sup"), 
            c = c("yo", "my", "dudes"))

x %>% 
  mutate(combined = str_c("(", a , " , '" , b , "' , '" , c, "' )" )) %>% 
  pull(combined) %>% 
  map_chr(., function(vals) str_c("SQL INSERT ", vals, " INTO TABLE")) %>% 
  walk(., print)

strangs <- map(.x = x, 
               .f = str_c("( ", a , " , " , b , " , " , c, " )" ))



INSERT INTO 
VALUES 



