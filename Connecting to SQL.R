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






## Use purrr to turn a preexisting data table into a vector of SQL query strings

x <- tibble(a = c(1, 2, 3), 
            b = c("hello", "world", "sup"), 
            c = c("yo", "my", "dudes"))



x %>% 
  mutate(combined = str_c("( ", a , " , '" , b , "' , '" , c, "' )" )) %>% 
  pull(combined) %>% 
  map_chr(., function(vals) str_c("INSERT INTO ", "flights (tailnum, year, month)", " VALUES ", vals)) %>%
  walk(., dbGetQuery, conn = con)
    

# make a function which can do the last bit of this code
insert_walk <- function(rtable, dbtable, connection){
  rvars <- colnames(rtable)
  rtable_name <- deparse(substitute(rtable))
  dbvars <- colnames(dbtable)
  dbtable_name <- deparse(substitute(dbtable))
output <- rtable %>% mutate(combined = str_c("( ",
                                             sym(str_c(rvars, collapse = " , ")),
                                             " ) ")) %>%
                     pull(combined) %>% 
          map_chr(., function(vals) str_c("INSERT INTO ", 
                                  str_c(dbtable_name, 
                                  " ( ", 
                                  str_c(dbvars, collapse = " , ") ,
                                  " )",
                                  " VALUES ", 
                                  vals)) ) %>%
         walk(., dbExecute, conn = connection)
return(output)
}


# Test with a small scale example

y <- tibble(a = c(1,2,3), b = c("like", "and", "subscribe"), c = c("sql", "is", "hard"))

copy_to(con, y , "y",
        temporary = F,
        overwrite = T
        )




insert_walk(y, x, con)



y2 <- tbl(con, "y") %>% collect()




################ Use dplyr merges to accomplish updating

#### Pipeline:
# 1. Shiny pulls table information from the database and stores it
# 2. Shiny collects streaming game data
# 3. At the end of the game, Shiny creates a joined table and puts
#   that back into the database, overwriting the old one, then closes
#   its connection to the database


# 1. Pull data (copy_to generates the table in the database:
# only run it once)
copy_to(con, nycflights13::flights, "flights",
        temporary = F,
        overwrite = T,
        append = F,
        indexes = list(
          c("year", "month", "day",
            "carrier", "tailnum", "dest")
        )
)

flights_db <- tbl(con, "flights")
flights_old <- flights_db %>% collect()


#The app needs to know particular things in order to get started
current_game_id <- flights_old %>% pull(year) %>% max() + 1


#2. Data is collected

new_data <- flights_old %>% filter(month < 2)



