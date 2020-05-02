# This script attempts to figure out how to connect between the Rstudio cloud and 
# a SQL database hosted on AWS using RPostgres



# Libraries ---------------------------------------------------------------
library(RPostgres)
library(tidyverse)
library(dbplyr)
library(DBI)

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
    

# make a function which can do the last bit of this code. WARNING: does not currently work

insert_walk <- function(rtable, dbtable, connection){
  rvars <- colnames(rtable)
  rtable_name <- deparse(substitute(rtable))
  dbvars <- colnames(dbtable)
  dbtable_name <- deparse(substitute(dbtable))
  browser()
output <- rtable %>% mutate(combined = str_c("( ",
                                            sym(str_c(rvars, collapse = ', "," , ')),
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








## Alternatively, one could use dbAppend, which is literally designed for this kind of thing. 

dbAppendTable(con, "y", x)



# So this works. Does this create problems when the database expects a bigserial?


players_dummy <- tibble(player_id = c(1,2,3,4,5), player_name = c("Michael", "Dewey", "DeweyDewey", "Mark", "Barrett"))

copy_to(con, players_dummy, "players", 
        temporary = F, 
        overwrite = T
        )


new_player <- tibble(player_id = c(6,7,8), player_name = c("Michael", "Dewey", "DeweyDewey"))

dbAppendTable(con, "players", new_player)
