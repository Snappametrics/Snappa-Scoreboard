# This script creates transition probabilities for "scores" and "states": the
# two variants used by the Markov snappa model

# The workflow for transition probabilities works in two steps: at the start of 
# a game, the app looks for an R file which contains transition probabilities
# for every team, and then pulls the ones that it wants and stores them
# as vals. Then, at the end of the game, a script is run which makes the 
# transition probabilities using the updated tables. Second portion kind of 
# depends on the speed with which the server can accomplish this, as the calculations
# are not trivial and there are a lot of unique teams. In the event that the 
# app cannot accomplish this, I'll just have to manually run the script which 
# creates the probabilities every now and again, and live with the fact that 
# the app won't always be perfectly up to date.

library(DBI)
library(RPostgres)
library(lubridate)
library(dbplyr)
library(tidyverse)

source("dbconnect.R")
# I take the functions for the Markov modeling from the script of functions
source("markov/Markov_model_functions.R")


# Get data
players = dbReadTable(con, "players")
player_stats = dbReadTable(con, "player_stats")
scores = dbReadTable(con, "scores")
game_stats = dbReadTable(con, "game_stats")

# No reason to be taking up time with a connection to the server
rm(con)

# helper_tables makes `team_combinations`, which will actually be the basis
# of the map that I perform. After this map, I will also mutate this
# to have a running_id for each team. That way, the app can reference
# the unique_team_combinations table to see which teams are which

unique_team_combinations = 
  helper_tables(player_stats, scores, c(1,2,NA,NA))$team_combinations %>%
  ungroup() %>%
  select(`1`,`2`,`3`,`4`) %>% 
  distinct() 



# And now we pmap
all_transitions = unique_team_combinations %>%
  pmap(function(...){
    team_vec = c(...)
    scores_transitions = transition_probabilities(player_stats, 
                                                  scores, 
                                                  "scores",
                                                  team_vec[[1]], team_vec[[2]],
                                                  team_vec[[3]], team_vec[[4]])
    states_transitions = transition_probabilities(player_stats, 
                                                  scores, 
                                                  "states",
                                                  team_vec[[1]], team_vec[[2]],
                                                  team_vec[[3]], team_vec[[4]])
    return(list("scores" = scores_transitions,
                "states" = states_transitions))
})


# My central bet at the end of this is that I can coerce everything into a list and
# make the team_id vector the name of each list element.

teams_vector = unique_team_combinations %>%
  mutate(team_vector = str_c("(", `1`, ",", `2`, 
                             if_else(!is.na(`3`), str_c(", ", `3`), ""),
                             if_else(!is.na(`4`), str_c(", ", `4`), ""),
                             ")"
                             )
         ) %>%
  pull(team_vector)

names(all_transitions) = teams_vector

# Average teams go here at the end


saveRDS(all_transitions, file = "markov/transition_probabilities.Rdata")

