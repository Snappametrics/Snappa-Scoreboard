# This is a script to run the Markov model so that I don't have to look at my
# Rmd file every time I want to run the model. It's also a space to clean
# up the script that runs the simulation into more discrete functions


library(DBI)
library(RPostgres)
library(lubridate)
library(dbplyr)
library(tidyverse)

source("dbconnect.R")

# Get data
players = dbReadTable(con, "players")
player_stats = dbReadTable(con, "player_stats")
scores = dbReadTable(con, "scores")
game_stats = dbReadTable(con, "game_stats")

# Get out of con
rm(con)


# This function creates all the "helper" tables that are needed in team_transitions
# and elsewhere in the funcitons. It is just easier to keep them all in one
# place, and to avoid making other functions take 6 arguments 

helper_tables = function(player_stats, scores, team_vector){
  
  team_combinations = player_stats %>% 
    group_by(game_id, team) %>%
    arrange(player_id) %>%
    mutate(position = row_number()) %>%
    distinct() %>% 
    pivot_wider(id_cols = c(game_id, team), names_from = position, values_from = player_id)
  
  team_vector = team_vector %>% sort()
  
  num_players = team_vector %>% length()
  while (num_players < 4){
    team_vector = team_vector %>% append(NA)
    num_players = team_vector %>% length()
  }
  
  # Make the expression which creates a filter 
  filter_vector = team_vector %>%
    imap(function(player, index){
      if (!is.na(player)){
        str_c("`", index, "` == ", player)
      } else
        str_c("is.na(`", index, "`) ")
    })
  
  filter_expression =  rlang::parse_exprs(
    paste0(filter_vector)
  )
  
  game_team_pair = team_combinations %>% 
    filter(!!!filter_expression) %>%
    select(game_id, team) 
  
  # We need to know how many shots occurred within each round of play in each game
  # to accurately assess the total number of observations 
  
  game_shots_pair = player_stats %>%
    filter(game_id %in% game_team_pair$game_id) %>%
    group_by(game_id, team) %>% 
    summarize(team_size = n(), .groups = "drop_last") %>%
    summarize(shots = max(team_size), .groups = "drop_last")
  
  # Now, we need to take our two slices of the scores,
  # which I'm first going to simplify and clean for 
  # joining . In order to give the scores table the ability to 'complete' the data
  # for shots in a given round, I add in a variable which calculates the running total of 
  # scores in that round
  scores_simplified = scores %>% 
    arrange(game_id, score_id) %>%
    group_by(game_id, round_num, scoring_team) %>%
    mutate(shot_order = row_number()) %>% 
    select(game_id, scoring_team, points_scored, round_num, shot_order) %>%
    filter(game_id %in% game_team_pair$game_id)
  # As it turns out, there are more games in the game_team_pair than there 
  # are in scores. Missing data is a bummer sometimes. So we have to delete
  # those games and only rely on the games from the overlap when we map
  
  offensive_scores = scores_simplified %>%
    left_join(game_team_pair, by = "game_id") %>% 
    filter(str_extract(round_num, "[A-Z]") == team,
           scoring_team == team) %>%
    # Need a number for round_num, not the letter
    mutate(round_num = as.integer(str_sub(round_num, 1, -2)))
  
  defensive_scores = scores_simplified %>% 
    left_join(game_team_pair, by = "game_id") %>% 
    filter(str_extract(round_num, "[A-Z]") != team, 
           scoring_team == team) %>%
    mutate(round_num = as.integer(str_sub(round_num, 1, -2)))
  
  # We need to know the total number of rounds that we have to fill in with our
  # shot counter
  total_rounds = game_team_pair %>% 
    left_join(game_stats %>% select(game_id, rounds), 
              by = "game_id") %>%
    mutate(offensive = ifelse(team == "A", ceiling(rounds / 2), floor(rounds / 2)),
           defensive = ifelse(team == "A", floor(rounds / 2), ceiling(rounds / 2))) %>%
    ungroup() %>%
    select(game_id, offensive, defensive)
  
  package = list("team_combinations" = team_combinations, 
                 "game_team_pair" = game_team_pair,
                 "game_shots_pair" = game_shots_pair,
                 "scores_simplified" = scores_simplified,
                 "offensive_scores" = offensive_scores,
                 "defensive_scores" = defensive_scores,
                 "total_rounds" = total_rounds) 
  return(package)
  
}




# A function to return the list of all transitions for a given team, used in 
# mapping 

team_transitions = function(tables, off_def, type){
  if (all(off_def != "offensive", off_def != "defensive")){
    stop("Either 'offensive' or 'defensive' must be supplied for argument `off_def`")
  }
  
  if (all(type != "scores", type != "states")){
    stop("type must be set to 'scores' or 'states', depending on the transition 
         matrix you want to obtain.")
  }
transition_list = tables$game_team_pair %>% 
    pull(game_id) %>%
    map(function(game){
      rounds_in_game = tables$total_rounds %>%
      filter(game_id == game) %>%
      pull(off_def)
    
    shots_per_round = tables$game_shots_pair %>%
      filter(game_id == game) %>%
      pull(shots)
    
   if (off_def == "offensive"){
     reference_table = tables$offensive_scores
   } else {
     reference_table = tables$defensive_scores
   }
    
    filtered_game = reference_table %>%
      filter(game_id == game)
    # declare factor variables
    filtered_game$round_num =
      factor(
        filtered_game$round_num,
        levels = seq(1, rounds_in_game))
    filtered_game$shot_order =
      factor(
        filtered_game$shot_order,
        levels = seq(1, shots_per_round))
    
    # For some reason, if you run this it duplicates the data frame some number
    # of times.  I don't know what's causing this, but for the meantime I will
    # call distinct and be done with it
    expanded_game = filtered_game %>%
      complete(
        expand(filtered_game,
               round_num,
               shot_order),
        fill = list(points_scored = 0)) %>%
      distinct() %>%
      group_by(game_id) %>%
      summarize(running_score = cumsum(points_scored), .groups = "drop")
    
    # setting our matrix to 51x51 means we're looking at score transitions
    # between 0 and 50
      
    running_score = expanded_game$running_score %>% append(0, after = 0)
    
    if (type == "scores"){
      matrix_this_game = matrix(data = 0, nrow = 51, ncol = 51)
      # Create a matrix that will be used for our transition counts
      for (i in 2:length(running_score)){
        old_score = running_score[i - 1]
        new_score = running_score[i]
        
        matrix_this_game[old_score + 1, new_score + 1] =
          matrix_this_game[old_score+ 1, new_score + 1] + 1
        
      }
    } else {
      matrix_this_game = matrix(data = 0, nrow = 8, ncol = 8)
        for (i in 3:length(running_score)){
          old_state = running_score[i - 1] - running_score[i - 2]
          new_state = running_score[i] - running_score[i - 1]
  
        matrix_this_game[old_state + 1, new_state + 1] =
        matrix_this_game[old_state+ 1, new_state + 1] + 1
  
      }
    }
  return(matrix_this_game)
  })

return(transition_list)
}


transition_probabilities = function(player_stats, scores, type, player_id_1, player_id_2, 
                                    player_id_3 = NA, player_id_4 = NA){
  
  
  if (any(c(!is.numeric(player_id_1), !is.numeric(player_id_2)))) {
    stop("'player_id_1` and `player_id_2` must be integers")
  }
  
  if (all(any(c(!is.na(player_id_3), 
                !is.na(player_id_4))
  ),
  any(c(!is.numeric(player_id_3), 
        !is.numeric(player_id_4))
  ))
  ){
    stop("player_id_3 and player_id_4 must be integers, if provided") 
  }
  teams_vector = c(player_id_1, player_id_2, player_id_3, player_id_4)
  
  analysis_tables = helper_tables(player_stats, scores, teams_vector)
  
  
  off_transition_list = team_transitions(analysis_tables, "offensive", type)
  
  matrix_rank = if_else(type == "scores", 51, 8)
  
  
  off_transition_counts = off_transition_list %>% 
    reduce(`+`, 
      .init = matrix(data = 0, nrow = matrix_rank, ncol  = matrix_rank)
    )
  
  row_totals = map_dbl(seq(1, matrix_rank), function(number) { 
                                 off_transition_counts[number,] %>% sum()
                                 }
                       )
  
  off_transition_probs = off_transition_counts / row_totals
  
  
  off_transition_probs[which(off_transition_probs %>% is.nan())] = 0
  
  
  def_transition_list = team_transitions(analysis_tables, "defensive", type)
  browser()
  def_transition_counts = def_transition_list %>% 
    reduce(`+`, .init = matrix(data = 0, nrow = matrix_rank, ncol  = matrix_rank))
        
  row_totals = map_dbl(seq(1, matrix_rank), 
                       function(number) { 
                         def_transition_counts[number,] %>% sum()
                         })
  def_transition_probs = def_transition_counts / row_totals
  def_transition_probs[which(def_transition_probs %>% is.nan())] = 0
  
  return_list = list("offensive" = off_transition_probs,
                     "defensive" = def_transition_probs)
  return(return_list)
}

im_mr_meeseeks = transition_probabilities(player_stats, scores, "scores", 2, 9)
