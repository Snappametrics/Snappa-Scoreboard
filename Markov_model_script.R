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
    filter(game_id %in% game_team_pair$game_id) %>%
    left_join(game_team_pair, by = "game_id") %>%
    filter(scoring_team == team) %>%
    mutate(off_def = case_when(str_extract(round_num, "[A-Z]") == team ~ "offense",
                               str_extract(round_num, "[A-Z]") != team ~ "defense"),
           round_num = as.integer(str_sub(round_num, 1, -2))
           )
    
  # As it turns out, there are more games in the game_team_pair than there 
  # are in scores. Missing data is a bummer sometimes. So we have to delete
  # those games and only rely on the games from the overlap when we map
  
  # offensive_scores = scores_simplified %>%
  #   left_join(game_team_pair, by = "game_id") %>% 
  #   filter(str_extract(round_num, "[A-Z]") == team,
  #          scoring_team == team) %>%
  #   # Need a number for round_num, not the letter
  #   mutate(round_num = as.integer(str_sub(round_num, 1, -2)))
  # 
  # defensive_scores = scores_simplified %>% 
  #   left_join(game_team_pair, by = "game_id") %>% 
  #   filter(str_extract(round_num, "[A-Z]") != team, 
  #          scoring_team == team) %>%
  #   mutate(round_num = as.integer(str_sub(round_num, 1, -2)))
  
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
                 "total_rounds" = total_rounds) 
  return(package)
  
}




# A function to return the list of all transitions for a given team, used in 
# mapping 

team_transitions = function(tables, type){
 
  if (all(type != "scores", type != "states")){
    stop("type must be set to 'scores' or 'states', depending on the transition 
         matrix you want to obtain.")
  }
transition_list = tables$game_team_pair %>% 
    pull(game_id) %>%
    map(function(game){
      rounds_in_game = tables$total_rounds %>%
      filter(game_id == game) %>%
      select(offensive, defensive)
    
    shots_per_round = tables$game_shots_pair %>%
      filter(game_id == game) %>%
      pull(shots)
    
 
    
    filtered_game = tables$scores_simplified %>%
      filter(game_id == game)
    
    # So this is an annoying situation but I need to briefly segment the
    # filtered table into offense and defense, declare their factors,
    # then expand both and combine them again. Then I'll have to use a very
    # careful arrangement of the combined table to get the proper running scores
    # value for that game. At least I don't have to declare off/def as a factor
    
    offense_filtered = filtered_game %>% filter(off_def == "offense")
    defense_filtered = filtered_game %>% filter(off_def == "defense")
    
    offensive_rounds = rounds_in_game$offensive
    defensive_rounds = rounds_in_game$defensive
    
    offense_filtered$round_num =
      factor(
        offense_filtered$round_num,
        levels = seq(1, offensive_rounds))
    offense_filtered$shot_order =
      factor(
        offense_filtered$shot_order,
        levels = seq(1, shots_per_round))  
    
    defense_filtered$round_num =
      factor(
        defense_filtered$round_num,
        levels = seq(1, defensive_rounds))
    defense_filtered$shot_order =
      factor(
        defense_filtered$shot_order,
        levels = seq(1, shots_per_round))  
   
    expanded_game_offense = offense_filtered %>%
      complete(
        expand(offense_filtered,
               round_num,
               shot_order,
               off_def),
        fill = list(points_scored = 0)) %>%
      distinct() 
    expanded_game_defense = defense_filtered %>%
      complete(
        expand(defense_filtered,
               round_num,
               shot_order,
               off_def),
        fill = list(points_scored = 0)) %>%
      distinct()
    
    
    expanded_game = bind_rows(expanded_game_offense,
                              expanded_game_defense)
    
    
    # Use offense/defense to figure out whether the team is A or B
    team = expanded_game %>% pull(scoring_team) %>% unique()
    
    if (length(team) == 0){
      browser()
    }
    
    # I need to make sure that the running score is actually correctly accounting
    # for the order of play in each game. While I have the game, also
    # make sure to add an extra value to the table 
    if (team == "A"){
      expanded_game = expanded_game %>% 
        arrange(round_num, desc(off_def), shot_order) %>%
        select(game_id, off_def, points_scored)
      expanded_game = bind_rows(tibble(game_id = game, off_def = "offense", points_scored = 0),
                                expanded_game)
    } else {
      expanded_game = expanded_game %>% 
        arrange(round_num, off_def, shot_order) %>%
        select(game_id, off_def, points_scored)
      expanded_game = bind_rows(tibble(game_id = game, off_def = "defense", points_scored = 0),
                                expanded_game)
    }
    
    
    
    score_side_table = expanded_game %>%
                        group_by(game_id) %>%
                        summarize(running_score = cumsum(points_scored),
                                  off_def = off_def,
                                  .groups = "drop")
    
    # setting our matrix to 51x51 means we're looking at score transitions
    # between 0 and 50
    
    if (type == "scores"){
      offense_matrix = matrix(data = 0, nrow = 51, ncol = 51)
      defense_matrix = matrix(data = 0, nrow = 51, ncol = 51)
      # Create a matrix that will be used for our transition counts
      for (i in 2:length(score_side_table$running_score)){
        old_score = score_side_table$running_score[i - 1]
        new_score = score_side_table$running_score[i]
        
        if (score_side_table$off_def[i] == "offense"){
          offense_matrix[old_score + 1, new_score + 1] =
            offense_matrix[old_score+ 1, new_score + 1] + 1
        } else { 
          defense_matrix[old_score + 1, new_score + 1] = 
            defense_matrix[old_score + 1, new_score + 1] + 1
        }
          
      }
    } else {
      offense_matrix = matrix(data = 0, nrow = 8, ncol = 8)
      defense_matrix = matrix(data = 0, nrow = 8, ncol = 8)
      for (i in 3:length(score_side_table$running_score)){
        old_state = score_side_table$running_score[i - 1] - 
          score_side_table$running_score[i - 2]
        new_state = score_side_table$running_score[i] - 
          score_side_table$running_score[i - 1]
        
        if (score_side_table$off_def[i] == "offense"){
          offense_matrix[old_state + 1, new_state + 1] =
            offense_matrix[old_state+ 1, new_state + 1] + 1
        } else { 
          defense_matrix[old_state + 1, new_state + 1] = 
            defense_matrix[old_state + 1, new_state + 1] + 1
        }
        
      }
    } 
  return(list("offense" = offense_matrix,
              "defense" = defense_matrix))
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
  
  browser()
  transitions = team_transitions(analysis_tables, type)
  matrix_rank = if_else(type == "scores", 51, 8)
  
  
  off_transition_counts = transitions$offense %>% 
    reduce(`+`, 
      .init = matrix(data = 0, nrow = matrix_rank, ncol  = matrix_rank)
    )
  row_totals = map_dbl(seq(1, matrix_rank), function(number) { 
                                 off_transition_counts[number,] %>% sum()
                                 }
                       )
  off_transition_probs = off_transition_counts / row_totals
  off_transition_probs[which(off_transition_probs %>% is.nan())] = 0
  
  
  def_transition_counts = transitions$defense %>% 
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
