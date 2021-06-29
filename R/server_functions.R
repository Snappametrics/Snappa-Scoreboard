library(extrafont)
library(tidyverse)
library(lubridate)
library(shiny)
library(shinyjs)


casualty_rules = tribble(~team_A, ~team_B, ~casualty_title, ~casualty_text,
                         12, 7, "12-7", "Roll off to see who is taking the kamikaze to the face",
                         7, 12, "12-7", "Roll off to see who is taking the kamikaze to the face",
                         18, 12, "War of 1812", "Everyone roll a die, the lowest roll takes a shot.",
                         12, 18, "War of 1812", "Everyone roll a die, the lowest roll takes a shot.",
                         20, 03, "2003", "Nevar forget: a 9/11 consists of a shot of fireball into a Sam Adams",
                         03, 20, "2003", "Nevar forget: a 9/11 consists of a shot of fireball into a Sam Adams")

sink_criteria = tribble(~points_scored, ~clink, 
                        3, F,
                        5, T,
                        7, T)
# For the sake of code simplicity, I'm going to define a function which
# writes player_stats_db off of scores. This should only require
# a scores table to be passed on, since the snappaneers table that is also
# called would always be the same
aggregate_player_stats = function(scores_df, snappaneers, game){
  scores_df %>% 
    # Join scores to snappaneers to get each player's team
    right_join(snappaneers, by = "player_id") %>% 
    # Fill in game_id for players who have not scored yet
    replace_na(list(game_id = game)) %>% 
    # Group by game and player, (team and shots are held consistent)
    group_by(game_id, player_id, team, shots) %>% 
    # Calculate summary stats
    summarise(total_points = sum(points_scored),
              ones = sum((points_scored == 1)),
              twos = sum((points_scored == 2)),
              threes = sum((points_scored == 3)),
              impossibles = sum((points_scored > 3)),
              paddle_points = sum(points_scored * (paddle | foot)),
              clink_points = sum(points_scored * clink),
              points_per_round = na_if(total_points / last(shots), Inf),
              off_ppr = sum(points_scored * !(paddle | foot)) / last(shots), 
              def_ppr = na_if(sum(points_scored * (paddle | foot)) / last(shots), Inf),
              toss_efficiency = sum((points_scored>0) * !(paddle | foot)) / last(shots), 
              .groups = "drop") %>% 
    # Replace NA values with 0s
    replace_na(list(total_points = 0, 
                    ones = 0, twos = 0, threes = 0, impossibles = 0, 
                    paddle_points = 0, clink_points = 0, 
                    points_per_round = 0, off_ppr = 0, def_ppr = 0, toss_efficiency = 0))

}

aggregate_player_stats_and_sinks = function(scores_df, snappaneers, game){
  
  if(is_integer(unique(scores_df$game_id))){
    game = unique(scores_df$game_id)
  }
  # This is the environment hopping mayhem. 
  # environments are kinda confusing, and I don't know how to call sink_criteria without passing it as an argument
  # Based Hadley trying to teach me: https://adv-r.hadley.nz/environments.html#environments
  
  # Check each parent environment for sink_criteria
  sink_criteria = rlang::env_parents(rlang::current_env()) %>% 
    keep(~rlang::env_has(., "sink_criteria")) %>% 
    # Only keep the one that does and use it to access the criteria
    map_dfr(., rlang::env_get, "sink_criteria")
  
  scores_df %>% 
    # Join scores to snappaneers to get each player's team
    right_join(snappaneers, by = "player_id") %>% 
    detect_sink(., sink_criteria) %>% 
    # Fill in game_id for players who have not scored yet
    replace_na(list(game_id = game, points_scored = 0, paddle = F, clink = F, foot = F)) %>% 
    # Group by game and player, (team and shots are held consistent)
    group_by(game_id, player_id, team, shots) %>% 
    # Calculate summary stats
    summarise(total_points = sum(points_scored),
              ones = sum((points_scored == 1)),
              twos = sum((points_scored == 2)),
              threes = sum((points_scored == 3)),
              normal_points = sum(points_scored * !(paddle | clink | sink)),
              sinks = sum(sink), # NEW
              sink_points = sum(points_scored*sink),
              paddle_sinks = sum(sink*paddle), # NEW
              impossibles = sum((points_scored > 3)),
              paddle_points = sum(points_scored * (paddle | foot)),
              foot_points = sum(points_scored * foot), # NEW
              clink_points = sum(points_scored * clink),
              points_per_round = na_if(total_points / last(shots), Inf),
              off_ppr = sum(points_scored * !(paddle | foot)) / last(shots), 
              def_ppr = na_if(sum(points_scored * (paddle | foot)) / last(shots), Inf),
              toss_efficiency = sum((points_scored>0) * !(paddle | foot)) / last(shots), 
              .groups = "drop") %>% 
    # Replace NA values with 0s
    replace_na(list(points_per_round = 0, off_ppr = 0, def_ppr = 0, toss_efficiency = 0))
  
}

detect_sink = function(scores, criteria){
  # Detect sinks in a dataframe of score data
  left_join(scores, 
            mutate(criteria, sink = T), by = c("points_scored", "clink")) %>% 
    replace_na(list(sink = F))
}

toss_percent_plus = function(x){
  str_c("+", round(x*100, 0), "%")
}
toss_percent_minus = function(x){
  str_c(round(x*100, 0), "%")
}

rebuttal_check <- function(a , b , round, points_to_win) {
  if (any(is.null(a),is.null(b))){
    check <- F
  } else{ 
    check <- case_when(
      (a >= points_to_win & a - b >= 2 & str_detect(round, "[Bb]")) ~ T, 
      (b >= points_to_win & b - a >= 2 & str_detect(round, "[Aa]")) ~ T,
      !any((a >= points_to_win & a - b >= 2 & str_detect(round, "[Bb]")), 
           (b >= points_to_win & b - a >= 2 & str_detect(round, "[Aa]"))) ~ F)
  }
  
  return(check)
}

add_shot_count = function(df, shot_num){
  add_count(df, team, name = "n_players") %>% # Count the number of rows for each team
    # Calculate the number of shots for each player by diving the team shots by 2
    # Team shots calculated using ceiling/floor because A always goes first
    mutate(baseline_shots = case_when(str_detect(team, "A") ~ ceiling(shot_num/2),
                                      str_detect(team, "B") ~ floor(shot_num/2)),
           # In cases where teams are uneven, we calculate the average shots a player had
           shots = baseline_shots*max(n_players)/n_players) %>% 
    select(-baseline_shots, -n_players)
}

validate_scores = function(player, shot, snappaneers, paddle, 
                           scores_table, round_vector = rounds, rebuttal = F){
  
  # Identify the scorer's team and ID
  players_team = snappaneers[snappaneers$player_name == player, "team", drop=TRUE]
  scorer_id = snappaneers[snappaneers$player_name == player, "player_id", drop=TRUE]
  
  # Typical Offense: Scoring on one's shot
  typical_offense = str_detect(round_vector[shot], players_team)
  
  # Typical Paddle: Scoring on the other team's shot
  typical_paddle = all(str_detect(round_vector[shot], players_team, negate = T), 
                       paddle)
  
  # NOTE: already scored in the rest of this function refers to having already scored a non-paddle shot
  # Players can only score once on a non-paddle shot
  not_already_scored = !(scorer_id %in% pull(filter(scores_table, round_num == round_vector[shot], paddle == F), player_id))
  
  
  # If teams are even
  if(nrow(distinct(count(snappaneers, team), n)) == 1){
    
    # A valid score is either a paddle or a typical offense and the scorer has not already scored
    # OR it's rebuttal
    valid_score = any(paddle,
                      all(typical_offense,
                          not_already_scored),
                      rebuttal)
    
    if(not_already_scored){
      valid_score_message = "That entry doesn't make sense for this round/shooter combination"
    }
    
    valid_score_message = "That player scored a non-paddle point already!"
    
  } else { # If teams are uneven:
    
    # Check if the scorer is on the team with fewer players (i.e. can score multiple non-paddle points)
    scorer_team_players = nrow(snappaneers[snappaneers$team == players_team, ])
    other_team_players = nrow(snappaneers[snappaneers$team != players_team, ])
    
    if((scorer_team_players < other_team_players)|rebuttal){ # If their team has fewer players OR Rebuttal:
      
      # NOTE: already scored in the rest of this function refers to having already scored a non-paddle shot
      # Players can only score once on a non-paddle shot
      scored_already = (nrow(filter(scores_table, round_num == round_vector[shot], paddle == F, player_id == scorer_id)) > 1)
      
      # Then they can have scored already
      valid_score = any(all(typical_offense,
                            !scored_already),
                        rebuttal,
                        paddle)
      
      # Score message if invalid
      valid_score_message = "That player scored a non-paddle point already!"
      
    } else { # If they have the larger team:
      # They cannot have scored already OR it's rebuttal
      valid_score = any(all(typical_offense,
                            not_already_scored),
                        paddle,
                        rebuttal)
      
      valid_score_message = "That player scored a non-paddle point already!"
      
    }
    
  }
  
  # Validate all valid scores
  if(valid_score){
    valid_score_message = "valid"
  }
  
  return(valid_score_message)
  
}




parse_round_num = function(round){
  str_replace_all(round, c("([0-9]+)A" = "(\\1*2)-1",
                           "([0-9]+)B" = "(\\1*2)")) %>% 
    map(., ~parse(text = .)) %>% 
    map_dbl(eval)
}

rowAny <- function(x) {
  rowSums(x) > 0
} 

db_update_player_stats = function(player_stats, specific_player, round_button = F){
  
  if(round_button){
    col_updates = select(player_stats, game_id, player_id, shots, points_per_round:toss_efficiency) %>% 
      group_by(player_id) %>% 
      # Transpose each player id's row
      group_map(~t(.), .keep = T) %>% 
      map(~str_c(rownames(.), " = ", ., collapse = ", ")) %>% 
      # Set names for use with imap
      set_names(player_stats$player_id)
    
    update_player_stats_queries = imap(col_updates, 
                                       ~str_c("UPDATE player_stats
                                             SET ", .x,
                                              " WHERE game_id = ", unique(player_stats$game_id),
                                              " AND player_id = ", .y, ";"))
    walk(update_player_stats_queries, ~dbExecute(con, .))
    return(invisible())
  }
  
  # Add quotes around character vars for update query
  player_stats = mutate(player_stats, across(where(is_character), ~str_c("'", ., "'")))
  
  
  # If updating the whole table
  
  if(missing(specific_player)){
    # Convert tibble to list of character strings in the format: COLNAME = VALUE
    # Separated for each player in player_stats
    col_updates = player_stats %>% 
      group_by(player_id) %>% 
      # Transpose each player id's row
      group_map(~t(.), .keep = T) %>% 
      map(~str_c(rownames(.), " = ", ., collapse = ", ")) %>% 
      # Set names for use with imap
      set_names(player_stats$player_id)
    
    update_player_stats_queries = imap(col_updates, 
                                       ~str_c("UPDATE player_stats
                                             SET ", .x,
                                              " WHERE game_id = ", unique(player_stats$game_id),
                                              " AND player_id = ", .y, ";"))
    walk(update_player_stats_queries, ~dbExecute(con, .))
  } else {
    
    
    # If a player is specified, only update their row
    
    col_updates = t(filter(player_stats, player_id == specific_player)) %>% 
      str_c(rownames(.), " = ", ., collapse = ", ")
    
    update_player_stats_query = str_c("UPDATE player_stats
                              SET ", col_updates,
                              " WHERE game_id = ", unique(player_stats$game_id),
                              " AND player_id = ", specific_player, ";")
    
    dbExecute(con, update_player_stats_query)
  }
  
  
  
}

db_update_round = function(round, game){
  # Update round number in game_stats
  dbExecute(con, 
            sql(str_c("UPDATE game_stats 
                 set last_round = '", round, "'
                ",
                      "WHERE game_id = ", game, ";")))
}

cooldown_check = function(casualties, scores, current_round, casualty_to_check, rounds){
  # Check whether the last casualty of a certain type occurred 
  # a full round (both teams shot) ago, determining whether the rule is reactivated
  #   rounds is the round_label vector i.e. "1A", "1B", etc.
  
  # Pull the round_num associated with the casualties in the current game
  # Also count the number of times -1 the casualty has occurred
  
  if(nrow(casualties) < 1){
    invisible()
  } else {

    # Increment the round by 2x the times the casualty has been repeated
    last_casualty_round = which(rounds == scores[scores$score_id == unique(casualties$score_id), "round_num"]) + (nrow(casualties)-1)*2
    
    which(rounds == current_round) - last_casualty_round < 2
    
    
  }

  
}
