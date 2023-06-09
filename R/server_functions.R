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


# Helper functions --------------------------------------------------------

rowAny <- function(x) {
  rowSums(x) > 0
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
  # not_already_scored = !(scorer_id %in% pull(filter(scores_table, round_num == round_vector[shot], paddle == F), player_id))
  not_already_scored = !(scorer_id %in% scores_table[scores_table$round_num == round_vector[shot] & scores_table$paddle == F, "player_id", drop=T])
  
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
      # scored_already = (nrow(filter(scores_table, round_num == round_vector[shot], paddle == F, player_id == scorer_id)) > 1)
      scored_already = (nrow(scores_table[scores_table$round_num == round_vector[shot] & 
                                            scores_table$paddle == F & 
                                            scores_table$player_id == scorer_id,]) > 1)
      
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


find_similar_games = function(player_id, player_stats, team_size = 2, opponent_size = 2){
  
  # Make a dataframe of team sizes in past games
  # Count team size for each game
  team_sizes = count(player_stats, game_id, team, name = "team_size") |> 
    # Pivot separate columns for team 
    pivot_wider(names_from = team, 
                values_from = team_size, 
                names_glue = "size_{team}")
  
  # Subset each player's stats  identify equivalent games
  player_stats[player_stats$player_id == player_id, ] %>% 
    # Join team sizes to player stats
    inner_join(team_sizes, by = "game_id") %>%
    # Keep cases where the team sizes are equivalent
    filter(if_else(team == "A", size_A, size_B) == team_size,
           if_else(team == "B", size_A, size_B) == opponent_size) |> 
    select(game_id, player_id, shots)
}


# Event Detection ---------------------------------------------------------

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


# Formatting --------------------------------------------------------------

toss_percent_plus = function(x){
  str_c("+", round(x*100, 0), "%")
}
toss_percent_minus = function(x){
  str_c(round(x*100, 0), "%")
}


# Calculate Stats -----------------------------------------------------

detect_sink = function(scores, criteria){
  if(missing(criteria)){
    criteria = tribble(~points_scored, ~clink, 
                       3, F,
                       5, T,
                       7, T)
  }
  # Detect sinks in a dataframe of score data
  left_join(scores, 
            mutate(criteria, sink = T), by = c("points_scored", "clink")) %>% 
    replace_na(list(sink = F))
}


# For the sake of code simplicity, I'm going to define a function which
# writes player_stats_db off of scores. This should only require
# a scores table to be passed on, since the snappaneers table that is also
# called would always be the same
aggregate_player_stats = function(scores_df, snappaneers, game){
  # Join scores to snappaneers to get each player's team
  right_join(scores_df, snappaneers, by = "player_id") %>% 
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
  
  # Join scores to snappaneers to get each player's team
  right_join(scores_df, snappaneers, by = "player_id") %>% 
    detect_sink(., sink_criteria) %>% 
    # Fill in game_id for players who have not scored yet
    replace_na(list(game_id = game, points_scored = 0, paddle = F, clink = F, foot = F)) %>% 
    # Group by game and player, (team and shots are held consistent)
    group_by(game_id, player_id, team, shots) %>% 
    # Calculate summary stats
    summarise(
      total_points = sum(points_scored),
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
      .groups = "drop"
    ) %>% 
    # Replace NA values with 0s
    replace_na(list(points_per_round = 0, off_ppr = 0, def_ppr = 0, toss_efficiency = 0))
  
}

make_summary_table = function(current_player_stats, player_stats, neers, team_name, current_round, past_scores){
  # Produce a team's performance summary and comparison to historical performance in equivalent games
  # 1. Get a list of games the player has played in
  # 2. Get a list of scores from historical games at the equivalent point in the game
  # 3. Calculate current game player stats
  # 4. Calculate historical player stats from 1 & 2
  # browser()
  # List players on the given team
  team_players = neers[neers$team == team_name, ]
  # Store current team and opponent sizes
  team_size = nrow(team_players) 
  opponent_size = nrow(neers[neers$team != team_name, ])
  
  # Store current game id and the given team's current player stats
  current_game = unique(current_player_stats$game_id)
  team_player_stats = current_player_stats[current_player_stats$team == team_name, ]
  current_shots = unique(team_player_stats$shots)
  
  
  
  # Make a historical stats table that is only comparing games which are similar
  # to the current one.
  # First, obtain a list of games in which the players on this team were on
  # an equally sized team. This is player specific, so map() is used
  
  
  
  # A not particularly elegant but probably working solution to making sure that games
  # are really, truly, apples-to-apples. Create a table of game_id, ally_team_size,
  # and opponent_team_size and merge it to player_stats to be used as a 
  
  
  # Make a dataframe of team sizes in past games
  # Count team size for each game
  team_sizes = count(player_stats, game_id, team, name = "team_size") %>% 
    # Pivot separate columns for team 
    pivot_wider(names_from = team, 
                values_from = team_size, 
                names_glue = "size_{team}")
  
  equivalent_games_player_stats = map(team_players$player_id, function(player){
    # Subset each player's stats  to each player's stats and identify equivalent games
    player_stats[player_stats$player_id == player & player_stats$game_id != current_game, ] %>% 
      # Join team sizes to player stats
      inner_join(team_sizes, by = "game_id") %>%
      # Keep cases where the team sizes are equivalent
      filter(if_else(team == "A", size_A, size_B) == team_size,
             if_else(team == "B", size_A, size_B) == opponent_size)
    
  })
  
  
  # make a unique subsection of the scores table which only considers the given player in the
  # given games. round_comparison should only be applied when a game is in progress. 
  # While we're here, also tell the display not to care about winners maybe? I could also set
  # a value here so that I don't have to execute a query later, but the issue becomes 
  
  ##
  ## Scenario 1: Game is in progress
  ##
  in_progress = isFALSE(pull(dbGetQuery(con, sql(str_c("SELECT game_complete FROM game_stats WHERE game_id =", current_game, ";"))), 
                             game_complete))
  if (in_progress){
    
    # Filter to scores which are:
    #   - only scored by players on this team
    #   - less than or equal to the current round
    scores_comparison = filter(past_scores,
                               player_id %in% team_players$player_id, # Only include players on this team
                               parse_round_num(round_num) <= parse_round_num(current_round))
    
    ##
    ## Scenario 2: Game is complete
    ##
  } else {
    
    scores_comparison = past_scores
  }
  
  # List scores which occurred at or before the current game's round
  historical_scores = imap_dfr(team_players$player_id, function(player, index){
    # Join each player's equivalent games to their scores from those games
    left_join(equivalent_games_player_stats[[index]], 
              scores_comparison, 
              by = c("game_id", "player_id")) %>% 
      filter(!(game_id %in% 38:48))
  })
  
  # When in progress, keep the shot counter generated from current_shots
  if(in_progress){
    historical_scores = mutate(historical_scores, shots = current_shots)
  }
  
  # Now, this table is going to be plugged in to the pipeline that currently exists in the team summary tab function.
  # That means I have to recreate player_stats using this table 
  
  # Calculate game performance in equivalent games
  comparison_player_stats = replace_na(historical_scores,
                                       list(points_scored = 0, paddle = F, clink = F, foot = F)) %>% 
    # Group by game and player, (team and shots are held consistent)
    group_by(game_id, player_id, shots) %>% 
    # Calculate summary stats
    summarise(total_points = sum(points_scored),
              ones = sum((points_scored == 1)),
              twos = sum((points_scored == 2)),
              threes = sum((points_scored == 3)),
              impossibles = sum((points_scored > 3)),
              paddle_points = sum(points_scored* (paddle | foot)),
              clink_points = sum(points_scored*clink),
              points_per_round = total_points / last(shots),
              off_ppr = sum(points_scored * !(paddle | foot))/ last(shots),
              def_ppr = paddle_points/last(shots),
              toss_efficiency = sum((points_scored>0)*!(paddle | foot ))/last(shots),
              .groups = "drop")
  
  # Filter player stats
  player_info = select(team_player_stats, game_id, player_id, team) %>% 
    inner_join(neers, by = c("player_id", "team")) 
  
  # Calculate current game performance
  #   - Team score
  #   - Which team is winning
  # Then join on player info
  player_summary = group_by(current_player_stats, team) %>% 
    mutate(team_score = sum(total_points)) %>% 
    ungroup() %>% 
    mutate(winning = (team_score == max(team_score))) %>% 
    filter(team == team_name) %>% 
    select(team, winning, player_id,  
           total_points, paddle_points, clink_points, threes, 
           points_per_round:toss_efficiency)
  
  historical_stats = select(comparison_player_stats, game_id, player_id, 
                            shots, total_points, paddle_points, clink_points, sinks = threes, 
                            points_per_round:toss_efficiency) %>%
    arrange(player_id, game_id) %>% 
    group_by(player_id) %>% 
    summarise(
      across(.cols = c(total_points, paddle_points, clink_points), .fns = mean, .names = "{col}_avg"),
      across(.cols = c(sinks), .fns = sum, .names = "{col}_total"),
      across(.cols = c(points_per_round, off_ppr, def_ppr, toss_efficiency),
             .fns = ~weighted.mean(., w = shots), 
             .names = "{col}_wavg"),
      .groups = "drop"
    )
  
  player_summary_historical = full_join(player_summary, historical_stats, by = "player_id") %>% 
    # Calculate the difference between current game and historical performance
    mutate(total_points_diff = total_points - total_points_avg,
           paddle_points_diff = paddle_points - paddle_points_avg,
           clink_points_diff = clink_points - clink_points_avg,
           points_per_round_diff = points_per_round - points_per_round_wavg,
           off_ppr_diff = off_ppr - off_ppr_wavg,
           def_ppr_diff = def_ppr - def_ppr_wavg,
           toss_efficiency_diff = toss_efficiency - toss_efficiency_wavg,
           # Format each difference for the table
           across(matches("points_diff"), ~str_c(if_else(.x >= 0, "+", ""), round(.x, 1))),
           across(matches("(per_round|ppr)_diff$"), ~str_c(if_else(.x >= 0, "+", ""), round(.x, 1))),
           toss_efficiency_diff = map_chr(toss_efficiency_diff, 
                                          ~case_when(. >= 0 ~ toss_percent_plus(.), 
                                                     . < 0 ~ toss_percent_minus(.)))) %>% 
    # Remove historical stats
    select(-ends_with("_avg"), -ends_with("wavg")) %>% 
    # Order columns
    select(starts_with("player"), team, winning, 
           contains("total_points"), contains("paddle"), contains("clink"), sinks = threes, 
           contains("per_round"), contains("off_"), contains("def_"), contains("toss"))
  
  inner_join(select(team_players, player_id, player_name),
             select(player_summary_historical,
                    # -contains("clink"), -contains("sink"),
                    -contains("points_per")), 
             by = "player_id")
  
}

#' Player Performance Summary
#'
#' @param game_started Start Game Input
#' @param player_stats Player Stats data
#' @param team_name Team
#' @param game_obj Current Game object
#' @param current_round Current Round
#' @param past_scores Scores data from past games
#' 
#' Summarise how a player is performing in relation to their past performance in "similar" games".
#' By similar, we mean games with the same team sizes e.g. 2v2, 2v3, 3v3, etc.
#' For a 2 player team in a 2v3, only games where they were on the team of 2 are considered to be similar.
#'
#' @return
#' @export
#'
#' @examples
player_performance_summary = function(
    game_started, 
    player_stats, 
    team_name, 
    game_obj = NULL, 
    current_round = NULL, 
    past_scores
){
  # Produce a team's performance summary and comparison to historical performance in equivalent games
  # 1. Get a list of games the player has played in
  # 2. Get a list of scores from historical games at the equivalent point in the game
  # 3. Calculate current game player stats
  # 4. Calculate historical player stats from 1 & 2
  # TODO: Specify relevant columns early on
  if(game_started == 0){
    ps_current = filter(player_stats, game_id == max(game_id))
    
    # Separate past game player stats
    ps_past = filter(player_stats, game_id != max(game_id)) |> 
      select(game_id, player_id, team, shots, total_points, paddle_points, clink_points, 
             points_per_round, off_ppr, def_ppr, toss_efficiency)
    current_game = unique(ps_current$game_id)
  } else {
    ps_current = game_obj$player_stats_db
    
    # Separate past game player stats
    ps_past = filter(player_stats, game_id != game_obj$game_id) |> 
      select(game_id, player_id, team, shots, total_points, paddle_points, clink_points, 
             points_per_round, off_ppr, def_ppr, toss_efficiency)
    
    current_game = game_obj$game_id
  }
  
  
  # List players on the given team
  ps_current_team = ps_current[ps_current$team == team_name, ]
  
  # Store current team and opponent sizes
  team_size = nrow(ps_current_team) 
  opponent_size = nrow(ps_current[ps_current$team != team_name, ]) # Perhaps subtract? nrow(ps_current) - team_size
  
  # Store current game id and the given team's current player stats
  current_shots = unique(ps_current_team$shots)
  
  # Make a historical stats table that is only comparing games which are similar
  # to the current game
  # 1) list games where player was in same team format e.g. 2v2,2v3,etc
  #  This is player specific, so map() is used
  
  # A not particularly elegant but probably working solution to making sure that games
  # are really, truly, apples-to-apples. Create a table of game_id, ally_team_size,
  # and opponent_team_size and merge it to player_stats to be used as a 
  
  # Make a dataframe of team sizes in past games
  # ps_comparable = map(ps_current_team[, "player_id", drop=T],
  ps_comparable = map(ps_current_team$player_id,
                      find_similar_games, player_stats = ps_past, team_size = team_size, opponent_size = opponent_size)
  
  ## Scenario 1: Game is in progress
  in_progress = (game_started != 0)
  if (in_progress){
    # browser()
    # Filter to scores which are:
    #   - only scored by players on this team
    #   - less than or equal to the current round
    # scores_comparable = filter(past_scores,
    #                            player_id %in% ps_current_team$player_id, # Only include players on this team
    #                            parse_round_num(round_num) <= parse_round_num(current_round))
    scores_comparable = semi_join(past_scores,
                                  ps_current_team, by = "player_id") |> # Only include players on this team
      filter(parse_round_num(round_num) <= parse_round_num(current_round))
    # scores_comparison = past_scores[past_scores$player_id %in% ps_current_team$player_id & parse_round_num(past_scores$round_num) <= parse_round_num(current_round), ]
    
  } else {
    ## Scenario 2: Game is complete
    scores_comparable = past_scores
  }
  
  # List scores which occurred at or before the current game's round
  scores_historical = imap_dfr(ps_current_team$player_id, function(player, index){
    # Join each player's comparable games to their scores from those games
    left_join(ps_comparable[[index]], 
              scores_comparable, 
              by = c("game_id", "player_id")) %>% 
      # Remove hard-coded games without player stats?
      filter(!(game_id %in% 38:48))
  })
  
  # When in progress, keep the shot counter generated from current_shots
  if(in_progress){
    scores_historical = mutate(scores_historical, shots = current_shots)
  }
  
  # Now, this table is going to be plugged in to the pipeline that currently exists in the team summary tab function.
  # That means I have to recreate player_stats using this table 
  
  # Calculate game performance in comparable games
  ps_historical = replace_na(scores_historical,
                             list(points_scored = 0, paddle = F, clink = F, foot = F)) %>% 
    # Group by game and player, (team and shots are held consistent)
    group_by(game_id, player_id, shots) %>% 
    # Calculate summary stats
    summarise(total_points = sum(points_scored),
              # ones = sum((points_scored == 1)),
              # twos = sum((points_scored == 2)),
              # threes = sum((points_scored == 3)),
              # impossibles = sum((points_scored > 3)),
              paddle_points = sum(points_scored* (paddle | foot)),
              clink_points = sum(points_scored*clink),
              points_per_round = total_points / last(shots),
              off_ppr = sum(points_scored * !(paddle | foot))/ last(shots),
              def_ppr = paddle_points/last(shots),
              toss_efficiency = sum((points_scored>0)*!(paddle | foot ))/last(shots),
              .groups = "drop")
  
  # Calculate current game performance
  #   - Team score
  #   - Which team is winning
  # Then join on player info
  current_game_stats = group_by(ps_current, team) %>% 
    mutate(team_score = sum(total_points)) %>% 
    ungroup() %>% 
    mutate(winning = (team_score == max(team_score))) %>% 
    filter(team == team_name) %>% 
    select(team, winning, player_id,  
           total_points, paddle_points, clink_points, #threes, 
           points_per_round:toss_efficiency)
  
  historical_avg = select(ps_historical, game_id, player_id, 
                          shots, total_points, paddle_points, clink_points, #sinks = threes, 
                          points_per_round:toss_efficiency) %>%
    arrange(player_id, game_id) %>% 
    group_by(player_id) %>% 
    summarise(
      across(.cols = c(total_points, paddle_points, clink_points), .fns = mean, .names = "{col}_avg"),
      # across(.cols = c(sinks), .fns = sum, .names = "{col}_total"),
      across(.cols = c(points_per_round, off_ppr, def_ppr, toss_efficiency),
             .fns = ~weighted.mean(., w = shots), 
             .names = "{col}_wavg"),
      .groups = "drop"
    )
  
  current_game_comparison = full_join(current_game_stats, historical_avg, by = "player_id") %>% 
    # Calculate the difference between current game and historical performance
    mutate(total_points_diff = total_points - total_points_avg,
           paddle_points_diff = paddle_points - paddle_points_avg,
           clink_points_diff = clink_points - clink_points_avg,
           points_per_round_diff = points_per_round - points_per_round_wavg,
           off_ppr_diff = off_ppr - off_ppr_wavg,
           def_ppr_diff = def_ppr - def_ppr_wavg,
           toss_efficiency_diff = toss_efficiency - toss_efficiency_wavg,
           # Format each difference for the table
           across(matches("points_diff"), ~str_c(if_else(.x >= 0, "+", ""), round(.x, 1))),
           across(matches("(per_round|ppr)_diff$"), ~str_c(if_else(.x >= 0, "+", ""), round(.x, 1))),
           toss_efficiency_diff = map_chr(toss_efficiency_diff, 
                                          ~case_when(. >= 0 ~ toss_percent_plus(.), 
                                                     . < 0 ~ toss_percent_minus(.))))
  
  current_game_comparison %>% 
    # Remove historical stats
    select(-ends_with("_avg"), -ends_with("wavg"), -contains("points_per")) %>% 
    # Order columns
    select(starts_with("player"), team, winning, 
           contains("total_points"), contains("paddle"), contains("clink"), #sinks = threes, 
           contains("per_round"), contains("off_"), contains("def_"), contains("toss"))
  
  
}



# Database updating -------------------------------------------------------



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
    col_updates = group_by(player_stats, player_id) %>% 
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



