# For the sake of code simplicity, I'm going to define a function which
# writes player_stats_db off of scores. This should only require
# a scores table to be passed on, since the snappaneers table that is also
# called would always be the same
app_update_player_stats = function(scores_df, neers, game){
  output = scores_df %>% 
    # Join scores to snappaneers to get each player's team
    right_join(neers, by = "player_id") %>% 
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
              def_ppr = na_if(sum(points_scored * (paddle | foot)) /last(shots), Inf),
              toss_efficiency = sum(!(paddle | foot)) / last(shots)) %>% 
    ungroup() %>% 
    # Replace NA values with 0s
    replace_na(list(total_points = 0, 
                    ones = 0, twos = 0, threes = 0, impossibles = 0, 
                    paddle_points = 0, clink_points = 0, 
                    points_per_round = 0, off_ppr = 0, def_ppr = 0, toss_efficiency = 0))
  
  output = troll_check(snappaneers = neers,
                       player_stats = output,
                       game_id = game)
  return(output)
}

toss_percent_plus = function(x){
  str_c("(+", round(x*100, 0), "%)")
}
toss_percent_minus = function(x){
  str_c("(", round(x*100, 0), "%)")
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

troll_check = function(session, snappaneers, player_stats, game_id){
  
  
  trolls = snappaneers %>%
    anti_join(player_stats, by = "player_id")
  
  bind_rows(player_stats, 
            tibble(
              game_id = rep(game_id, times = nrow(trolls)),
              player_id = pull(trolls, player_id), 
              team = pull(trolls, team), 
              total_points = rep(integer(1), times = nrow(trolls)), # Weirdly enough, integer(1) is a 0 integer vector of length 1
              shots = pull(trolls, shots),
              ones = rep(integer(1), times = nrow(trolls)),
              twos = rep(integer(1), times = nrow(trolls)),
              threes = rep(integer(1), times = nrow(trolls)),
              impossibles = rep(integer(1), times = nrow(trolls)),
              paddle_points = rep(integer(1), times = nrow(trolls)),
              clink_points = rep(integer(1), times = nrow(trolls)),
              points_per_round = rep(double(1), times = nrow(trolls)),
              off_ppr = rep(double(1), times = nrow(trolls)),
              def_ppr = rep(double(1), times = nrow(trolls)),
              toss_efficiency = rep(double(1), times = nrow(trolls))
            )
  )
  
}

aggregate_player_stats = function(scores, players){
  scores %>% 
    # Join scores to snappaneers to get each player's team
    left_join(players, by = "player_id") %>% 
    # Group by game and player, (team and shots are held consistent)
    group_by(game_id, player_id, team, shots) %>% 
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
              toss_efficiency = sum(!(paddle | foot ))/last(shots)) %>% 
    ungroup()
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

db_update_player_stats = function(player_stats, specific_player){
  
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

cooldown_check = function(current_game, current_round, casualty_to_check, rounds){
  # Check whether the last casualty of a certain type occurred 
  # a full round (both teams shot) ago, determining whether the rule is reactivated
  #   rounds is the round_label vector i.e. "1A", "1B", etc.
  
  # Pull the round_num associated with the casualties in the current game
  # Also count the number of times -1 the casualty has occurred
  last_casualty_df = dbGetQuery(con, sql(str_c("SELECT sc.round_num, COUNT(*)-1 AS times 
                                                FROM casualties
                                                JOIN scores sc
                                                USING (game_id, score_id)
                                                WHERE casualty_type = '", casualty_to_check, 
                                               "' AND game_id = ", current_game,
                                               "GROUP BY sc.round_num")))
  
  # Increment the round by 2x the times the casualty has been repeated
  last_casualty_round = which(rounds == last_casualty_df$round_num) + last_casualty_df$times*2
  
  which(rounds == current_round) - last_casualty_round < 2
}
