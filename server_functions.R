# For the sake of code simplicity, I'm going to define a function which
# writes player_stats_db off of scores. This should only require
# a scores table to be passed on, since the snappaneers table that is also
# called would always be the same
app_update_player_stats = function(scores_df, neers, game){
  g.id = game
  output = scores_df %>% 
    # Join scores to snappaneers to get each player's team
    left_join(neers, by = "player_id") %>% 
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
  
  output = troll_check(snappaneers = neers,
                       player_stats = output,
                       game_id = g.id)
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

db_update_player_stats = function(player_stats){
  current_game = unique(player_stats$game_id)
  del_rows = sql(str_c("DELETE FROM player_stats 
                 WHERE game_id = ", current_game, ";"))
  
  dbExecute(con, del_rows)
  
  dbWriteTable(con, "player_stats", player_stats, append = T)
}

db_update_round = function(round, game){
  # Update round number in game_stats
  dbExecute(con, 
            sql(str_c("UPDATE game_stats 
                 set last_round = '", round, "'
                ",
                      "WHERE game_id = ", game, ";")))
}


