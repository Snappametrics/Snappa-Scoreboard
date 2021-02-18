# This is a script to run the Markov model so that I don't have to look at my
# Rmd file every time I want to run the model. It's also a space to clean
# up the script that runs the simulation into more discrete functions


# This function creates all the "helper" tables that are needed in team_transitions
# and elsewhere in the funcitons. It is just easier to keep them all in one
# place, and to avoid making other functions take 6 arguments 

helper_tables = function(player_stats, scores, team_vector){
  
  team_combinations = player_stats %>% 
    group_by(game_id, team) %>%
    arrange(player_id) %>%
    mutate(position = row_number()) %>%
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
    
    offense_filtered = filtered_game %>% 
      filter(off_def == "offense") %>%
      ungroup() %>%
      select(round_num, shot_order, off_def, points_scored)
    
    defense_filtered = filtered_game %>% 
      filter(off_def == "defense") %>%
      ungroup() %>%
      select(round_num, shot_order, off_def, points_scored)
    
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
        fill = list(points_scored = 0,
                    off_def = "offense")) 

    expanded_game_defense = defense_filtered %>%
      complete(
        expand(defense_filtered,
               round_num,
               shot_order,
               off_def),
        fill = list(points_scored = 0,
                    off_def = "defense"))
    
    
    expanded_game = bind_rows(expanded_game_offense,
                              expanded_game_defense)
    
    # Use the information from game_team_pair to figure out ordering
    team = tables$game_team_pair %>%
      filter(game_id == game) %>%
      pull(team)

    # I need to make sure that the running score is actually correctly accounting
    # for the order of play in each game. While I have the game, also
    # make sure to add an extra value to the table 
    if (team == "A"){
      expanded_game = expanded_game %>% 
        arrange(round_num, desc(off_def), shot_order) %>%
        select(off_def, points_scored)
      expanded_game = bind_rows(tibble(off_def = "offense", points_scored = 0),
                                expanded_game)
    } else {
      expanded_game = expanded_game %>% 
        arrange(round_num, off_def, shot_order) %>%
        select(off_def, points_scored)
      expanded_game = bind_rows(tibble(off_def = "defense", points_scored = 0),
                                expanded_game)
    }
    
    
    
    score_side_table = expanded_game %>%
                        summarize(running_score = cumsum(points_scored),
                                  off_def = off_def,
                                  .groups = "drop")
    # If this is only one observation long, then this game isn't in scores, so dip
    if (nrow(score_side_table) == 1){
      if (type == "scores") {
        offense_matrix = matrix(data = 0, nrow = 51, ncol = 51)
        defense_matrix = matrix(data = 0, nrow = 51, ncol = 51)
      } else{
        offense_matrix = matrix(data = 0, nrow = 8, ncol = 8)
        defense_matrix = matrix(data = 0, nrow = 8, ncol = 8)
      }
      return(list("offense" = offense_matrix,
                  "defense" = defense_matrix))
    } else {
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
    }
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
  
  
  transitions = team_transitions(analysis_tables, type)
  matrix_rank = if_else(type == "scores", 51, 8)
  
  off_transition_counts = transitions[[1]]$offense
  if (length(transitions) > 1){
    for (i in 2:length(transitions)){
      off_transition_counts = off_transition_counts + transitions[[i]]$offense
    }
  } else { 
    invisible() 
    }
    
  row_totals = map_dbl(seq(1, matrix_rank), function(number) { 
                                 off_transition_counts[number,] %>% sum()
                                 }
                       )
    off_transition_probs = off_transition_counts / row_totals
    off_transition_probs[which(off_transition_probs %>% is.nan())] = 0
  
  
  def_transition_counts = transitions[[1]]$defense
  if (length(transitions) > 1){
  for (i in 2:length(transitions)){
    def_transition_counts = def_transition_counts + transitions[[i]]$defense
  }
  } else {
    invisible()
  }
  row_totals = map_dbl(seq(1, matrix_rank), function(number) { 
    def_transition_counts[number,] %>% sum()
  }
  )
  def_transition_probs = def_transition_counts / row_totals
  def_transition_probs[which(def_transition_probs %>% is.nan())] = 0
  
  return_list = list("offensive" = off_transition_probs,
                     "defensive" = def_transition_probs)
  return(return_list)
}

in_rebuttal = function(a, b, round, points_to_win){
  case_when(all(a >= points_to_win, a - b >= 2, round == "A") ~ T, 
            all(b >= points_to_win, b - a >= 2, round == "B") ~ T,
            !any(all(a >= points_to_win, a - b >= 2, round == "A"),
                 all(b >= points_to_win, b - a >= 2, round == "B")) ~ F)
}

# Still a rather complex function, but for now I'm leaving it
markov_single_game = function(A_team_size, B_team_size, shots, team_A_transitions, team_A_backup,
                              team_B_transitions, team_B_backup, points_to_win,
                              scores_A, scores_B){
  # We'll track scores between teams according to a time series of each team's score.
  # The first score for both teams is 0
  team_A_history = c(scores_A) 
  team_B_history = c(scores_B)
  
  # Initialize the game conditions
  game_over = F
  rebuttal_tag_A = F
  rebuttal_tag_B = F
  A_in_rebuttal = F
  B_in_rebuttal = F
  
  while (game_over == F){
    
    # Simulate A's offense. Each team gets separate draws at values from their respective
    # matrix. Because we're looking at each team and don't need to be as dynamic as
    # we usually would with rebuttal check. 
    
    round = "A"
    
    
    if (rebuttal_tag_B){
      B_in_rebuttal = in_rebuttal(A_score, B_score, round, points_to_win)
      if (B_in_rebuttal){
        game_over = T
        break()
      }
    }
    

    
    for (i in 1:shots){
      
      
      A_state = team_A_history %>% last() + 1
      
      
      if (A_state >= 52){
        current_probs_A = 0
      } else {
        current_probs_A = team_A_transitions$offensive[A_state,]
      }
      
      # In the event that this didn't work OR the team is stuck somewhere, 
      # replace the current
      # transition with the backup
      if (any(all(current_probs_A == 0), any(current_probs_A == 1 ),
              any(current_probs_A %>% is.infinite()))){
        # In the event that your game has just started, you can just
        # assume the state to be 0. This will likely only be an issue
        # for the team that is on defense in round 1, where subtracting
        # the most recent score from the one before doesn't work
        if (length(team_A_history) == 1){
          A_state = 0
        } else {
          A_state =team_A_history[length(team_A_history)] - team_A_history[length(team_A_history) - 1] 
        }
        
        current_probs_A = team_A_backup$offensive[A_state + 1, ]
        if (all(current_probs_A == 0)){
          current_probs_A = team_A_backup$offensive[1, ]
        }
        shot_draw_A = which(rmultinom(1,1, current_probs_A) == 1)
        points = shot_draw_A - 1
        team_A_history = team_A_history %>% 
          append(team_A_history[length(team_A_history)] + points)
      } else {
        shot_draw_A = which(rmultinom(1,1,current_probs_A) == 1)
        #Update A's history with the new score
        team_A_history = team_A_history %>% 
          append(shot_draw_A - 1)
      }
      
      
      # If A is in rebuttal this round and just exited, untag them
      A_score = team_A_history %>% last()
      B_score = team_B_history %>% last()
      if (rebuttal_tag_A){
        A_in_rebuttal = (B_score - A_score >= 2)
        if (!A_in_rebuttal){
          # Escaped rebuttal
          rebuttal_tag_A = F
        }
      }
      
      
      B_state = team_B_history %>% last() + 1
      
      
      # Occasionally, games run away and we get 51+ point game projections
      # Not likely empirically, but anything goes when games are allowed to be close.
      # I'll just put a condition in to handle that case by working off of the
      # states probabilities 
      if (B_state >= 52){
        current_probs_B = 0
      } else {
        current_probs_B = team_B_transitions$defensive[B_state,]
      }
      
      
      if (any(all(current_probs_B == 0), any(current_probs_B == 1 ),
              any(current_probs_B %>% is.infinite()))){
        if (length(team_B_history) == 1){
          B_state = 0
        } else {
          B_state =team_B_history[length(team_B_history)] - team_B_history[length(team_B_history) - 1] 
        }

        current_probs_B = team_B_backup$defensive[B_state + 1, ]
        
        # So, there's still a possibility that this ends up being 0. This is,
        # at least in part, an artifact of the problem of moving scores
        # based only on scoring points, and not accurately tracking the 
        # true time series of points. There's simply no way that Matthew and I,
        # with as many games as we have, have never transitioned from 2 points
        # to something else on defense. For now, I just use the probability of
        # a transition from 0 points 
        if (all(current_probs_B == 0)){
          current_probs_B = team_B_backup$defensive[1, ]
        }
        
        shot_draw_B = which(rmultinom(1,1, current_probs_B) == 1)
        points = shot_draw_B - 1
        team_B_history = team_B_history %>% 
          append(team_B_history[length(team_B_history)] + points)
      } else {
        shot_draw_B = which(rmultinom(1,1,current_probs_B) == 1)
        #Update A's history with the new score
        team_B_history = team_B_history %>% 
          append(shot_draw_B - 1)
      }      
      
      A_score = team_A_history %>% last()
      B_score = team_B_history %>% last()
      
      
      
    }
    # Tag B for rebuttal, if the round gets back to A and this tag is still
    # true, then the game is over. Only check this on the last shot
    
    B_in_rebuttal = in_rebuttal(A_score, B_score, round, points_to_win)
    if (B_in_rebuttal){
      rebuttal_tag_B = T
    }
    
    
    # Start B's Round
    round = "B"
    
    if (rebuttal_tag_A){
      A_in_rebuttal = in_rebuttal(A_score, B_score, round, points_to_win)
      if (A_in_rebuttal){
        game_over = T
        break()
      }
    }  
    # Simulate B's round
    for (i in 1:shots){
      
      B_state = team_B_history %>% last() + 1
      if (B_state >= 52){
        current_probs_B = 0
      } else {
        current_probs_B = team_B_transitions$offensive[B_state,]
      }
      
      if (any(all(current_probs_B == 0), any(current_probs_B == 1 ),
              any(current_probs_B %>% is.infinite()))){

        B_state =team_B_history[length(team_B_history)] - team_B_history[length(team_B_history) - 1] 
        
        current_probs_B = team_B_backup$offensive[B_state + 1, ]
        if (all(current_probs_B == 0)){
          current_probs_B = team_B_backup$offensive[1, ]
        }
        
        shot_draw_B = which(rmultinom(1,1, current_probs_B) == 1)
        points = shot_draw_B - 1
        team_B_history = team_B_history %>% 
          append(team_B_history[length(team_B_history)] + points)
      } else {
        shot_draw_B = which(rmultinom(1,1,current_probs_B) == 1)
        #Update A's history with the new score
        team_B_history = team_B_history %>% 
          append(shot_draw_B - 1)
      }      
      
      A_score = team_A_history %>% last()
      B_score = team_B_history %>% last()
      if (rebuttal_tag_B){
        B_in_rebuttal = (A_score - B_score >= 2)
        if (!B_in_rebuttal){
          # Escaped rebuttal
          rebuttal_tag_B = F
        }
      }
      
      A_state = team_A_history %>% last() + 1
      if (A_state >= 52){
        current_probs_A = 0
      } else {
        current_probs_A = team_A_transitions$defensive[A_state,]
      }    
      if (any(all(current_probs_A == 0), any(current_probs_A == 1 ),
              any(current_probs_A %>% is.infinite()))){
        
        A_state =team_A_history[length(team_A_history)] - team_A_history[length(team_A_history) - 1] 
  
        current_probs_A = team_A_backup$defensive[A_state + 1, ]
        if (all(current_probs_A == 0)){
          current_probs_A = team_A_backup$defensive[1, ]
        }
        shot_draw_A = which(rmultinom(1,1, current_probs_A) == 1)
        points = shot_draw_A - 1
        team_A_history = team_A_history %>% 
          append(team_A_history[length(team_A_history)] + points)
      } else {
        shot_draw_A = which(rmultinom(1,1,current_probs_A) == 1)
        #Update A's history with the new score
        team_A_history = team_A_history %>% 
          append(shot_draw_A - 1)
      }
      
      A_score = team_A_history %>% last()
      B_score = team_B_history %>% last()
      
      
      
      
    }
    # Check if A is in rebuttal at the end of the round
    
    A_in_rebuttal = in_rebuttal(A_score, B_score, round, points_to_win)
    if (A_in_rebuttal){
      rebuttal_tag_A = T
    }
    
  }
  # Now that the game is over, collect the relevant information and dip
  A_scores = team_A_history[-1]
  B_scores = team_B_history[-1]
  
  num_rounds = length(A_scores) / (shots * 2)
  if (num_rounds %% 1 != 0){
    num_rounds = num_rounds %>% ceiling()
    rounds = str_c(rep(1:num_rounds, each = shots * 2), rep(c(rep("A", shots), rep("B", shots)), num_rounds)) %>%
      head(-shots)
  } else {
    rounds = str_c(rep(1:num_rounds, each = shots * 2), rep(c(rep("A", shots), rep("B", shots)), num_rounds))
  }

  names(A_scores) = rounds
  names(B_scores) = rounds
  
  return(list(team_A = A_scores, team_B = B_scores,
              won = case_when(A_scores %>% last() > B_scores %>% last() ~ "A",
                              A_scores %>% last() < B_scores %>% last() ~ "B"),
              final_round = names(A_scores) %>% last()) )
}


markov_simulate_games = function(team_A, team_B, iterations = 50, points_to_win = 21,
                                 transitions_list = NULL,
                                 current_scores_A = 0,
                                 current_scores_B = 0){
  # Prevents anything from being displayed in the app if the markov_vals reactive
  # is passed as a null.
  if (any(is.null(current_scores_A), is.null(current_scores_B))){
    stop("Click the button above to run your simulations")
  }
  
  # obtain the transition probs
  if (is.null(transitions_list)){
    team_A_transitions = transition_probabilities(player_stats, scores, "scores", team_A[1], team_A[2], team_A[3], team_A[4])
    team_A_backup = transition_probabilities(player_stats, scores, "states", team_A[1], team_A[2], team_A[3], team_A[4])
    team_B_transitions = transition_probabilities(player_stats, scores, "scores", team_B[1], team_B[2], team_B[3], team_B[4])
    team_B_backup = transition_probabilities(player_stats, scores, "states", team_B[1], team_B[2], team_B[3], team_B[4])
  } else {
    
    team_A_name = str_c("(", team_A[1], ",", team_A[2], 
                        if_else(!is.na(team_A[3]), str_c(", ", team_A[3]), ""),
                        if_else(!is.na(team_A[4]), str_c(", ", team_A[4]), ""),
                        ")"
      )
      
      
    team_B_name = str_c("(", team_B[1], ",", team_B[2], 
                        if_else(!is.na(team_B[3]), str_c(", ", team_B[3]), ""),
                        if_else(!is.na(team_B[4]), str_c(", ", team_B[4]), ""),
                        ")"
    )
      
    team_A_transitions = transitions_list[[team_A_name]]$scores
    team_A_backup = transitions_list[[team_A_name]]$states
    team_B_transitions = transitions_list[[team_B_name]]$scores
    team_B_backup = transitions_list[[team_B_name]]$states
  }  
  # At this point, I run the same simulation as before, but now I map it over
  # a sequence to iterate
  A_size = length(team_A %>% discard(is.na))
  B_size = length(team_B %>% discard(is.na))
  
  shots = max(A_size, B_size)
  
  
  games_record = c(seq(1, iterations)) %>% map(function(iteration_number){
    markov_single_game(A_size, B_size, shots, 
                       team_A_transitions, team_A_backup, 
                       team_B_transitions, team_B_backup,
                       points_to_win = points_to_win,
                       current_scores_A,
                       current_scores_B
                      )
  })
  return(games_record)
}

###### Analysis #####
## This is no longer the focus of this script. However, these functions
# will be useful once the model is integrated into the app

# hi_there = markov_simulate_games(c(2,9, NA, NA), c(2, 9, NA, NA), 1000)
# 
# # Answer the basic questions
# # Who won more? What's their win rate?
# wins = hi_there %>% map_chr( function(element){
#   element$won
# }) 
# A_winrate = length(wins[wins == "A"])/length(wins)
# B_winrate = 1 - A_winrate
# 
# # In the games where the winningest team won, what was the modal score?
# winners = if_else(which(c(A_winrate, B_winrate) == max(c(A_winrate, B_winrate))) == 1,
#                   "A",
#                   "B")
# 
# # Using a matrix here so that I can treat each pairing of points as a unique
# # value, rather than each team's individual total (meaning that I am truly 
# # looking for the pair of scores which are modal in the set of games that 
# # the winning team won)
# 
# scores_matrices = seq(1, length(hi_there)) %>% map(function(game_number){
#   if (hi_there[[game_number]]$won != winners){
#     matrix = matrix(0, nrow = 51, ncol = 51)
#   } else {
#     team_A_score = hi_there[[game_number]]$team_A %>% last()
#     team_B_score = hi_there[[game_number]]$team_B %>% last()
#     matrix = matrix(0, nrow = 51, ncol = 51)
#     matrix[team_A_score, team_B_score] = 1
#   }
#   return(matrix)
# })
# 
# scores_matrix = scores_matrices %>% reduce(`+`, .init = matrix(0, nrow = 51, ncol = 51))
# modal_score_position = which(scores_matrix == max(scores_matrix), arr.ind = T)
# modal_A_score = modal_score_position[1]
# modal_B_score = modal_score_position[2]
