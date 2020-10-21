library(ggplot2)
library(ggpattern)
library(tidyverse)
library(DBI)
library(RPostgres)

# Start simple by designing a data frame to handle a probit model. This will be a
# dataframe with observations at the team level. I think that it makes the most sense 
# to model whether or not team A won in each game, and then use an array of dummies for
# being on or against team A

source("dbconnect.R")



game_stats = dbReadTable(con, "game_stats")
player_stats = dbReadTable(con, "player_stats") 
players = dbReadTable(con, "players")
#Probably not needed
scores = dbReadTable(con, "scores")


#create a helper with pivot_wider that will record the player id's on each
# team in each game. Then, once I summarize df, I can simply join it in

team_players_wide = player_stats %>% 
  group_by(game_id, team) %>% 
  mutate(number = row_number()) %>%
  pivot_wider(id_cols= game_id, names_from = c(team, number), values_from = player_id)

valid_players = player_stats %>%
  filter(player_id %in% players$player_id[which(
  player_stats %>% 
    count(player_id) %>%  
    pull(n) >= 10)]
  ) %>%
  pull(player_id) %>%  unique()


# Challenge: only take in those games which featured players that have played 
# more than 10 games



valid_games = player_stats %>%
  select(game_id, player_id) %>%
  group_by(game_id) %>%
  nest() %>%
  arrange(game_id) %>%
  mutate(all_valid = map_lgl(data, function(player_vec){
    all(pull(player_vec) %in% valid_players)
  })
  ) %>% filter(all_valid) %>%
  pull(game_id)


# I should take the difference in the teams' ratios as my independent variables,
# so it's probably best to just difference these things out
#DF 1
df_pivot = player_stats %>% 
        group_by(game_id, team) %>%
        summarize(points = sum(total_points),
                  across(ones:clink_points, 
                         function(column){sum(column) / points}),
                  across(points_per_round:def_ppr, sum),
                  # Take the average of player's TE to find team TE
                  team_te = mean(toss_efficiency)
                  ) %>%
      # pivot_wider to make this one observation per game
        pivot_wider(names_from = team, 
                    names_glue = "{.value}_{team}", 
                    values_from = points:team_te
                    ) %>%
        mutate(team_a_won = if_else(points_A > points_B, 1, 0)
               ) %>%
        left_join(team_players_wide, by = "game_id")
      

#DF 2 
df_pct_difference = player_stats %>% 
          filter(game_id %in% valid_games) %>%
          group_by(game_id, team) %>%
          summarize(points = sum(total_points),
                    across(ones:clink_points, 
                           function(column){sum(column) / points}),
                    across(points_per_round:def_ppr, sum),
                    # Take the average of player's TE to find team TE
                    team_te = mean(toss_efficiency)
          ) %>%
          arrange(game_id, team) %>%
          summarize(team = team,
                    across(ones:def_ppr, function(column){column - last(column)})
          ) %>%
          left_join(df_pivot %>% select(game_id, team_a_won), by = "game_id") %>%
          filter(team == "A") %>% 
          select(-team) %>% 
          left_join(team_players_wide, by = "game_id")
        
# DF 3
df_abs_difference = player_stats %>%
  filter(player_id %in% players$player_id[which(
                                          player_stats %>% 
                                            count(player_id) %>%  
                                            pull(n) >= 10)]
         ) %>%
  group_by(game_id, team) %>%
  summarize(points = sum(total_points),
            across(ones:clink_points, 
                   function(column){sum(column)}),
            across(points_per_round:def_ppr, sum),
            # Take the average of player's TE to find team TE
            team_te = mean(toss_efficiency),
  ) %>% 
  arrange(game_id, team) %>%
  summarize(team = team,
            across(ones:def_ppr, function(column){column - last(column)})
  ) %>%
  left_join(df_pivot %>% select(game_id, team_a_won), by = "game_id") %>%
  filter(team == "A") %>% 
  select(-team) %>% 
  left_join(team_players_wide, by = "game_id")




run_probit_model = function(df) {

model_ids = tibble(player_id =  
                     unique(c(df$A_1, df$A_2, df$A_3, df$A_4,
                              df$B_1, df$B_2, df$B_3, df$B_4)
                            ) %>% discard(is.na)) 
            
                   
# Create an expression to make the mutations to the table to add dummies


cols_to_make_A = rlang::parse_exprs(
  paste0('ifelse(',  model_ids %>% pull(player_id), '%in% c(A_1,A_2,A_3,A_4), 1,0)')
  )
cols_to_make_B = rlang::parse_exprs(
  paste0('ifelse(',  model_ids %>% pull(player_id), '%in% c(B_1,B_2,B_3,B_4), 1,0)')
)



df_dummies = df %>% mutate(!!!cols_to_make_A, !!!cols_to_make_B) %>% ungroup()


names(df_dummies) = c(names(df), 
                      str_c("A_", model_ids %>% 
                              left_join(players, by = "player_id") %>% 
                              arrange(player_id) %>% 
                              pull(player_name)
                            ),
                      str_c("B_", model_ids %>% 
                              left_join(players, by = "player_id") %>% 
                              arrange(player_id) %>% 
                              pull(player_name)
                      )
                      )
df_dummies = df_dummies %>%
  select(-c(game_id, "A_1", "A_2", "A_3", "A_4", "B_1", "B_2", "B_3", "B_4", 
                                        "A_Dewey", "B_Dewey"))




df_dummies$team_a_won = factor(df_dummies$team_a_won)
# The model. At first a very noisy model

model = glm(team_a_won ~ ., family = binomial(link = "probit"), 
    data = df_dummies)

return(model)
## model summary
summary(model)


}

pct_model = run_probit_model(df_pct_difference)


# See how a given player influences the winrate relative to the dummy left out 
player_influence = function(df, player_name){
  # generate a tibble that can be filled in with values for the contrast
  df_A = df %>% slice(0) %>%
    
  
  
}