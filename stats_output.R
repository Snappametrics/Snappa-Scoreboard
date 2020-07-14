theme_snappa = function(){
  theme_minimal(
    base_family = "Inter",
    base_size = 14
  ) %+replace%
    theme(
      title = element_text(family = "Inter")
    )
}

top_scorers_tab = left_join(players_tbl, player_stats_tbl, by = "player_id") %>%
  select(-player_id) %>% 
  # Calculate leaderboard
  group_by(player_name) %>% 
  summarise(
    total_points = sum(total_points),
    offensive_points = sum(off_ppr*shots),
    defensive_points = sum(def_ppr*shots),
    ones = sum(ones),
    twos = sum(twos),
    threes = sum(threes),
    paddle_points = sum(paddle_points),
    clink_points = sum(clink_points),
    total_shots = sum(shots),
    points_per_round = weighted.mean(points_per_round, w = shots),
    off_ppr = weighted.mean(off_ppr, w = shots),
    def_ppr = weighted.mean(def_ppr, w = shots),
    toss_efficiency = weighted.mean(toss_efficiency, w = shots)
  ) %>% 
  ungroup() %>% 
  filter_at(vars(-player_name), any_vars(!is.na(.))) %>% 
  mutate(rank = rank(-total_points)) %>% 
  select(rank, everything(), -offensive_points:-clink_points) %>% 
  arrange(rank) %>% 
  gt(id = "leaderboard") %>% 
  tab_header(title = "Snappa Leaderboard") %>% 
  # Column names
  cols_label(
    rank = "Rank", 
    player_name = "Player",
    total_points = "Points",
    total_shots = "Shots Taken",
    points_per_round = "Points per Round (PPR)",
    off_ppr = "Offensive PPR",
    def_ppr = "Defensive PPR",
    toss_efficiency = "Toss Efficiency"
  ) %>% 
  # Format integers
  fmt_number(
    columns = vars(rank, total_points, total_shots),
    decimals = 0
  ) %>% 
  # Format doubles
  fmt_number(
    columns = vars(points_per_round, off_ppr, def_ppr),
    decimals = 3
  ) %>% 
  # Format percentages
  fmt_percent(
    columns = vars(toss_efficiency),
    decimals = 1
  ) %>% 
  tab_footnote(
    footnote = "Defensive points are scored from paddles.",
    locations = cells_column_labels(columns = vars(def_ppr))
  ) %>% 
  tab_footnote(
    footnote = "% of tosses which are successful.",
    locations = cells_column_labels(columns = vars(toss_efficiency))
  ) %>%
  opt_footnote_marks(marks = "letters")



score_progression = scores_tbl %>% 
  arrange(game_id, score_id) %>% 
  group_by(game_id) %>% 
  mutate(
    score_a = cumsum((scoring_team=="a")*points_scored),
    score_b = cumsum((scoring_team=="b")*points_scored)
  ) %>% 
  ungroup() %>% 
  count(score_a, score_b)

max_score = summarise_at(score_progression, vars(-n), max) %>% 
  pull() %>% 
  max()

score_prog_plot = score_progression %>% 
  ggplot(aes(x = score_b, y = score_a))+
  geom_bin2d(binwidth = c(1,1))+
  scale_fill_gradient(name = "Frequency", low = "#ffeda0", high = "#f03b20")+
  labs(x = "Team B",
       y = "Team A",
       title = "Scoring Heatmap")+
  scale_x_continuous(breaks = seq.int(from = 0, to = max_score, by = 5))+
  scale_y_continuous(breaks = seq.int(from = 0, to = max_score, by = 5))+
  theme_snappa()
