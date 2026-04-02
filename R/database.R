# R/database.R
# All database read/write functions, centralised here so SQL lives in one place.



# Polling functions -----------------------------------------------------------

db_poll_completed_tables = function(con, tbls) {
  # Returns a named list of data frames, one per table in tbls.
  # Used as the valueFunc for the db_tbls reactivePoll.
  set_names(
    map(tbls, function(table) {
      dbGetQuery(
        con,
        sql(str_c(
          "SELECT * FROM ", table,
          if_else(
            table %in% c("scores", "player_stats", "game_stats"),
            " WHERE game_id IN (SELECT game_id FROM game_stats WHERE game_complete is true)",
            ""
          )
        ))
      )
    }),
    tbls
  )
}

db_poll_recent_scores = function(con) {
  dbGetQuery(con, sql("SELECT * FROM recent_scores"))
}



# Leaderboard -----------------------------------------------------------------

calculate_leaderboard_stats = function(
    con,
    min_date = ymd("2023-01-01"),
    max_date = ceiling_date(today(), unit = "year")) {

  games_filtered = tbl(con, "game_stats") |>
    mutate(game_start_date = as.Date(game_start),
           game_end_date = as.Date(game_end)) |>
    filter(game_start_date >= min_date,
           game_end_date <= max_date) |>
    select(game_id, points_a, points_b)

  score_stats = games_filtered |>
    left_join(tbl(con, "scores"), by = "game_id") |>
    mutate(sinks = case_when(points_scored == 3 & !clink ~ 1,
                             T ~ 0),
           paddle_sinks = case_when(points_scored == 3 & !clink & paddle ~ 1,
                                    T ~ 0),
           foot_sinks = case_when(points_scored == 3 & !clink & foot ~ 1,
                                  T ~ 0),
           foot_paddle_points = case_when(foot ~ points_scored,
                                          T ~ 0)) |>
    group_by(player_id) |>
    summarise(
      across(c(sinks, paddle_sinks, foot_paddle_points, foot_sinks), \(x) sum(x, na.rm = T))
    )

  games_filtered |>
    left_join(tbl(con, "player_stats"), by = "game_id") |>
    mutate(game_won = case_when(points_a > points_b & team == "A" ~ 1,
                                points_a < points_b & team == "B" ~ 1,
                                T ~ 0)) |>
    group_by(player_id) |>
    summarise(
      games_played = n(),
      win_pct = sum(game_won, na.rm = T) / n(),
      across(c(total_points, clink_points, paddle_points), \(x) sum(x, na.rm = T)),
      points_per_game = mean(total_points, na.rm = T),
      toss_efficiency = sum(shots * toss_efficiency, na.rm = T) / sum(shots, na.rm = T),
      offensive_points = sum(off_ppr * shots, na.rm = T),
      off_ppg = mean(off_ppr * shots, na.rm = T),
      defensive_points = sum(def_ppr * shots, na.rm = T),
      def_ppg = mean(def_ppr * shots, na.rm = T)
    ) |>
    left_join(score_stats, by = "player_id") |>
    replace_na(list(sinks = 0, paddle_sinks = 0, foot_paddle_points = 0, foot_sinks = 0)) |>
    inner_join(tbl(con, "players"), by = "player_id") |>
    arrange(desc(total_points)) |>
    mutate(rank = row_number())
}



# Player stats writes ---------------------------------------------------------

db_update_player_stats = function(player_stats, specific_player, round_button = F) {

  if (round_button) {
    col_updates = select(player_stats, game_id, player_id, shots, points_per_round:toss_efficiency) %>%
      group_by(player_id) %>%
      group_map(~t(.), .keep = T) %>%
      map(~str_c(rownames(.), " = ", ., collapse = ", ")) %>%
      set_names(player_stats$player_id)

    update_player_stats_queries = imap(col_updates,
                                       ~str_c("UPDATE player_stats
                                             SET ", .x,
                                              " WHERE game_id = ", unique(player_stats$game_id),
                                              " AND player_id = ", .y, ";"))
    walk(update_player_stats_queries, ~dbExecute(con, .))
    return(invisible())
  }

  # Quote character vars using the DB driver's quoting (handles apostrophes, etc.)
  player_stats = mutate(player_stats, across(where(is_character), ~as.character(dbQuoteLiteral(con, .))))

  if (missing(specific_player)) {
    col_updates = group_by(player_stats, player_id) %>%
      group_map(~t(.), .keep = T) %>%
      map(~str_c(rownames(.), " = ", ., collapse = ", ")) %>%
      set_names(player_stats$player_id)

    update_player_stats_queries = imap(col_updates,
                                       ~str_c("UPDATE player_stats
                                             SET ", .x,
                                              " WHERE game_id = ", unique(player_stats$game_id),
                                              " AND player_id = ", .y, ";"))
    walk(update_player_stats_queries, ~dbExecute(con, .))
  } else {
    col_updates = t(filter(player_stats, player_id == specific_player)) %>%
      str_c(rownames(.), " = ", ., collapse = ", ")

    update_player_stats_query = str_c("UPDATE player_stats
                              SET ", col_updates,
                              " WHERE game_id = ", unique(player_stats$game_id),
                              " AND player_id = ", specific_player, ";")

    dbExecute(con, update_player_stats_query)
  }
}

db_update_round = function(round, game) {
  dbExecute(con,
            "UPDATE game_stats SET last_round = $1 WHERE game_id = $2",
            params = list(round, as.integer(game)))
}
