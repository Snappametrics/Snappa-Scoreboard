snappa_server <- function(input, output, session, game_session, game_state, ui_state, con) {

  observeEvent(input$debug, {
    browser()
  })

  w <- Waiter$new(
    html = tagList(
      spin_pixel(),
      str_c("Yeeting Imaginary Dice Into The Sky")
    ))

  # Team Input module -------------------------------------------------------

  ti <- teamInputServer("team_input", con, game_session, game_state, ui_state)

  # Game Summary reactive (defined before scoreboardServer so it can be passed in)
  game_summary <- reactive({
    if (!isTRUE(ti$started())) {
      df         <- filter(game_session$db_tbls()[["game_stats"]], game_id == max(game_id))
      subtitle_a <- if_else(df$points_a > df$points_b, "the winners.", "the losers.")
      subtitle_b <- if_else(df$points_a < df$points_b, "the winners.", "the losers.")
      list(df = df, subtitle_a = subtitle_a, subtitle_b = subtitle_b)
    } else {
      df <- replace_na(game_state$game_stats_db,
                       list(points_a = game_state$current_scores$team_A,
                            points_b = game_state$current_scores$team_B))
      score_difference <- abs(df$points_a - df$points_b)
      subtitle_a <- if_else(df$points_a > df$points_b, "in the lead.", str_c("chasing ", score_difference, "."))
      subtitle_b <- if_else(df$points_a < df$points_b, "in the lead.", str_c("chasing ", score_difference, "."))
      list(df = df, subtitle_a = subtitle_a, subtitle_b = subtitle_b)
    }
  })

  # Scoreboard module -------------------------------------------------------

  scoreboardServer("scoreboard", con, game_session, game_state, ui_state,
                   snappaneers  = ti$snappaneers,
                   score_to     = ti$score_to,
                   round_num    = ti$round_num,
                   started      = ti$started,
                   game_summary = game_summary)

  # Sidebar menu ------------------------------------------------------------

  output$sidebar_menu <- renderUI({
    if (isTRUE(ti$started())) {
      sidebarMenu(
        menuItem("Scoreboard",   tabName = "scoreboard",   icon = icon("window-maximize"), selected = T),
        menuItem("Career Stats", tabName = "career_stats", icon = icon("chart-column")),
        menuItem("Player Stats", tabName = "player_stats", icon = icon("chart-line"))
      )
    } else {
      sidebarMenu(
        menuItem("Player Input", tabName = "player_input", icon = icon("users"), selected = T),
        menuItem("Career Stats", tabName = "career_stats", icon = icon("chart-column")),
        menuItem("Player Stats", tabName = "player_stats", icon = icon("chart-line"))
      )
    }
  })

  # Recent Scores -----------------------------------------------------------

  output$recent_scores_rt <- renderReactable({
    sentence_width <- as.numeric(dbGetQuery(con, sql("SELECT MAX(char_length(what_happened)*6.5) FROM recent_scores")))

    column_defs <- list(
      scoring_team = colDef(show = F),
      player_name  = colDef(
        align    = "right",
        minWidth = 80,
        style    = JS(str_c("function(rowInfo) {
                    var value = rowInfo.row['scoring_team']
                    if (value == 'A') {
                      var color = '", snappa_pal[2], "'
                    } else {
                      var color = '", snappa_pal[3], "'
                    }
                    return { color: color, fontWeight: 'bold', padding: '5px' }
                                     }"))
      ),
      what_happened = colDef(width = sentence_width)
    )

    reactable(head(game_session$recent_scores(), n = 5),
              compact = T,
              defaultColDef = colDef(name = "", style = list(padding = "5px 0px"),
                                     headerStyle = list(alignSelf = "flex-end", display = "none")),
              columns = column_defs)
  })

  output$downloadData <- downloadHandler(
    filename = function() { paste('data-', Sys.Date(), '.csv', sep = '') },
    content  = function(con) { write.csv(game_state$scores_db, con) }
  )

  # Game Summary Stats ------------------------------------------------------

  team_a_summary_stats <- reactive({
    scores <- game_session$db_tbls()[["scores"]]
    if (!isTRUE(ti$started())) {
      past_games_scores <- filter(scores, game_id != max(game_id))
      player_performance_summary(game_started = 0L,
                                 player_stats = game_session$db_tbls()[["player_stats"]],
                                 team_name    = "A",
                                 past_scores  = past_games_scores)
    } else {
      past_games_scores <- filter(scores, game_id != game_session$game_id)
      player_performance_summary(game_started  = 1L,
                                 game_session  = game_session,
                                 game_state    = game_state,
                                 player_stats  = game_session$db_tbls()[["player_stats"]],
                                 team_name     = "A",
                                 current_round = ti$round_num(),
                                 past_scores   = past_games_scores)
    }
  }, label = "Team A Summary")

  team_b_summary_stats <- reactive({
    scores <- game_session$db_tbls()[["scores"]]
    if (!isTRUE(ti$started())) {
      past_games_scores <- filter(scores, game_id != max(game_id))
      player_performance_summary(game_started = 0L,
                                 player_stats = game_session$db_tbls()[["player_stats"]],
                                 team_name    = "B",
                                 past_scores  = past_games_scores)
    } else {
      past_games_scores <- filter(scores, game_id != game_session$game_id)
      player_performance_summary(game_started  = 1L,
                                 game_session  = game_session,
                                 game_state    = game_state,
                                 player_stats  = game_session$db_tbls()[["player_stats"]],
                                 team_name     = "B",
                                 current_round = ti$round_num(),
                                 past_scores   = past_games_scores)
    }
  }, label = "Team B Summary")

  output$a_breakdown <- renderPlot({
    max_player_points <- max(team_b_summary_stats()$total_points, team_a_summary_stats()$total_points) + 1
    if (!isTRUE(ti$started())) {
      player_score_breakdown(
        snappaneers = select(filter(game_session$db_tbls()[["player_stats"]], game_id == max(game_id), team == "A"), player_id, team, shots),
        scores      = filter(game_session$db_tbls()[["scores"]], game_id == max(game_id)),
        ps_players  = game_session$players,
        ps_team     = "A",
        chart_max   = max_player_points)
    } else {
      player_score_breakdown(
        snappaneers = select(filter(game_state$player_stats_db, game_id == game_session$game_id, team == "A"), player_id, team, shots),
        scores      = game_state$scores_db,
        ps_players  = game_session$players,
        ps_game     = game_session$game_id,
        ps_team     = "A",
        chart_max   = max_player_points)
    }
  }, bg = snappa_pal[1])

  output$b_breakdown <- renderPlot({
    max_player_points <- max(team_b_summary_stats()$total_points, team_a_summary_stats()$total_points) + 1
    if (!isTRUE(ti$started())) {
      player_score_breakdown(
        snappaneers = select(filter(game_session$db_tbls()[["player_stats"]], game_id == max(game_id), team == "B"), player_id, team, shots),
        scores      = filter(game_session$db_tbls()[["scores"]], game_id == max(game_id)),
        ps_players  = game_session$players,
        ps_team     = "B",
        chart_max   = max_player_points)
    } else {
      player_score_breakdown(
        snappaneers = select(filter(game_state$player_stats_db, game_id == game_session$game_id, team == "B"), player_id, team, shots),
        scores      = game_state$scores_db,
        ps_players  = game_session$players,
        ps_game     = game_session$game_id,
        ps_team     = "B",
        chart_max   = max_player_points)
    }
  }, bg = snappa_pal[1])

  output$game_flow <- renderPlot({
    if (!isTRUE(ti$started())) {
      game_flow(player_stats = filter(game_session$db_tbls()[["player_stats"]], game_id == max(game_id)),
                players      = game_session$players,
                scores       = filter(game_session$db_tbls()[["scores"]], game_id == max(game_id)),
                game         = filter(game_session$db_tbls()[["game_stats"]], game_id == max(game_id))$game_id)
    } else {
      game_flow(player_stats = game_state$player_stats_db,
                players      = game_session$players,
                scores       = game_state$scores_db,
                game         = game_session$game_id)
    }
  })

  output$team_a_summary <- renderReactable({
    team_summary_tab_rt(right_join(game_session$players, team_a_summary_stats(), by = "player_id"))
  })

  output$team_b_summary <- renderReactable({
    team_summary_tab_rt(right_join(game_session$players, team_b_summary_stats(), by = "player_id"))
  })

  # Game summary button (in header — root session) --------------------------

  observeEvent(input$game_summary, {
    game_summary_modal(game_summary()$df, ti$round_num(),
                       game_summary()$subtitle_a, game_summary()$subtitle_b)
  })

  # Career Stats module -----------------------------------------------------

  careerStatsServer("career", con)

  # Player Stats module -----------------------------------------------------

  playerStatsServer("player_stats", con)

}
