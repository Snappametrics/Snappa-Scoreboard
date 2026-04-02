playerStatsUI = function(id) {
  player_stats_tab(ns = NS(id))
}

playerStatsServer = function(id, con) {
  moduleServer(id, function(input, output, session) {

    # Win/loss record per game, used by player_form_data
    player_game_stats = reactive({
      completed_games = tbl(con, "game_stats") |>
        filter(game_complete) |>
        select(game_id, points_a, points_b)

      inner_join(tbl(con, "players"),
                 tbl(con, "player_stats"),
                 by = "player_id") |>
        inner_join(completed_games, by = "game_id") |>
        mutate(winning = if_else(points_a > points_b, "A", "B"),
               won_game = if_else(team == winning, "Won", "Lost")) |>
        select(player_id, game_id, won_game)
    })

    player_form_data = reactive({
      player_selected = as.integer(input$player_select)

      player_career = tbl(con, "player_stats") |>
        filter(player_id == player_selected) |>
        mutate(avg_points = mean(!!sym(input$stat_select)),
               max_points = max(!!sym(input$stat_select))) |>
        select(game_id, player_id, !!sym(input$stat_select), avg_points, max_points)

      if (input$sample_select != "All") {
        player_career = player_career |>
          slice_max(order_by = game_id, n = as.numeric(input$sample_select))
      }

      recent_games = player_career |>
        left_join(player_game_stats(), by = c("player_id", "game_id")) |>
        arrange(game_id) |>
        mutate(game_num = row_number(), .before = "player_id")

      collect(recent_games)
    })

    teammate_stats = reactive({
      dbGetQuery(con,
                 "SELECT * FROM teammate_stats WHERE player_id = $1",
                 params = list(as.integer(input$player_select)))
    })

    output$teammate_tab_rt = renderReactable({
      select(teammate_stats(), -1:-2) %>%
        reactable(
          defaultSorted = "games_played",
          columns = list(
            teammate = colDef(
              name = "Teammate"
            ),
            games_played = colDef(
              name = "Games Played",
              defaultSortOrder = "desc"
            ),
            win_pct = colDef(
              name = "Win %",
              defaultSortOrder = "desc",
              cell = function(value) {
                value <- str_c(format(value * 100, nsmall = 1), "%")
                value <- format(value, width = 6, justify = "right")
                bar_chart(value, width = value, fill = snappa_pal[5], background = "#DEDDDD")
              },
              align = "right"
            ),
            avg_points = colDef(
              name = "Avg. Points",
              defaultSortOrder = "desc",
              format = colFormat(digits = 2)
            ),
            avg_paddle_points = colDef(
              name = "Avg. Paddle Points",
              defaultSortOrder = "desc",
              format = colFormat(digits = 2)
            )
          ),
          compact = T, defaultPageSize = 10
        )
    })

    output$game_history_title = renderText({
      player_name = dbGetQuery(con,
                               "SELECT player_name FROM players WHERE player_id = $1",
                               params = list(as.integer(input$player_select)))$player_name
      str_c(player_name, "'s Game History")
    })

    player_game_history = reactive({
      dbGetQuery(con,
                 "SELECT game_id,
                      to_date(gs.game_start, 'YYYY-MM-DD') as date,
                      (to_timestamp(gs.game_end, 'YYYY-MM-DD HH24:MI:SS')-to_timestamp(gs.game_start, 'YYYY-MM-DD HH24:MI:SS')) AS game_length,
                      ps.team,
                      CASE ps.team
                          WHEN 'A' THEN gs.points_a || ' - ' || gs.points_b
                          WHEN 'B' THEN gs.points_b || ' - ' || gs.points_a
                      END AS final_score,
                      teammates.teammates,
                      round(ps.shots::numeric, 2) as shots,
                      ps.total_points,
                      ps.clink_points, ps.paddle_points,
                      sc.foot_paddles,
                      sc.sinks, sc.paddle_sinks, sc.foot_sinks,
                      ps.points_per_round, ps.off_ppr, ps.def_ppr, ps.toss_efficiency
                 FROM player_stats ps
                 INNER JOIN game_stats gs USING (game_id)
                 INNER JOIN (SELECT game_id, team, string_agg(players.player_name, ', ') AS teammates
                             FROM player_stats
                             INNER JOIN players USING (player_id)
                             WHERE player_id != $1
                             GROUP BY game_id, team
                             ORDER BY game_id DESC) AS teammates USING (game_id, team)
                 INNER JOIN (SELECT scores.game_id,
                                    scores.player_id,
                                    sum(scores.points_scored) AS total_points,
                                    sum(CASE WHEN scores.points_scored = 3 AND scores.clink = false THEN 1 ELSE 0 END) AS sinks,
                                    sum(CASE WHEN scores.points_scored = 3 AND scores.clink = false AND scores.paddle = true THEN 1 ELSE 0 END) AS paddle_sinks,
                                    SUM(CASE WHEN scores.foot = TRUE THEN scores.points_scored ELSE 0 END) AS foot_paddles,
                                    SUM(CASE WHEN scores.points_scored = 3 AND scores.clink = FALSE AND scores.foot = TRUE THEN 1 ELSE 0 END) AS foot_sinks
                             FROM scores
                             GROUP BY scores.game_id, scores.player_id) sc USING (game_id, player_id)
                 WHERE ps.player_id = $1
                 ORDER BY game_id DESC",
                 params = list(as.integer(input$player_select)))
    })

    output$player_game_stats = renderReactable({
      reactable(
        player_game_history(),
        defaultSorted = "game_id",
        defaultSortOrder = "desc",
        columns = list(
          game_id = colDef(name = "Game", width = 78),
          date = colDef(name = "Date", width = 107),
          game_length = colDef(name = "Game Length"),
          team = colDef(name = "Team", width = 72, sortable = F),
          final_score = colDef(
            name = "Final Score", width = 75,
            style = function(value) {
              blue_team = as.numeric(str_extract(value, "^[0-9]{1,2}"))
              red_team  = as.numeric(str_extract(value, "[0-9]{1,2}$"))
              bg_color  = if_else(blue_team > red_team, snappa_pal[5], snappa_pal[2])
              list(background = bg_color, color = snappa_pal[1])
            },
            sortable = F
          ),
          teammates     = colDef(name = "Teammate(s)", width = 115, sortable = F),
          shots         = colDef(name = "Shots", width = 77),
          total_points  = colDef(name = "Points", width = 81),
          clink_points  = colDef(name = "Clink Points", width = 81),
          paddle_points = colDef(name = "Paddle Points", width = 86),
          foot_paddles  = colDef(name = "Foot Paddles"),
          sinks         = colDef(name = "Sinks"),
          paddle_sinks  = colDef(name = "Paddle Sinks"),
          foot_sinks    = colDef(name = "Foot Sinks"),
          points_per_round = colDef(name = "Points per Round (PPR)", format = colFormat(digits = 2)),
          off_ppr       = colDef(name = "Off. PPR", format = colFormat(digits = 2)),
          def_ppr       = colDef(name = "Def. PPR", format = colFormat(digits = 2)),
          toss_efficiency = colDef(name = "Toss Efficiency", format = colFormat(digits = 1, percent = T))
        ),
        compact = T
      )
    })

    output$player_form = renderPlot({
      player_form_plot(input$stat_select, player_form_data())
    })

    overall_player_stats = reactive({
      dbGetQuery(con, sql("SELECT * FROM basic_career_stats")) %>%
        filter(player_id == !!input$player_select)
    })

    output$general_stats = renderReactable({
      mutate(overall_player_stats(),
             sink_freq = HTML(if_else(sinks > 0,
                                     str_c("<span style='font-weight: 500;'>Every </span>",
                                           round(1 / (sinks / games_played), 1),
                                           "<span style='font-weight: 500;'> games</span>"),
                                     "TBD"))) %>%
        select(`GAMES` = games_played,
               `WIN %` = win_pct,
               `SINKS` = sinks,
               `SINK FREQUENCY` = sink_freq) %>%
        reactable(
          fullWidth = T,
          rowStyle = list(alignItems = "center"),
          defaultColDef = colDef(
            footer = JS("function(cellInfo) { return cellInfo.column.id }"),
            footerStyle = list(fontSize = "13px", borderTop = "none", fontWeight = 600),
            style = list(padding = "3px 4px"),
            headerStyle = list(display = "none"),
            align = "center"
          ),
          columns = list(
            games_played = colDef(name = "GAMES"),
            `WIN %` = colDef(format = colFormat(percent = T, digits = 1)),
            `SINK FREQUENCY` = colDef(html = T, style = list(fontSize = "14px"))
          )
        )
    })

    output$paddle_stats = renderReactable({
      select(overall_player_stats(),
             `PADDLE POINTS` = paddle_points,
             `PADDLE SINKS`  = paddle_sinks,
             `FOOT PADDLES`  = foot_paddles,
             `FOOT SINKS`    = foot_sinks) %>%
        reactable(
          fullWidth = T,
          rowStyle = list(alignItems = "center"),
          defaultColDef = colDef(
            footer = JS("function(cellInfo) { return cellInfo.column.id }"),
            footerStyle = list(fontSize = "13px", borderTop = "none", fontWeight = 600),
            format = colFormat(digits = 0, separators = T),
            style = list(padding = "3px 4px"),
            headerStyle = list(display = "none"),
            align = "center"
          )
        )
    })

    casualty_stats = reactive({
      req(input$player_select)
      filter(dbGetQuery(con, sql("SELECT * FROM casualty_stats")),
             player_id == input$player_select) %>%
        right_join(
          tibble(
            player_id    = as.integer(input$player_select),
            casualty_type = c("Sunk", "Self sink", "Team sink", "12-7", "War of 1812", "2003")
          ),
          by = c("player_id", "casualty_type")
        ) %>%
        replace_na(list(casualties = 0))
    })

    output$casualty_stats_plot = renderPlot({
      req(input$player_select)

      no_casualties = tibble(
        x = 0,
        y = 0,
        group = casualty_stats()[casualty_stats()$casualties == 0, "casualty_type", drop = T]
      ) %>%
        group_split(group)

      waffle_rows = 6

      waffle_cols = filter(dbGetQuery(con, sql("SELECT * FROM casualty_stats")),
                           casualties == max(casualties)) %>%
        transmute(columns = ceiling(casualties / waffle_rows)) %>%
        deframe()

      waffle_list = uncount(casualty_stats(), weights = casualties) %>%
        group_split(casualty_type)

      waffle_sample_size = map_dbl(waffle_list, nrow)

      waffle_data = map2_dfr(waffle_list, waffle_sample_size,
                             ~waffle_iron(.x, rows = min(c(.y, waffle_rows)),
                                          mapping = aes_d(group = casualty_type))) %>%
        bind_rows(no_casualties) %>%
        group_by(group) %>%
        mutate(
          casualties = if_else(x == 0, 0L, n()),
          group_lab  = factor(group,
                              levels = c("Sunk", "Self sink", "Team sink", "12-7", "War of 1812", "2003"),
                              labels = str_wrap(c("Sunk", "Self sink", "Team sink", "Pearl Harbour", "War of 1812", "2003"), 8))
        ) %>%
        ungroup()

      labels = list(x = mean(c(1, waffle_cols)), y = waffle_rows + 1.3)

      ggplot(waffle_data, aes(x, y)) +
        geom_tile(data = waffle_data[waffle_data$x > 0, ],
                  aes(fill = group), colour = snappa_pal[1], size = 2) +
        geom_text(data = distinct(waffle_data, group_lab, casualties),
                  aes(label = casualties, x = labels$x, y = labels$y),
                  colour = "gray20", size = 6, hjust = 0.5, family = "Inter Medium") +
        scale_fill_manual(values = c("Sunk"         = snappa_pal[3],
                                     "Self sink"    = snappa_pal[4],
                                     "Team sink"    = snappa_pal[2],
                                     "12-7"         = snappa_pal[5],
                                     "War of 1812"  = snappa_pal[6],
                                     "2003"         = snappa_pal[7]),
                          guide = guide_none()) +
        scale_x_continuous(limits = c(.5, waffle_cols + 1)) +
        scale_y_continuous(limits = c(.5, waffle_rows + 1.3), expand = expansion(add = c(0, .6))) +
        facet_wrap(~group_lab, nrow = 2, strip.position = "top") +
        theme_snappa(md = T, base_size = 20, plot_margin = margin(10, 10, 10, 10)) +
        theme(axis.text = element_blank(), axis.text.y.left = element_blank(),
              axis.title = element_blank(), axis.line = element_blank(),
              panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
              panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
              strip.text = element_text(face = "bold", hjust = .4, margin = margin(b = 0)))
    })

  })
}
