careerStatsUI = function(id) {
  career_stats_tab(ns = NS(id))
}

careerStatsServer = function(id, con) {
  moduleServer(id, function(input, output, session) {

    output$leaderboard_rt = renderReactable({
      req(input$leaderboard_range)
      leaderboard_stats = calculate_leaderboard_stats(
        con,
        min_date = input$leaderboard_range[1],
        max_date = input$leaderboard_range[2]
      )
      leaderboard_table_rt(collect(leaderboard_stats))
    })

    output$leaderboard_date_filter = renderUI({
      ns = session$ns
      games = tbl(con, "game_stats") |>
        summarise(min_date = min(as.Date(game_start), na.rm = T))
      current_date = today(tzone = "America/Los_Angeles")
      tagList(
        dateRangeInput(ns("leaderboard_range"), label = "Timeframe",
                       startview = "year",
                       start = floor_date(current_date, unit = "year"), end = current_date,
                       min = pull(games, min_date), max = current_date, format = "M d, yyyy"),
        actionButton(ns("leaderboard_all"),        label = "All",            class = "btn-primary"),
        actionButton(ns("leaderboard_past_year"),  label = "Past 12 Months", class = "btn-primary"),
        actionButton(ns("leaderboard_past_6mo"),   label = "Past 6 Months",  class = "btn-primary"),
        actionButton(ns("leaderboard_past_3mo"),   label = "Past 3 Months",  class = "btn-primary"),
        actionButton(ns("leaderboard_past_month"), label = "Past Month",     class = "btn-info")
      )
    })

    observeEvent(input$leaderboard_all, {
      games = tbl(con, "game_stats") |>
        summarise(min_date = min(as.Date(game_start), na.rm = T))
      updateDateRangeInput(inputId = "leaderboard_range",
                           start = pull(games, min_date),
                           end = today(tzone = "America/Los_Angeles"))
    })
    observeEvent(input$leaderboard_past_year, {
      updateDateRangeInput(inputId = "leaderboard_range",
                           start = today(tzone = "America/Los_Angeles") %m-% months(12),
                           end = today(tzone = "America/Los_Angeles"))
    })
    observeEvent(input$leaderboard_past_6mo, {
      updateDateRangeInput(inputId = "leaderboard_range",
                           start = today(tzone = "America/Los_Angeles") %m-% months(6),
                           end = today(tzone = "America/Los_Angeles"))
    })
    observeEvent(input$leaderboard_past_3mo, {
      updateDateRangeInput(inputId = "leaderboard_range",
                           start = today(tzone = "America/Los_Angeles") %m-% months(3),
                           end = today(tzone = "America/Los_Angeles"))
    })
    observeEvent(input$leaderboard_past_month, {
      updateDateRangeInput(inputId = "leaderboard_range",
                           start = today(tzone = "America/Los_Angeles") %m-% months(1),
                           end = today(tzone = "America/Los_Angeles"))
    })

    output$scoring_heatmap = renderPlot({
      score_heatmap(tbl(con, "score_progression"))
    }, res = 96)

    output$heatmap_info = renderUI({
      req(input$heat_hover)
      x = round(input$heat_hover$x, 0)
      y = round(input$heat_hover$y, 0)
      freq = filter(tbl(con, "score_progression"), score_a == y, score_b == x) %>%
        pull(n)
      HTML(str_c(
        "<p><span style='font-weight:500'>Team B</span>: ", x, "  ",
        "<span style='font-weight:500'>Team A</span>: ", y, "</p>",
        "<p><span style='font-weight:500'>How many occurrences?</span>: ", freq
      ))
    })

  })
}
