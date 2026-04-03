teamInputUI <- function(id) {
  team_input_tab(ns = NS(id))
}

teamInputServer <- function(id, con, game_session, game_state, ui_state) {
  moduleServer(id, function(input, output, session) {

    # Reactive for the score to play to
    score_to <- reactive({ input$score_to })

    # Increment round number
    round_num <- reactive({ rounds[game_state$shot_num] })

    # Active input buttons: list of player inputs which are not null
    active_player_inputs <- reactive({
      list(
        "A1" = input$name_A1, "A2" = input$name_A2, "A3" = input$name_A3,
        "A4" = input$name_A4, "A5" = input$name_A5,
        "B1" = input$name_B1, "B2" = input$name_B2, "B3" = input$name_B3,
        "B4" = input$name_B4, "B5" = input$name_B5
      ) %>%
        discard(is_null)
    })

    player_inputs <- reactive({
      tribble(
        ~input, ~team, ~player_name, ~expected,
        "A1", "A", input$name_A1, T,
        "A2", "A", input$name_A2, T,
        "A3", "A", input$name_A3, input$add_player_A3,
        "A4", "A", input$name_A4, input$add_player_A4,
        "A5", "A", input$name_A5, input$add_player_A5,
        "B1", "B", input$name_B1, T,
        "B2", "B", input$name_B2, T,
        "B3", "B", input$name_B3, input$add_player_B3,
        "B4", "B", input$name_B4, input$add_player_B4,
        "B5", "B", input$name_B5, input$add_player_B5
      )
    })

    expected_player_inputs <- reactive({
      player_inputs() |> filter(expected)
    })

    # Snappaneers: Team | Player name | Player ID | Shots
    snappaneers <- reactive({
      player_inputs() |>
        filter(player_name != "") |>
        select(-expected) |>
        filter(player_name != "") %>%
        left_join(game_session$players, by = "player_name") %>%
        add_shot_count(shot_num = game_state$shot_num)
    })

    # Vector of players, with current players removed
    current_choices <- reactive({
      dbGetQuery(con, "SELECT player_id, player_name FROM thirstiest_players") %>%
        anti_join(., snappaneers(), by = "player_name") %>%
        pull(player_name)
    })

    # Track whether the game has started
    started <- reactiveVal(NULL)

    # Validate start button state
    output$validate_start <- reactive({
      if (length(unique(expected_player_inputs()$player_name)) != nrow(expected_player_inputs()) |
          any(expected_player_inputs()$player_name == "")) {
        shinyjs::disable("start_game")
      } else {
        shinyjs::enable("start_game")
      }
    })

    # Incomplete Game ---------------------------------------------------------

    observe({
      validate(
        need(
          !dbGetQuery(con, "SELECT game_complete FROM game_stats WHERE game_id = (SELECT MAX(game_id) FROM game_stats)")[1, 1],
          message = FALSE
        )
      )
      lost_game_id <- dbGetQuery(con, "SELECT game_id FROM game_stats WHERE game_id = (SELECT MAX(game_id) FROM game_stats)")[1, 1]

      total_lost_game_score <- dbGetQuery(con, str_c("SELECT SUM(total_points) FROM player_stats WHERE game_id = ", lost_game_id))[1, 1] %>%
        replace_na(0)

      if (total_lost_game_score == 0) {
        delete_query <- sql("DELETE FROM game_stats WHERE game_id = (SELECT MAX(game_id) FROM game_stats);")
        dbExecute(con, delete_query)
      } else {
        last_game_ps_tbl <- tbl(con, "incomplete_game") |>
          select(game_id) |>
          left_join(tbl(con, "player_stats"), by = "game_id") |>
          inner_join(tbl(con, "players"), by = "player_id") |>
          select(player_name, team, total_points)

        restart_game_popup(last_game_ps_tbl, ns = session$ns)
      }
    })

    # Resume yes: restore lost game state
    observeEvent(input$resume_yes, {
      lost_game <- tbl(con, "incomplete_game")

      lost_player_stats <- lost_game |>
        select(game_id) |>
        left_join(tbl(con, "player_stats"), by = "game_id") |>
        left_join(tbl(con, "players"), by = "player_id")

      lost_players <- lost_player_stats |>
        select(player_name, team) |>
        group_by(team) |>
        mutate(player_input = str_c("name_", team, row_number())) |>
        ungroup() |>
        collect()

      input_list <- lost_players |>
        select(player_input, player_name) |>
        deframe()

      iwalk(input_list, function(name, id) {
        updateSelectizeInput(session, inputId = id, selected = name)
      })

      lost_game_id <- collect(lost_game)$game_id

      game_state$current_scores$team_A <- dbGetQuery(con, str_c("SELECT SUM(total_points) FROM player_stats WHERE team = 'A' AND game_id = ", lost_game_id))[1, 1] %>%
        as.numeric()

      game_state$current_scores$team_B <- dbGetQuery(con, str_c("SELECT SUM(total_points) FROM player_stats WHERE team = 'B' AND game_id = ", lost_game_id))[1, 1] %>%
        as.numeric()

      game_state$score_id <- dbGetQuery(con, str_c("SELECT MAX(score_id) FROM scores WHERE game_id = ", lost_game_id))[1, 1] %>%
        as.numeric()

      game_state$scores_db <- dbGetQuery(con, str_c("SELECT * FROM scores WHERE game_id = ", lost_game_id))
      game_session$game_id <- lost_game_id
      game_state$shot_num <- parse_round_num(collect(lost_game)$last_round)
      game_state$game_stats_db <- collect(lost_game)
      game_state$player_stats_db <- collect(lost_player_stats)
      game_state$casualties <- as_tibble(dbGetQuery(con, str_c("SELECT * FROM casualties WHERE game_id = ", lost_game_id)))

      removeModal()
      delay(500, shinyjs::click("start_game"))
    })

    # Resume no: delete incomplete game
    observeEvent(input$resume_no, {
      removeModal()
      delete_query <- "DELETE FROM game_stats WHERE game_id = (SELECT MAX(game_id) FROM game_stats);"
      dbExecute(con, delete_query)
    })

    # Game Start --------------------------------------------------------------

    observeEvent(input$start_game, {
      if (as.integer(collect(tally(tbl(con, "incomplete_game")))) == 0) {
        arena_select_popup()
      }

      game_state$cooldowns <- reactivePoll(
        intervalMillis = 100 * 70,
        session = session,
        checkFunc = function() { nrow(game_state$casualties) },
        valueFunc = function() {
          map(
            unique(casualty_rules$casualty_title),
            ~cooldown_check(
              casualties = game_state$casualties[game_state$casualties$casualty_type == .x, ],
              scores = game_state$scores_db,
              current_round = round_num(),
              rounds = rounds
            )
          ) |>
            set_names(unique(casualty_rules$casualty_title))
        }
      )

      showNotification(str_c("Game is being played to ", input$score_to, " points!"), type = "message")

      started(TRUE)
    })

    observeEvent(input$arena_select, {
      # Add new players to the players table
      iwalk(snappaneers()$player_name, function(die_thrower, index) {
        if (!(die_thrower %in% game_session$players$player_name)) {
          dbAppendTable(con, "players",
                        tibble(
                          player_id = game_session$new_player_id,
                          player_name = die_thrower
                        ))

          game_session$players <- collect(tbl(con, "players"))
          game_session$new_player_id <- game_session$new_player_id + 1
        } else {
          invisible()
        }
      })

      # Check if the last game was finished
      if (!dbGetQuery(con, "SELECT game_complete FROM game_stats WHERE game_id = (SELECT MAX(game_id) FROM game_stats)")[1, 1]) {

        lost_game <- dbGetQuery(con, "SELECT MAX(game_id) FROM game_stats")[1, 1]
        lost_game_stats <- dbGetQuery(con, str_c("SELECT * FROM game_stats WHERE game_id = ", lost_game))
        lost_player_stats <- dbGetQuery(con, str_c("SELECT * FROM player_stats WHERE game_id = ", lost_game))

        game_state$current_scores$team_A <- dbGetQuery(con, str_c("SELECT SUM(total_points) FROM player_stats WHERE team = 'A' AND game_id = ", lost_game))[1, 1] %>%
          as.numeric()

        game_state$current_scores$team_B <- dbGetQuery(con, str_c("SELECT SUM(total_points) FROM player_stats WHERE team = 'B' AND game_id = ", lost_game))[1, 1] %>%
          as.numeric()

        game_state$score_id <- dbGetQuery(con, str_c("SELECT MAX(score_id) FROM scores WHERE game_id = ", lost_game))[1, 1] %>%
          as.numeric()

        game_state$scores_db <- dbGetQuery(con, str_c("SELECT * FROM scores WHERE game_id = ", lost_game))
        game_session$game_id <- lost_game
        game_state$shot_num <- parse_round_num(lost_game_stats$last_round)
        game_state$game_stats_db <- lost_game_stats
        game_state$player_stats_db <- lost_player_stats
        game_state$casualties <- as_tibble(dbGetQuery(con, str_c("SELECT * FROM casualties WHERE game_id = ", lost_game)))

      } else {
        game_state$current_scores$team_A <- 0
        game_state$current_scores$team_B <- 0

        game_session$game_id <- as.integer(dbGetQuery(con, "SELECT MAX(game_id)+1 FROM game_stats"))

        game_state$game_stats_db <- bind_rows(game_state$game_stats_db,
                                              tibble(
                                                game_id       = game_session$game_id,
                                                num_players   = nrow(snappaneers()),
                                                game_start    = strtrim(as.character(now(tzone = "America/Los_Angeles")), 19),
                                                game_end      = NA_character_,
                                                night_dice    = NA,
                                                points_a      = NA_integer_,
                                                points_b      = NA_integer_,
                                                rounds        = NA_integer_,
                                                ones          = NA_integer_,
                                                twos          = NA_integer_,
                                                threes        = NA_integer_,
                                                impossibles   = NA_integer_,
                                                paddle_points = NA_integer_,
                                                clink_points  = NA_integer_,
                                                game_complete = F,
                                                last_round    = "1A",
                                                arena         = input$arena_select
                                              ))

        dbWriteTable(conn = con, name = "game_stats",
                     value = game_state$game_stats_db, append = T)

        game_state$player_stats_db <- aggregate_player_stats(game_state$scores_db,
                                                              snappaneers(),
                                                              game = game_session$game_id)

        dbWriteTable(conn = con, name = "player_stats",
                     value = game_state$player_stats_db, append = T)
      }
    })

    # Extra Player Toggle Observers -------------------------------------------

    ## Team A
    observe({ toggleState(id = "player-input-A3", condition = input$add_player_A3) })
    observeEvent(isFALSE(input$add_player_A3), { reset("player-input-A3") }, ignoreInit = T)

    observe({ toggleState(id = "player-input-A4", condition = input$add_player_A4) })
    observeEvent(isFALSE(input$add_player_A4), { reset("player-input-A4") }, ignoreInit = T)

    observe({ toggleState(id = "player-input-A5", condition = input$add_player_A5) })
    observeEvent(isFALSE(input$add_player_A5), { reset("player-input-A5") }, ignoreInit = T)

    ## Team B
    observe({ toggleState(id = "player-input-B3", condition = input$add_player_B3) })
    observeEvent(isFALSE(input$add_player_B3), { reset("player-input-B3") }, ignoreInit = T)

    observe({ toggleState(id = "player-input-B4", condition = input$add_player_B4) })
    observeEvent(isFALSE(input$add_player_B4), { reset("player-input-B4") }, ignoreInit = T)

    observe({ toggleState(id = "player-input-B5", condition = input$add_player_B5) })
    observeEvent(isFALSE(input$add_player_B5), { reset("player-input-B5") }, ignoreInit = T)

    # Return reactive values to parent -----------------------------------------

    list(
      started     = started,
      snappaneers = snappaneers,
      score_to    = score_to,
      round_num   = round_num
    )
  })
}
