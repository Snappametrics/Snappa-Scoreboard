scoreboardUI <- function(id) {
  scoreboard_tab(ns = NS(id))
}

scoreboardServer <- function(id, con, game_session, game_state, ui_state,
                              snappaneers, score_to, round_num, started,
                              game_summary) {
  moduleServer(id, function(input, output, session) {

    # Enable tifu button once game starts
    observe({
      req(isTRUE(started()))
      shinyjs::enable("tifu")
    })

    # Score Outputs -----------------------------------------------------------

    output$score_A <- renderText({ game_state$current_scores$team_A })
    output$score_B <- renderText({ game_state$current_scores$team_B })

    output$playing_to <- renderUI({
      p("Playing to: ", strong(score_to()))
    })

    output$round_num <- renderUI({
      team_colours <- list("A" = "#e26a6a", "B" = "#2574a9")
      HTML(str_c(
        '<h3 class="numbers">',
        str_extract(round_num(), "[0-9]+"),
        '<span style="color:', team_colours[[str_extract(round_num(), "[AB]+")]], ';">',
        str_extract(round_num(), "[AB]+"), "</span>",
        "</h3>"
      ))
    })

    output$round_control_buttons <- renderUI({
      team_colours <- list("A" = "danger", "B" = "primary")
      div(class = "round-control",
        actionBttn(session$ns("previous_round"),
                   label = "Previous Round", style = "jelly", icon = icon("arrow-left"),
                   color = team_colours[[str_extract(round_num(), "[AB]+")]], size = "lg"),
        actionBttn(session$ns("next_round"),
                   label = "Pass the dice", style = "jelly", icon = icon("arrow-right"),
                   color = team_colours[[str_extract(round_num(), "[AB]+")]], size = "lg")
      )
    })

    output$active_die_left <- renderUI({
      switch_is_even <- (ui_state$switch_counter %% 2 == 0)
      if (switch_is_even) {
        img(src = "die_hex.png", style = str_c("background: transparent;display: flex;transform: scale(1.25);position: relative;top: -1vh; display:",
                                               if_else(str_extract(round_num(), "[AB]+") == "B", "block;", "none;")))
      } else {
        img(src = "die_hex.png", style = str_c("background: transparent;display: flex;transform: scale(1.25);position: relative;top: -1vh; display:",
                                               if_else(str_extract(round_num(), "[AB]+") == "A", "block;", "none;")))
      }
    })

    output$active_die_right <- renderUI({
      switch_is_even <- (ui_state$switch_counter %% 2 == 0)
      if (switch_is_even) {
        img(src = "die_hex.png", style = str_c("background: transparent;display: flex;transform: scale(1.25);position: relative;top: -1vh; display:",
                                               if_else(str_extract(round_num(), "[AB]+") == "A", "block;", "none;")))
      } else {
        img(src = "die_hex.png", style = str_c("background: transparent;display: flex;transform: scale(1.25);position: relative;top: -1vh; display:",
                                               if_else(str_extract(round_num(), "[AB]+") == "B", "block;", "none;")))
      }
    })

    output$skip_error_msg <- renderText({ game_state$error_msg })

    # Score Validation Outputs ------------------------------------------------

    output$A_score_val <- renderUI({
      validate(
        need(
          validate_scores(player = input$scorer,
                          shot = game_state$shot_num,
                          snappaneers = snappaneers(),
                          paddle = any(input$paddle, input$foot),
                          scores_table = game_state$scores_db,
                          rebuttal = game_state$rebuttal_tag) == "valid",
          message = "That entry doesn't make sense for this round/shooter combination"),
        if (snappaneers()[snappaneers()$player_name == input$scorer, "player_id", drop = T] %in%
            game_state$scores_db[game_state$scores_db$round_num == round_num() & game_state$scores_db$paddle == F, "player_id", drop = T]) {
          need(
            validate_scores(player = input$scorer,
                            shot = game_state$shot_num,
                            snappaneers = snappaneers(),
                            paddle = any(input$paddle, input$foot),
                            scores_table = game_state$scores_db,
                            rebuttal = game_state$rebuttal_tag) == "valid",
            message = "That person has already scored a non paddle point this round")
        }
      )
      actionButton(session$ns("ok_A"), "OK")
    })

    output$B_score_val <- renderUI({
      validate(
        need(
          validate_scores(player = input$scorer,
                          shot = game_state$shot_num,
                          snappaneers = snappaneers(),
                          paddle = any(input$paddle, input$foot),
                          scores_table = game_state$scores_db,
                          rebuttal = game_state$rebuttal_tag) == "valid",
          message = "That entry doesn't make sense for this round/shooter combination"
        ),
        if (snappaneers()[snappaneers()$player_name == input$scorer, "player_id", drop = T] %in%
            game_state$scores_db[game_state$scores_db$round_num == round_num() & game_state$scores_db$paddle == F, "player_id", drop = T]) {
          need(
            validate_scores(player = input$scorer,
                            shot = game_state$shot_num,
                            snappaneers = snappaneers(),
                            paddle = any(input$paddle, input$foot),
                            scores_table = game_state$scores_db,
                            rebuttal = game_state$rebuttal_tag) == "valid",
            message = "That person has already scored a non paddle point this round")
        }
      )
      actionButton(session$ns("ok_B"), "OK")
    })

    # Switch Sides ------------------------------------------------------------

    observeEvent(input$switch_sides, {
      ui_state$switch_counter <- ui_state$switch_counter + 1
      switch_is_even <- (ui_state$switch_counter %% 2 == 0)

      if (switch_is_even) {
        removeUI(paste0("#", session$ns("ScoreboardUI")), immediate = T)
        insertUI(selector = paste0("#", session$ns("dice-row")),
                 ui = team_scoreboard_ui("B", "A", ns = session$ns),
                 where = "afterEnd")
      } else {
        removeUI(paste0("#", session$ns("ScoreboardUI")), immediate = T)
        insertUI(selector = paste0("#", session$ns("dice-row")),
                 ui = team_scoreboard_ui(ns = session$ns),
                 where = "afterEnd")
      }
    })

    # Halftime ----------------------------------------------------------------

    observeEvent(req(sum(game_state$scores_db$points_scored) >= score_to()), {
      sendSweetAlert(session,
                     title = "Halftime",
                     type = "info",
                     timer = 2450,
                     timerProgressBar = T,
                     btn_labels = NA,
                     imageUrl = "gifs/futurama-change-places.gif", customClass = "halftime",
                     text = HTML(str_c("Change places!")), html = T)

      insertUI(selector = paste0("#", session$ns("switch_sides")),
               where = "afterEnd",
               ui = tags$audio(src = "change_places.mp3", type = "audio/mp3", autoplay = NA, controls = NA, class = "sound-effect"))

      shinyjs::click("switch_sides")

      last_score <- game_state$scores_db[max(game_state$scores_db$score_id), ]
      sink_casualty_popup(session, score_row = last_score,
                          players = snappaneers()[snappaneers()$team != last_score$scoring_team, "player_name", drop = T])
    }, once = T, ignoreNULL = T)

    # High Noon ---------------------------------------------------------------

    observeEvent(input$highnoon_manual, {
      highnoon_popup(snappaneers()$player_name, ns = session$ns)
    }, ignoreNULL = T)

    observeEvent(req(str_detect(round_num(), "^12[AB]")), {
      delay(runif(n = 1, min = 1000, max = 1200), {
        highnoon_popup(snappaneers()$player_name, ns = session$ns)
      })

      last_score <- game_state$scores_db[max(game_state$scores_db$score_id), ]
      sink_casualty_popup(session, score_row = last_score,
                          players = snappaneers()[snappaneers()$team != last_score$scoring_team, "player_name", drop = T])
    }, once = T, ignoreNULL = T)

    # Casualty Popups ---------------------------------------------------------

    observe({
      req(isTRUE(started()))
      validate(
        need(vctrs::vec_in(game_state$current_scores, haystack = casualty_rules[, 1:2]), label = "casualty score"),
        need(purrr::none(game_state$cooldowns(), rlang::is_true), label = "cooldowns")
      )
      casualty_popup(session,
                     score = game_state$current_scores,
                     rules = casualty_rules,
                     players = snappaneers()$player_name)
    })

    observeEvent(input$casualty_manual, {
      casualty_popup(session,
                     score = game_state$current_scores,
                     rules = casualty_rules,
                     players = snappaneers()$player_name)
    })

    observeEvent(ignoreInit = T, c(input$casualty, input$highnoon), {
      if (is.null(input$casualty) && is.character(input$highnoon)) {
        casualty <- select(snappaneers(), starts_with("player")) %>%
          deframe() %>%
          pluck(input$highnoon)

        new_casualty <- tibble(
          casualty_id    = as.numeric(dbGetQuery(con, sql("SELECT MAX(casualty_id)+1 FROM casualties"))),
          game_id        = game_session$game_id,
          score_id       = game_state$score_id,
          player_id      = casualty,
          casualty_type  = "High noon",
          reported_player = NA_integer_
        )
      } else {
        validate(need(input$casualty, label = "Casualty"))

        casualty <- select(snappaneers(), starts_with("player")) %>%
          deframe() %>%
          pluck(input$casualty)

        type <- casualty_rules$casualty_title[vctrs::vec_match(game_state$current_scores, haystack = casualty_rules[, 1:2])]

        new_casualty <- tibble(
          casualty_id    = as.numeric(dbGetQuery(con, sql("SELECT MAX(casualty_id)+1 FROM casualties"))),
          game_id        = game_session$game_id,
          score_id       = game_state$score_id,
          player_id      = casualty,
          casualty_type  = type,
          reported_player = NA_integer_
        )
      }

      validate(need(is_tibble(new_casualty), label = "New casualty"))

      game_state$casualties <- add_row(game_state$casualties, new_casualty)
      dbWriteTable(conn = con, name = "casualties", value = new_casualty, append = T)

      last_score <- game_state$scores_db[max(game_state$scores_db$score_id), ]
      sink_casualty_popup(session, score_row = last_score,
                          players = snappaneers()[snappaneers()$team != last_score$scoring_team, "player_name", drop = T])
    })

    observeEvent(input$sink_casualty, {
      casualty <- select(snappaneers(), starts_with("player")) %>%
        deframe() %>%
        pluck(input$sink_casualty)

      new_casualty <- tibble(
        casualty_id    = as.numeric(dbGetQuery(con, sql("SELECT MAX(casualty_id)+1 FROM casualties"))),
        game_id        = game_session$game_id,
        score_id       = game_state$score_id,
        player_id      = casualty,
        casualty_type  = "Sunk",
        reported_player = NA_integer_
      )

      game_state$casualties <- add_row(game_state$casualties, new_casualty)
      dbWriteTable(conn = con, name = "casualties", value = new_casualty, append = T)

      insertUI(selector = paste0("#", session$ns("switch_sides")),
               where = "afterEnd",
               ui = tags$audio(src = "sploosh.mp3", type = "audio/mp3", autoplay = NA, controls = NA, class = "sound-effect"))
    })

    # TIFU (Friendly Fire) ----------------------------------------------------

    observeEvent(input$tifu, {
      showModal(tifu_casualty_popup(players = isolate(snappaneers()), ns = session$ns))
    })

    output$casualty_validation <- renderUI({
      if (input$casualty_type == "Team sink") {
        sinker_team <- snappaneers()[snappaneers()$player_id == input$tifu_accused, "team", drop = T]
        sinkee_team <- snappaneers()[snappaneers()$player_id == input$tifu_casualty, "team", drop = T]
        validate(
          need(sinker_team == sinkee_team, message = "That's not a team sink!"),
          need(input$tifu_accused != input$tifu_casualty, message = "That's a self sink!")
        )
      } else {
        validate(
          need(input$tifu_accused == input$tifu_casualty, message = "That's not a self sink!")
        )
      }
      actionButton(session$ns("tifu_confirm"), label = "Report",
                   class = "btn-primary", style = "background-color: var(--red);color: var(--bg-col);")
    })

    observeEvent(input$tifu_confirm, {
      new_casualty <- tibble(
        casualty_id    = as.numeric(dbGetQuery(con, sql("SELECT MAX(casualty_id)+1 FROM casualties"))),
        game_id        = game_session$game_id,
        score_id       = NA_integer_,
        player_id      = as.integer(input$tifu_casualty),
        casualty_type  = input$casualty_type,
        reported_player = if_else(input$tifu_casualty == input$tifu_accused, NA_integer_, as.integer(input$tifu_accused))
      )

      game_state$casualties <- add_row(game_state$casualties, new_casualty)
      dbWriteTable(conn = con, name = "casualties", value = new_casualty, append = T)

      removeModal()

      showNotification(if_else(input$casualty_type == "Team sink",
                               str_c("Nothing wrong with just a little bit of horseplay every now and then, ",
                                     game_session$players[match(as.integer(input$tifu_casualty), game_session$players$player_id), "player_name", drop = T], "!"),
                               "The good news is that you only get 1 peasant point!"),
                       duration = 7, closeButton = F)
    })

    # Round Control -----------------------------------------------------------

    observeEvent(input$previous_round, {
      validate(
        need(game_state$shot_num > 1, label = "Can't go below 0", message = "It's the first round still")
      )
      game_state$shot_num <- game_state$shot_num - 1

      game_state$player_stats_db <- aggregate_player_stats(game_state$scores_db, snappaneers(), game = game_session$game_id)
      db_update_player_stats(game_state$player_stats_db, round_button = T)
      db_update_round(round = round_num(), game = game_session$game_id)
    })

    observeEvent(input$next_round, {
      if (game_state$rebuttal_tag == T) {
        if (game_state$rebuttal == T) {
          click("finish_game")
          game_state$shot_num <- game_state$shot_num - 1
        } else {
          game_state$rebuttal_tag <- F
        }
      }

      game_state$shot_num <- game_state$shot_num + 1

      if (game_state$current_scores$team_A == 0 & game_state$current_scores$team_B == 0) {
        invisible()
      } else {
        game_state$player_stats_db <- aggregate_player_stats(game_state$scores_db, snappaneers(), game = game_session$game_id)
        db_update_player_stats(game_state$player_stats_db, round_button = T)
      }

      game_state$rebuttal <- rebuttal_check(a = game_state$current_scores$team_A,
                                            b = game_state$current_scores$team_B,
                                            round = round_num(),
                                            points_to_win = score_to())

      db_update_round(round = round_num(), game = game_session$game_id)

      if (game_state$rebuttal == T) {
        game_state$rebuttal_tag <- T
        game_notification(rebuttal = T, round = round_num(), current_scores = game_state$current_scores)
      }
    })

    # Scoring -----------------------------------------------------------------

    ## Team A -----------------------------------------------------------------

    observeEvent(input$A_score_button, {
      game_state$error_msg <- NULL
      eligible_shooters <- snappaneers()[snappaneers()$team == "A", "player_name", drop = T] %>% sample()
      showModal(score_check(team = "A", players = eligible_shooters, round = round_num(), ns = session$ns))
    })

    observeEvent(input$ok_A, {
      score <- as.integer(input$score)
      game_state$score <- score

      if (!is.null(game_state$score)) {
        removeModal()
        game_state$print <- TRUE

        game_state$current_scores$team_A <- game_state$current_scores$team_A + game_state$score
        game_state$score_id <- as.integer(game_state$score_id + 1)

        scorer_pid <- game_session$players[game_session$players$player_name == input$scorer, "player_id", drop = T]
        scorers_team <- snappaneers()[snappaneers()$player_name == input$scorer, "team", drop = T]
        shooting_team_lgl <- all(str_detect(round_num(), "A"), scorers_team == "A")

        new_score <- tibble(
          score_id     = game_state$score_id,
          game_id      = game_session$game_id,
          player_id    = scorer_pid,
          scoring_team = "A",
          round_num    = round_num(),
          points_scored = score,
          shooting     = shooting_team_lgl,
          paddle       = any(input$foot, input$paddle),
          clink        = input$clink,
          foot         = input$foot
        )

        sink_casualty_popup(session, score_row = new_score,
                            players = snappaneers()[snappaneers()$team == "B", "player_name", drop = T])

        game_state$scores_db <- bind_rows(game_state$scores_db, new_score)

        dbWriteTable(con, "scores",
                     anti_join(game_state$scores_db,
                               dbGetQuery(con, str_c("SELECT * FROM scores WHERE game_id = ", game_session$game_id)),
                               by = "score_id"),
                     append = T)

        game_state$player_stats_db <- aggregate_player_stats(game_state$scores_db, snappaneers(), game = game_session$game_id)
        db_update_player_stats(game_state$player_stats_db, specific_player = scorer_pid)

        if (input$paddle & str_detect(snappaneers()[snappaneers()$player_name == input$scorer, "team", drop = T], "[Aa]")) {
          game_notification()
        }
        if (input$paddle & str_detect(snappaneers()[snappaneers()$player_name == input$scorer, "team", drop = T], "[Bb]")) {
          showNotification("It's a bold strategy Cotton, let's see if it pays off for them.")
        }
      } else {
        game_state$error_msg <- "You did not input anything."
      }

      game_state$rebuttal <- rebuttal_check(game_state$current_scores$team_A,
                                            game_state$current_scores$team_B,
                                            round_num(), score_to())

      if (game_state$rebuttal == T & game_state$rebuttal_tag == T) {
        game_notification(rebuttal = T, round = round_num(), current_scores = game_state$current_scores)
      }

      if (game_state$rebuttal_tag == T & game_state$rebuttal == F) {
        game_state$rebuttal_tag <- F
        team_in_rebuttal <- str_sub(round_num(), start = -1)
        text_colour <- if_else(team_in_rebuttal == "A", snappa_pal[2], snappa_pal[3])
        showNotification(HTML(str_c("<span style='color:", text_colour, "'>Team ",
                                    team_in_rebuttal, "</span>", " has exited rebuttal!")),
                         duration = 20, closeButton = F)
      }
    })

    ## Team B -----------------------------------------------------------------

    observeEvent(input$B_score_button, {
      game_state$error_msg <- NULL
      eligible_shooters <- snappaneers()[snappaneers()$team == "B", "player_name", drop = T] %>% sample()
      showModal(score_check(team = "B", players = eligible_shooters, round = round_num(), ns = session$ns))
    })

    observeEvent(input$ok_B, {
      score <- as.integer(input$score)
      game_state$score <- score

      if (!is.null(game_state$score)) {
        removeModal()
        game_state$print <- TRUE

        game_state$current_scores$team_B <- game_state$current_scores$team_B + game_state$score
        game_state$score_id <- as.integer(game_state$score_id + 1)

        scorer_pid <- game_session$players[game_session$players$player_name == input$scorer, "player_id", drop = T]
        scorers_team <- snappaneers()[snappaneers()$player_name == input$scorer, "team", drop = T]
        shooting_team_lgl <- all(str_detect(round_num(), "[Bb]"), scorers_team == "B")

        new_score <- tibble(
          score_id     = game_state$score_id,
          game_id      = game_session$game_id,
          player_id    = scorer_pid,
          scoring_team = "B",
          round_num    = round_num(),
          points_scored = score,
          shooting     = shooting_team_lgl,
          paddle       = any(input$paddle, input$foot),
          clink        = input$clink,
          foot         = input$foot
        )

        sink_casualty_popup(session, score_row = new_score,
                            players = snappaneers()[snappaneers()$team == "A", "player_name", drop = T])

        game_state$scores_db <- bind_rows(game_state$scores_db, new_score)

        dbWriteTable(con, "scores",
                     anti_join(game_state$scores_db,
                               dbGetQuery(con, str_c("SELECT * FROM scores WHERE game_id = ", game_session$game_id)),
                               by = "score_id"),
                     append = T)

        game_state$player_stats_db <- aggregate_player_stats(game_state$scores_db, snappaneers(), game = game_session$game_id)
        db_update_player_stats(game_state$player_stats_db, specific_player = scorer_pid)

        if (input$paddle & str_detect(snappaneers()[snappaneers()$player_name == input$scorer, "team", drop = T], "[Bb]")) {
          game_notification()
        }
        if (input$paddle & str_detect(snappaneers()[snappaneers()$player_name == input$scorer, "team", drop = T], "A")) {
          showNotification("It's a bold strategy Cotton, let's see if it pays off for them.")
        }
      } else {
        game_state$error_msg <- "You did not input anything."
      }

      game_state$rebuttal <- rebuttal_check(game_state$current_scores$team_A,
                                            game_state$current_scores$team_B,
                                            round_num(), score_to())

      if (game_state$rebuttal == T & game_state$rebuttal_tag == T) {
        game_notification(rebuttal = T, round = round_num(), current_scores = game_state$current_scores)
      }

      if (game_state$rebuttal_tag == T & game_state$rebuttal == F) {
        game_state$rebuttal_tag <- F
        team_in_rebuttal <- str_sub(round_num(), start = -1)
        text_colour <- if_else(team_in_rebuttal == "A", snappa_pal[2], snappa_pal[3])
        showNotification(HTML(str_c("<span style='color:", text_colour, "'>Team ",
                                    team_in_rebuttal, "</span>", " has exited rebuttal!")),
                         duration = 20, closeButton = F)
      }
    })

    # Undo Score --------------------------------------------------------------

    observeEvent(input$undo_score_A, {
      validate(
        need(game_state$current_scores$team_A > 0, label = "Team A hasn't scored yet!")
      )
      confirmSweetAlert(
        session,
        type = "warning",
        inputId = "undo_score_A_confirm",
        title = "Remove this score?",
        text = reactableOutput(session$ns("last_score_A")),
        closeOnClickOutside = T,
        html = T
      )
    })

    observeEvent(input$undo_score_B, {
      validate(
        need(game_state$current_scores$team_B > 0, label = "Team B hasn't scored yet!")
      )
      confirmSweetAlert(
        session,
        type = "warning",
        inputId = "undo_score_B_confirm",
        title = "Remove this score?",
        text = reactableOutput(session$ns("last_score_B")),
        closeOnClickOutside = T,
        html = T
      )
    })

    output$last_score_A <- renderReactable({
      validate(
        need(game_state$current_scores$team_A > 0, label = "Team A hasn't scored yet!")
      )
      last_score <- filter(group_by(game_state$scores_db, scoring_team), scoring_team == "A", score_id == max(score_id)) %>%
        ungroup() %>%
        inner_join(snappaneers(), by = "player_id")

      col_list <- last_score_col_list
      emoji_to_show <- map(select(last_score, paddle:foot), isTRUE) %>% keep(isTRUE) %>% names()
      cols_to_show <- col_list[c("player_name", "round_num", "points_scored", emoji_to_show)]

      select(last_score, player_name, round_num, points_scored, any_of(emoji_to_show)) %>%
        reactable(compact = T, sortable = F, filterable = F, fullWidth = F, wrap = T, columns = cols_to_show)
    })

    output$last_score_B <- renderReactable({
      validate(
        need(game_state$current_scores$team_B > 0, label = "Team B hasn't scored yet!")
      )
      last_score <- filter(group_by(game_state$scores_db, scoring_team), scoring_team == "B", score_id == max(score_id)) %>%
        ungroup() %>%
        inner_join(snappaneers(), by = "player_id")

      col_list <- list(
        player_name   = colDef(name = "Player", maxWidth = 100),
        round_num     = colDef(name = "Round", maxWidth = 70),
        points_scored = colDef(name = "Pts", maxWidth = 50),
        paddle = colDef(name = "", width = 30, cell = function(value) { if (value) emo::ji("waving_hand") else "" }),
        clink  = colDef(name = "", width = 30, cell = function(value) { if (value) emo::ji("ear") else "" }),
        foot   = colDef(name = "", width = 30, cell = function(value) { if (value) emo::ji("foot") else "" })
      )

      emoji_to_show <- map(select(last_score, paddle:foot), isTRUE) %>% keep(isTRUE) %>% names()
      cols_to_show <- col_list[c("player_name", "round_num", "points_scored", emoji_to_show)]

      select(last_score, player_name, round_num, points_scored, any_of(emoji_to_show)) %>%
        reactable(compact = T, sortable = F, filterable = F, fullWidth = F, wrap = T, columns = cols_to_show)
    })

    observeEvent(input$undo_score_A_confirm, {
      validate(
        need(isTRUE(input$undo_score_A_confirm), label = "Nothin' to see here..")
      )
      last_score <- game_state$scores_db[game_state$scores_db$scoring_team == "A", "score_id"] %>% max()
      last_score_row <- game_state$scores_db[game_state$scores_db$score_id == last_score, c("points_scored", "clink")]

      game_state$scores_db <- game_state$scores_db[game_state$scores_db$score_id != last_score, ] %>%
        mutate(score_id = if_else(score_id > last_score, as.integer(score_id - 1), score_id))

      game_state$current_scores$team_A <- game_state$current_scores$team_A - last_score_row$points_scored
      game_state$score_id <- as.integer(game_state$score_id - 1)

      dbExecute(con, str_c("DELETE FROM scores WHERE score_id = ", last_score, " AND game_id = ", game_session$game_id, ";"))

      game_state$player_stats_db <- aggregate_player_stats(game_state$scores_db, snappaneers(), game = game_session$game_id)
      db_update_player_stats(game_state$player_stats_db)

      if (vctrs::vec_in(last_score_row, tribble(~points_scored, ~clink, 3, F, 5, T, 7, T))) {
        dbExecute(con, str_c("DELETE FROM casualties WHERE score_id = ", last_score,
                             " AND game_id = ", game_session$game_id, " AND casualty_type = 'Sunk'"))
        game_state$casualties <- game_state$casualties[!((game_state$casualties$score_id == last_score) & game_state$casualties$casualty_type == "Sunk"), ]
      }
    })

    observeEvent(input$undo_score_B_confirm, {
      validate(
        need(isTRUE(input$undo_score_B_confirm), label = "Nothin' to see here..")
      )
      last_score <- filter(game_state$scores_db, scoring_team == "B") %>% pull(score_id) %>% max()
      last_score_row <- filter(game_state$scores_db, score_id == last_score) %>% select(points_scored, clink)

      game_state$scores_db <- filter(game_state$scores_db, score_id != last_score) %>%
        mutate(score_id = if_else(score_id > last_score, as.integer(score_id - 1), score_id))

      game_state$current_scores$team_B <- game_state$current_scores$team_B - last_score_row$points_scored
      game_state$score_id <- as.integer(game_state$score_id - 1)

      dbExecute(con, str_c("DELETE FROM scores WHERE score_id = ", last_score, " AND game_id = ", game_session$game_id))

      game_state$player_stats_db <- aggregate_player_stats(game_state$scores_db, snappaneers(), game = game_session$game_id)
      db_update_player_stats(game_state$player_stats_db)

      if (vctrs::vec_in(last_score_row, tribble(~points_scored, ~clink, 3, F, 5, T, 7, T))) {
        dbExecute(con, str_c("DELETE FROM casualties WHERE score_id = ", last_score,
                             " AND game_id = ", game_session$game_id, " AND casualty_type = 'Sunk'"))
        game_state$casualties <- filter(game_state$casualties, !((score_id == last_score) & casualty_type == "Sunk"))
      }
    })

    # Finish Game -------------------------------------------------------------

    observeEvent(input$finish_game, {
      shinyWidgets::confirmSweetAlert(
        inputId = "finish_game_sure",
        title = "Finish game",
        text = "Are you sure?",
        type = "warning"
      )
    })

    observeEvent(input$finish_game_sure, {
      game_state$game_over <- T
      finalize_game(game_session, game_state, con, snappaneers(), score_to(), round_num(), session)
      game_summary_modal(game_summary()$df, round_num(),
                         game_summary()$subtitle_a, game_summary()$subtitle_b)
    })

    observeEvent(input$send_to_db, {
      finalize_game(game_session, game_state, con, snappaneers(), score_to(), round_num(), session)
      game_summary_modal(game_summary()$df, round_num(),
                         game_summary()$subtitle_a, game_summary()$subtitle_b)
    })

  })
}
