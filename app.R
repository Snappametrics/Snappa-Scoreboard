#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#





library(DBI)
library(RPostgres)
library(tidyverse)
library(shiny)
library(lubridate)
library(dbplyr)
library(shinyjs)
library(shinyWidgets)
library(gt)
library(extrafont)


source("dbconnect.R")
source("ui_functions.R")

# Prior to app startup ----------------------------------------------------

# Round numbers and labels
rounds = str_c(rep(1:100, each = 2), rep(c("A", "B"), 100))
round_labels = rep(c("Pass the dice", "Next round"),100)



# DB Tables ---------------------------------------------------------------


# Pull db tables for tibble templates
players_tbl = tbl(con, "players") %>% collect()
scores_tbl = tbl(con, "scores") %>% collect()
player_stats_tbl = tbl(con, "player_stats") %>% collect()
game_stats_tbl = tbl(con, "game_stats") %>% collect()



# Scores doesn't need to be pulled but will be referenced later


# Functions ---------------------------------------------------------------





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






# Career Stats ------------------------------------------------------------




score_progression = scores_tbl %>% 
  arrange(game_id, score_id) %>% 
  group_by(game_id) %>% 
  mutate(
    score_a = cumsum((scoring_team=="a")*points_scored),
    score_b = cumsum((scoring_team=="b")*points_scored)
  ) %>% 
  ungroup() %>% 
  count(score_a, score_b)

score_prog_plot = score_heatmap(score_progression)





# UI ----------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- navbarPage(title = "Snappa Scoreboard", id = "navbar", selected = "Player Input",
  theme = "app.css",
  useShinyjs(),
  
  


# Start Screen ------------------------------------------------------------

        
        tabPanel("Player Input", icon = icon("users"),
                 # Fluid Row - 3 columns
                 fluidRow(
                   team_input_ui("a", pull(players_tbl, player_name)),
                   
                   # Column 2 - empty
                   column(4),
                   
                   # Column 3 - Team B
                   team_input_ui("b", pull(players_tbl, player_name))
                   ),
                 
                 
                # Second row - 3 columns
                 fluidRow(
                   # Column 1 - empty
                   column(4),
                   # Column 2
                   #    - Start game button
                   #    - Score to play to
                   column(4,  align = "center",
                          disabled(actionBttn("start_game", 
                                              label = "Throw some dice?", style = "pill", color = "primary")),
                          uiOutput("validate_start"),
                          br(),
                          
                          awesomeRadio(inputId = "play_to", 
                                       label = "What score are you playing to?", 
                                       choices = list("21" = 1, "32" = 2), 
                                       selected = 1, inline = T),
                          br(),
                          helpText("Note: All players must enter their name before the game can begin")
                          ),
                   column(4)
                   ),
                 
                 # Try to make Icons work
                 
                # fluidRow(
                #   column(3, 
                #          actionButton("one_point", label = "Off the Table", 
                #                       style = 
                #                         'img {
                #                         border: 1px solid #ddd;
                #                         border-radius: 50%;
                #                         
                #                         width: 50%;
                #                         height: 100px;
                #                         margin-left: 0px;
                #                         margin-right: 0px;
                #                         
                #                       }'
                #          ))
                # )
        ),
                
        

# Stats Pane --------------------------------------------------------------

  tabPanel("Career Stats", icon = icon("bar-chart"),
           fluidRow(
             column(6,
                    
                    wellPanel(style = "height: 600px; background-color: #fff",
                      gt_output("career_stats_table")
                    )
                    ),
             column(1),
             column(5,
                    wellPanel(style = "height: 750px;",
                      plotOutput("scoring_heatmap", height = "600px", width = "auto",
                                 hover = hoverOpts(id = "heat_hover", delay = 100, delayType = c("debounce"))),
                      uiOutput("heatmap_info")
                    )
             )
           )

           )


        

# Debugging ---------------------------------------------------------------


      # fluidRow(
      #   column(2, align = "center",
      #          h3("players"),
      #          tableOutput("db_output_players")
      #          ),
      #   column(5, align = "center",
      #          h3("scores"),
      #          tableOutput("db_output_scores")
      #   ),
      #   column(5, align = "center",
      #          h3("player_stats"),
      #          tableOutput("db_output_player_stats")
      #   )
      # 
      #   )
  )
  
  





# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  
  #Change the html of an icon
  # html(id = "one_point", 
  #      html = '<img src="off_the_table.png" alt="off_table" class = "center">'
  # )
  
  add_shot_count = function(df){
    
    add_count(df, .data$team, name = "n_players") %>% # Count the number of rows for each team
      # Calculate the number of shots for each player by diving the team shots by 2
      # Team shots calculated using ceiling/floor because A always goes first
      mutate(baseline_shots = case_when(str_detect(.data$team, "a") ~ ceiling(vals$shot_num/2),
                                        str_detect(.data$team, "b") ~ floor(vals$shot_num/2)),
             # In cases where teams are uneven, we calculate the average shots a player had
             shots = .data$baseline_shots*max(.data$n_players)/.data$n_players) %>% 
      select(-baseline_shots, -n_players)
  }
  
  
    

# Reactive Values Object ---------------------------------------------------------


  
  # Create object to store reactive values
  vals <- reactiveValues(
    # Initialize new game, player, and score IDs, as well as the shot number
    game_id = NULL,
    new_player_id = sum(dbGetQuery(con, "SELECT MAX(player_id) FROM players"),1),
    score_id = as.integer(0),
    shot_num = as.integer(1),
    
    # DB Tables
    game_stats_db = game_stats_tbl %>% slice(0) %>% select(1:5),
    player_stats_db = player_stats_tbl %>% slice(0),
    players_db = players_tbl,
    scores_db = scores_tbl %>% slice(0),

    # dataframe of the players and their teams
    # Current Scores
    current_scores = tibble(
      team_a = 0,
      team_b = 0
    ),
    
    rebuttal = NULL,
    rebuttal_tag = F, 
    
    

    # Values used in scoring events
    score = NULL,
    error_msg = NULL,
    print = FALSE,
    
    score_to = NULL,
    
    # Holds the trolls (more for simplicity of code
    # than direct need)
    trolls = NULL,
    
    #Records when the extra player ui's are open and 
    # allows the app to pay attention to empty strings
    # only during select times
    want_a3 = F,
    want_a4 = F,
    want_b3 = F,
    want_b4 = F,
    
    switch_counter = 1
  )
  
  
  

# Player Inputs, Snappaneers, Other Reactives --------------------------------------

  
  
  # Increment round number
  round_num = reactive({
    rounds[vals$shot_num]
  })
  
  # Active input buttons
  #   - List of player inputs which are not null
  active_player_inputs = reactive({
    list("a1" = input$name_a1, "a2" = input$name_a2, "a3" = input$name_a3, "a4" = input$name_a4, 
         "b1" = input$name_b1, "b2" = input$name_b2, "b3" = input$name_b3, "b4" = input$name_b4) %>% 
      discard(is_null)
  })
  
  # Snappaneers - | Team | Player name | Player ID  |
  snappaneers = reactive({
    
    tibble(
      # Team pulls the first letter from their input name
      team = str_extract(names(active_player_inputs()), ".{1}"),
      player_name = active_player_inputs() %>% flatten_chr()
    ) %>% 
      # Remove empty player inputs
      filter(player_name != "") %>% 
      left_join(vals$players_db, by = "player_name") %>% 
      # Add shot count
      add_shot_count()
  })
  
  # Vector of players, with current players removed
  current_choices = reactive({
    anti_join(players_tbl, snappaneers(), by = "player_name") %>% 
      pull(player_name)
  })
  
  # Length of active player inputs
  num_players = reactive({
    length(active_player_inputs()[active_player_inputs() != ""])
  })
  
  top_scorers_tab = reactive({
    inner_join(vals$players_db, tbl(con, "player_stats") %>% collect(), by = "player_id") %>%
    select(-player_id) %>% 
    # Calculate leaderboard
    group_by(player_name) %>% 
    summarise(
      games_played = n(),
      points_per_game = mean(total_points),
      total_points = sum(total_points),
      offensive_points = sum(off_ppr*shots),
      defensive_points = sum(def_ppr*shots),
      ones = sum(ones),
      twos = sum(twos),
      threes = sum(threes),
      paddle_points = sum(paddle_points),
      clink_points = sum(clink_points),
      total_shots = sum(shots),
      off_ppg = mean(off_ppr*shots),
      def_ppg = mean(def_ppr*shots),
      toss_efficiency = weighted.mean(toss_efficiency, w = shots)
    ) %>% 
    ungroup() %>% 
    filter_at(vars(-player_name), any_vars(!is.na(.))) %>% 
    mutate(rank = rank(-total_points)) %>% 
    select(rank, everything(), -offensive_points:-clink_points) %>% 
    arrange(rank)
  })
  
  
  
  
  

  

# Outputs -----------------------------------------------------------------
  
  # Switch between pass the dice and next round
  output$selector_ui <- renderUI({
    fillRow(actionBttn("previous_round", 
                       label = "Previous Round", style = "jelly", icon = icon("arrow-left"), color = "primary", size = "lg"),
            actionBttn("next_round", 
                       label = round_labels[vals$shot_num], style = "jelly", icon = icon("arrow-right"), color = "primary", size = "lg"))
    
  })
  

# Score Validation --------------------------------------------------------


  output$a_score_val = renderUI({
    # Check that the round/shooter combination makes sense / indicated a paddle
    validate(
      need(
        any(
          # Typical Offense
          str_detect(rounds[vals$shot_num], "[Aa]") & 
            str_detect(pull(filter(snappaneers(), player_name == input$scorer), team), "[Aa]"),
          # Typical Paddle
          str_detect(rounds[vals$shot_num], "[Bb]") &
            str_detect(pull(filter(snappaneers(), player_name == input$scorer), team), "[Aa]") & 
            input$paddle == T,
          # Somebody messed up on the other team
          str_detect(pull(filter(snappaneers(), player_name == input$scorer), team), "[Bb]") & 
            input$paddle == T),
        message = "That entry doesn't make sense for this round/shooter combination"),
      if (pull(filter(snappaneers(), player_name == input$scorer), player_id) %in%
          pull(filter(vals$scores_db, round_num == round_num() & paddle == F), player_id)){
        need(input$paddle == T,
             message = "That person has already scored a non paddle point this round")
        
      }
    )

    actionButton("ok_a", "OK")
  })
  
  output$b_score_val = renderUI({
    validate(
      # General needs for typical shooting
      need(
        any(
          # Typical Offense
          str_detect(rounds[vals$shot_num], "[Bb]") & 
            str_detect(pull(filter(snappaneers(), player_name == input$scorer), team), "[Bb]"),
          # Typical Paddle
          str_detect(rounds[vals$shot_num], "[Aa]") &
            str_detect(pull(filter(snappaneers(), player_name == input$scorer), team), "[Bb]") & 
            input$paddle == T,
          # Somebody messed up on the other team (can happen on offense or defense)
          str_detect(pull(filter(snappaneers(), player_name == input$scorer), team), "[Aa]") & 
            input$paddle == T),
        message = "That entry doesn't make sense for this round/shooter combination"
      ),
      # Make sure that the last person to score in this round on offense can't paddle
      if (pull(filter(snappaneers(), player_name == input$scorer), player_id) %in%
          pull(filter(vals$scores_db, round_num == round_num() & paddle == F), player_id)){
        need(input$paddle == T,
             message = "That person has already scored a non paddle point this round")
        
      } 
    )
      actionButton("ok_b", "OK")
  })
  
  # output$active_die_a = renderUI({
  #   icon("dice-d6")
  #   
  #   if(str_detect(round_num(), "A")){
  #     die_col = "#fffff"
  #   } else {
  #     die_col = "#e26a6a"
  #   }
  #   tags$i(class = "fa fa-dice-d6", 
  #          style = paste("color:", die_col))
  # })
  
  # Output the round number
  output$round_num = renderText({
    round_num()
  })
  
  # Output Team A's score
  output$score_a = renderText({
    vals$current_scores$team_a
  })
  
  output$player_names_a = renderText({
    snappaneers() %>% 
      filter(team == "A") %>% 
      pull(player_name) %>% 
      str_c(., collapse = ", ")
  })
  
  # Output Team B's score
  output$score_b = renderText({
    vals$current_scores$team_b
  })
  
  output$player_names_b = renderText({
    snappaneers() %>% 
      filter(team == "B") %>% 
      pull(player_name) %>% 
      str_c(., collapse = ", ")
  })
  
  # Recent Scores
  output$recent_scores = render_gt({
    
    top_n(vals$scores_db, 5, score_id) %>% 
      arrange(-score_id) %>% 
      left_join(select(snappaneers(), player_id, player_name)) %>% 
      recent_score_sentence() %>% 
      gt() %>% 
      tab_options(column_labels.hidden = T)
  })
  
  # Output error message
  output$skip_error_msg <- renderText({
    vals$error_msg
  })
  
  # Download button
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(vals$scores_db, con)
    }
  )
  

# Stats Pane --------------------------------------------------------------

  output$career_stats_table = render_gt({
    top_scorers_tab() %>% 
      leaderboard_table()
      
  })
  
  output$scoring_heatmap = renderPlot({
    score_prog_plot
  })
  
  output$heatmap_info <- renderUI({
    req(input$heat_hover)
    x <- round(input$heat_hover$x, 0)
    y <- round(input$heat_hover$y, 0)
    
    freq = filter(score_progression, score_a == y, score_b == x) %>% 
      pull(n)
    
    HTML(str_c("<h3>Score</h3>",
          "<p><span style='font-weight:500'>Team A</span>: ", x, "  ", "<span style='font-weight:500'>Team B</span>: ", y, "</p>",
          "<p><span style='font-weight:500'>Frequency</span>: ", freq))
  })
  
  # For debugging
  
  output$db_output_players = renderTable({
    vals$players_db
  })
  output$db_output_scores = renderTable({
    vals$scores_db
  })
  output$db_output_player_stats = renderTable({
    vals$player_stats_db
  })
  output$db_output_game_history = renderTable({
    vals$game_stats_db
  })
  output$snappaneers = renderTable({
    snappaneers()
  })
  
  
  
  
  

# Events ------------------------------------------------------------------
  
  


# Game Start Validation ---------------------------------------------------

  
  observeEvent(input$switch_sides, {
    
    vals$switch_counter = vals$switch_counter+1
    
    switch_is_even = (vals$switch_counter %% 2 == 0)
    
    
    if(switch_is_even){
      removeUI("#ScoreboardUI", immediate=T)
      insertUI(selector = "#bottom_buttons", ui = team_scoreboard_ui("b", "a"), where = "beforeBegin")
      # removeTab("navbar", target = "Scoreboard", session)
      # insertTab("navbar", target = "Player Input", tab = team_scoreboard_ui("b", "a"), select = T)
    } else {
      removeUI("#ScoreboardUI", immediate = T)
      insertUI(selector = "#bottom_buttons", ui = team_scoreboard_ui(), where = "beforeBegin")
      # removeTab("navbar", target = "Scoreboard", session)
      # insertTab("navbar", target = "Player Input", tab = team_scoreboard_ui(), select = T)
    }
    

  })
  
  
  # Create a UI output which validates that there are four players and the names are unique
  output$validate_start = reactive({
    # If one of the first two players on each team
    # is removed, disable the button again.
    # This goes above the validate check because 
    # it needs to be updating before the validate
    # check is failed, or else the logic isn't
    # going to pass through
    
    if(any(input$name_a1 == "",
           input$name_a2 == "",
           input$name_b1 == "",
           input$name_b2 == "")){
      shinyjs::disable("start_game")
    }
    
    validate(
      need(input$name_a1 != "", label = "Player A1"),
      need(input$name_a2 != "", label = "Player A2"), 
      need(input$name_b1 != "", label = "Player B1"), 
      need(input$name_b2 != "", label = "Player B2")
      )
    
    #Record the players that you need to be looking for
    # (i.e., which ui elements are open right now?)
    
    
    # If the number of unique snappaneer names is the same as the number of active player inputs
    #   => enable start button
    
    if(sum(length(unique(snappaneers()$player_name)), 
           sum(
             c(isTRUE(active_player_inputs()$a3 == "" & vals$want_a3), 
             isTRUE(active_player_inputs()$a4 == "" & vals$want_a4), 
             isTRUE(active_player_inputs()$b3 == "" & vals$want_b3), 
             isTRUE(active_player_inputs()$b4 == "" & vals$want_b4)) 
             )
           ) == num_players()){ 
      shinyjs::enable("start_game")
    } 
    
    # If the number of unique snappaneer names is not the same as the number of active player inputs
    #   => disable start button
    if(sum(length(unique(snappaneers()$player_name)), 
           sum(
             c(isTRUE(active_player_inputs()$a3 == "" & vals$want_a3), 
                isTRUE(active_player_inputs()$a4 == "" & vals$want_a4), 
                isTRUE(active_player_inputs()$b3 == "" & vals$want_b3), 
                isTRUE(active_player_inputs()$b4 == "" & vals$want_b4)
                ) 
           )
    ) != num_players()){ 
      
    shinyjs::disable("start_game")
    }
    

  })
  # Game Start --------------------------------------------------------------
  
  # When we click "Start Game", 
  #   - Add new players to the players table
  #   - switch to the scoreboard
  #   - Set the score outputs and shot number to 0
  #   - Record the score we're playing to
  #   - Initialize the current game's player_stats table
  observeEvent(input$start_game, {

# Add Scoreboard ----------------------------------------------------------

    scoreboard_tab = tabPanel("Scoreboard", icon = icon("window-maximize"), 
                              div(
                                fluidRow(id = "scoreboardrow", 
                                         column(4, align = "left"#, 
                                                # # Potentially useful button later
                                                # dropdown(
                                                #   gt_output("stats_a"),
                                                #   style = "unite",
                                                #   size = "lg", 
                                                #   label = "Stats",
                                                #   icon = icon("dice"),
                                                #   animate = animateOptions(
                                                #     enter = animations$fading_entrances$fadeInLeft,
                                                #     exit = animations$fading_exits$fadeOutLeft
                                                #   ))
                                                ), 
                                         column(4, align = "center", 
                                                actionBttn("switch_sides", 
                                                           "Switch Sides", style = "unite", color = "primary", icon = icon("refresh"), size = "sm")),
                                         column(4)),
                                team_scoreboard_ui(), 
                                div(id = "bottom_buttons",
                                    fluidRow(
                                      column(width =4, offset = 4, align = "center",
                                             # Recent Scores
                                             dropdown(
                                               inputId = "recent_scores",
                                               gt_output("recent_scores"),
                                               style = "unite",
                                               size = "lg", 
                                               up = T,
                                               label = "Recent Scores",
                                               icon = icon("backward"),
                                               animate = animateOptions(
                                                 enter = animations$fading_entrances$fadeInUp,
                                                 exit = animations$fading_exits$fadeOutDown
                                               )
                                             ))
                                      
                                    ),
                                    fluidRow(
                                      column(width = 4, offset = 4, align = "center",
                                             actionBttn("new_game", "Restart game", style = "unite", color = "warning"),
                                             actionBttn("finish_game", "Finish game", style = "unite", color = "warning")
                                      )
                                    )
                                    )
                                
                                )
                              )
    insertTab("navbar", tab = scoreboard_tab, target = "Player Input", position = "after", select = T)  
    hideTab("navbar", "Player Input")
    
  
    # Add new players to the players table
    iwalk(snappaneers()$player_name, function(die_thrower, index){
      # If the player is not in the players table
      if(!(die_thrower %in% vals$players_db$player_name)){
        
        # Add a row to the players table with the new player's name and new ID
        vals$players_db = bind_rows(vals$players_db,
                                 tibble(
                                   player_id = vals$new_player_id,
                                   player_name = die_thrower))
        
        # Increment the ID for the next new player
        vals$new_player_id = vals$new_player_id+1
        
      } else {
        invisible()
      }
    })
    
    # Switch to the scoreboard
    updateTabsetPanel(session, "switcher", selected = "scoreboard")
    
    # Set the score outputs and shot number to 0
    vals$current_scores$team_a = 0
    vals$current_scores$team_b = 0
    vals$scores_db = slice(scores_tbl, 0)
    vals$game_id = as.integer(sum(dbGetQuery(con, "SELECT MAX(game_id) FROM game_stats"),1 , na.rm = T))
    
    vals$game_stats_db = bind_rows(vals$game_stats_db,
              tibble(
                game_id = vals$game_id,
                num_players = nrow(snappaneers()),
                game_start = as.character(now(tzone = "America/Los_Angeles")),
                game_end = NA_character_,
                night_dice = NA#,
                # points_a = NA_integer_,
                # points_b = NA_integer_,
                # rounds = NA_integer_,
                # ones = NA_integer_,
                # twos = NA_integer_,
                # threes = NA_integer_,
                # impossibles = NA_integer_,
                # paddle_points = NA_integer_,
                # clink_points = NA_integer_
              ))
    
    # Record the score we're playing to
    vals$score_to = case_when(input$play_to == 1 ~ 21,
                              input$play_to == 2 ~ 32)
    
    
    # Initialize the current game's player_stats table
    vals$player_stats_db = slice(vals$player_stats_db, 0)
    # Previous code, but might be useful if/when we want to pull in historical data too
    # bind_rows(vals$player_stats_db,
    #           tibble(
    #             game_id = rep(vals$game_id, vals$num_players),
    #             player_id = filter(vals$players_db, player_name %in% snappaneers()$player_name) %>% pull(player_id),
    #             total_points = rep(0, vals$num_players),
    #             ones = rep(0, vals$num_players),
    #             twos = rep(0, vals$num_players),
    #             threes = rep(0, vals$num_players),
    #             impossibles = rep(0, vals$num_players)
    #           ))


  })
  
  
  
  

# Next Round --------------------------------------------------------------
  
  # When previous round button is pushed
  observeEvent(input$previous_round, {
    validate(
      need(vals$shot_num > 1, label = "Can't go below 0", message = "It's the first round still")
    )
    vals$shot_num = vals$shot_num-1
  })

  # When next round button is pushed
  observeEvent(input$next_round, {
    if (vals$rebuttal_tag == T){
      if (vals$rebuttal == T){
        click("finish_game")
      } else {
        vals$rebuttal_tag = F
      }
    } else{
    }
    
    vals$shot_num = vals$shot_num+1

    vals$rebuttal = rebuttal_check(a = vals$current_scores$team_a, b = vals$current_scores$team_b,
                                   round = round_num(), points_to_win = vals$score_to)
      
    if (vals$rebuttal == T) {
      vals$rebuttal_tag = T
      showNotification(str_c("Rebuttal: ", "Team ", 
                             str_sub(round_num(), start = -1),
                             " needs ", str_c(
                               abs(vals$current_scores$team_a - vals$current_scores$team_b) - 1),
                             " points to bring it back"
                             )
                      ) 
    } else {
    }
      
    })
  

# New Players -------------------------------------------------------------

  getInputs <- function(pattern){
    reactives <- names(reactiveValuesToList(input))
    reactives[grep(pattern,reactives)]
  }
  
  # New Player A3
  #   - Add A3 text input
  #   - Remove the add new player action button
  observeEvent(input$extra_player_a3, {
    # Set input want to true
    vals$want_a3 = T
    
    # Get add player button inputs
    vals <- paste0("#",getInputs("extra_player_a3"))
    add_player_input(vals, "a", 3, current_choices(), session)
    
  })
  
  # Remove A3
  #   - Insert add new player action button
  #   - Remove A3 player name input
  observeEvent(input$remove_a3, {
    remove_p3_input("a", session)

    #Don't consider these elements when looking at
    # total length of players. Prevents the game
    # from getting locked out after players have
    # been added
    vals$want_a3 = F
    vals$want_a4 = F
    
  })
  
  
  # New Player A4
  #   - Add A4 text input
  #   - Remove the add new player action button
  observeEvent(input$extra_player_a4, {
    # Set input want to true
    vals$want_a4 = T
    
    # Get UI inputs for extra player button
    vals <- paste0("#",getInputs("extra_player_a4"))
    
    add_player_input(vals, "a", 4, current_choices(), session)
    
  })
  
  # Remove A4
  #   - Insert add new player action button
  #   - Remove A4 player name input
  observeEvent(input$remove_a4, {
    remove_p4_input("a", session)
    
    vals$want_a4 = F
    
  })  
  
  
  # New Player B3
  #   - Add B3 text input
  #   - Remove the add new player action button
  observeEvent(input$extra_player_b3, {
    
    # Set want check to true
    vals$want_b3 = T
    
    # Get inputs for add player button
    vals <- paste0("#",getInputs("extra_player_b3"))
    
    add_player_input(vals, "b", 3, current_choices(), session)
  })

  # Remove B3
  #   - Insert add new player action button
  #   - Remove B3 player name input
  observeEvent(input$remove_b3, {
    remove_p3_input("b", session)
    
    #Don't consider these elements when looking at
    # total length of players. Prevents the game
    # from getting locked out after players have
    # been added
    vals$want_b3 = F
    vals$want_b4 = F
    
  })
  
  # New Player B4
  #   - Add B4 text input
  #   - Remove the add new player action button
  observeEvent(input$extra_player_b4, {
    # Set want check to true
    vals$want_b4 = T
    
    # Get add player button inputs
    vals <- paste0("#",getInputs("extra_player_b4"))
    
    add_player_input(vals, "b", 4, current_choices(), session)
  })
  

  # Remove B4
  #   - Insert add new player action button
  #   - Remove B4 player name input
  observeEvent(input$remove_b4, {
    remove_p4_input("b", session)
    # Tells later checks to not worry about this
    # empty slot in active_player_names
    vals$want_b4 = F

  })  
    

  
  
  
    
  

# Scoring -----------------------------------------------------------------
  
  #TODO: Fix score_id, game_id, and num_points_scored in scores_db in vals: change from dbl to int


# Team A ------------------------------------------------------------------

  
  observeEvent(input$a_score_button, {
    vals$error_msg <- NULL
    
    eligible_shooters = filter(snappaneers(), team == "a") %>% 
      pull(player_name) %>% 
      sample()
    
    showModal(
      score_check(team = "a", 
                  players = eligible_shooters))
  })
  
  # Team A presses score button
  observeEvent(input$ok_a, {
    # validate(
    #   need(input$score < 8, label = "C'mon, you did not score that many points")
    # )
    # Check that the round/shooter combination makes sense / indicated a paddle
    validate(
      need(
        any(
          # Typical Offense
          str_detect(rounds[vals$shot_num], "[Aa]") & 
            str_detect(pull(filter(snappaneers(), player_name == input$scorer), team), "[Aa]"),
          # Typical Paddle
          str_detect(rounds[vals$shot_num], "[Bb]") &
            str_detect(pull(filter(snappaneers(), player_name == input$scorer), team), "[Aa]") & 
            input$paddle == T,
          # Somebody messed up on the other team
          str_detect(rounds[vals$shot_num], "[Aa]") &
          str_detect(pull(filter(snappaneers(), player_name == input$scorer), team), "[Bb]") & 
            input$paddle == T),
        message = "That entry doesn't make sense for this round/shooter combination"),
      if (pull(filter(snappaneers(), player_name == input$scorer), player_id) %in%
          pull(filter(vals$scores_db, round_num == round_num() & paddle == F), player_id)){
        need(input$paddle == T,
             message = "That person has already scored a non paddle point this round")
        
      }
    )  

    # set score
    score = as.integer(input$score)
    vals$score <- score
    
    
    # Check score i not null, remove the dialog box
    if (!is.null(vals$score)) {
      removeModal()
      vals$print <- TRUE
      
      # Update the team score
      vals$current_scores$team_a = vals$current_scores$team_a + vals$score
      
      # Increment the score_id
      vals$score_id = as.integer(vals$score_id+1)
      
      ## Identify scoring characteristics
      # Player ID
      scorer_pid = pull(filter(vals$players_db, player_name == input$scorer), player_id)
      # Were they shooting?
      scorers_team = pull(filter(snappaneers(), player_name == input$scorer), team) # pull the scorer's team from snappaneers
      shooting_team_lgl = all(str_detect(round_num(), "[Aa]"), scorers_team == "a") # Are they on team A & did they score for team A?
      
      # Add the score to the scores table
      vals$scores_db = bind_rows(vals$scores_db,
                                 tibble(
                                   score_id = vals$score_id,
                                   game_id = vals$game_id,
                                   player_id = scorer_pid,
                                   scoring_team = "a",
                                   round_num = round_num(),
                                   points_scored = score,
                                   shooting = shooting_team_lgl,
                                   paddle = any(input$foot, input$paddle),
                                   clink = input$clink,
                                   foot = input$foot
                                 ))
      

      # Update game stats table
      vals$player_stats_db = vals$scores_db %>% 
        # Join scores to snappaneers to get each player's team
        left_join(snappaneers(), by = "player_id") %>% 
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
      
      # Congratulate paddlers
      if(input$paddle & str_detect(pull(filter(snappaneers(), player_name == input$scorer), team), "[Aa]") ){
        showNotification("That's some hot shit!", id = "paddle")
      }
      if(input$paddle & str_detect(pull(filter(snappaneers(), player_name == input$scorer), team), "[Bb]") ){
        showNotification("It's a bold strategy Cotton, let's see if it pays off for them.")
      }
    } else {
      vals$error_msg <- "You did not input anything."
    }
    
    # If the game is in rebuttal, remind players
    # of the points needed to bring it back
    vals$rebuttal = rebuttal_check(vals$current_scores$team_a, 
                                   vals$current_scores$team_b,
                                   round_num(), vals$score_to)
    
    #    if (!is.null(vals$rebuttal)) {
    if (vals$rebuttal == T & vals$rebuttal_tag == T) {
      showNotification(str_c("Rebuttal: ", "Team ", 
                             str_sub(round_num(), start = -1),
                             " needs ", str_c(
                               abs(vals$current_scores$team_a - vals$current_scores$team_b) - 1),
                             " points to bring it back"
      )
      )
    } else {
      
    }
    validate(
      need((vals$current_scores$team_a == 18 && vals$current_scores$team_b == 12) || (vals$current_scores$team_a == 12 && vals$current_scores$team_b == 18), label = "eighteen_twelve")
    )
    
    
    sendSweetAlert(session, 
                   title = "1812",
                   text = "Everyone roll a die, lowest roll takes a shot.",
                   type = "warning")

  })
  # Undo score
  observeEvent(input$undo_score_a, {
    validate(
      need(vals$current_scores$team_a > 0, label = "Team A hasn't scored yet!")
    )

    # Select the ID which is the max on Team A
    last_score = filter(vals$scores_db, scoring_team == "a") %>% 
      pull(score_id) %>% 
      max()
    
    # Pull the number of points the last score was worth
    last_score_pts = filter(vals$scores_db, score_id == last_score) %>% 
      pull(points_scored)
    
    # Reduce the score ID for any scores which have happened since the score which is being removed
    # Note that score undo-ing is team specific
    vals$scores_db = filter(vals$scores_db, score_id != last_score) %>% 
      mutate(score_id = if_else(score_id > last_score, as.integer(score_id-1), score_id))
    
    # Reduce the team's score and score_id
    vals$current_scores$team_a = vals$current_scores$team_a - last_score_pts
    vals$score_id = as.integer(vals$score_id-1)
  })
  
  
  # Team B ---------------------------------------------------------
  
  
  observeEvent(input$b_score_button, {
    vals$error_msg <- NULL
    
    eligible_shooters = filter(snappaneers(), team == "b") %>% 
      pull(player_name) %>% 
      sample()
    
    showModal(
      score_check(
        team = "b", 
        players = eligible_shooters))
    
  })
  
  # Score validation
  observeEvent(input$ok_b, {
    # validate(
    #   need(input$score < 8, label = "C'mon, you did not score that many points")
    # )
    validate(
      need(
        any(
          # Typical Offense
          str_detect(rounds[vals$shot_num], "[Bb]") & 
            str_detect(pull(filter(snappaneers(), player_name == input$scorer), team), "[Bb]"),
          # Typical Paddle
          str_detect(rounds[vals$shot_num], "[Aa]") &
            str_detect(pull(filter(snappaneers(), player_name == input$scorer), team), "[Bb]") & 
            input$paddle == T,
          # Somebody messed up on the other team (can happen on offense or defense)
          str_detect(rounds[vals$shot_num], "[Bb]") &
          str_detect(pull(filter(snappaneers(), player_name == input$scorer), team), "[Aa]") & 
            input$paddle == T),
        message = "That entry doesn't make sense for this round/shooter combination"),
      if (pull(filter(snappaneers(), player_name == input$scorer), player_id) %in%
          pull(filter(vals$scores_db, round_num == round_num() & paddle == F), player_id)){
        need(input$paddle == T,
             message = "That person has already scored a non paddle point this round")
        
      }
    )
    #Set Score
    score = as.integer(input$score)
    vals$score <- score
    
    if (!is.null(vals$score)) {
      removeModal()
      vals$print <- TRUE
      
      # Update Team B's score
      vals$current_scores$team_b = vals$current_scores$team_b + vals$score
      
      # Increment the score_id
      vals$score_id = as.integer(vals$score_id+1)
      
      ## Identify scoring characteristics
      # Player ID
      scorer_pid = pull(filter(vals$players_db, player_name == input$scorer), player_id)
      # Were they shooting?
      scorers_team = pull(filter(snappaneers(), player_name == "Shaunt"), team)
      shooting_team_lgl = all(str_detect(round_num(), "[Bb]"), scorers_team == "b")
      
      # Add the score to the scores table
      vals$scores_db = bind_rows(vals$scores_db,
                              tibble(
                                score_id = vals$score_id,
                                game_id = vals$game_id,
                                player_id = scorer_pid,
                                scoring_team = "b",
                                round_num = round_num(),
                                points_scored = score,
                                shooting = shooting_team_lgl,
                                paddle = any(input$paddle, input$foot),
                                clink = input$clink,
                                foot = input$foot
                              ))
      
      # Update player stats
      vals$player_stats_db = vals$scores_db %>% 
        left_join(snappaneers(), by = "player_id") %>% 
        group_by(game_id, player_id, team, shots) %>% 
        summarise(total_points = sum(points_scored),
                  ones = sum((points_scored == 1)),
                  twos = sum((points_scored == 2)),
                  threes = sum((points_scored == 3)),
                  impossibles = sum((points_scored > 3)),
                  paddle_points = sum(points_scored*(paddle | foot)),
                  clink_points = sum(points_scored*clink),
                  points_per_round = total_points / last(shots),
                  off_ppr = sum(points_scored*!(paddle | foot))/ last(shots),
                  def_ppr = paddle_points/last(shots),
                  toss_efficiency = sum(!(paddle | foot))/last(shots)) %>% 
        ungroup()
      
      
      # Congratulate paddlers for good offense, chide those who paddled against their own team
      if(input$paddle & str_detect(pull(filter(snappaneers(), player_name == input$scorer), team), "[Bb]") ){
        showNotification("That's some hot shit!")
      }
      if(input$paddle & str_detect(pull(filter(snappaneers(), player_name == input$scorer), team), "[Aa]") ){
        showNotification("It's a bold strategy Cotton, let's see if it pays off for them.")
      }
    } else {
      vals$error_msg <- "You did not input anything."
    }
    
    # If the game is still in rebuttal in rebuttal, remind players
    # of the points needed to bring it back
    vals$rebuttal = rebuttal_check(vals$current_scores$team_a, 
                                   vals$current_scores$team_b,
                                   round_num(), vals$score_to)
    
    #    if (!is.null(vals$rebuttal)) {
    if (vals$rebuttal == T & vals$rebuttal_tag == T) {
      showNotification(str_c("Rebuttal: ", "Team ", 
                             str_sub(round_num(), start = -1),
                             " needs ", str_c(
                               abs(vals$current_scores$team_a - vals$current_scores$team_b) - 1),
                             " points to bring it back"
      )
      )
    } else {
      
    }
    
    validate(
      need((vals$current_scores$team_a == 18 && vals$current_scores$team_b == 12) || (vals$current_scores$team_a == 12 && vals$current_scores$team_b == 18), label = "eighteen_twelve")
    )
    
    
    sendSweetAlert(session, 
                   title = "1812",
                   text = "Everyone roll a die, lowest roll takes a shot.",
                   type = "warning")
  })
  
  # Undo score
  observeEvent(input$undo_score_b, {
    validate(
      need(vals$current_scores$team_b > 0, label = "Team B hasn't scored yet!")
    )

    # Select the ID which is the max on Team B
    last_score = filter(vals$scores_db, scoring_team == "b") %>% 
      pull(score_id) %>% 
      max()
    
    # Pull the number of points the last score was worth
    last_score_pts = filter(vals$scores_db, score_id == last_score) %>% 
      pull(points_scored)
    
    # Reset any scores which have happened since the score being erased
    vals$scores_db = filter(vals$scores_db, score_id != last_score) %>% 
      mutate(score_id = if_else(score_id > last_score, as.integer(score_id-1), score_id))
    
    vals$current_scores$team_b = vals$current_scores$team_b - last_score_pts
    vals$score_id = as.integer(vals$score_id-1)
  })
  
  
  

# End of the game ---------------------------------------------------------

  
  
  observeEvent(input$finish_game, {
    showModal(
      modalDialog(easyClose = T,
        helpText(h2("End Game", align = "center"),
                 p("Is the game over?", align = "center")),
        footer = tagList(
          actionBttn("finish_game_sure", "Yes", style = "bordered", color = "warning"),
        )
      )
    )

  })
  
  observeEvent(input$finish_game_sure, {
    showModal(
      modalDialog(
                  h2("Well full send that data to the SnappaDB!"),
                  # Download to csv
                  downloadBttn("downloadData", "Download", style = "unite", color = "warning"),
                  # Send to DB
                  actionBttn("send_to_db", "Send to the SnappaDB", style = "unite", color = "warning",
                             icon = icon("cloud-upload-alt"))
      )
    )
    
  })
  

# Send to DB --------------------------------------------------------------

  
  
  observeEvent(input$send_to_db, {
   
    
    # Sanity check: Only submit games which have reached their conclusion
    validate(need(any(vals$current_scores$team_a >= vals$score_to,
                      vals$current_scores$team_b >= vals$score_to), 
                  message = "Your game hasn't ended yet. Please finish the current game or restart before submitting",
                  label = "check_game_over"))
    
    
    
    #update player_stats one more time so that points per shot is accurate
    vals$player_stats_db = vals$scores_db %>% 
      left_join(snappaneers(), by = "player_id") %>% 
      group_by(game_id, player_id, team, shots) %>% 
      summarise(total_points = sum(points_scored),
                ones = sum((points_scored == 1)),
                twos = sum((points_scored == 2)),
                threes = sum((points_scored == 3)),
                impossibles = sum((points_scored > 3)),
                paddle_points = sum(points_scored*paddle),
                clink_points = sum(points_scored*clink),
                points_per_round = total_points / last(shots),
                off_ppr = sum(points_scored*!paddle)/ last(shots),
                def_ppr = paddle_points/last(shots),
                toss_efficiency = sum(!paddle)/last(shots)) %>% 
      ungroup()
    
    
    # Make sure that everyone is in the player_stats table, i.e., 
    # record the trolls in the dungeon
    vals$trolls = snappaneers() %>%
                  anti_join(vals$player_stats_db, by = "player_id")
    
    vals$player_stats_db = bind_rows(vals$player_stats_db, 
                tibble(
                  game_id = rep(vals$game_id, times = nrow(vals$trolls)),
                  player_id = pull(vals$trolls, player_id), 
                  team = pull(vals$trolls, team), 
                  total_points = rep(integer(1), times = nrow(vals$trolls)), # Weirdly enough, integer(1) is a 0 integer vector of length 1
                  shots = pull(vals$trolls, shots),
                  ones = rep(integer(1), times = nrow(vals$trolls)),
                  twos = rep(integer(1), times = nrow(vals$trolls)),
                  threes = rep(integer(1), times = nrow(vals$trolls)),
                  impossibles = rep(integer(1), times = nrow(vals$trolls)),
                  paddle_points = rep(integer(1), times = nrow(vals$trolls)),
                  clink_points = rep(integer(1), times = nrow(vals$trolls)),
                  points_per_round = rep(integer(1), times = nrow(vals$trolls)),
                  off_ppr = rep(integer(1), times = nrow(vals$trolls)),
                  def_ppr = rep(integer(1), times = nrow(vals$trolls)),
                  toss_efficiency = rep(integer(1), times = nrow(vals$trolls))
                  )
                )
    
    
    
    # Update Game History
    # Calculate game-level stats from game stats players
    game_stats = vals$player_stats_db %>% 
      group_by(game_id) %>% 
      summarise(points_a = sum((team == "a")*total_points),
                points_b = sum((team == "b")*total_points),
                rounds = as.integer(vals$shot_num-1),
                ones = sum(ones),
                twos = sum(twos),
                threes = sum(threes),
                impossibles = sum(impossibles),
                paddle_points = sum(paddle_points),
                clink_points = sum(clink_points))

    vals$game_stats_db = vals$game_stats_db %>% 
      replace_na(list(game_end = as.character(now(tzone = "America/Los_Angeles")))) %>% 
      mutate(night_dice = if_else(now(tzone = "America/Los_Angeles") %>% hour() > 20, T, F)) %>% 
      left_join(game_stats, by = "game_id")
    
    dbAppendTable(
      conn = con, 
      name = "game_stats",
      value = vals$game_stats_db)
    
    dbAppendTable(
      conn = con, 
      name = "players",
      value = anti_join(vals$players_db, players_tbl))
    
    # Update Scores
    dbAppendTable(
      conn = con, 
      name = "scores",
      value = vals$scores_db)
    
    # Update player_stats
    dbAppendTable(
      conn = con, 
      name = "player_stats",
      value = vals$player_stats_db)
    
    # Confirmation that data was sent to db
    sendSweetAlert(session, 
                   title = "The die is cast",
                   text = "Data sent to SnappaDB",
                   type = "success")
    
  })
  

# Restart game ------------------------------------------------------------

  
  
  observeEvent(input$new_game, {
    
    showModal(
      modalDialog( title = "Restart game", easyClose = T,
        helpText("Are you sure?"),
        footer = tagList(
          actionBttn("new_game_sure", "Yup", style = "unite", color = "warning")
        )
      )
    )
    
  })
  
  observeEvent(input$new_game_sure, {
    # On a new game:
    # 1. Switch to start screen
    updateTabsetPanel(session, "switcher", selected = "start_screen")
    
    # 2. Reset player inputs
    walk2(c("name_a1", "name_a2", "name_b1", "name_b2"), c("Player 1", "Player 2", "Player 1", "Player 2"), 
         function(id, lab) updateSelectizeInput(session, inputId = id, label = lab, c(`Player Name`='', pull(players_tbl, player_name)), 
                                           options = list(create = TRUE)))
    
    # 3. Reset reactive values
    vals$game_stats_db = game_stats_tbl %>% slice(0) %>% select(1:5)
    vals$player_stats_db = player_stats_tbl %>% slice(0)
    vals$players_db = tbl(con, "players") %>% collect()
    vals$scores_db = scores_tbl %>% slice(0)
    vals$score_id = as.integer(0)
    vals$shot_num = as.integer(1)
    

    
    removeModal()
    
    
  })



  
}

# Disconnect from the server at the end  
onStop(function() {
  dbDisconnect(conn = con)
})

# Run the application 
shinyApp(ui = ui, server = server)





# Notes -------------------------------------------------------------------

