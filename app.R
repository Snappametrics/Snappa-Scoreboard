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


validate_scores = function(player, shot, snappaneers, paddle, scores_table, round_vector = rounds, rebuttal = F){
  
  players_team = pull(filter(snappaneers, player_name == player), team) %>% toupper()
  scorer_id = pull(filter(snappaneers, player_name == player), player_id)
  
  # Typical Offense
  typical_offense = str_detect(round_vector[shot], players_team)
  
  # Typical Paddle
  typical_paddle = all(str_detect(round_vector[shot], players_team, negate = T), 
                       paddle == T)
  
  # NOTE: already scored in the rest of this function refers to having already scored a non-paddle shot
  # Players can only score once on a non-paddle shot
  not_already_scored = !(scorer_id %in% pull(filter(scores_table, round_num == round_vector[shot], paddle == F), player_id))
  

  # If teams are even
  if(nrow(distinct(count(snappaneers, team), n)) == 1){
    
    
    valid_score = any(paddle,
                      all(typical_offense,
                          not_already_scored))
    
    if(not_already_scored){
      valid_score_message = "That entry doesn't make sense for this round/shooter combination"
    }
    
    valid_score_message = "That player scored a non-paddle point already!"
    
  } else { # If teams are uneven:
    
    # Check if the scorer is on the team with fewer players (i.e. can score multiple non-paddle points)
    scorer_team_players = nrow(filter(snappaneers, team == players_team))
    other_team_players = nrow(filter(snappaneers, team != players_team))
    
    if((scorer_team_players < other_team_players)|rebuttal){ # If their team has fewer players OR Rebuttal:
      
      # NOTE: already scored in the rest of this function refers to having already scored a non-paddle shot
      # Players can only score once on a non-paddle shot
      scored_already = (nrow(filter(scores_table, round_num == round_vector[shot], paddle == F, player_id == scorer_id)) > 1)
      
      # Then they can have scored already
      valid_score = any(all(typical_offense,
                            !scored_already),
                        rebuttal,
                        paddle)
      
      # Score message if invalid
      valid_score_message = "That player scored a non-paddle point already!"
      
    } else { # If they have the larger team:
      # They cannot have scored already
      valid_score = any(all(typical_offense,
                            not_already_scored),
                        paddle)
      
      valid_score_message = "That player scored a non-paddle point already!"
      
    }
    
  }
  
  # Validate all valid scores
  if(valid_score){
    valid_score_message = "valid"
  }
  
  return(valid_score_message)
  
}

troll_check = function(session, snappaneers, player_stats, game_id){
  
  
  trolls = snappaneers %>%
    anti_join(player_stats, by = "player_id")
  
  bind_rows(player_stats, 
            tibble(
                   game_id = rep(game_id, times = nrow(trolls)),
                   player_id = pull(trolls, player_id), 
                   team = pull(trolls, team), 
                   total_points = rep(integer(1), times = nrow(trolls)), # Weirdly enough, integer(1) is a 0 integer vector of length 1
                   shots = pull(trolls, shots),
                   ones = rep(integer(1), times = nrow(trolls)),
                   twos = rep(integer(1), times = nrow(trolls)),
                   threes = rep(integer(1), times = nrow(trolls)),
                   impossibles = rep(integer(1), times = nrow(trolls)),
                   paddle_points = rep(integer(1), times = nrow(trolls)),
                   clink_points = rep(integer(1), times = nrow(trolls)),
                   points_per_round = rep(double(1), times = nrow(trolls)),
                   off_ppr = rep(double(1), times = nrow(trolls)),
                   def_ppr = rep(double(1), times = nrow(trolls)),
                   toss_efficiency = rep(double(1), times = nrow(trolls))
                 )
  )
  
}

# For the sake of code simplicity, I'm going to define a function which
# writes player_stats_db off of scores. This should only require
# a scores table to be passed on, since the snappaneers table that is also
# called would always be the same
app_update_player_stats = function(scores_df, neers){
   g.id = unique(scores_df$game_id)
   output = scores_df %>% 
    # Join scores to snappaneers to get each player's team
    left_join(neers, by = "player_id") %>% 
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
  
  output = troll_check(snappaneers = neers,
                                     player_stats = output,
                                     game_id = g.id)
  return(output)
}



db_update_player_stats = function(player_stats){
  current_game = unique(player_stats$game_id)
  del_rows = sql(str_c("DELETE FROM player_stats 
                 WHERE game_id = ", current_game, ";"))
  
  dbExecute(con, del_rows)
  
  dbAppendTable(con, "player_stats", player_stats)
}

# For use with restarting a lost game: First extract the table
# with the teams and the number of players for each team for the given
# game_id. This function isn't all that necesary in its own right, but
# I think it helps for readability
extract_team_sizes = function(g.id){
  output = tbl(con, "player_stats") %>% collect() %>% 
    filter(game_id == g.id) %>%
    count(team)
  return(output)
}

generate_round_num = function(df, g.id){
  # Generate the possible sequences for assignment
  shot_nums = rep(1:200, each = 1)
  A_geq = rep(1:100, each = 2)
  B_geq = c(0, A_geq[-200])

  # The values of the shot nums from each team also
  # need to be recorded. Unique works in this case because
  # we update shot number for every team member simultaneously
  A_shots = tbl(con, "player_stats") %>% 
    filter(game_id == g.id & team == "A") %>% 
    pull(shots) %>%
    unique()
  B_shots = tbl(con, "player_stats") %>% 
    filter(game_id == g.id & team == "B") %>% 
    pull(shots) %>%
    unique()
    
  # I generate a multiplier which expresses the difference
  # between A's players and B's players. For the if statements
  # which follow, I could either reason on the number of players  
  # on each team or the multiplier. Each of these has their own
  # drawbacks, but I'll take mathematical complication as the
  # downside if the gain is having a simple battery of "if" 
  # statements. 
  multiplier = df %>% filter(team == "A") %>% pull(n) / 
    df %>% filter(team == "B") %>% pull(n) 
  
  
  # Reason on the possible sequence of shots according
  # to the multiplier
  if(multiplier > 1) {
    A_seq = A_geq
    B_seq = c(0, seq(from = 1, to = (100 * multiplier - multiplier), by = multiplier))
  } else if (multiplier == 1){
    A_seq = A_geq
    B_seq = B_geq
  } else {
    A_seq = seq(from = 1, to = (100 / multiplier), by = (1/multiplier))
    B_seq = B_geq
  }
  
  # Now build the table for checking the round number
  check_table = tibble(shots = shot_nums, 
                       A = A_seq,
                       B = B_seq)

  # Finally, pull the value from shots when A and B meet
  # the conditions
  value = check_table %>% 
    filter(A == A_shots, B == B_shots) %>%
    pull(shots)
  return(value)
}


# Career Stats ------------------------------------------------------------




score_progression = scores_tbl %>% 
  arrange(game_id, score_id) %>% 
  group_by(game_id) %>% 
  mutate(
    score_a = cumsum((scoring_team=="A")*points_scored),
    score_b = cumsum((scoring_team=="B")*points_scored)
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
                   team_input_ui("A", pull(players_tbl, player_name)),
                   
                   # Column 2 - empty
                   column(4,  align = "center",
                          disabled(actionBttn("start_game", 
                                              label = "Throw some dice?", style = "pill", color = "primary")),
                          uiOutput("validate_start"),
                          
                          helpText("Note: All players must enter their name before the game can begin"),

                          awesomeRadio(inputId = "play_to", 
                                       label = "What score are you playing to?", 
                                       choices = list("21" = 1, "32" = 2), 
                                       selected = 1, inline = T)),
                          
                   
                   # Column 3 - Team B
                   team_input_ui("B", pull(players_tbl, player_name))
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
      mutate(baseline_shots = case_when(str_detect(.data$team, "A") ~ ceiling(vals$shot_num/2),
                                        str_detect(.data$team, "B") ~ floor(vals$shot_num/2)),
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
      team_A = 0,
      team_B = 0
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
    want_A3 = F,
    want_A4 = F,
    want_B3 = F,
    want_B4 = F,

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
    list("A1" = input$name_A1, "A2" = input$name_A2, "A3" = input$name_A3, "A4" = input$name_A4, 
         "B1" = input$name_B1, "B2" = input$name_B2, "B3" = input$name_B3, "B4" = input$name_B4) %>% 
      discard(is_null)
  })
  
  # Snappaneers - | Team | Player name | Player ID  | Shots
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


  output$A_score_val = renderUI({
    # Check that the round/shooter combination makes sense / indicated a paddle
    validate(
      need(
        validate_scores(player = input$scorer,
                        shot = vals$shot_num, 
                        snappaneers = snappaneers(), 
                        paddle = any(input$paddle, input$foot), 
                        scores_table = vals$scores_db,
                        rebuttal = vals$rebuttal_tag) == "valid",
        message = "That entry doesn't make sense for this round/shooter combination"),
      if (pull(filter(snappaneers(), player_name == input$scorer), player_id) %in%
          pull(filter(vals$scores_db, round_num == round_num() & paddle == F), player_id)){
        need(
           validate_scores(player = input$scorer,
                               shot = vals$shot_num, 
                               snappaneers = snappaneers(), 
                               paddle = any(input$paddle, input$foot), 
                               scores_table = vals$scores_db,
                           rebuttal = vals$rebuttal_tag) == "valid",
             message = "That person has already scored a non paddle point this round")
        
      }
    )

    actionButton("ok_A", "OK")
  })
  
  output$B_score_val = renderUI({
    validate(
      # General needs for typical shooting
      need(
        validate_scores(player = input$scorer,
                        shot = vals$shot_num, 
                        snappaneers = snappaneers(), 
                        paddle = any(input$paddle, input$foot), 
                        scores_table = vals$scores_db,
                        rebuttal = vals$rebuttal_tag) == "valid",
        message = "That entry doesn't make sense for this round/shooter combination"
      ),
      # Make sure that the last person to score in this round on offense can't paddle
      if (pull(filter(snappaneers(), player_name == input$scorer), player_id) %in%
          pull(filter(vals$scores_db, round_num == round_num() & paddle == F), player_id)){
        need(
          validate_scores(player = input$scorer,
                            shot = vals$shot_num, 
                            snappaneers = snappaneers(), 
                            paddle = any(input$paddle, input$foot), 
                            scores_table = vals$scores_db,
                          rebuttal = vals$rebuttal_tag) == "valid",
             message = "That person has already scored a non paddle point this round")
        
      } 
    )
      actionButton("ok_B", "OK")
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
  output$score_A = renderText({
    vals$current_scores$team_A
  })
  
  output$player_names_A = renderText({
    snappaneers() %>% 
      filter(team == "A") %>% 
      pull(player_name) %>% 
      str_c(., collapse = ", ")
  })
  
  # Output Team B's score
  output$score_B = renderText({
    vals$current_scores$team_B
  })
  
  output$player_names_B = renderText({
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
          "<p><span style='font-weight:500'>Team B</span>: ", x, "  ", "<span style='font-weight:500'>Team A</span>: ", y, "</p>",
          "<p><span style='font-weight:500'>Frequency</span>: ", freq))
  })
  
  

# Restart Game Outputs ----------------------------------------------------

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

  

# Check Existing Game -----------------------------------------------------


# Very start of game: display a popup message
# if the previous game is incomplete
  
observe({
  validate(
    need(
      tbl(con, "game_stats") %>% 
        filter(game_id == max(game_id, na.rm=T)) %>%
        pull(game_complete) %>% 
        isFALSE(),
      message = FALSE
    )
  )
  lost_game_id = tbl(con, "game_stats") %>% pull(game_id) %>% max()
  
  # Pass an additional check to see if the game which is in question is a 0-0 or not. 
  total_lost_game_score = tbl(con, "player_stats") %>% 
    collect() %>% 
    filter(game_id == lost_game_id) %>%
    pull(total_points) %>%
    sum()
  
  # Discard that game if it's 0-0 and continue on with business as usual, else
  # allow players to restart
  if (total_lost_game_score == 0){
    delete_query = sql("DELETE FROM game_stats WHERE game_id = (SELECT MAX(game_id) FROM game_stats);")
    dbExecute(con, delete_query)
  } else {
  
  showModal(
    modalDialog(
      p("There's an incomplete game in the snappa database, would 
                  you like to restart it?", align = 'center'),
      br(),
      p("Warning: saying 'No' will delete the previous game from the database", align = 'center'),
      br(),
      h3("Summary of the Previous Game", align = 'center'),
      br(),
      renderUI({glance_ui_game(lost_game_id)}),
      title = "Incomplete Game",
      footer = tagList(
                fluidRow(
                  actionBttn("resume_no",
                             label = "No",
                             style = "unite",
                             color = "danger"),
                  actionBttn("resume_yes",
                             label = "Yes",
                             style = "unite", 
                             color = "warning")
                )
              ),
      size = "l",
      easyClose = F,
      fade = F
    )
   
  )
  }
  
})


# Game Start Validation ---------------------------------------------------

  
  observeEvent(input$switch_sides, {
    
    vals$switch_counter = vals$switch_counter+1
    
    switch_is_even = (vals$switch_counter %% 2 == 0)
    
    
    if(switch_is_even){
      removeUI("#ScoreboardUI", immediate=T)
      insertUI(selector = "#bottom_buttons", ui = team_scoreboard_ui("B", "A"), where = "beforeBegin")
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
    
    if(any(input$name_A1 == "",
           input$name_A2 == "",
           input$name_B1 == "",
           input$name_B2 == "")){
      shinyjs::disable("start_game")
    }
    
    validate(
      need(input$name_A1 != "", label = "Player A1"),
      need(input$name_A2 != "", label = "Player A2"), 
      need(input$name_B1 != "", label = "Player B1"), 
      need(input$name_B2 != "", label = "Player B2")
      )
    
    #Record the players that you need to be looking for
    # (i.e., which ui elements are open right now?)
    
    
    # If the number of unique snappaneer names is the same as the number of active player inputs
    #   => enable start button
    
    if(sum(length(unique(snappaneers()$player_name)), 
           sum(
             c(isTRUE(active_player_inputs()$A3 == "" & vals$want_A3), 
             isTRUE(active_player_inputs()$A4 == "" & vals$want_A4), 
             isTRUE(active_player_inputs()$B3 == "" & vals$want_B3), 
             isTRUE(active_player_inputs()$B4 == "" & vals$want_B4)) 
             )
           ) == num_players()){ 
      shinyjs::enable("start_game")
    } 
    
    # If the number of unique snappaneer names is not the same as the number of active player inputs
    #   => disable start button
    if(sum(length(unique(snappaneers()$player_name)), 
           sum(
             c(isTRUE(active_player_inputs()$A3 == "" & vals$want_A3), 
                isTRUE(active_player_inputs()$A4 == "" & vals$want_A4), 
                isTRUE(active_player_inputs()$B3 == "" & vals$want_B3), 
                isTRUE(active_player_inputs()$B4 == "" & vals$want_B4)
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
                                    # fluidRow(
                                    #   column(width =4, offset = 4, align = "center",
                                    #          # Recent Scores
                                    #          dropdown(
                                    #            inputId = "recent_scores",
                                    #            gt_output("recent_scores"),
                                    #            style = "unite",
                                    #            size = "lg", 
                                    #            up = T,
                                    #            label = "Recent Scores",
                                    #            icon = icon("backward"),
                                    #            animate = animateOptions(
                                    #              enter = animations$fading_entrances$fadeInUp,
                                    #              exit = animations$fading_exits$fadeOutDown
                                    #            )
                                    #          ))
                                    #   
                                    # ),
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
        
        # Update the players database right here with the player name
        
        dbAppendTable(con, "players", 
          tibble(
            player_id = vals$new_player_id,
            player_name = die_thrower
          )
        )
        
        # Increment the ID for the next new player
        vals$new_player_id = vals$new_player_id+1
        
        
        
        
      } else {
        invisible()
      }
    })
    
    # Switch to the scoreboard
    updateTabsetPanel(session, "switcher", selected = "scoreboard")
    # Using isFALSE also denies character(0) in the event that we're starting on a fresh table. Nice!
    if (tbl(con, "game_stats") %>% 
              filter(game_id == max(game_id)) %>% 
              pull(game_complete) %>% 
              isFALSE()) {
        
        lost_game = tbl(con, "game_stats") %>% 
          filter(game_id == max(game_id, na.rm = T)) %>% 
          pull(game_id)
      
        lost_game_stats = tbl(con, "game_stats") %>% 
          filter(game_id == lost_game)
      
        lost_player_stats = tbl(con, "player_stats") %>% 
          filter(game_id == lost_game)
      
        lost_game_scores = list(team_a = lost_game_stats)
      
      # Set the score outputs and shot number to the values from the last game
        vals$current_scores$team_A = lost_player_stats %>% 
          filter(team == "A" & game_id == lost_game) %>%
          pull(total_points) %>%
          sum()
        
        vals$current_scores$team_B = lost_player_stats %>% 
          filter(team == "B" & game_id == lost_game) %>%
          pull(total_points) %>%
          sum()
        
        vals$score_id = tbl(con, "scores") %>% 
          filter(game_id == lost_game) %>%
          pull(score_id) %>%
          max()
        

        vals$scores_db = tbl(con, "scores") %>% filter(game_id == lost_game) %>% collect()
        vals$game_id = lost_game
        vals$shot_num = extract_team_sizes(vals$game_id) %>% generate_round_num(g.id = vals$game_id)
        
        vals$game_stats_db = collect(lost_game_stats)
        
        # Initialize the current game's player_stats table
        vals$player_stats_db = collect(lost_player_stats)
        
    } else {
      
      # Set the score outputs and shot number to 0
      vals$current_scores$team_a = 0
      vals$current_scores$team_b = 0
      vals$scores_db = slice(scores_tbl, 0)
      vals$game_id = as.integer(sum(dbGetQuery(con, "SELECT MAX(game_id) FROM game_stats"),1 , na.rm = T))
      
      vals$game_stats_db = bind_rows(vals$game_stats_db,
                                     tibble(
                                       game_id = vals$game_id,
                                       num_players = nrow(snappaneers()),
                                       game_start = as.character(now()),
                                       game_end = NA_character_,
                                       night_dice = NA,
                                       points_a = NA_integer_,
                                       points_b = NA_integer_,
                                       rounds = NA_integer_,
                                       ones = NA_integer_,
                                       twos = NA_integer_,
                                       threes = NA_integer_,
                                       impossibles = NA_integer_,
                                       paddle_points = NA_integer_,
                                       clink_points = NA_integer_,
                                       game_complete = F
                                     ))
      
      
      # Initialize the current game's player_stats table
      vals$player_stats_db = slice(vals$player_stats_db, 0)
      
      dbAppendTable(
        conn = con, 
        name = "game_stats",
        value = vals$game_stats_db
      )
    }
    
    

    # Record the score we're playing to
    vals$score_to = case_when(input$play_to == 1 ~ 21,
                              input$play_to == 2 ~ 32)
    
    
    

  })
  


  
# Restart a game after indicating you would like to do so
  observeEvent(input$resume_yes, {
    
    
    lost_game = tbl(con, "game_stats") %>% filter(game_id == max(game_id)) %>% pull(game_id)
    
    lost_player_stats = tbl(con, "player_stats") %>% filter(game_id == lost_game)
    
    players = tbl(con, "players")
    
    lost_players = lost_player_stats %>%
      left_join(players) %>%
      select(player_name, team) %>% 
      group_by(team) %>% 
      mutate(player_input = str_c("name_", team, row_number())) %>% 
      ungroup() %>% 
      collect()
    
    input_list = lost_players %>%
      select(player_input, player_name) %>% 
      deframe()
    

    iwalk(input_list, function(name, id){
      updateSelectizeInput(session, inputId = id, selected = name)
    
    })
    removeModal()
    delay(1000, shinyjs::click("start_game"))
    
  })

# Close the modal dialog if you say no, set an observer value to TRUE, and remove
# the old game from the DB
  
observeEvent(input$resume_no, {
  removeModal()
  
  delete_query = sql("DELETE FROM game_stats WHERE game_id = (SELECT MAX(game_id) FROM game_stats);")
  dbExecute(con, delete_query)
})
  
  

# Next Round --------------------------------------------------------------
  
  # When previous round button is pushed
  observeEvent(input$previous_round, {
    validate(
      need(vals$shot_num > 1, label = "Can't go below 0", message = "It's the first round still")
    )
    vals$shot_num = vals$shot_num-1
    
    
    # This is for the case when there hasn't been a scoring point yet, which causes this to fail in the transition
    # between rounds 1A and 1B. Clumsy, perhaps, but it works
      # Update player stats in the app
      vals$player_stats_db = app_update_player_stats(vals$scores_db, snappaneers())    
      #Update the DB with the new player_stats
      db_update_player_stats(vals$player_stats_db)
      
    
    
    
    
    
  })
  

  
  
  # When next round button is pushed
  observeEvent(input$next_round, {
    
    if (vals$rebuttal_tag == T){
      if (vals$rebuttal == T){
        click("finish_game")
        vals$shot_num = vals$shot_num - 1
      } else {
        vals$rebuttal_tag = F
      }
    } else{
    }
    
    vals$shot_num = vals$shot_num+1
    if (vals$current_scores$team_A == 0 &
        vals$current_scores$team_B == 0){
      invisible()
    } else {
      
    # Update player stats in the app
    vals$player_stats_db = app_update_player_stats(vals$scores_db, snappaneers())    
    #Update the DB with the new player_stats (adds to shots)
    db_update_player_stats(vals$player_stats_db)
    }

    vals$rebuttal = rebuttal_check(a = vals$current_scores$team_A, b = vals$current_scores$team_B,
                                   round = round_num(), points_to_win = vals$score_to)
    
    if (vals$rebuttal == T) {
      vals$rebuttal_tag = T
      showNotification(str_c("Rebuttal: ", "Team ", 
                             str_sub(round_num(), start = -1),
                             " needs ", str_c(
                               abs(vals$current_scores$team_A - vals$current_scores$team_B) - 1),
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
  observeEvent(input$extra_player_A3, {
    # Set input want to true
    vals$want_A3 = T
    
    # Get add player button inputs
    vals <- paste0("#",getInputs("extra_player_A3"))
    add_player_input(vals, "A", 3, current_choices(), session)
    
  })
  
  # Remove A3
  #   - Insert add new player action button
  #   - Remove A3 player name input
  observeEvent(input$remove_A3, {
    remove_p3_input("A", session)

    #Don't consider these elements when looking at
    # total length of players. Prevents the game
    # from getting locked out after players have
    # been added
    vals$want_A3 = F
    vals$want_A4 = F
    
  })
  
  
  # New Player A4
  #   - Add A4 text input
  #   - Remove the add new player action button
  observeEvent(input$extra_player_A4, {
    # Set input want to true
    vals$want_A4 = T
    
    # Get UI inputs for extra player button
    vals <- paste0("#",getInputs("extra_player_A4"))
    
    add_player_input(vals, "A", 4, current_choices(), session)
    
  })
  
  # Remove A4
  #   - Insert add new player action button
  #   - Remove A4 player name input
  observeEvent(input$remove_A4, {
    remove_p4_input("A", session)
    
    vals$want_A4 = F
    
  })  
  
  
  # New Player B3
  #   - Add B3 text input
  #   - Remove the add new player action button
  observeEvent(input$extra_player_B3, {
    
    # Set want check to true
    vals$want_B3 = T
    
    # Get inputs for add player button
    vals <- paste0("#",getInputs("extra_player_B3"))
    
    add_player_input(vals, "B", 3, current_choices(), session)
  })

  # Remove B3
  #   - Insert add new player action button
  #   - Remove B3 player name input
  observeEvent(input$remove_B3, {
    remove_p3_input("B", session)
    
    #Don't consider these elements when looking at
    # total length of players. Prevents the game
    # from getting locked out after players have
    # been added
    vals$want_B3 = F
    vals$want_B4 = F
    
  })
  
  # New Player B4
  #   - Add B4 text input
  #   - Remove the add new player action button
  observeEvent(input$extra_player_B4, {
    # Set want check to true
    vals$want_B4 = T
    
    # Get add player button inputs
    vals <- paste0("#",getInputs("extra_player_B4"))
    
    add_player_input(vals, "B", 4, current_choices(), session)
  })
  

  # Remove B4
  #   - Insert add new player action button
  #   - Remove B4 player name input
  observeEvent(input$remove_B4, {
    remove_p4_input("B", session)
    # Tells later checks to not worry about this
    # empty slot in active_player_names
    vals$want_B4 = F

  })  
    

  
  
  
    
  

# Scoring -----------------------------------------------------------------
  
  #TODO: Fix score_id, game_id, and num_points_scored in scores_db in vals: change from dbl to int


# Team A ------------------------------------------------------------------

  
  observeEvent(input$A_score_button, {
    vals$error_msg <- NULL
    
    eligible_shooters = filter(snappaneers(), team == "A") %>% 
      pull(player_name) %>% 
      sample()
    
    showModal(
      score_check(team = "A", 
                  players = eligible_shooters))
  })
  
  # Team A presses score button
  observeEvent(input$ok_A, {
  
    # set score
    score = as.integer(input$score)
    vals$score <- score
    
    
    # Check score i not null, remove the dialog box
    if (!is.null(vals$score)) {
      removeModal()
      vals$print <- TRUE
      
      # Update the team score
      vals$current_scores$team_A = vals$current_scores$team_A + vals$score
      
      # Increment the score_id
      vals$score_id = as.integer(vals$score_id+1)
      
      ## Identify scoring characteristics
      # Player ID
      scorer_pid = pull(filter(vals$players_db, player_name == input$scorer), player_id)
      # Were they shooting?
      scorers_team = pull(filter(snappaneers(), player_name == input$scorer), team) # pull the scorer's team from snappaneers
      shooting_team_lgl = all(str_detect(round_num(), "A"), scorers_team == "A") # Are they on team A & did they score for team A?
      
      # Add the score to the scores table
      vals$scores_db = bind_rows(vals$scores_db,
                                 tibble(
                                   score_id = vals$score_id,
                                   game_id = vals$game_id,
                                   player_id = scorer_pid,
                                   scoring_team = "A",
                                   round_num = round_num(),
                                   points_scored = score,
                                   shooting = shooting_team_lgl,
                                   paddle = any(input$foot, input$paddle),
                                   clink = input$clink,
                                   foot = input$foot
                                 ))
      #Update the db with the new score
  
      dbAppendTable(con, "scores", anti_join(vals$scores_db, tbl(con, "scores") %>% collect()))
      
      
      # Update player stats table
      vals$player_stats_db = app_update_player_stats(vals$scores_db, snappaneers())
      db_update_player_stats(vals$player_stats_db)

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
    vals$rebuttal = rebuttal_check(vals$current_scores$team_A, 
                                   vals$current_scores$team_B,
                                   round_num(), vals$score_to)
    
    #    if (!is.null(vals$rebuttal)) {
    if (vals$rebuttal == T & vals$rebuttal_tag == T) {
      showNotification(str_c("Rebuttal: ", "Team ", 
                             str_sub(round_num(), start = -1),
                             " needs ", str_c(
                               abs(vals$current_scores$team_A - vals$current_scores$team_B) - 1),
                             " points to bring it back"
      )
      )
    } else {
      
    }
    validate(
      need((vals$current_scores$team_A == 18 && vals$current_scores$team_B == 12) || (vals$current_scores$team_A == 12 && vals$current_scores$team_B == 18), label = "eighteen_twelve")
    )
    
    
    sendSweetAlert(session, 
                   title = "1812",
                   text = "Everyone roll a die, lowest roll takes a shot.",
                   type = "warning")

  })
  # Undo score
  observeEvent(input$undo_score_A, {
    validate(
      need(vals$current_scores$team_A > 0, label = "Team A hasn't scored yet!")
    )

    # Select the ID which is the max on Team A
    last_score = filter(vals$scores_db, scoring_team == "A") %>% 
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
    vals$current_scores$team_A = vals$current_scores$team_A - last_score_pts
    vals$score_id = as.integer(vals$score_id-1)
    
    #Remove the value from the snappaDB
    dbSendQuery(con,
      str_c("DELETE FROM scores WHERE score_id = ", last_score, " AND game_id = ", vals$game_id, ";")
    )
    
    
    # Update player stats table in the app
    vals$player_stats_db = app_update_player_stats(vals$scores_db, snappaneers())
   #Update the DB with the new player_stats
    db_update_player_stats(vals$player_stats_db)
    
  })
  
  
  # Team B ---------------------------------------------------------
  
  
  observeEvent(input$B_score_button, {
    vals$error_msg <- NULL
    
    eligible_shooters = filter(snappaneers(), team == "B") %>% 
      pull(player_name) %>% 
      sample()
    
    showModal(
      score_check(
        team = "B", 
        players = eligible_shooters))

  })
  
  # Score validation
  observeEvent(input$ok_B, {
    #Set Score
    score = as.integer(input$score)
    vals$score <- score
    
    if (!is.null(vals$score)) {
      removeModal()
      vals$print <- TRUE
      
      # Update Team B's score
      vals$current_scores$team_B = vals$current_scores$team_B + vals$score
      
      # Increment the score_id
      vals$score_id = as.integer(vals$score_id+1)
      
      ## Identify scoring characteristics
      # Player ID
      scorer_pid = pull(filter(vals$players_db, player_name == input$scorer), player_id)
      # Were they shooting?
      scorers_team = pull(filter(snappaneers(), player_name == "Shaunt"), team)
      shooting_team_lgl = all(str_detect(round_num(), "[Bb]"), scorers_team == "B")
      
      # Add the score to the scores table
      vals$scores_db = bind_rows(vals$scores_db,
                              tibble(
                                score_id = vals$score_id,
                                game_id = vals$game_id,
                                player_id = scorer_pid,
                                scoring_team = "B",
                                round_num = round_num(),
                                points_scored = score,
                                shooting = shooting_team_lgl,
                                paddle = any(input$paddle, input$foot),
                                clink = input$clink,
                                foot = input$foot
                              ))
      #Update the server with the new score
      dbAppendTable(con, "scores", 
                    anti_join(vals$scores_db, tbl(con, "scores") %>% collect()))
      
      
      # Update player stats in the app
      vals$player_stats_db = app_update_player_stats(vals$scores_db, snappaneers())    
      #Update the DB with the new player_stats
      db_update_player_stats(vals$player_stats_db)
      
      
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
    vals$rebuttal = rebuttal_check(vals$current_scores$team_A, 
                                   vals$current_scores$team_B,
                                   round_num(), vals$score_to)
    
    #    if (!is.null(vals$rebuttal)) {
    if (vals$rebuttal == T & vals$rebuttal_tag == T) {
      showNotification(str_c("Rebuttal: ", "Team ", 
                             str_sub(round_num(), start = -1),
                             " needs ", str_c(
                               abs(vals$current_scores$team_A - vals$current_scores$team_B) - 1),
                             " points to bring it back"
      )
      )
    } else {
      
    }
    
    validate(
      need((vals$current_scores$team_A == 18 && vals$current_scores$team_B == 12) || (vals$current_scores$team_A == 12 && vals$current_scores$team_B == 18), label = "eighteen_twelve")
    )
    
    
    sendSweetAlert(session, 
                   title = "1812",
                   text = "Everyone roll a die, lowest roll takes a shot.",
                   type = "warning")
  })
  
  # Undo score
  observeEvent(input$undo_score_B, {
    validate(
      need(vals$current_scores$team_B > 0, label = "Team B hasn't scored yet!")
    )

    # Select the ID which is the max on Team B
    last_score = filter(vals$scores_db, scoring_team == "B") %>% 
      pull(score_id) %>% 
      max()
    
    # Pull the number of points the last score was worth
    last_score_pts = filter(vals$scores_db, score_id == last_score) %>% 
      pull(points_scored)
    
    # Reset any scores which have happened since the score being erased
    vals$scores_db = filter(vals$scores_db, score_id != last_score) %>% 
      mutate(score_id = if_else(score_id > last_score, as.integer(score_id-1), score_id))
    
    vals$current_scores$team_B = vals$current_scores$team_B - last_score_pts
    vals$score_id = as.integer(vals$score_id-1)
    
    
    #Remove the value from the snappaDB
    dbSendQuery(con,
                str_c("DELETE FROM scores WHERE score_id = ", last_score, 
                      " AND game_id = ", vals$game_id)
    )    
    # Update player_stats 
    vals$player_stats_db = app_update_player_stats(vals$scores_db, snappaneers())
    #Update the DB with the new player_stats
    db_update_player_stats(vals$player_stats_db)
    
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

    # CODE TO USE IN RESUME GAME VALIDATION
    #
    # validate(need(any(vals$current_scores$team_a >= vals$score_to,
    #                   vals$current_scores$team_b >= vals$score_to),
    #               message = "Your game hasn't ended yet. Please finish the current game or restart before submitting",
    #               label = "check_game_over"))

    # Update Game History
    # Calculate game-level stats from game stats players, and vary it based on whether the game is actually complete or not
    # to test it I use rebuttal since this is the one point in time where we can basically be certain that a game is/isn't over
    # Checking vals$rebuttal here is redundant if we have already clicked next round, but this is necessary in games where
    # players clicked "finish game" since rebuttal is checked on the next round button
    vals$rebuttal = rebuttal_check(a = vals$current_scores$team_A, b = vals$current_scores$team_B,
                                   round = round_num(), points_to_win = vals$score_to)
    
    

    if(vals$rebuttal == T){
    game_stats = vals$player_stats_db %>% 
      group_by(game_id) %>% 
      summarise(points_a = sum((team == "A")*total_points),
                points_b = sum((team == "B")*total_points),
                rounds = as.integer(vals$shot_num-1),
                ones = sum(ones),
                twos = sum(twos),
                threes = sum(threes),
                impossibles = sum(impossibles),
                paddle_points = sum(paddle_points),
                clink_points = sum(clink_points),
                game_complete = T)
    } else {
      game_stats = vals$player_stats_db %>% 
        group_by(game_id) %>% 
        summarise(points_a = sum((team == "A")*total_points),
                  points_b = sum((team == "B")*total_points),
                  rounds = as.integer(vals$shot_num-1),
                  ones = sum(ones),
                  twos = sum(twos),
                  threes = sum(threes),
                  impossibles = sum(impossibles),
                  paddle_points = sum(paddle_points),
                  clink_points = sum(clink_points),
                  game_complete = F)
    }
    # This uses select because the column names were no longer matching the DB ones after joining
    vals$game_stats_db = vals$game_stats_db %>% 
      replace_na(list(game_end = as.character(now(tzone = "America/Los_Angeles")))) %>% 
      mutate(night_dice = if_else(now(tzone = "America/Los_Angeles") %>% hour() > 20, T, F)) %>% 
      left_join(game_stats, by = "game_id", suffix = c("_old", "")) %>% 
      select(-contains("_old", ignore.case = F))
    
    
    # As with player_stats, I perform the update by deleting the relevant row in the DB table and reinserting the
    # one that we need
    
    del_game_row = sql(str_c("DELETE FROM game_stats 
                 WHERE game_id = ", vals$game_id, ";"))
    
    dbExecute(con, del_game_row)
  
    dbAppendTable(
      conn = con, 
      name = "game_stats",
      value = vals$game_stats_db)
    
    # Update player stats table one final time
    vals$player_stats_db = app_update_player_stats(vals$scores_db, snappaneers())
    
    db_update_player_stats(vals$player_stats_db)
    
    
    # Should be made unnecessary by adding
    # players immediately 
    
    # dbAppendTable(
    #   conn = con, 
    #   name = "players",
    #   value = anti_join(vals$players_db, players_tbl))
    
    #Update Scores
    dbAppendTable(
      conn = con,
      name = "scores",
      value = vals$scores_db)
    
    # Update player_stats
    # dbAppendTable(
    #   conn = con, 
    #   name = "player_stats",
    #   value = vals$player_stats_db)
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
    walk2(c("name_A1", "name_A2", "name_B1", "name_B2"), c("Player 1", "Player 2", "Player 1", "Player 2"), 
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

