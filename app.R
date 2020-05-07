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

# Prior to app startup ----------------------------------------------------

# Create pop-up dialog box when someone scores
score_check <- function(team, players) {
  # Identify which team scored
  team_scored = paste("ok", team, sep = "_")
  
  # Ask how many points were scored and by whom
  modalDialog(align = "center", 
    numericInput("score", label = "Noice, how many points?",
                 value = 1, min = 1, max = 9,
                 step = 1),
    materialSwitch("paddle", label = "Was it a paddle?", status = "success", inline=T),
    
    radioButtons("scorer", 
                 label = h3("Who scored?"), 
                choices = players),
    textOutput("skip_error_msg"),
    footer = tagList(
      modalButton("Cancel"),
      actionButton(team_scored, "OK")
    )
  )
  
}

rebuttal_check <- function(a , b , round, points_to_win) {
  if (any(is.null(a),is.null(b))){
    check <- F
  } else{ 
  check <- case_when(
      (a >= points_to_win & a - b >= 2 & str_detect(round, "B")) ~ T, 
      (b >= points_to_win & b - a >= 2 & str_detect(round, "A")) ~ T,
      !any((a >= points_to_win & a - b >= 2 & str_detect(round, "B")), 
         (b >= points_to_win & b - a >= 2 & str_detect(round, "A"))) ~ F)
  }

  return(check)
}


rounds = str_c(rep(1:100, each = 2), rep(c("A", "B"), 100))
round_labels = rep(c("Pass the dice", "Next round"),100)


#### Talking to the database ####

con <- dbConnect(RPostgres::Postgres(),
                          user = "postgres",
                          password = rstudioapi::askForPassword("connection password"),
                          host = "snappabase.cvoo4ewh2y4x.us-west-1.rds.amazonaws.com",
                          port = 5432,
                          dbname = "Snappa Scoreboard"
       )

dbListTables(con)

# Pull db tables for tibble templates
players_tbl = tbl(con, "players") %>% collect()
scores_tbl = tbl(con, "scores") %>% collect()
game_stats_tbl = tbl(con, "game_stats") %>% collect()
game_history_tbl = tbl(con, "game_history") %>% collect()






# Scores doesn't need to be pulled but will be referenced later







# UI ----------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- fluidPage(
  # includeCSS("www/bootstrap.css"),
  # includeCSS("www/styles.css"),
  useShinyjs(),
  # Switching mechanism
  tags$style("#switcher { display:none; }"),
  # Application title
  titlePanel("Snappa Scoreboard"),
  
  
  
  
  tabsetPanel(
        # Switching mechanism
        id = "switcher",
        

# Start Screen ------------------------------------------------------------

        
        tabPanel("start_screen", 
                 # Enter Player Names
                 fluidRow(
                   column(4, align = "center",
                          h1(strong("Team A"), style = "align: center"),
                          selectizeInput('name_a1', 'Player 1', c(`Player Name`='', pull(players_tbl, player_name)), options = list(create = TRUE)),
                          selectizeInput('name_a2', 'Player 2', c(`Player Name`='', pull(players_tbl, player_name)), options = list(create = TRUE)),
                          actionButton("extra_player_a3", label = "+ Add Player")

                   ),
                   column(4),
                   column(4, align = "center",
                          h1(strong("Team B"), style = "align: center"),
                          selectizeInput('name_b1', 'Player 1', c(`Player Name`='', pull(players_tbl, player_name)), options = list(create = TRUE)),
                          selectizeInput('name_b2', 'Player 2', c(`Player Name`='', pull(players_tbl, player_name)), options = list(create = TRUE)),
                          actionButton("extra_player_b3", label = "+ Add Player")
                   )
                   ),
                 
                 fluidRow(
                   column(4),
                   column(4, align = "center",
                          numericInput("play_to", "What score are you playing to?", value = 21, min = 21, max = 40)),
                   column(4)
                 ),
                
                 fluidRow(
                   column(4),
                   column(4,  align = "center",
                          disabled(actionButton("start_game", "Throw some dice?")),
                          uiOutput("validate_start"),
                          br(),
                          helpText("Note: All players must enter their name before the game can begin")
                   ),
                   column(4)
                 )
        ),
        

# Scoreboard --------------------------------------------------------------


        tabPanel("scoreboard", 
                 
                 # Scoreboard
                 fluidRow(
                   # Team A
                   column(width = 4, align = "center",
                          wellPanel(
                            h1("Team A"),
                            h2(textOutput("score_a")),
                            actionButton("a_score_button", label = "We scored!"),
                            h3(textOutput("player_names_a"))
                            # tags$style(type="text/css", "h1, h3, #a_score_button { height: 50px; width: 100%; text-align:center; font-size: 20px; display: block;}")
                          )
                          
                   ),
                   
                   # Round
                   column(width = 4, align = "center",
                          h1("Round"),
                          h3(textOutput("round_num")),
                          uiOutput("selector_ui")
                          # tags$style(type="text/css", "h1, h3, #selector_ui { height: 50px; width: 100%; text-align:center; font-size: 20px; display: block;}")
                   ),
                   # Team B
                   column(width = 4,  align = "center",
                          wellPanel(
                            h1("Team B"),
                            h2(textOutput("score_b")),
                            actionButton("b_score_button", label = "We scored!"),
                            h3(textOutput("player_names_b"))
                            # tags$style(type="text/css", "h1, h3, #b_score_button { height: 50px; width: 100%; text-align:center; font-size: 20px; display: block;}")
                          )
                          
                   ), 
                   tags$style(type = "text/css", "#score_a, #score_b, #round_num {font-size: 120px;}")
                 ), 
                 fluidRow(
                   column(width = 4, offset = 4, align = "center",
                          actionButton("new_game", "Restart game"),
                          
                          actionButton("finish_game", "Finish game")
                          )
                   )
                 )

              ),
      fluidRow(
        column(2, align = "center",
               h3("players"),
               tableOutput("db_output_players")
               ),
        column(5, align = "center",
               h3("scores"),
               tableOutput("db_output_scores")
        ),
        column(5, align = "center",
               h3("game_stats"),
               tableOutput("db_output_game_stats")
        )
        
        )
  )
  
  





# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  

# Reactive Values ---------------------------------------------------------

snappaneers = reactive({
  
  active_player_inputs = list("a1" = input$name_a1, "a2" = input$name_a2, "a3" = input$name_a3, "a4" = input$name_a4, 
                              "b1" = input$name_b1, "b2" = input$name_b2, "b3" = input$name_b3, "b4" = input$name_b4) %>% 
    discard(is_null)
  
  
   tibble(
     team = str_extract(names(active_player_inputs), ".{1}"),
     player_name = active_player_inputs %>% flatten_chr()
     ) %>% 
     filter(player_name !="")
  })


num_players = reactive({
    
    nrow(snappaneers())
  })



  
  # Create object to store reactive values
  vals <- reactiveValues(
    # Initialize new game, player, and score IDs, as well as the shot number
    game_id = sum(dbGetQuery(con, "SELECT MAX(game_id) FROM game_stats"),1 , na.rm = T),
    new_player_id = sum(dbGetQuery(con, "SELECT MAX(player_id) FROM players"),1),
    score_id = 0,
    shot_num = 1,
    
    # DB Tables
    game_history_db = game_history_tbl %>% slice(0),
    game_stats_db = game_stats_tbl %>% slice(0),
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
    #TODO: add new player names and ui elements to allow up to 4v4 pa to be recorded
    
    # Record the total number of players. Useful for later times when we have to 
    # flexibly account for games with variable total players. For now, 4 is enough
    num_players = 4,
    

    # Values used in scoring events
    score = NULL,
    error_msg = NULL,
    print = FALSE
    
    

  )
  
  # Increment round number
  round_num = reactive({
    rounds[vals$shot_num]
  })
  
  # Rebuttal. Logic is complete 
  # 1. When either team's score is >=21 and the other team's score is two less (or lesser)
   # rebuttal = reactive({
   #   case_when(
   #     any((vals$score_a >= 21 & vals$score_a - vals$score_b >= 2 & str_detect(round_num, "B")), 
   #    (vals$score_b >= 21 & vals$score_b - vals$score_a >= 2 & str_detect(round_num, "A"))) ~ 1,
   #    !any((vals$score_a >= 21 & vals$score_a - vals$score_b >= 2 & str_detect(round_num, "B")), 
   #    (vals$score_b >= 21 & vals$score_b - vals$score_a >= 2 & str_detect(round_num, "A"))) ~ 0) 
   # })

  

  

# Outputs -----------------------------------------------------------------
  
  getInputs <- function(pattern){
    reactives <- names(reactiveValuesToList(input))
    reactives[grep(pattern,reactives)]
  }


  # Switch between pass the dice and next round
  output$selector_ui <- renderUI({
    fillRow(actionButton("previous_round", label = "Previous Round"),
            actionButton("next_round", label = round_labels[vals$shot_num]))
    
  })
  
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
  
  # For debugging
  
  output$db_output_players = renderTable({
    vals$players_db
  })
  output$db_output_scores = renderTable({
    vals$scores_db
  })
  output$db_output_game_stats = renderTable({
    vals$game_stats_db
  })
  output$db_output_game_history = renderTable({
    vals$game_history_db
  })
  
  
  
  
  

# Events ------------------------------------------------------------------
  
  

# Game Start --------------------------------------------------------------
  
  # Create a UI output which validates that there are four players and the names are unique
  output$validate_start = reactive({
    req(input$name_a1, input$name_a2, input$name_b1, input$name_b2)
    if(length(unique(snappaneers()$player_name)) == num_players()){ 
      shinyjs::enable("start_game")
    } 
    
    if(length(unique(snappaneers()$player_name)) != num_players()){
      shinyjs::disable("start_game")
      }
              
      

  })

  
  # When we click "Start Game", switch to the scoreboard
  observeEvent(input$start_game, {
    
    

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
    vals$shot_num = 1
    
    
    
    # Initialize the current game's game_stats table
    vals$game_stats_db = slice(vals$game_stats_db, 0)
    # Previous code, but might be useful if/when we want to pull in historical data too
    # bind_rows(vals$game_stats_db,
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

    vals$rebuttal = rebuttal_check(vals$current_scores$team_a, 
                                    vals$current_scores$team_b,
                                    round_num(), input$play_to)
      
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

  
  
  # New Player A3
  observeEvent(input$extra_player_a3, {
    vals <- paste0("#",getInputs("extra_player_a3"))

    insertUI(
      selector = "#extra_player_a3",
      where = "afterEnd",
      ui = tagList(
        selectizeInput('name_a3', 'Player 3', c(`Player Name`='', pull(players_tbl, player_name)), options = list(create = TRUE)),
        actionButton("extra_player_a4", label = "+ Add Player")
      )
    )
    removeUI(
      selector = vals,
      multiple = F
    )
  })
  
  # New Player A4
  observeEvent(input$extra_player_a4, {
    vals <- paste0("#",getInputs("extra_player_a4"))
    
    insertUI(
      selector = "#extra_player_a4",
      where = "afterEnd",
      ui = selectizeInput('name_a4', 'Player 4', c(`Player Name`='', pull(players_tbl, player_name)), options = list(create = TRUE))
    )
    
    removeUI(
      selector = vals,
      multiple = F
    )
  })
  
  # New Player B3
  observeEvent(input$extra_player_b3, {
    vals <- paste0("#",getInputs("extra_player_b3"))
    
    insertUI(
      selector = "#extra_player_b3",
      where = "afterEnd",
      ui = tagList(
        selectizeInput('name_b3', 'Player 3', c(`Player Name`='', pull(players_tbl, player_name)), options = list(create = TRUE)),
        actionButton("extra_player_b4", label = "+ Add Player")
      )
    )
    removeUI(
      selector = vals,
      multiple = F
    )
  })
  
  # New Player B4
  observeEvent(input$extra_player_b4, {
    vals <- paste0("#",getInputs("extra_player_b4"))
    
    insertUI(
      selector = "#extra_player_b4",
      where = "afterEnd",
      ui = selectizeInput('name_b4', 'Player 4', c(`Player Name`='', pull(players_tbl, player_name)), options = list(create = TRUE))
    )
    
    removeUI(
      selector = vals,
      multiple = F
    )
  })
    
  
  

# Scoring -----------------------------------------------------------------

  
  # When team A's score button is pushed
  observeEvent(input$a_score_button, {
    vals$error_msg <- NULL
    showModal(
      score_check(team = "a", 
                  players = arrange(snappaneers(), team) %>% pull(player_name)))
  })
  
  
  observeEvent(input$b_score_button, {
    vals$error_msg <- NULL
    showModal(
      score_check(
        team = "b", 
        players = arrange(snappaneers(), desc(team)) %>% pull(player_name)))
    
  })
  
  

# Validate scores ---------------------------------------------------------
  # Team A
  observeEvent(input$ok_a, {
    validate(
      need(input$score < 8, label = "C'mon, you did not score that many points")
    )
    # Check that the round/shooter combination makes sense / indicated a paddle
    validate(
      need(
        any(
          # Typical Offense
            str_detect(rounds[vals$shot_num], "A") & 
              str_detect(pull(filter(snappaneers(), player_name == input$scorer), team), "A"),
            # Typical Paddle
            str_detect(rounds[vals$shot_num], "B") &
              str_detect(pull(filter(snappaneers(), player_name == input$scorer), team), "A") & 
              input$paddle == T,
            # Somebody messed up on the other team
            str_detect(pull(filter(snappaneers(), player_name == input$scorer), team), "B") & 
              input$paddle == T),
        message = "That entry doesn't make sense for this round/shooter combination")
    )  

    # set score
    vals$score <- input$score
    
    if (!is.null(vals$score)) {
      removeModal()
      vals$print <- TRUE
      
      # Update the team score
      vals$current_scores$team_a = vals$current_scores$team_a + vals$score
      
      # Increment the score_id
      vals$score_id = vals$score_id+1
      
      # Add the score to the scores table
      vals$scores_db = bind_rows(vals$scores_db,
                              tibble(
                                score_id = vals$score_id,
                                game_id = vals$game_id,
                                player_id = pull(filter(vals$players_db, player_name == input$scorer), player_id),
                                paddle = input$paddle,
                                round_num = round_num(),
                                points_scored = input$score,
                                shooting = str_detect(round_num(), "A")
                              ))
      
      vals$game_stats_db = vals$scores_db %>% 
        group_by(game_id, player_id) %>% 
        summarise(total_points = sum(points_scored),
                  ones = sum((points_scored == 1)),
                  twos = sum((points_scored == 2)),
                  threes = sum((points_scored == 3)),
                  impossibles = sum((points_scored > 3)))
      
      # Congratulate paddlers
      if(input$paddle & str_detect(pull(filter(snappaneers(), player_name == input$scorer), team), "A") ){
        showNotification("That's some hot shit!")
      }
      if(input$paddle & str_detect(pull(filter(snappaneers(), player_name == input$scorer), team), "B") ){
        showNotification("It's a bold strategy Cotton, let's see if it pays off for them.")
      }
    } else {
      vals$error_msg <- "You did not input anything."
    }
  
    # If the game is in rebuttal, remind players
    # of the points needed to bring it back
    vals$rebuttal = rebuttal_check(vals$current_scores$team_a, 
                                   vals$current_scores$team_b,
                                   round_num(), input$play_to)
    
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
#    } else {
      
#    }
  
  })
  
    
  
  # Team B
  observeEvent(input$ok_b, {
    validate(
      need(input$score < 8, label = "C'mon, you did not score that many points")
    )
    validate(
      need(
        any(
          # Typical Offense
          str_detect(rounds[vals$shot_num], "B") & 
            str_detect(pull(filter(snappaneers(), player_name == input$scorer), team), "B"),
          # Typical Paddle
          str_detect(rounds[vals$shot_num], "A") &
            str_detect(pull(filter(snappaneers(), player_name == input$scorer), team), "B") & 
            input$paddle == T,
          # Somebody messed up on the other team (can happen on offense or defense)
          str_detect(pull(filter(snappaneers(), player_name == input$scorer), team), "A") & 
            input$paddle == T),
      message = "That entry doesn't make sense for this round/shooter combination"
      )
    )
    #Set Score
    vals$score <- input$score
    
    if (!is.null(vals$score)) {
      removeModal()
      vals$print <- TRUE
      
      vals$current_scores$team_b = vals$current_scores$team_b + vals$score
      
      # Increment the score_id
      vals$score_id = vals$score_id+1
      
      # Add the score to the scores table
      vals$scores_db = bind_rows(vals$scores_db,
                              tibble(
                                score_id = vals$score_id,
                                game_id = vals$game_id,
                                player_id = pull(filter(vals$players_db, player_name == input$scorer), player_id),
                                paddle = input$paddle,
                                round_num = round_num(),
                                points_scored = input$score,
                                shooting = str_detect(round_num(), "B")
                              ))
      
      vals$game_stats_db = vals$scores_db %>% 
        group_by(game_id, player_id) %>% 
        summarise(total_points = sum(points_scored),
                  ones = sum((points_scored == 1)),
                  twos = sum((points_scored == 2)),
                  threes = sum((points_scored == 3)),
                  impossibles = sum((points_scored > 3)))
      
      
      # Congratulate paddlers for good offense, chide those who paddled against their own team
      if(input$paddle & str_detect(pull(filter(snappaneers(), player_name == input$scorer), team), "B") ){
        showNotification("That's some hot shit!")
      }
      if(input$paddle & str_detect(pull(filter(snappaneers(), player_name == input$scorer), team), "A") ){
        showNotification("It's a bold strategy Cotton, let's see if it pays off for them.")
      }
    } else {
      vals$error_msg <- "You did not input anything."
    }
    
    # If the game is still in rebuttal in rebuttal, remind players
    # of the points needed to bring it back
    vals$rebuttal = rebuttal_check(vals$current_scores$team_a, 
                                   vals$current_scores$team_b,
                                   round_num(), input$play_to)
    
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
    #    } else {
    
    #    }    
  })
  
  
  

# End of the game ---------------------------------------------------------

  
  
  observeEvent(input$finish_game, {
    showModal(
      modalDialog(
        helpText(h2("End Game", align = "center"),
                 p("Is the game over?", align = "center")),
        footer = tagList(
          actionButton("finish_game_sure", "Yes"),
          modalButton("No")
        )
      )
    )

  })
  
  observeEvent(input$finish_game_sure, {
    showModal(
      modalDialog(title = "Okay, well, full send that data to the SnappaDB!",
                  
                  fluidRow(
                    column(width = 3),
                    column(width = 3,
                           downloadButton("downloadData", "Download")
                    ),
                    column(width = 3,
                           actionButton("send_to_db", "Send to the SnappaDB")
                    ),
                    column(width = 3)
                    
                  )
      )
    )
    
  })
  
  observeEvent(input$send_to_db, {
    # Update Players
    dbAppendTable(
      conn = con, 
      name = "players",
      value = anti_join(vals$players_db, players_tbl))
    
    # Update Scores
    dbAppendTable(
      conn = con, 
      name = "scores",
      value = vals$scores_db)
    
    # Update game_stats
    dbAppendTable(
      conn = con, 
      name = "game_stats",
      value = vals$game_stats_db)
  })
  

# Restart game ------------------------------------------------------------

  
  
  observeEvent(input$new_game, {
    
    showModal(
      modalDialog( title = "Restart game", 
        helpText("Are you sure?"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("new_game_sure", "Yup")
        )
      )
    )
    
  })
  
  observeEvent(input$new_game_sure, {
    # On a new game:
    updateTabsetPanel(session, "switcher", selected = "start_screen")
    
    walk2(c("name_a1", "name_a2", "name_b1", "name_b2"), c("Player 1", "Player 2", "Player 1", "Player 2"), 
         function(id, lab) updateSelectizeInput(session, inputId = id, label = lab, c(`Player Name`='', pull(players_tbl, player_name)), 
                                           options = list(create = TRUE)))
    removeModal()
  })
  


  
}
# Doesn't currently work
# onSessionEnded(dbDisconnect(conn = con))
# Run the application 
shinyApp(ui = ui, server = server)





# Notes -------------------------------------------------------------------

