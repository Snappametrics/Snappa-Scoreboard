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


# Prior to app startup ----------------------------------------------------

# Create pop-up dialog box when someone scores
score_check <- function(team, players) {
  # Identify which team scored
  team_scored = paste("ok", team, sep = "_")
  
  # Ask how many points were scored and by whom
  modalDialog(
    numericInput("score", label = "Noice, how many points?",
                 value = 1, min = 1, max = 9,
                 step = 1),
    checkboxInput("paddle", "Was it a paddle?"),
    
    selectInput("scorer", label = h3("Who scored?"), 
                choices = players),
    textOutput("skip_error_msg"),
    footer = tagList(
      modalButton("Cancel"),
      actionButton(team_scored, "OK")
    )
  )
  
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
game_history_tabl = tbl(con, "game_history") %>% collect()






# Scores doesn't need to be pulled but will be referenced later







# UI ----------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Switching mechanism
  tags$style("#switcher { display:none; }"),
  # Application title
  titlePanel("Snappa Scoreboard"),
  
  
  
  
  sidebarLayout(

    # Sidebar ----
    sidebarPanel(
      "So you want to ya some da?",
      
      actionButton("new_game", "New game"),
      
      
      actionButton("finish_game", "Finish game")
      #TODO: Tip of the day
    ),
    
    
    
    mainPanel(
      
      tabsetPanel(
        # Switching mechanism
        id = "switcher",
        

# Start Screen ------------------------------------------------------------

        
        tabPanel("start_screen", 
                 # Enter Player Names
                 fluidRow(
                   column(5,
                          h1(strong("Team A"), style = "align: center"),
                          selectizeInput('name_a1', 'Player 1', c(`Player Name`='', pull(players_tbl, player_name)), options = list(create = TRUE))
                   ),
                   column(2),
                   column(5,
                          h1(strong("Team B"), style = "align: center"),
                          selectizeInput('name_b1', 'Player 1', c(`Player Name`='', pull(players_tbl, player_name)), options = list(create = TRUE))
                   )
                   ),
                 fluidRow(
                   column(5,
                          selectizeInput('name_a2', 'Player 2', c(`Player Name`='', pull(players_tbl, player_name)), options = list(create = TRUE))
                          ),
                   column(2),
                   column(5,
                          selectizeInput('name_b2', 'Player 2', c(`Player Name`='', pull(players_tbl, player_name)), 
                                         options = list(create = TRUE))
                          )
                   ),
                 # Start Game
                 fluidRow(
                   column(4),
                   column(4,
                          actionButton("start_game", "Throw some dice?"),
                          uiOutput("validate_start"),
                          tags$style(type='text/css', "#start_game { horizontal-align: middle; display: block;}")),
                   
                   column(4)
                 ),
                 fluidRow(
                   helpText("Note: All players must enter their name before the game can begin", 
                            style = "text-align:center; display: block")
                   )
        ),
        
        ## Scoreboard
        tabPanel("scoreboard", 
                 
                 # Scoreboard
                 fluidRow(
                   # Team A
                   column(width = 4,
                          wellPanel(
                            h1("Team A"),
                            h3(textOutput("score_a")),
                            actionButton("a_score_button", label = "We scored!"),
                            tags$style(type="text/css", "h1, h3, #a_score_button { height: 50px; width: 100%; text-align:center; font-size: 20px; display: block;}")
                          )
                          
                   ),
                   
                   # Round
                   column(width = 4,
                          h1("Round"),
                          h3(textOutput("round_num")),
                          uiOutput("selector_ui"),
                          tags$style(type="text/css", "h1, h3, #selector_ui { height: 50px; width: 100%; text-align:center; font-size: 20px; display: block;}")
                   ),
                   # Team B
                   column(width = 4, 
                          wellPanel(
                            h1("Team B"),
                            h3(textOutput("score_b")),
                            actionButton("b_score_button", label = "We scored!"),
                            tags$style(type="text/css", "h1, h3, #b_score_button { height: 50px; width: 100%; text-align:center; font-size: 20px; display: block;}")
                          )
                          
                   )
                 )
        )
      )
    )
  )
  
  
  
)





# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  

# Reactive Values ---------------------------------------------------------

  
  # Create object to store reactive values
  vals <- reactiveValues(
    # Initialize new game, player, and score IDs, as well as the shot number
    game_id = bit64::as.integer64(sum(dbGetQuery(con, "SELECT MAX(game_id) FROM game_stats"),1)),
    new_player_id = sum(dbGetQuery(con, "SELECT count(*) FROM players"),1),
    score_id = 0,
    shot_num = 1,
    
    # DB Tables
    game_history_db = game_history_tbl,
    game_stats_db = game_stats_tbl %>% slice(0),
    players_db = players_tbl,
    scores_db = scores_tbl %>% slice(0),

    # dataframe of the players and their teams
    snappaneers = tibble(
      team = as.character(),
      player_name = as.character()
      ),
    # Current Scores
    current_scores = tibble(
      team_a = 0,
      team_b = 0
    ),
    
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
   rebuttal = reactive({
     case_when(
       any((vals$score_a >= 21 & vals$score_a - vals$score_b >= 2 & str_detect(round_num, "B")), 
      (vals$score_b >= 21 & vals$score_b - vals$score_a >= 2 & str_detect(round_num, "A"))) ~ 1,
      !any((vals$score_a >= 21 & vals$score_a - vals$score_b >= 2 & str_detect(round_num, "B")), 
      (vals$score_b >= 21 & vals$score_b - vals$score_a >= 2 & str_detect(round_num, "A"))) ~ 0) 
   })

  

  

# Outputs -----------------------------------------------------------------

  # Switch between pass the dice and next round
  output$selector_ui <- renderUI({
    actionButton("next_round", label = round_labels[vals$shot_num])
  })
  
  # Output the round number
  output$round_num = renderText({
    round_num()
  })
  
  # Output Team A's score
  output$score_a = renderText({
    vals$current_scores$team_a
  })
  
  # Output Team B's score
  output$score_b = renderText({
    vals$current_scores$team_b
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
      write.csv(vals$scores, con)
    }
  )
  
  
  
  
  

# Events ------------------------------------------------------------------
  
  

# Game Start --------------------------------------------------------------

  
  # When we click "Start Game", switch to the scoreboard
  observeEvent(input$start_game, {
    req(input$name_a1, input$name_a2, input$name_b1, input$name_b2)

    # Fill the snappaneers with the current players and their teams
    vals$snappaneers = bind_rows(vals$snappaneers,
      tibble(
        team = rep(c("A", "B"), each = 2),
        player_name = c(input$name_a1, input$name_a2,  input$name_b1,  input$name_b2)
      )
    )
    
    # Create a UI output which validates that there are four players and the names are unique
    output$validate_start = renderUI({
      if(all(map_lgl(vals$snappaneers$player_name, str_detect, "[A-z]."))){
        validate(
          need(length(unique(vals$snappaneers$player_name)) == 4, message = "Player names need to be unique")
        )
      }
    })
    
    # Add new players to the players table
    iwalk(vals$snappaneers$player_name, function(die_thrower, index){
      # If the player is not in the players table
      if(!(die_thrower %in% vals$players$player_name)){
        
        # Add a row to the players table with the new player's name and new ID
        vals$players = bind_rows(vals$players,
                                 tibble(
                                   player_id = bit64::as.integer64(vals$new_player_id),
                                   player_name = die_thrower))
        
        # Increment the ID for the next new player
        vals$new_player_id = vals$new_player_id+1
        
      } else {
        invisible()
      }
    })
    
    # Switch to the scoreboard
    updateTabsetPanel(session, "switcher", selected = "scoreboard")
    
    # Set the score outputs to 0
    vals$current_scores$team_a = 0
    vals$current_scores$team_b = 0
    
    
    # Initialize the current game's game_stats table
    vals$game_stats = bind_rows(vals$game_stats,
                                tibble(
                                  game_id = rep(vals$game_id, vals$num_players),
                                  player_id = filter(vals$players, player_name %in% vals$snappaneers$player_name) %>% pull(player_id),
                                  total_points = rep(0, vals$num_players),
                                  ones = rep(0, vals$num_players),
                                  twos = rep(0, vals$num_players),
                                  threes = rep(0, vals$num_players),
                                  impossibles = rep(0, vals$num_players)
                                ))


  })
  
  
  
  

# Next Round --------------------------------------------------------------

  # When next round button is pushed
  observeEvent(input$next_round, {
    vals$shot_num = vals$shot_num+1
  })
  
  
  

# Scoring -----------------------------------------------------------------

  
  # When team A's score button is pushed
  observeEvent(input$a_score_button, {
    vals$error_msg <- NULL
    showModal(
      score_check(team = "a", 
                  players = arrange(vals$snappaneers, team) %>% pull(player_name)))
  })
  
  
  observeEvent(input$b_score_button, {
    vals$error_msg <- NULL
    showModal(
      score_check(
        team = "b", 
        players = arrange(vals$snappaneers, desc(team)) %>% pull(player_name)))
    
  })
  
  

# Validate scores ---------------------------------------------------------
  # Team A
  observeEvent(input$ok_a, {
    validate(
      need(input$score < 8, label = "C'mon, you did not score that many points")
    )
    # Check that scoring defenders indicated a paddle
    validate(
      need(
        any(str_detect(pull(filter(vals$snappaneers, input$scorer %in% player_name), team), "A"),
            str_detect(pull(filter(vals$snappaneers, input$scorer %in% player_name), team), "B") & input$paddle == F),
        message = "Defensive players can only score by paddling") 
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
      vals$scores = bind_rows(vals$scores,
                              tibble(
                                score_id = vals$score_id,
                                game_id = vals$game_id,
                                player_id = pull(filter(vals$players, player_name == input$scorer), player_id),
                                paddle = input$paddle,
                                round_num = round_num(),
                                points_scored = input$score,
                                shooting = str_detect(round_num(), "A")
                                ))
      # Congratulate paddlers
      if(input$paddle){
        showNotification("That's some hot shit!")
      }
    } else {
      vals$error_msg <- "You did not input anything."
    }
  })
  
  # Team B
  observeEvent(input$ok_b, {
    validate(
      need(input$score < 8, label = "C'mon, you did not score that many points")
    )
    validate(
      need(
        any(str_detect(pull(filter(vals$snappaneers, input$scorer %in% player_name), team), "B"),
            str_detect(pull(filter(vals$snappaneers, input$scorer %in% player_name), team), "A") & input$paddle == F),
        message = "Defensive players can only score by paddling") 
    )
    vals$score <- input$score
    
    if (!is.null(vals$score)) {
      removeModal()
      vals$print <- TRUE
      
      vals$current_scores$team_b = vals$current_scores$team_b + vals$score
      
      # Increment the score_id
      vals$score_id = vals$score_id+1
      
      # Add the score to the scores table
      vals$scores = bind_rows(vals$scores,
                              tibble(
                                score_id = vals$score_id,
                                game_id = vals$game_id,
                                player_id = pull(filter(vals$players, player_name == input$scorer), player_id),
                                paddle = input$paddle,
                                round_num = round_num(),
                                points_scored = input$score,
                                shooting = str_detect(round_num(), "B")
                              ))
      # Congratulate paddlers
      if(input$paddle){
        showNotification("That's some hot shit!")
      }
    } else {
      vals$error_msg <- "You did not input anything."
    }
  })
  
    
  

# End of the game ---------------------------------------------------------

  
  
  observeEvent(input$finish_game, {
    showModal(
      modalDialog(
        helpText("Are you sure?"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("finish_game_sure", "OK")
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
    browser()
    # Update Players
    dbAppendTable(
      conn = con, 
      name = "players",
      value = anti_join(players_tbl, vals$players_db))
    
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
      modalDialog(
        helpText("Are you sure?"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("new_game_sure", "OK")
        )
      )
    )
    
  })
  
  observeEvent(input$new_game_sure, {
    
    updateTabsetPanel(session, "switcher", selected = "start_screen")
    vals$scores = slice(scores_tbl, 0)
    vals$shot_num = 1
    walk2(c("name_a1", "name_a2", "name_b1", "name_b2"), c("Player 1", "Player 2", "Player 1", "Player 2"), 
         function(id, lab) updateSelectizeInput(session, inputId = id, label = lab, c(`Player Name`='', pull(players_tbl, player_name)), 
                                           options = list(create = TRUE)))
    removeModal()
  })
  
  observeEvent(input$send_to_db, {
    # Append any new players
    new_players = vals$players %>% 
      anti_join(tbl(con, "players") %>% collect(), by = "player_id")
    
    dbAppendTable(con, "players", new_players)
    # Append scores
    dbAppendTable(con, "scores", vals$scores)
    # Append game
    dbAppendTable(con, "game_stats", vals$game_stats)
  })
  

  
}
# Doesn't currently work
# onSessionEnded(dbDisconnect(conn = con))
# Run the application 
shinyApp(ui = ui, server = server)
