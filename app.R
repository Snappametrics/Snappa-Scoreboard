#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(DBI)
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
games_tbl = tbl(con, "games") %>% collect()






# Scores doesn't need to be pulled but will be referenced later



# Updating the db at the end ----------------------------------------------
# I'd be amazed if this is the correct location for this block 
# of code. I'm betting it's not.









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
      helpText("Note: All players must enter their name before the game can begin"),
      
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
                          shiny::HTML("<br><br><center> <h1>Team A</h1> </center><br>"),
                          selectizeInput('name_p1', 'Player 1', c(`Player Name`='', pull(players_tbl, player_name)), options = list(create = TRUE))
                   ),
                   column(2),
                   column(5,
                          shiny::HTML("<br><br><center> <h1>Team B</h1> </center><br>"),
                          selectizeInput('name_p3', 'Player 3', c(`Player Name`='', pull(players_tbl, player_name)), options = list(create = TRUE))
                   )
                   ),
                 fluidRow(
                   column(5,
                          selectizeInput('name_p2', 'Player 2', c(`Player Name`='', pull(players_tbl, player_name)), options = list(create = TRUE))
                          ),
                   column(2),
                   column(5,
                          selectizeInput('name_p4', 'Player 4', c(`Player Name`='', pull(players_tbl, player_name)), options = list(create = TRUE))
                          )
                   ),
                 # Start Game
                 fluidRow(
                   column(4),
                   column(4,
                          actionButton("start_game", "Throw some dice?"),
                          tags$style(type='text/css', "#start_game { horizontal-align: middle;}")),
                   
                   column(4)
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
                            actionButton("b_score_button", label = "We scored!")
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
  
  # Create object to store reactive values
  vals <- reactiveValues(
    # game_id
    game_id = sum(dbGetQuery(con, "SELECT count(*) FROM games"),1),
    new_player_id = sum(dbGetQuery(con, "SELECT count(*) FROM players"),1),
    score_id = 0,
    shot_num = 1,
    snappaneers = c(),

    name_p1 = NULL,
    name_p2 = NULL,
    name_p3 = NULL,
    name_p4 = NULL,
    
    # Players' scores
    p1_score = 0,
    p2_score = 0,
    p3_score = 0,
    p4_score = 0,
    
    
    # Scores
    score_a = 0,
    score_b = 0,
    
    # Values used in scoring events
    score = NULL,
    error_msg = NULL,
    print = FALSE,
    
    # Scores table
    players = tbl(con, "players") %>% collect(),
    scores = tbl(con, "scores") %>% collect() %>% slice(0),
    games = tbl(con, "games") %>% collect()
    

  )
  
  #TODO: create list of players and their team
  players = reactiveValues(
    team_a = list(NULL, NULL),
    team_b = list(NULL, NULL)
  )
  
  # Increment round number
  round_num = reactive({
    rounds[vals$shot_num]
  })
  
  # Rebuttal
  # 1. When either team's score is >=21 and the other team's score is two less (or lesser)
  # rebuttal = reactive({
  #   case_when(
  #     # E
  #     any((vals$score_a >= 21 & vals$score_b <= 19), (vals$score_b >= 21 & vals$score_a <= 19))
  #   )
  # })
  

  

# Outputs -----------------------------------------------------------------

  # Switch between pass the dice and next round
  output$selector_ui <- renderUI({
    actionButton("next_round", label = round_labels[vals$shot_num])
  })
  
  
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
    req(input$name_p1, input$name_p2, input$name_p3, input$name_p4)
    
    vals$snappaneers = c(input$name_p1,    input$name_p2,  input$name_p3,  input$name_p4)
    
    
    
    iwalk(vals$snappaneers, function(die_thrower, index){
      if(!(die_thrower %in% vals$players$player_name)){
        
        vals$players = bind_rows(vals$players,
                                 tibble(
                                   player_id = bit64::as.integer64(vals$new_player_id),
                                   player_name = die_thrower))
        vals$new_player_id = vals$new_player_id+1
        
      } else {
        invisible()
      }
    })
    
    updateTabsetPanel(session, "switcher", selected = "scoreboard")
    
    vals$current_scores$team_a = 0
    vals$current_scores$team_b = 0
    vals$p1_score = 0
    vals$p2_score = 0
    vals$p3_score = 0
    vals$p4_score = 0
    
    

    players$team_a = c(input$name_p1, input$name_p2)
    players$team_b = c(input$name_p3, input$name_p4)
    
    vals$games = bind_rows(vals$games,
                           tibble(
                             game_id = bit64::as.integer64(vals$game_id),
                             start = as_datetime(today()),
                             player_a1 = 1,
                             player_a2 = 2,
                             player_b1 = 3,
                             player_b2 = 4
                           ))
    
    
  })
  
  
  
  

# Next Round --------------------------------------------------------------

  
  observeEvent(input$next_round, {
    vals$shot_num = vals$shot_num+1
  })
  
  
  

# Scoring -----------------------------------------------------------------

  
  # When team A's score button is pushed
  observeEvent(input$a_score_button, {
    vals$error_msg <- NULL
    showModal(score_check("a", vals$snappaneers))

    
  })
  
  
  observeEvent(input$b_score_button, {
    vals$error_msg <- NULL
    showModal(score_check("b", vals$snappaneers))
  })
  
  
  # Validate submission
  observeEvent(input$ok_a, {
    vals$score <- input$score
    
    if (!is.null(vals$score)) {
      removeModal()
      vals$print <- TRUE
      vals$current_scores$team_a = vals$current_scores$team_a + vals$score
      
      vals$score_id = vals$score_id+1
      vals$scores = bind_rows(vals$scores,
                              tibble(
                                score_id = vals$score_id,
                                game_id = vals$game_id,
                                player_id = pull(filter(vals$players, player_name == input$scorer), player_id),
                                player = input$scorer,
                                paddle = input$paddle,
                                round_num = round_num(),
                                points_scored = input$score,
                                shooting = str_detect(round_num(), "A")
                                ))
      if(input$paddle){
        showNotification("That's some hot shit!")
      }
    } else {
      vals$error_msg <- "You did not input anything."
    }
  })
  
  observeEvent(input$ok_b, {
    vals$score <- input$score
    
    if (!is.null(vals$score)) {
      removeModal()
      vals$print <- TRUE
      vals$current_scores$team_b = vals$current_scores$team_b + vals$score
      
      vals$score_id = vals$score_id+1
      vals$scores = bind_rows(vals$scores,
                              tibble(
                                score_id = vals$score_id,
                                game_id = vals$game_id,
                                player_id = pull(filter(vals$players, player_name == input$scorer), player_id),
                                player = input$scorer,
                                paddle = input$paddle,
                                round_num = round_num(),
                                points_scored = input$score,
                                shooting = str_detect(round_num(), "B")
                              ))
      if(input$paddle){
        showNotification("That's some hot shit!")
      }
    } else {
      vals$error_msg <- "You did not input anything."
    }
  })
  
  

# End of the game ---------------------------------------------------------

  
  
  observeEvent(input$finish_game, {
    browser()
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
      modalDialog(title = "Okay, well save that data!",
                  downloadButton("downloadData", "Download"))
    )
    
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
    walk(c("name_p1", "name_p2", "name_p3", "name_p4"), 
         function(id) updateTextInput(session, inputId = id, value = "", 
                                      placeholder = "A thrower needs a name"))
    removeModal()
  })
  
  

  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
