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
scoreCheck <- function(team, players) {
  # Identify which team scored
  team_scored = paste("ok", team, sep = "_")
  
  # Ask how many points were scored and by whom
  modalDialog(
    numericInput("score", label = "Noice, how many points?",
                 value = 1, min = 1, max = 9,
                 step = 1),
    
    selectInput("scorer", label = h3("Who scored?"), 
                choices = players),
    textOutput("skip_error_msg"),
    footer = tagList(
      modalButton("Cancel"),
      actionButton(team_scored, "OK")
    )
  )
  
}

scores_template = tibble(
  game_id = as.numeric(),
  player_id = as.character(),
  round_num = as.character(),
  points_scored = as.numeric(),
  shooting = as.logical()
)

games_template = tibble(
  game_id = as.numeric(0),
  start = as_datetime(today()),
  player_a1 = as.numeric(0),
  player_a2 = as.numeric(0),
  player_b1 = as.numeric(0),
  player_b2 = as.numeric(0)
)

players_template = tibble(
  player_name = as.character(),
  player_id = as.numeric()
)

rounds = str_c(rep(1:100, each = 2), rep(c("A", "B"), 100))
round_labels = rep(c("Pass the dice", "Next round"),100)



# TODO: Access games table for game_id
# TODO: Access players table for previous players



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
                          textInput("name_p1", label = "Player 1",placeholder = "A thrower needs a name")),
                   column(2),
                   column(5,
                          shiny::HTML("<br><br><center> <h1>Team B</h1> </center><br>"),
                          textInput("name_p3", label = "Player 3",placeholder = "A thrower needs a name"))
                 ),
                 fluidRow(
                   column(5,
                          textInput("name_p2", label = "Player 2",placeholder = "A thrower needs a name")),
                   column(2),
                   column(5,
                          textInput("name_p4", label = "Player 4",placeholder = "A thrower needs a name"))
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
    score = NULL,
    error_msg = NULL,
    print = FALSE,
    
    # Scores
    score_a = 0,
    score_b = 0,

    # Player names
    name_p1 = NULL,
    name_p2 = NULL,
    name_p3 = NULL,
    name_p4 = NULL,
    
    # Players' scores
    p1_score = 0,
    p2_score = 0,
    p3_score = 0,
    p4_score = 0,
    
    # Scores table
    scores = scores_template,
    # Games table
    games = games_template,
    # Players template
    players = players_template,
    
    shot_num = 1

  )
  
  #TODO: create list of players and their team
  players = reactiveValues(
    team_a = list(NULL, NULL),
    team_b = list(NULL, NULL)
  )
  

  

# Outputs -----------------------------------------------------------------

  # Switch between pass the dice and next round
  output$selector_ui <- renderUI({
    actionButton("next_round", label = round_labels[vals$shot_num])
  })
  
  # Increment round number
  round_num = reactive({
    rounds[vals$shot_num]
  })
  output$round_num = renderText({
    round_num()
  })
  
  # Output Team A's score
  output$score_a = renderText({
    vals$score_a
  })
  
  # Output Team B's score
  output$score_b = renderText({
    vals$score_b
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
    
    updateTabsetPanel(session, "switcher", selected = "scoreboard")
    
    vals$score_a = 0
    vals$score_b = 0
    vals$p1_score = 0
    vals$p2_score = 0
    vals$p3_score = 0
    vals$p4_score = 0
    
    
    vals$name_p1 = input$name_p1
    vals$name_p2 = input$name_p2
    vals$name_p3 = input$name_p3
    vals$name_p4 = input$name_p4
    
    players$team_a = c(input$name_p1, input$name_p2)
    players$team_b = c(input$name_p3, input$name_p4)
    
    
  })
  
  
  
  observeEvent(input$next_round, {
    vals$shot_num = vals$shot_num+1
  })
  
  
  # When team A's score button is pushed
  observeEvent(input$a_score_button, {
    vals$error_msg <- NULL
    showModal(scoreCheck("a", players$team_a))

    
  })
  
  
  observeEvent(input$b_score_button, {
    vals$error_msg <- NULL
    showModal(scoreCheck("b", players$team_b))
  })
  
  
  # Validate submission
  observeEvent(input$ok_a, {
    vals$score <- input$score
    
    if (!is.null(vals$score)) {
      removeModal()
      vals$print <- TRUE
      vals$score_a = vals$score_a + vals$score
      vals$scores = bind_rows(vals$scores,
                              tibble(
                                player = input$scorer,
                                round_num = round_num(),
                                points_scored = input$score,
                                shooting = str_detect(round_num(), "A")
                                ))
    } else {
      vals$error_msg <- "You did not input anything."
    }
  })
  
  observeEvent(input$ok_b, {
    vals$score <- input$score
    
    if (!is.null(vals$score)) {
      removeModal()
      vals$print <- TRUE
      vals$score_b = vals$score_b + vals$score
      
      vals$scores = bind_rows(vals$scores,
                              tibble(
                                player = input$scorer,
                                round_num = round_num(),
                                points_scored = input$score,
                                shooting = str_detect(round_num(), "B")
                              ))
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
    vals$scores = scores
    vals$shot_num = 1
    walk(c("name_p1", "name_p2", "name_p3", "name_p4"), function(id) updateTextInput(session, inputId = id, value = "", placeholder = "A thrower needs a name"))
    removeModal()
  })
  

  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
