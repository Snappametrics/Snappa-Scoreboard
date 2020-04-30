#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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
  player = as.character(),
  round_num = as.numeric(),
  points_scored = as.numeric(),
  shooting = as.logical()
)

library(shiny)

# UI ----------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Switching mechanism
  tags$style("#switcher { display:none; }"),
  # Application title
  titlePanel("Snappa Scoreboard"),
  
  
  
  
  sidebarLayout(
    
    sidebarPanel(
      "So you want to ya some da?",
      actionButton("finish_game", "Finish game")
      #TODO: Tip of the day
    ),
    
    
    
    mainPanel(
      
      tabsetPanel(
        # Switching mechanism
        id = "switcher",
        
        ## Start Screen
        tabPanel("start_screen", 
                 # Enter Player Names
                 fluidRow(
                   column(5,
                          shiny::HTML("<br><br><center> <h1>Team A</h1> </center><br>"),
                          textInput("name_p1", label = "Player 1")),
                   column(2),
                   column(5,
                          shiny::HTML("<br><br><center> <h1>Team B</h1> </center><br>"),
                          textInput("name_p3", label = "Player 3"))
                 ),
                 fluidRow(
                   column(5,
                          textInput("name_p2", label = "Player 2")),
                   column(2),
                   column(5,
                          textInput("name_p4", label = "Player 4"))
                 ),
                 # Start Game
                 fluidRow(
                   column(5),
                   column(2,
                          actionButton("start_game", "Throw some dice?")),
                   column(5)
                 )
        ),
        
        ## Scoreboard
        tabPanel("scoreboard", 
                 
                 # Scoreboard
                 fluidRow(
                   # Team A
                   column(width = 4,
                          h1("Team A"),
                          h3(textOutput("score_a")),
                          actionButton("a_score_button", label = "We scored!")
                   ),
                   
                   # Round
                   column(width = 4,
                          h1("Round"),
                          h3(textOutput("round_num")),
                          actionButton("next_round", label = "Next Round")
                   ),
                   # Team B
                   column(width = 4,
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





# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Create object to store reactive values
  vals <- reactiveValues(
    score = NULL,
    error_msg = NULL,
    print = FALSE,
    score_a = 0,
    score_b = 0,
    name_p1 = NULL,
    name_p2 = NULL,
    name_p3 = NULL,
    name_p4 = NULL,
    p1_score = 0,
    p2_score = 0,
    p3_score = 0,
    p4_score = 0,
    scores = scores_template
  )
  
  #TODO: create list of players and their team
  players = reactiveValues(
    team_a = list(NULL, NULL),
    team_b = list(NULL, NULL)
  )
  
  round_num = reactiveVal({
    1
  })
  

# Outputs -----------------------------------------------------------------

  
  # Increment round number
  output$round_num = renderText({
    as.character(round_num())
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
  
  

# Events ------------------------------------------------------------------

  
  # When we click "Start Game", switch to the scoreboard
  observeEvent(input$start_game, {
    updateTabsetPanel(session, "switcher", selected = "scoreboard")
    
    vals$name_p1 = input$name_p1
    vals$name_p2 = input$name_p2
    vals$name_p3 = input$name_p3
    vals$name_p4 = input$name_p4
    
    players$team_a = c(input$name_p1, input$name_p2)
    players$team_b = c(input$name_p3, input$name_p4)
  })
  
  observeEvent(input$next_round, {
    next_round = round_num() + 1
    round_num(next_round)
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
                                shooting = F
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
                                shooting = F
                              ))
    } else {
      vals$error_msg <- "You did not input anything."
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(vals$scores, con)
    }
  )
  
  
  
  observeEvent(input$finish_game, {
    
    showModal(
      modalDialog(title = "Save that data!",
                downloadButton("downloadData", "Download"))
    )
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
