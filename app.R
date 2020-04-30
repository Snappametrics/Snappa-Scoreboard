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
    p4_score = 0
  )
  
  # Create pop-up dialog box when someone scores
  scoreCheck <- function(team) {
    # Identify which team scored
    team_scored = paste("ok", team, sep = "_")
    
    # Ask how many points were scored and by whom
    modalDialog(
      numericInput("score", label = "Noice, how many points?",
                   value = 1, min = 1, max = 9,
                   step = 1),
      
      selectInput("scorer", label = h3("Who scored?"), 
                  choices = list(players()[team])),
      browser(),
      textOutput("skip_error_msg"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(team_scored, "OK")
      )
    )
  }
  
  # Show modal when button is clicked
  observeEvent(input$a_score_button, {
    vals$error_msg <- NULL
    showModal(scoreCheck("A"))
  })
  
  
  observeEvent(input$b_score_button, {
    vals$error_msg <- NULL
    showModal(scoreCheck("B"))
  })
  
  
  # Validate submission
  observeEvent(input$ok_A, {
    vals$score <- input$score
    
    if (!is.null(vals$score)) {
      removeModal()
      vals$print <- TRUE
      vals$score_a = vals$score_a + vals$score
    } else {
      vals$error_msg <- "You did not input anything."
    }
  })
  
  observeEvent(input$ok_B, {
    vals$score <- input$score
    
    if (!is.null(vals$score)) {
      removeModal()
      vals$print <- TRUE
      vals$score_b = vals$score_b + vals$score
    } else {
      vals$error_msg <- "You did not input anything."
    }
  })
  
  # Output error message
  output$skip_error_msg <- renderText({
    vals$error_msg
  })
  
  # Output inputted text
  output$print <- renderPrint({
    if (vals$print) {
      vals$score
    } else {
      NULL
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
