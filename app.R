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

rounds = str_c(rep(1:100, each = 2), rep(c("A", "B"), 100))
round_labels = rep(c("Pass the dice", "Next round"),100)



# Database connection -----------------------------------------------------


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



# Functions ---------------------------------------------------------------



# Create pop-up dialog box when someone scores
score_check <- function(team, players) {
  # Identify which team scored
  team_scored = paste("ok", team, sep = "_")
  
  # Ask how many points were scored and by whom
  modalDialog(align = "center", 
              h2(str_c("Team ", str_to_upper(team), " Scored")),
              numericInput("score", label = "Points",
                           value = 1, min = 1, max = 9,
                           step = 1, width = "10%"),
              # Who Scored?
              # awesomeRadio(
              #   inputId = "scorer", label = "Who scored?", 
              #   choices = players, inline=T),
              prettyCheckbox(
                inputId = "paddle", label = "Was it a paddle?", icon = icon("check")
              ),
              radioGroupButtons(
                inputId = "scorer",
                label = "Who scored?",
                choices = players,
                individual = T,
                checkIcon = list(
                  yes = tags$i(class = "fa fa-check-square"),
                  no = tags$i(class = "fa fa-square-o")
                )
              ),

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
      (a >= points_to_win & a - b >= 2 & str_detect(round, "[Bb]")) ~ T, 
      (b >= points_to_win & b - a >= 2 & str_detect(round, "[Aa]")) ~ T,
      !any((a >= points_to_win & a - b >= 2 & str_detect(round, "[Bb]")), 
           (b >= points_to_win & b - a >= 2 & str_detect(round, "[Aa]"))) ~ F)
  }
  
  return(check)
}

extra_player_ui = function(player){
  
  player_team = str_extract(player, "[A-z]")
  player_num = as.numeric(str_extract(player, "[0-9]"))
  div_id = paste0("add_remove_", player)
  
  
  tags$div(id = div_id,
           # Add extra player text input (inside fluid row)
           fluidRow(
             tagList(
               selectizeInput(inputId = paste0("name_", player), 
                              label = paste('Player', player_num), c(`Player Name`='', pull(players_tbl, player_name)), options = list(create = TRUE), width = "46%")
             ),
             actionBttn(
               inputId = paste0("remove_", player),  label = "X", style = "jelly", color = "danger", size = "sm"),
             tags$style(paste0("#add_remove_", player, " {position: relative;} #remove_", player, " {position: relative; top:-8ch; left:10ch; z-index:1;}")),
           ),
           if(player_num < 4){
             actionButton(paste0("extra_player_", player_team, player_num+1), 
                          label = "+ Add Player")
           } else{
             invisible()
           }
           
  )
}






# UI ----------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- fluidPage(
  # includeCSS("www/bootstrap.css"),
  # includeCSS("www/styles.css"),
  useShinyjs(),
  tags$style("#start_game, #previous_round, #next_round, #a_score_button, #b_score_button, #scorer, #paddle, #score { font-size: 130% }"),
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
                          wellPanel(
                            style = "opacity: 0.92; background: #e26a6a",
                          h1(strong("Team A"), style = "align: center; color: white; font-size: 700%; margin-top:30px;margin-bottom:30px;"),
                          selectizeInput('name_a1', 'Player 1', c(`Player Name`='', pull(players_tbl, player_name)),  options = list(create = TRUE), width = "50%"),
                          selectizeInput('name_a2', 'Player 2', c(`Player Name`='', pull(players_tbl, player_name)), options = list(create = TRUE), width = "50%"),
                          actionButton("extra_player_a3", label = "+ Add Player"), 
                          tags$style(type = "text/css", "#name_a1, #name_a2, #name_a3, #name_a4 {font-size: 120px; color: white; margin-top:30px;margin-bottom:30px;}")#
                          )

                   ),
                   column(4),
                   column(4, align = "center",
                          wellPanel(
                            style = "opacity: 0.92; background: #2574a9",
                          h1(strong("Team B"), style = "align: center; color: white; font-size: 700%; margin-top:30px;margin-bottom:30px;"),
                          selectizeInput('name_b1', 'Player 1', c(`Player Name`='', pull(players_tbl, player_name)), options = list(create = TRUE), width = "50%"),
                          selectizeInput('name_b2', 'Player 2', c(`Player Name`='', pull(players_tbl, player_name)), options = list(create = TRUE), width = "50%"),
                          actionButton("extra_player_b3", label = "+ Add Player"),
                          tags$style(type = "text/css", "#name_b1, #name_b2, #name_b3, #name_b4 {font-size: 120px; color: white; margin-top:30px;margin-bottom:30px; } label.control-label[for^='name_'] {color: white;}}")#  
                          )
                   )
                   ),#
                 
                 
                
                 fluidRow(
                   column(4),
                   column(4,  align = "center",
                          disabled(actionButton("start_game", "Throw some dice?")),
                          uiOutput("validate_start"),
                          br(),
                          
                          radioButtons("play_to", "What score are you playing to?", 
                                               choices = list("21" = 1, "32" = 2), selected = 1,
                                               inline = T),
                          br(),
                          helpText("Note: All players must enter their name before the game can begin")
                          ),
                   column(4)
                   ),
                 
                 # Try to make Icons work
                 
                fluidRow(
                  column(3, 
                         actionButton("one_point", label = "Off the Table", 
                                      style = 
                                        'img {
                                        border: 1px solid #ddd;
                                        border-radius: 50%;
                                        
                                        width: 50%;
                                        height: 100px;
                                        margin-left: 0px;
                                        margin-right: 0px;
                                        
                                      }'
                         ))
                )
        ),
                
        

# Scoreboard --------------------------------------------------------------


        tabPanel("scoreboard", 
                 
                 # Scoreboard
                 fluidRow(
                   # Team A
                   column(width = 4, align = "center",
                          wellPanel(
                            style = "opacity: 0.92; background: #e26a6a",
                            h1("Team A", style = "color: white;"),
                            h2(textOutput("score_a")),
                            actionButton("a_score_button", label = "We scored!"),
                            h3(textOutput("player_names_a"))
                            # tags$style(type="text/css", "h1, h3, #a_score_button { height: 50px; width: 100%; text-align:center; font-size: 20px; display: block;}")
                          )
                          
                   ),
                   
                   # Round
                   column(width = 4, align = "center",
                          h1("Round"),
                          h3(textOutput("round_num"), style = "font-size:120px;"),
                          uiOutput("selector_ui")
                   ),
                   # Team B
                   column(width = 4,  align = "center",
                          wellPanel(
                            style = "opacity: 0.92; background: #2574a9",
                            h1("Team B", style = "color: white;"),
                            h2(textOutput("score_b")),
                            actionButton("b_score_button", label = "We scored!"),
                            h3(textOutput("player_names_b")), 
                            tags$style(type = "text/css", "#score_a, #score_b {font-size: 120px; color: white;}")
                          )
                          
                   )
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
  
  #Change the html of an icon
  html(id = "one_point", 
       html = '<img src="off_the_table.png" alt="off_table" class = "center">'
  )
  
  
    

# Reactive Values ---------------------------------------------------------

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
      left_join(vals$players_db, by = "player_name")
  })

  # Length of active player inputs
  num_players = reactive({
    length(active_player_inputs()[active_player_inputs() != ""])
  })



  
  # Create object to store reactive values
  vals <- reactiveValues(
    # Initialize new game, player, and score IDs, as well as the shot number
    game_id = NULL,
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
    want_b4 = F
  )
  
  # Increment round number
  round_num = reactive({
    rounds[vals$shot_num]
  })
  
  

  

# Outputs -----------------------------------------------------------------
  
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

  
  # When we click "Start Game", 
  #   - Add new players to the players table
  #   - switch to the scoreboard
  #   - Set the score outputs and shot number to 0
  #   - Record the score we're playing to
  #   - Initialize the current game's game_stats table
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
    vals$game_id = sum(dbGetQuery(con, "SELECT MAX(game_id) FROM game_stats"),1 , na.rm = T)
    
    # Record the score we're playing to
    vals$score_to = case_when(input$play_to == 1 ~ 21,
                              input$play_to == 2 ~ 32)
    
    
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
                                    round_num(), vals$score_to)
      
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
    
    # Insert extra player UI
    insertUI(
      selector = "#extra_player_a3",
      where = "afterEnd",
      ui = extra_player_ui("a3")
    )
      
    # Remove add player button       
    removeUI(
      selector = vals,
      multiple = F
    )
  })
  
  # Remove A3
  #   - Insert add new player action button
  #   - Remove A3 player name input
  observeEvent(input$remove_a3, {
    insertUI(selector = "#add_remove_a3",
             where = "afterEnd",
             ui = actionButton("extra_player_a3", label = "+ Add Player")
    )
  
    
    removeUI(selector = "#add_remove_a3",
             multiple = F)
    updateSelectizeInput(session, "name_a3", choices = c(`Player Name`='', pull(players_tbl, player_name)), selected = character(0))
    updateSelectizeInput(session, "name_a4", choices = c(`Player Name`='', pull(players_tbl, player_name)), selected = character(0))
    
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
    
    # Insert extra player UI
    insertUI(
      selector = "#extra_player_a4",
      where = "afterEnd",
      ui = extra_player_ui("a4")
    )
    
    # Remove add player button
    removeUI(
      selector = vals,
      multiple = T
    )
  })
  
  # Remove A4
  #   - Insert add new player action button
  #   - Remove A4 player name input
  observeEvent(input$remove_a4, {
    # Insert add player button
    insertUI(selector = "#add_remove_a4",
             where = "afterEnd",
             ui = actionButton("extra_player_a4", label = "+ Add Player")
    )
    # Remove player text input
    removeUI(selector = "#add_remove_a4",
             multiple = F)

    # Tells later checks to not worry about this
    # empty slot in active_player_names
    vals$want_a4 = F
    updateSelectizeInput(session, "name_a4", choices = c(`Player Name`='', pull(players_tbl, player_name)), selected = character(0))
    
  })  
  
  
  # New Player B3
  #   - Add B3 text input
  #   - Remove the add new player action button
  observeEvent(input$extra_player_b3, {
    
    # Set want check to true
    vals$want_b3 = T
    
    # Get inputs for add player button
    vals <- paste0("#",getInputs("extra_player_b3"))
    
    # Insert extra player UI
    insertUI(
      selector = "#extra_player_b3",
      where = "afterEnd",
      ui = extra_player_ui("b3")
    )
    
    # Remove add player button
    removeUI(
      selector = vals,
      multiple = F
    )
  })

  # Remove B3
  #   - Insert add new player action button
  #   - Remove B3 player name input
  observeEvent(input$remove_b3, {
    insertUI(selector = "#add_remove_b3",
             where = "afterEnd",
             ui = actionButton("extra_player_b3", label = "+ Add Player")
    )
    
    
    removeUI(selector = "#add_remove_b3",
             multiple = F)
    updateSelectizeInput(session, "name_b3", choices = c(`Player Name`='', pull(players_tbl, player_name)), selected = character(0))
    updateSelectizeInput(session, "name_b4", choices = c(`Player Name`='', pull(players_tbl, player_name)), selected = character(0))
    
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
    
    # Insert playertext input
    insertUI(
      selector = "#extra_player_b4",
      where = "afterEnd",
      ui = extra_player_ui("b4")
    )
    
    # Remove add player button
    removeUI(
      selector = vals,
      multiple = T
    )
  })
  

  # Remove B4
  #   - Insert add new player action button
  #   - Remove B4 player name input
  observeEvent(input$remove_b4, {
    insertUI(selector = "#add_remove_b4",
             where = "afterEnd",
             ui = actionButton("extra_player_b4", label = "+ Add Player")
    )
    removeUI(selector = "#add_remove_b4",
             multiple = F)
    shinyjs::reset("#name_b4")
    # Tells later checks to not worry about this
    # empty slot in active_player_names
    vals$want_b4 = F
    updateSelectizeInput(session, "name_b4", choices = c(`Player Name`='', pull(players_tbl, player_name)), selected = character(0))
    
  })  
    

  
  
  
    
  

# Scoring -----------------------------------------------------------------


# Team A ------------------------------------------------------------------

  
  observeEvent(input$a_score_button, {
    vals$error_msg <- NULL
    showModal(
      score_check(team = "a", 
                  players = arrange(snappaneers(), team) %>% pull(player_name)))
  })
  
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
          str_detect(rounds[vals$shot_num], "[Aa]") & 
            str_detect(pull(filter(snappaneers(), player_name == input$scorer), team), "[Aa]"),
          # Typical Paddle
          str_detect(rounds[vals$shot_num], "[Bb]") &
            str_detect(pull(filter(snappaneers(), player_name == input$scorer), team), "[Aa]") & 
            input$paddle == T,
          # Somebody messed up on the other team
          str_detect(pull(filter(snappaneers(), player_name == input$scorer), team), "[Bb]") & 
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
                                   shooting = str_detect(round_num(), "[Aa]")
                                 ))
      
      # Update game stats table
      vals$game_stats_db = vals$scores_db %>% 
        left_join(snappaneers(), by = "player_id") %>% 
        mutate(shots = case_when(str_detect(team, "a") ~ ceiling(vals$shot_num/2),
                                 str_detect(team, "b") ~ floor(vals$shot_num/2))) %>% 
        group_by(game_id, player_id, team, shots) %>% 
        summarise(total_points = sum(points_scored),
                  ones = sum((points_scored == 1)),
                  twos = sum((points_scored == 2)),
                  threes = sum((points_scored == 3)),
                  impossibles = sum((points_scored > 3)),
                  paddle_points = sum(points_scored*paddle),
                  points_per_round = total_points / last(shots),
                  off_ppr = sum(points_scored*!paddle)/ last(shots),
                  def_ppr = paddle_points/last(shots),
                  toss_efficiency = sum(!paddle)/last(shots)) %>% 
        ungroup()
      
      # Congratulate paddlers
      if(input$paddle & str_detect(pull(filter(snappaneers(), player_name == input$scorer), team), "[Aa]") ){
        showNotification("That's some hot shit!")
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
    #    } else {
    
    #    }
    
  })
  
  # Team B ---------------------------------------------------------
  
  
  observeEvent(input$b_score_button, {
    vals$error_msg <- NULL
    showModal(
      score_check(
        team = "b", 
        players = arrange(snappaneers(), desc(team)) %>% pull(player_name)))
    
  })
  
  # Score validation
  observeEvent(input$ok_b, {
    validate(
      need(input$score < 8, label = "C'mon, you did not score that many points")
    )
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
          str_detect(pull(filter(snappaneers(), player_name == input$scorer), team), "[Aa]") & 
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
                                shooting = str_detect(round_num(), "[Bb]")
                              ))
      
      # Update game stats
      vals$game_stats_db = vals$scores_db %>% 
        left_join(snappaneers(), by = "player_id") %>% 
        mutate(shots = case_when(str_detect(team, "a") ~ ceiling(vals$shot_num/2),
                                 str_detect(team, "b") ~ floor(vals$shot_num/2))) %>% 
        group_by(game_id, player_id, team, shots) %>% 
        summarise(total_points = sum(points_scored),
                  ones = sum((points_scored == 1)),
                  twos = sum((points_scored == 2)),
                  threes = sum((points_scored == 3)),
                  impossibles = sum((points_scored > 3)),
                  paddle_points = sum(points_scored*paddle),
                  points_per_round = total_points / last(shots),
                  off_ppr = sum(points_scored*!paddle)/ last(shots),
                  def_ppr = paddle_points/last(shots),
                  toss_efficiency = sum(!paddle)/last(shots)) %>% 
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
    
    # Sanity check: Only submit games which have reached their conclusion
    validate(need(any(vals$current_scores$team_a >= vals$score_to,
                      vals$current_scores$team_b >= vals$score_to), 
                  message = "Your game hasn't ended yet. Please finish the current game or restart before submitting",
                  label = "check_game_over"))
    
    
    
    #update game_stats one more time so that points per shot is accurate
    vals$game_stats_db = vals$scores_db %>% 
      left_join(snappaneers(), by = "player_id") %>% 
      mutate(shots = case_when(str_detect(team, "a") ~ ceiling(vals$shot_num/2),
                               str_detect(team, "b") ~ floor(vals$shot_num/2))) %>% 
      group_by(game_id, player_id, team, shots) %>% 
      summarise(total_points = sum(points_scored),
                ones = sum((points_scored == 1)),
                twos = sum((points_scored == 2)),
                threes = sum((points_scored == 3)),
                impossibles = sum((points_scored > 3)),
                paddle_points = sum(points_scored*paddle),
                points_per_round = total_points / last(shots),
                off_ppr = sum(points_scored*!paddle)/ last(shots),
                def_ppr = paddle_points/last(shots),
                toss_efficiency = sum(!paddle)/last(shots)) %>% 
      ungroup()
    
    
    # Make sure that everyone is in the game_stats table, i.e., 
    # record the trolls in the dungeon
    vals$trolls = tibble(
                  player_id = pull(filter(vals$players_db, player_name %in% snappaneers()$player_name), player_id)) %>%
                  anti_join(vals$game_stats_db, by = "player_id")
    
    vals$game_stats_db = bind_rows(vals$game_stats_db, 
                tibble(
                  game_id = rep(vals$game_id, times = length(vals$trolls)),
                  player_id = pull(vals$trolls, player_id), 
                  total_points = rep(0, times = length(vals$trolls)),
                  shots = rep(0, times = length(vals$trolls)),
                  ones = rep(0, times = length(vals$trolls)),
                  twos = rep(0, times = length(vals$trolls)),
                  threes = rep(0, times = length(vals$trolls)),
                  impossibles = rep(0, times = length(vals$trolls)),
                  paddle_points = rep(0, times = length(vals$trolls)),
                  points_per_round = rep(0, times = length(vals$trolls)),
                  off_ppr = rep(0, times = length(vals$trolls)),
                  def_ppr = rep(0, times = length(vals$trolls)),
                  toss_efficiency = rep(0, times = length(vals$trolls))
                  )
                )
    
    
    
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
    
    # Confirmation that data was sent to db
    sendSweetAlert(session, 
                   title = "The die is cast",
                   text = "Data sent to SnappaDB",
                   type = "success")
    
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

