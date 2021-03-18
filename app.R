#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



library(extrafont)
library(DBI)
library(RPostgres)
library(tidyverse)
library(lubridate)
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(gt)
library(reactable)
library(ggrepel)
library(ggtext)
library(patchwork)
library(waiter)
library(htmltools)


source("database/db_connect.R")
source("ui_functions.R")
source("server_functions.R")
source("markov/Markov_model_functions.R")


# Prior to app startup ----------------------------------------------------

# Round numbers and labels
rounds = str_c(rep(1:100, each = 2), rep(c("A", "B"), 100))
round_labels = rep(c("Pass the dice", "Next round"),100)

casualty_rules = tribble(~team_A, ~team_B, ~casualty_title, ~casualty_text,
                         12, 7, "12-7", "Roll off to see who is taking the kamikaze to the face",
                         7, 12, "12-7", "Roll off to see who is taking the kamikaze to the face",
                         18, 12, "War of 1812", "Everyone roll a die, the lowest roll takes a shot.",
                         12, 18, "War of 1812", "Everyone roll a die, the lowest roll takes a shot.",
                         20, 03, "2003", "Nevar forget: a 9/11 consists of a shot of fireball into a Sam Adams",
                         03, 20, "2003", "Nevar forget: a 9/11 consists of a shot of fireball into a Sam Adams")

# DB Tables ---------------------------------------------------------------


# Pull db tables for tibble templates
players_tbl = dbGetQuery(con, "SELECT * FROM players")
scores_tbl = dbGetQuery(con, "SELECT * FROM scores") 
player_stats_tbl = dbGetQuery(con, "SELECT * FROM player_stats")
game_stats_tbl = dbGetQuery(con, "SELECT * FROM game_stats") 

# Makea list of table templates
tbls = c("players", "scores", "player_stats", "game_stats", "score_progression", "career_stats")
tbl_templates = map(tbls, function(table){
  dbGetQuery(con, str_c("SELECT * FROM ", table, " LIMIT 0")) 
}) %>% 
  set_names(tbls)





# UI ----------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- dashboardPagePlus(
  collapse_sidebar = TRUE,
  dashboardHeaderPlus(title = tagList(
    # Corner logo
    span(class = "logo-lg", "Snappa Scoreboard"), 
    img(class = "logo-mini", src = "die_hex.png", style = "padding:.25vw;")),
    
    enable_rightsidebar = TRUE,
    rightSidebarIcon = "gears",
    
    # Left side header
    left_menu = tagList(
      dropdownBlock2(
        id = "recent_scores_dropdown",
        title = "Recent Scores",
        icon = "backward",
        badgeStatus = NULL,
        reactableOutput("recent_scores_rt"),
        disabled(actionBttn("game_summary", 
                   "Detailed Game Summary",
                   style = "material-flat",
                   color = "primary",
                   icon = icon("chart-bar"),
                   size = "sm"))
      ),
      # Right side header
    ),
    tags$li(class = "dropdown", socialButton(
        url = "https://github.com/mdewey131/Snappa-Scoreboard",
        type = "github"
    ),
    style = "padding-top:9px;"
    )
    ),
  dashboardSidebar(
    sidebarMenuOutput("sidebar_menu"),
    collapsed = TRUE
  ),
  rightsidebar = rightSidebar(
    background = "dark",
    rightSidebarTabContent(
      id = 1,
      title = "Game Options",
      icon = "desktop",
      active = TRUE,
      align = "center",
      sliderInput(
        inputId = "score_to",
        label = "What score are you playing to?",
        min = 21, max = 50, value = 21
      ),
      actionBttn("tifu", "Friendly Fire", 
                 style = "material-flat",
                 size = "sm", color = "danger"),
      actionBttn("new_game", "Restart", 
                 icon = icon("plus"), size = "sm",
                 style = "material-flat", color = "warning"),
      actionBttn("finish_game", "Finish", 
                 icon = icon("check"), size = "sm",
                 style = "material-flat", color = "warning")
    ),
    rightSidebarTabContent(
      id = 2,
      title = "Tab 2",
      textInput("caption", "Caption", "Data Summary")
    )
    ),
  dashboardBody(
    tabItems(

      # Player Input ------------------------------------------------------------

      
      tabItem(tabName = "player_input",
              fluidRow(
                team_input_ui("A", 
                              player_choices = dbGetQuery(con, "SELECT player_name FROM thirstiest_players")[,1]),
                
                # Column 2 - empty
                column(4,  align = "center",
                       pickerInput(
                         inputId = "arena_select",
                         label = "Arena",
                         selected = "Greenhaus 2: Electric Boogaloo",
                         choices = c("Greenhaus", "Ventura", "Greenhaus 2: Electric Boogaloo"),
                         options = pickerOptions(
                           mobile = T,
                           showTick = T
                         )
                       ),
                       disabled(actionBttn("start_game", 
                                           label = "Throw dice?", style = "pill", color = "primary", 
                                           icon = icon("dice"), size = "sm")),
                       uiOutput("validate_start"),
                       
                       helpText("Note: All players must enter their name before the game can begin")
                       # 
                       # awesomeRadio(inputId = "play_to", 
                       #              label = "What score are you playing to?", 
                       #              choices = list("21" = 1, "32" = 2), 
                       #              selected = 1, inline = T)
                       ),
                       
                
                # Column 3 - Team B
                team_input_ui("B", 
                              player_choices = dbGetQuery(con, "SELECT player_name FROM thirstiest_players")[,1])
              )
              ),


      # Scoreboard --------------------------------------------------------------


      
      tabItem(tabName = "scoreboard", #icon = icon("window-maximize"), 
               div(
                 fluidRow(id = "dice-row", 
                          column(4, align = "center", 
                                 uiOutput("active_die_left")),
                          column(4, align = "center",
                                 actionBttn("switch_sides", 
                                            "Switch Sides", style = "material-flat", 
                                            color = "primary", 
                                            icon = icon("refresh"), size = "sm")),
                          column(4, align = "center", 
                                 uiOutput("active_die_right"))
                          ),
                 team_scoreboard_ui(), 
                 
                 wellPanel(class = "buttons-row",
                           fluidRow(column(width = 5, align = "left",
                                           # Recent Scores
                                           dropdown(
                                             class = "recent_scores",
                                             inputId = "recent_scores_",
                                             gt_output("recent_scores"),
                                             style = "bordered",
                                             status = "primary",
                                             size = "sm", 
                                             up = T,
                                             label = "Recent Scores",
                                             icon = icon("backward"),
                                             animate = animateOptions(
                                               enter = animations$fading_entrances$fadeInUp,
                                               exit = animations$fading_exits$fadeOutDown
                                             )
                                           )
                           ),
                           column(width = 2, align = "center"
                           ),
                           column(width = 5, align = "right"
                           )
                           )
                 )
                 
               )
      ),

# Career Stats ------------------------------------------------------------

      
      
      tabItem(tabName = "career_stats",
              boxPlus(width = 12,
                    style = str_c("background:", snappa_pal[1]), align = "center",
                    div(class = "top-snappaneers",
                        div(class = "snappaneers-header",
                            div(class = "snappaneers-title", "Top Snappaneers"),
                            "The deadliest die-throwers in all the land."
                        ),
                        reactableOutput("leaderboard_rt", width = "100%"),
                        div(class = "caption",
                          p("Toss efficiency = point-scoring tosses as % total tosses"),
                          p("Players need to play at least 5 games to be eligible for achievements.")
                        )
                    )
                ),
                boxPlus(width = 12,
                    style = str_c("background:", snappa_pal[1]), align = "center",
                                 plotOutput("scoring_heatmap", height = "75vw",
                                            hover = hoverOpts(id = "heat_hover", delay = 100, delayType = c("debounce"))),
                                 uiOutput("heatmap_info")
                )

              ),

# Player Stats ------------------------------------------------------------

      
      tabItem(tabName = "player_stats",
              fluidRow(
                # Filters
                boxPlus(width = 12,
                        # Player Select
                        selectInput("player_select", label = "Player", selectize = F,
                                    choices = dbGetQuery(con, sql("SELECT DISTINCT player_name, p.player_id
                                                            FROM players AS p
                                                            INNER JOIN player_stats AS ps
                                                            ON p.player_id = ps.player_id")) %>%
                                      deframe() %>% sample())
                )
              )
              ,
              fluidRow(
                # General and Paddle Stat boxes
                uiOutput("player_stats_headers")
              )
              ,
              fluidRow(
                boxPlus(title = "Player Form",
                        collapsible = T,
                        closable = F,
                        status = "primary",
                        fluidRow(class = "last-n-games",
                                 column(width = 5,
                                        # Stat selection
                                        selectInput("stat_select", label = NULL, selectize = F,
                                                    choices = c("Total Points" = "total_points", 
                                                                "Paddle Points" = "paddle_points", 
                                                                "Toss Efficiency" = "toss_efficiency"), 
                                                    selected = "total_points")), 
                                 column(width = 1, style = "padding-right:3vw;padding-left:0",
                                        tags$span("Last", style = "font-weight:600;")
                                 ),
                                 column(width = 3,
                                        # Sample size selection
                                        selectInput("sample_select", label = NULL, selectize = F, 
                                                    choices = c(5, 10, 20, "All"), 
                                                    selected = 5)),
                                 column(width = 1, style = "padding-left:0;",
                                        tags$span(" games", style = "font-weight:600;")
                                 )
                                 
                        ),
                        plotOutput("player_form")
              ),
              # Top Teammates
              boxPlus(title = "Top Teammates",
                      collapsible = T,
                      closable = F,
                      status = "primary",
                      # gt_output("teammate_tab")
                      reactableOutput("teammate_tab_rt",
                                      width = "100%")
              )
              # Form plot
              
              ),
              fluidRow(
                boxPlus(title = textOutput("game_history_title"),
                        collapsible = T, width = 12,
                        closable = F,
                        collapsed = T,
                        status = "primary",
                        reactableOutput("player_game_stats"))
              )
              
              ),

# Edit teams --------------------------------------------------------
      tabItem(tabName = "edit_teams",
              fluidRow(
                team_edit_column("A"),

                # Column 2 - empty
                column(4,  align = "center"),
                team_edit_column("B")
              )
      ),

# Win Probability Model ---------------------------------------------
    tabItem(tabName = "markov_model_summary",
        div(id = "waiter",
                boxPlus(title = "Simulation Parameters",
                width = 12,
                collapsable = T, 
                closable = F,
                fluidRow(
                  column(width = 4, align = "left", 
                    uiOutput("simulation_score_A")
                  ),
                  column(width = 4, align = "center",
                    sliderTextInput(
                      inputId = "num_simulations",
                      label = "Number of Simulations", 
                      choices = c(1, 50, 100, 250, 500, 1000), selected = 1,
                      grid = TRUE
                    ),
                    actionBttn("simulation_go",
                               "Run the Simulations!",
                               color = 'primary',
                               style = 'pill',
                               size = "lg")     
                  ),
                  column(width = 4, align = "right",
                    uiOutput("simulation_score_B")
                  )
                )
                    
            ),
        div(class = "simulation_results",
            uiOutput('simulation_warning'),
            uiOutput("simulation_blurb"),
            plotOutput("simulation_probability_bar",
                       height = 100),
            boxPlus(title = "Team Score Shares by Game",
                    collapsible = T,
                    closable = F,
                    plotOutput("simulation_score_shares")),
            boxPlus(title = "Overlap of Total Scores",
                    collapsible = T,
                    closable = F,
                    plotOutput("simulation_overlap"))
            )
      )
    )
),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "app.css")
    )
  ),
  useShinyjs(),
  # This is supposed to go at the top tho
  use_waiter()




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
  # This is an initial value which will be overwritten when you run
  # the simulations
  w = Waiter$new(
   html = tagList(
     spin_pixel(),
     str_c("Yeeting Imaginary Dice Into The Sky"
     )
   ))
  output$sidebar_menu <- renderUI({
    
    
    if(input$start_game) {
      sidebarMenu(
        menuItem("Scoreboard", 
                 tabName = "scoreboard", 
                 icon = icon("window-maximize"), selected = T),
        menuItem("Career Stats", 
                 tabName = "career_stats", 
                 icon = icon("bar-chart")),
        menuItem("Player Stats", tabName = "player_stats",
                 icon = icon("chart-line")),
        menuItem("Edit Teams", 
                 tabName = "edit_teams",
                 icon = icon("edit")),
        menuItem("Win Probabilities", tabName = "markov_model_summary",
                 icon = icon("flask"))
      )
      
    } else {
      sidebarMenu(
        menuItem("Player Input", 
                 tabName = "player_input", 
                 icon = icon("users"), selected = T),
        menuItem("Career Stats", 
                 tabName = "career_stats", 
                 icon = icon("bar-chart")),
        menuItem("Player Stats", tabName = "player_stats",
                 icon = icon("chart-line"))
      )
      
    }
    
  })
  

  
  
    

# Reactive Values Object ---------------------------------------------------------
  
  # reactivePoll watches for changes in the value of checkFunc at the interval
  #   when it notices changes, it updates using valueFunc
  # This checkFunc should update our tables when a game is complete
  


  
  # Create object to store reactive values
  vals <- reactiveValues(
    # Initialize new game, player, and score IDs, as well as the shot number
    game_id = NULL,
    new_player_id = sum(dbGetQuery(con, "SELECT MAX(player_id) FROM players"),1),
    score_id = as.integer(0),
    shot_num = as.integer(1),
    
    # Current game Tables
    game_stats_db = tbl_templates$game_stats %>% select(1:5),
    player_stats_db = tbl_templates$player_stats,
    scores_db = tbl_templates$scores,
    
    # Live data
    # Updates when a game is completed
    db_tbls = reactivePoll(
      intervalMillis = 1000*60,
      session = session,
      checkFunc = function() {dbGetQuery(con, sql("SELECT COUNT(*) FROM game_stats where game_complete is true"))},
      valueFunc = function() {
        map(tbls,
            function(table){
              dbGetQuery(con,
                         sql(
                           str_c("SELECT * FROM ", table,
                                 if_else(table %in% c("scores", "player_stats", "game_stats"),
                                         " WHERE game_id IN (SELECT game_id FROM game_stats WHERE game_complete is true)",
                                         "")
                           )
                         )
              )
            }) %>% 
          set_names(tbls)
      }
    ),
    recent_scores = reactivePoll(
      intervalMillis = 100*30,
      session = session,
      checkFunc = function() {dbGetQuery(con, sql("SELECT COUNT(*) FROM recent_scores"))},
      valueFunc = function() {dbGetQuery(con, sql("SELECT * FROM recent_scores"))}
    ),
    
    
    # setup cooldowns as a list of false bools
    cooldowns = list(F,F,F) %>% 
      set_names(unique(casualty_rules$casualty_title)),


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

    switch_counter = 1,
    game_over = F,
    
    markov_vals = list("iterations" = 1,
                       "A_score" = 0,
                       "B_score" = 0)
  )
  
  
  

# Player Inputs, Snappaneers, Other Reactives --------------------------------------

  # A reactive for the current score that we're playing to
  score_to = reactive({
    input$score_to
  })
  
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
      left_join(vals$db_tbls()[["players"]], by = "player_name") %>% 
      # Add shot count
      add_shot_count(shot_num = vals$shot_num)
  })
  
  # Vector of players, with current players removed
  current_choices = reactive({
    dbGetQuery(con, "SELECT player_id, player_name FROM thirstiest_players") %>% 
    anti_join(., snappaneers(), by = "player_name") %>% 
      pull(player_name)
  })
  
  # Length of active player inputs
  num_players = reactive({
    length(active_player_inputs()[active_player_inputs() != ""])
  })
  
  
  
  
  

# Outputs -----------------------------------------------------------------
  


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
  

  # Output the round number
  output$round_num = renderUI({
    team_colours = list("A" = "#e26a6a", "B" = "#2574a9")
    HTML(str_c('<h3 class="numbers">', 
               str_extract(round_num(), "[0-9]+"), 
               '<span style="color:', team_colours[[str_extract(round_num(), "[AB]+")]], ';">', str_extract(round_num(), "[AB]+"), "</span>",
               "</h3>"))
  })
  
  output$round_control_buttons = renderUI({
    team_colours = list("A" = "danger", "B" = "primary")
    column(width=12, align = "center",
           div( id = 'round_control_buttons',
             actionBttn("previous_round", 
                      label = "Previous Round", style = "jelly", icon = icon("arrow-left"), color = 
                        team_colours[[str_extract(round_num(), "[AB]+")]], size = "lg"),
             actionBttn("next_round", 
                      label = "Pass the dice", style = "jelly", icon = icon("arrow-right"), 
                      color = team_colours[[str_extract(round_num(), "[AB]+")]], size = "lg")
           )
    )
  })
  
  # Die icon indicating the active team
  output$active_die_left = renderUI({
    # switch_counter is a counter for how many times switch_sides 
    # even means that B should be on the left side
    switch_is_even = (vals$switch_counter %% 2 == 0)
    
    
    if(switch_is_even){
      # If sides have been switched
      img(src = "die_hex.png", style = str_c("background: transparent;display: flex;transform: scale(1.25);position: relative;top: -1vh; display:", 
                                             if_else(str_extract(round_num(), "[AB]+") == "B", "block;", "none;")))
    } else {
      img(src = "die_hex.png", style = str_c("background: transparent;display: flex;transform: scale(1.25);position: relative;top: -1vh; display:", 
                                             if_else(str_extract(round_num(), "[AB]+") == "A", "block;", "none;")))
    }
  })
  
  output$active_die_right = renderUI({
    # switch_counter is a counter for how many times switch_sides 
    # even means that A should be on the right side
    switch_is_even = (vals$switch_counter %% 2 == 0)
    
    if(switch_is_even){
    img(src = "die_hex.png", style = str_c("background: transparent;display: flex;transform: scale(1.25);position: relative;top: -1vh; display:", 
                                           if_else(str_extract(round_num(), "[AB]+") == "A", "block;", "none;")))
      } else {
        img(src = "die_hex.png", style = str_c("background: transparent;display: flex;transform: scale(1.25);position: relative;top: -1vh; display:", 
                                               if_else(str_extract(round_num(), "[AB]+") == "B", "block;", "none;")))
      }
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
  

  output$recent_scores_rt = renderReactable({

    # Take the max sentence length to set width of column in col defs
    sentence_width = as.numeric(dbGetQuery(con, sql("SELECT MAX(char_length(what_happened)*6.5) FROM recent_scores")))
    
    column_defs = list(
      scoring_team = colDef(show = F),
      player_name = colDef(
        align = "right",
        maxWidth = 80,
        # Use team colours for player names
        style = JS(str_c("function(rowInfo) {
                    var value = rowInfo.row['scoring_team']
                    if (value == 'A') {
                      var color = '", snappa_pal[2], "'
                    } else {
                      var color = '", snappa_pal[3], "'
                    }
                    return { color: color, fontWeight: 'bold', padding: '5px' }
                                     }")
        )
      ),
      what_happened = colDef(
        width = sentence_width
      )
    )

    # Take top 5 recent scores
    head(vals$recent_scores(), n = 5) %>% 
      reactable(compact = T, 
                defaultColDef = colDef(name = "", style = list(padding = "5px 0px"),
                                       # Hide header
                                       headerStyle = list(
                                         alignSelf = "flex-end",
                                         display = "none"
                                       )),
                columns = column_defs
                )
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

# Game Summary Stats ------------------------------------------------------
  

  
  game_summary = function(df) {
    
    modalDialog(title = str_c("Score: ", df$points_a, " - ", df$points_b),  style = str_c("background-color: ", snappa_pal[1], ";"),
      
      # h1(str_c(vals$current_scores$team_A, " - ", vals$current_scores$team_B), align = "center"),
      # ,
      # Tables
      fluidRow(
        column(6, align = "center",
               gt_output("team_a_summary")
        ),
        column(6,align = "center",# offset = 2,
               gt_output("team_b_summary")
        )
      ),
      # Summary plot
        plotOutput("summary_plot", height = "50vh"),
      reactableOutput("scores_tbl"),
      
      footer = NULL,
      easyClose = T,
      size = "l"
    )
  }
  
  output$summary_plot = renderPlot({
    game_summary_plot(player_stats = vals$player_stats_db,
                      players = vals$db_tbls()[["players"]],
                      scores = vals$scores_db,
                      game = vals$game_id)
  })
  
  
  
  
  
  

  output$team_a_summary = render_gt({
    make_summary_table(current_player_stats = vals$player_stats_db, 
                       player_stats = vals$db_tbls()[["player_stats"]],
                       neers = snappaneers(), 
                       team_name = "A", 
                       current_round = as.numeric(str_sub(round_num(), 1, -2)), 
                       past_scores = vals$db_tbls()[["scores"]]) %>%
      team_summary_tab(.,
                       game_over = vals$game_over, 
                       team = "A",
                       score_difference = abs(vals$current_scores$team_A - vals$current_scores$team_B))
  })  
  
  output$team_b_summary = render_gt({
    make_summary_table(current_player_stats = vals$player_stats_db, 
                       player_stats = vals$db_tbls()[["player_stats"]],
                       neers = snappaneers(), 
                       team_name = "B", 
                       current_round = as.numeric(str_sub(round_num(), 1, -2)), 
                       past_scores = vals$db_tbls()[["scores"]]) %>%
      team_summary_tab(.,
                       game_over = vals$game_over, 
                       team = "B",
                       score_difference = abs(vals$current_scores$team_A - vals$current_scores$team_B))
  })  
  
  
  
  
  
  


# Stats Outputs --------------------------------------------------------------
  

  output$leaderboard_rt = renderReactable({
    # Create the rank column, arrange the data, and select the columns
    aggregated_data = vals$db_tbls()[["career_stats"]]
    
    # Separate out those with under 5 games
    dividing_line = min(aggregated_data[aggregated_data$games_played < 5, "rank"])
      
    leaderboard_table_rt(aggregated_data, dividing_line = dividing_line)
  })

  

  output$scoring_heatmap = renderPlot({
    score_heatmap(vals$db_tbls()[["score_progression"]])
  })
  
  output$heatmap_info <- renderUI({
    req(input$heat_hover)
    x <- round(input$heat_hover$x, 0)
    y <- round(input$heat_hover$y, 0)
    
    freq = filter(vals$db_tbls()[["score_progression"]], score_a == y, score_b == x) %>% 
      pull(n)
    
    HTML(str_c("<p><span style='font-weight:500'>Team B</span>: ", x, "  ", "<span style='font-weight:500'>Team A</span>: ", y, "</p>",
          "<p><span style='font-weight:500'>How many occurrences?</span>: ", freq))
  })
  
  
  
  
  
  player_game_stats = reactive({
    inner_join(vals$db_tbls()[["players"]], 
               vals$db_tbls()[["player_stats"]], 
               by = "player_id") %>%
      inner_join(dbGetQuery(con, "SELECT game_id, points_a, points_b FROM game_stats WHERE game_complete IS true"), 
                 by = "game_id") %>% 
      # Identify which games were won
      mutate(winning = if_else(points_a > points_b, "A", "B"),
             won_game = (team == winning))
  })
  
  
  
  
  
  # Reactive list of data for a given player's previous 5 games
  player_form_data = reactive({
    recent_games = filter(vals$db_tbls()[["player_stats"]], player_id == input$player_select) %>% 
      mutate(avg_points = mean(!!sym(input$stat_select)),
             max_points = max(!!sym(input$stat_select))) %>% 
      when(input$sample_select != "All" ~ (.) %>% 
             slice_max(order_by = game_id, n = as.numeric(input$sample_select)), 
           ~ (.)) %>% 
      arrange(game_id) %>% 
      mutate(game_num = row_number()) %>% 
      inner_join(select(player_game_stats(), player_id, game_id, won_game))
    
    career_high = unique(recent_games$max_points)

    list("data" = recent_games,
         "x_lims" = c(min(recent_games$game_num)-.5, max(recent_games$game_num)+.5),
         "career_high" = career_high)
    
  })
  
  
  
  
  
  teammate_stats = reactive({
    dbGetQuery(con, 
               sql(str_c(
                 "SELECT * FROM teammate_stats ",
                 "WHERE player_id = ", input$player_select
               )))
  })
  
  
  
  
  
  output$teammate_tab_rt = renderReactable({
    teammate_stats() %>% 
      select(-1:-2) %>% 
      reactable(
        defaultSorted = "games_played",
        columns = list(
          teammate = colDef(
            name = "Teammate"
          ),
          games_played = colDef(
            name = "Games Played",
            defaultSortOrder = "desc"
          ),
          win_pct = colDef(
            name = "Win %",
            defaultSortOrder = "desc",
            # Render the bar charts using a custom cell render function
            cell = function(value) {
              # Format as percentages with 1 decimal place
              value <- str_c(format(value* 100, nsmall = 1), "%")
              # Fix width here to align single and double-digit percentages
              value <- format(value, width = 6, justify = "right")
              bar_chart(value, width = value, fill = snappa_pal[5], background = "#DEDDDD")
            },
            # And left-align the columns
            align = "right"
          ),
          avg_points = colDef(
            name = "Avg. Points",
            defaultSortOrder = "desc",
            format = colFormat(digits = 2)
          ),
          avg_paddle_points = colDef(
            name = "Avg. Paddle Points",
            defaultSortOrder = "desc",
            format = colFormat(digits = 2)
          )
        ),
        compact = T
      )
      
  })
  
  output$game_history_title = renderText({
    str_c(players_tbl[players_tbl$player_id == input$player_select, "player_name"], 
          "'s Game History")
  })
  
  player_game_history = reactive({
    dbGetQuery(con, 
               sql(
                 str_c(
                   "SELECT game_id, 
                        to_date(gs.game_start, 'YYYY-MM-DD') as date, 
                    		(to_timestamp(gs.game_end, 'YYYY-MM-DD HH24:MI:SS')-to_timestamp(gs.game_start, 'YYYY-MM-DD HH24:MI:SS')) AS game_length,
                    		ps.team,
                    		CASE ps.team 
                    			WHEN 'A' THEN gs.points_a || ' - ' || gs.points_b
                    			WHEN 'B' THEN gs.points_b || ' - ' || gs.points_a
                    		END AS final_score,
                    		teammates.teammates,
                    		round(ps.shots::numeric, 2) as shots,
                    		ps.total_points,
                    		ps.clink_points, ps.paddle_points, 
                    		sc.foot_paddles,
                    		sc.sinks,sc.paddle_sinks, sc.foot_sinks,
                    		ps.points_per_round, ps.off_ppr, ps.def_ppr, ps.toss_efficiency
                    FROM player_stats ps
                    INNER JOIN game_stats gs
                    USING (game_id)
                    INNER JOIN (SELECT game_id, team, string_agg(players.player_name, ', ') AS teammates
                    			FROM  player_stats
                    			INNER JOIN players
                    			USING (player_id)
                    			WHERE player_id !=", input$player_select,"
                    			GROUP BY game_id, team
                    			ORDER BY game_id DESC) AS teammates
                    USING (game_id, team)
                    INNER JOIN ( SELECT scores.game_id,
                                scores.player_id,
                                sum(scores.points_scored) AS total_points,
                                sum(
                                    CASE
                                        WHEN scores.points_scored = 3 AND scores.clink = false THEN 1
                                        ELSE 0
                                    END) AS sinks,
                                sum(
                                    CASE
                                        WHEN scores.points_scored = 3 AND scores.clink = false AND scores.paddle = true THEN 1
                                        ELSE 0
                                    END) AS paddle_sinks,
                    			SUM(
                    				CASE
                    					WHEN scores.foot = TRUE THEN scores.points_scored
                    				ELSE 0
                    				END) AS foot_paddles,
                    			SUM(
                    				CASE
                    					WHEN scores.points_scored = 3 AND scores.clink = FALSE AND scores.foot = TRUE THEN 1
                    				ELSE 0
                    				END) AS foot_sinks
                               FROM scores
                              GROUP BY scores.game_id, scores.player_id) sc
                    USING (game_id, player_id)
                    WHERE ps.player_id = ", input$player_select, "
                    ORDER BY game_id DESC")))
  })
  
  output$player_game_stats = renderReactable({
    player_game_history() %>% 
      reactable(
        defaultSorted = "game_id",
        defaultSortOrder = "desc",
        columns = list(
          game_id = colDef(
            name = "Game", width = 78
          ),
          date = colDef(
            name = "Date", width = 107
          ),
          game_length = colDef(
            name = "Game Length"
          ),
          team = colDef(
            name = "Team", width = 72,
            sortable = F
          ),
          final_score = colDef(
            name = "Final Score", width = 75,
            style = function(value) {
              blue_team = as.numeric(str_extract(value, "^[0-9]{1,2}"))
              red_team = as.numeric(str_extract(value, "[0-9]{1,2}$"))
              bg_color = if_else(blue_team > red_team, snappa_pal[5], snappa_pal[2])
              
              list(background = bg_color,
                   color = snappa_pal[1])
            },
            sortable = F
          ),
          teammates = colDef(
            name = "Teammate(s)", width = 115,
            sortable = F
          ),
          shots = colDef(
            name = "Shots", width = 77
          ),
          total_points = colDef(
            name = "Points", width = 81
          ),
          clink_points = colDef(
            name = "Clink Points", width = 81
          ),
          paddle_points = colDef(
            name = "Paddle Points", width = 86
          ),
          foot_paddles = colDef(
            name = "Foot Paddles"
          ),
          sinks = colDef(
            name = "Sinks"
          ),
          paddle_sinks = colDef(
            name = "Paddle Sinks"
          ),
          foot_sinks = colDef(
            name = "Foot Sinks"
          ),
          points_per_round = colDef(
            name = "Points per Round (PPR)",
            format = colFormat(digits = 2)
          ),
          off_ppr = colDef(
            name = "Off. PPR",
            format = colFormat(digits = 2)
          ),
          def_ppr = colDef(
            name = "Def. PPR",
            format = colFormat(digits = 2)
          ),
          toss_efficiency = colDef(
            name = "Toss Efficiency",
            format = colFormat(digits = 1, percent = T)
          )
        ),
        compact = T
      )
  })
  
  
  
  
  

  # Player form plot
  output$player_form = renderPlot({
    
    stat_name = str_to_title(str_replace(input$stat_select, "_", " "))

    # X axis title conditional on number of games chosen
    # plot_title = str_c(stat_name, ": ", 
    #                 if_else(input$sample_select == "All", 
    #                         str_c("All games (n = ", max(pluck(player_form_data(), "x_lims"))-.5, ")"), 
    #                 paste("Last", input$sample_select, "games")))
    
    plot = pluck(player_form_data(), "data") %>% 
      ggplot(., aes(x = game_num, y = !!sym(input$stat_select)))+
      # Bars
      geom_col(aes(fill = won_game), width = .5)+
      # Career avg. line
      geom_segment(aes(x = min(game_num)-.45, y = avg_points, 
                       xend = max(game_num)+.45, yend = avg_points), 
                   lty = "dashed", size = .75)+
      # X axis
      scale_x_continuous(limits = pluck(player_form_data(), "x_lims"), expand = expansion())+
      scale_fill_manual(values = snappa_pal[c(2, 5)], labels = c("Lost", "Won"), 
                        guide = guide_legend(title = NULL, reverse = T))
    
    if(input$stat_select == "toss_efficiency"){
      plot = plot +
        labs(x = expression(More ~ Recent ~ Games %->% ""), y = "Toss Efficiency", 
             caption = str_c("- - - -  Career Avg. (", 
                             scales::percent(unique(pluck(player_form_data(), "data")[["avg_points"]])), ")"))+
        scale_y_continuous(breaks = scales::pretty_breaks(), 
                           labels = scales::percent,
                           expand = expansion(),
                           limits = c(0, 1))
      
    } else {
      plot = plot +
        labs(x = expression(More ~ Recent ~ Games %->% ""), y = stat_name, 
             # title = plot_title,
             caption = str_c("- - - -  Career Avg. (", 
                             scales::comma(unique(pluck(player_form_data(), "data")[["avg_points"]]), accuracy = 1), " points)"))+
        scale_y_continuous(breaks = scales::pretty_breaks(), expand = expansion(),
                           limits = c(0, pluck(player_form_data(), "career_high")*1.25))
    }
    plot+
      theme_snappa()+
      theme(axis.text.x = element_blank(),
            legend.position = "bottom",
            legend.key.height = unit(.25, "cm"))
    
  })
  
  
  
  
  
  output$player_stats_headers = renderUI({
    player_stats = dbGetQuery(con, 
                              sql(str_c("SELECT games_played, win_pct, paddle_points, sinks, paddle_sinks, foot_paddles, foot_sinks
                                        FROM basic_career_stats ", 
                                        "WHERE player_id = ", input$player_select))
                              )
    div(
      box(width = 6, status = "success", title = "General Stats", collapsible = T, 
          # Games Played
          column(
            width = 3,
            descriptionBlock(
              header = player_stats$games_played,
              text = "GAMES"
            )
          ),
          # Win %
          column(
            width = 2,
            descriptionBlock(
              header = scales::percent(player_stats$win_pct),
              text = "WIN %"
            )
          ),
          # Sinks
          column(
            width = 2,
            descriptionBlock(
              header = player_stats$sinks,
              text = "SINK(S)"
            )
          ),
          column(
            width = 5,
            descriptionBlock(
              header = HTML(if_else(player_stats$sinks > 0, 
                                    str_c("<span style='font-weight: 500;'>Every </span>", 
                                          round(1/(player_stats$sinks/player_stats$games_played), 1), 
                                          "<span style='font-weight: 500;'> games</span>"),
                                    "TBD")),
              text = "SINK FREQUENCY"
            )
          )
      ),
      box(width = 6, status = "success", title = "Paddle Stats", collapsible = T,
          # Paddle points
          column(
            width = 3,
            descriptionBlock(
              header = player_stats$paddle_points,
              text = "PADDLE POINTS"
            )
          ),
          # Paddle Sinks
          column(
            width = 3,
            descriptionBlock(
              header = player_stats$paddle_sinks,
              text = "PADDLE SINK(S)"
            )
          ),
          # Foot Paddles
          column(
            width = 3,
            descriptionBlock(
              header = player_stats$foot_paddles,
              text = "FOOT PADDLES"
            )
          ),
          # Foot Sinks
          column(
            width = 3,
            descriptionBlock(
              header = player_stats$foot_sinks,
              text = "FOOT SINK(S)"
            )
          )
          
      )
    )


  })
  
  


# Markov Model Reactives/Outputs ------------------------------------------


  
  team_transition_names = reactive({
    team_A = snappaneers() %>%
      filter(team == "A") %>% 
      arrange(player_id) %>% 
      mutate(n = row_number()) %>% 
      pivot_wider(id_cols = team, names_from = n, values_from = player_id) %>%
      ungroup() %>%
      select(-team)
    
    # Make each team a simple vector 
    team_A = as.vector(team_A)
    while (length(team_A) < 4){
      team_A = append(team_A, NA)
    }
    
    team_B = snappaneers() %>%
      filter(team == "B") %>% 
      arrange(player_id) %>%
      mutate(n = row_number()) %>% 
      pivot_wider(id_cols = team, names_from = n, values_from = player_id) %>%
      ungroup() %>%
      select(-team)
    
    team_B = as.vector(team_B)     
    while (length(team_B) < 4) {
      team_B = append(team_B, NA)
    }
    return(list("team_A" = team_A,
                "team_B" = team_B))
      
  })

  markov_simulated_output = reactive({
    markov_simulate_games(team_transition_names()$team_A, 
                          team_transition_names()$team_B,
                          iterations = vals$markov_vals$iterations ,
                          points_to_win = score_to(),
                          transitions_list = readRDS("markov/transition_probabilities.Rdata"),
                          current_scores_A = vals$markov_vals$A_score,
                          current_scores_B = vals$markov_vals$B_score)
  })
  
  game_simulations = reactive({
    markov_simulated_output()$games_record
  })

  markov_average_team_warning = reactive({
    markov_simulated_output()$warning
  })
  
  markov_summary = reactive({
    markov_summary_data(game_simulations())
  })

  markov_ui_elements = reactive({
    if (is.null(vals$markov_vals$A_score)){
      invisible()
    } else {
     
       w$show()
    }
    visuals = markov_visualizations(markov_summary())
    
    if (length(markov_summary()$winner) == 1){
      winning_team = if_else(markov_summary()$winner == "A",
                             "Team A",
                             "Team B")
      winning_color = ifelse(winning_team == "Team A",
                             snappa_pal[2],
                             snappa_pal[3])
      
    } else{
      winning_team =  "both teams"
      winning_color = "green"
    }
    
    on.exit({
      w$hide()
    })
    return(list("viz" = visuals,
                "team" = winning_team,
                "color" = winning_color)
           )
      })
  
# Update the simulation parameters when the values are changed
observeEvent(input$simulation_go, {
  vals$markov_vals = list("iterations" = as.integer(input$num_simulations),
                          "A_score" = input$simulation_scores_A,
                          "B_score" = input$simulation_scores_B)

} )
#Output to make the team score inputs reactive
output$simulation_score_A = renderUI({
  numericInput("simulation_scores_A", label = "Team A Starting Score",
               value = vals$current_scores$team_A, min = 0, max = 50)
})
output$simulation_score_B = renderUI({
  numericInput("simulation_scores_B", label = "Team B Starting Score",
               value = vals$current_scores$team_B, min = 0, max = 50)
})

output$simulation_warning = renderUI({
    fluidRow(
      column(12,
        HTML(str_c('<h3 style = \'color: red; text-align: center\'>',
            if_else(markov_average_team_warning() != 'none',
              str_c('Warning: ', 
                  if_else(markov_average_team_warning() == 'Both teams', 'Neither team has ', 
                          str_c(markov_average_team_warning(), ' does not have ')
                          ),
                  'an entry in the Markov simulation model. Aggregate values for 
                  teams of the same size have been used instead. </span>'),
              '<span style = \'display: none \' >'
            )
          )
        )
      )
    )

})

output$simulation_blurb = renderUI({
      fluidRow(
        column(12,
               align = "center",
               h3(
                 HTML(
                   str_c("Across ", vals$markov_vals$iterations,
                          ifelse(vals$markov_vals$iterations == 1, " game ", " games ") ,
                          "which ",
                          ifelse(vals$markov_vals$iterations == 1,"begins ", "begin "),
                          "at ", 
                          "<span style='color:", snappa_pal[2], "'>",
                          vals$markov_vals$A_score, "</span>", " - ",
                          "<span style='color:", snappa_pal[3], "'>",
                          vals$markov_vals$B_score, "</span>", ", ",
                          "<span style='color:", markov_ui_elements()$color, "'>",
                          markov_ui_elements()$team, "</span> ",
                          ifelse(markov_summary()$tie == T, "win ", "wins"), 
                          ifelse(vals$markov_vals$iterations == 1,
                                 ".", 
                                 str_c(" an estimated ", 
                                 round(markov_summary()$winrate * 100, 2),
                                "% of games.")
                                )
                   )
                       
                )
               ),
              ##TODO: Move this to its own output below the main probability bar
              h3(
                HTML(
                  str_c(
                    ifelse(markov_summary()$tie == F, 
                            str_c("In the ",
                              ifelse(vals$markov_vals$iterations == 1, "game ", "games "
                                     ),
                            "that ",
                            "<span style='color:", markov_ui_elements()$color, "'>",
                            markov_ui_elements()$team, "</span>", 
                            ifelse(vals$markov_vals$iterations == 1, " won, ", " wins, "
                                   )),
                            str_c("In the games played, ")
                           ),
                    ifelse(vals$markov_vals$iterations == 1,
                           "the final score is ",
                           "the most common final score is "
                           ), 
                    "<span style='color:", snappa_pal[2], "'>",
                    markov_summary()$modal_A, "</span>", " - ",
                    "<span style='color:", snappa_pal[3], "'>",
                    markov_summary()$modal_B, "</span>",
                    ifelse(vals$markov_vals$iterations == 1,
                           ".",
                           str_c(", which is observed in ",
                                 markov_summary()$modal_freq,
                                " games, or ", 
                                round((markov_summary()$modal_freq / vals$markov_vals$iterations) * 100, 2),
                                "% of the time."
                                )
                          )
                    )
                  )
              )
        )
      )
})

# A little reactive styling for this bar, which looks really bad if it doesn't
# span nearly the entire page
set_plot_width <- function(session, output_width_name){
  function() { 
    session$clientData[[output_width_name]] 
  }
}
output$simulation_probability_bar = 
  renderPlot({markov_ui_elements()$viz$win_probability},
             width = set_plot_width(session, "output_simulation_probability_bar_width"),
             height = 75 ,
    bg = "#ecf0f5"
)
  


output$simulation_score_shares =
  renderPlot({markov_ui_elements()$viz$shares
    })

output$simulation_overlap = 
  renderPlot({markov_ui_elements()$viz$overlap
  })


# Restart Game Outputs ----------------------------------------------------

  # For debugging
  
  # output$db_output_players = renderTable({
  #   vals$db_tbls()[["players"]]
  # })
  # output$db_output_scores = renderTable({
  #   vals$db_tbls()[["scores"]]
  # })
  # output$db_output_player_stats = renderTable({
  #   vals$db_tbls()[["player_stats"]]
  # })
  # output$db_output_game_history = renderTable({
  #   vals$db_tbls()[["game_stats"]]
  # })
  # output$snappaneers = renderTable({
  #   snappaneers()
  # })
  
  
# # Generates outputs for the edit teams page
output$edit_team_A <- renderUI({
  tagList(team_edit_ui("A", pull(vals$db_tbls()[["players"]], player_name), active_player_inputs()))
})

output$edit_team_B <- renderUI({
  tagList(team_edit_ui("B", pull(vals$db_tbls()[["players"]], player_name), active_player_inputs()))
})
    

# Events ------------------------------------------------------------------

  


# Check Existing Game -----------------------------------------------------


# Very start of game: display a popup message
# if the previous game is incomplete
  
observe({
  validate(
    need(
      dbGetQuery(con, "SELECT game_complete FROM game_stats WHERE game_id = (SELECT MAX(game_id) FROM game_stats)") %>% 
        pull() %>% 
        isFALSE(),
      message = FALSE
    )
  )
  lost_game_id = dbGetQuery(con, "SELECT game_id FROM game_stats WHERE game_id = (SELECT MAX(game_id) FROM game_stats)") %>% pull()
  
  # Pass an additional check to see if the game which is in question is a 0-0 or not. 
  total_lost_game_score = dbGetQuery(con, str_c("SELECT SUM(total_points) FROM player_stats WHERE game_id = ", lost_game_id)) %>% 
    pull() %>% 
    replace_na(0)
  
  # Discard that game if it's 0-0 and continue on with business as usual, else
  # allow players to restart
  if (total_lost_game_score == 0){
    delete_query = sql("DELETE FROM game_stats WHERE game_id = (SELECT MAX(game_id) FROM game_stats);")
    dbExecute(con, delete_query)
  } else {
  
  showModal(
    modalDialog(
      title = "Hol' up, did you want to continue the previous game?",
      style = str_c("background-color:", snappa_pal[1], ";"),
      div(
        h3("Summary of the Previous Game", 
           align = 'center')
      ),
      
      # br(),
      
      # br(),
      
      br(),
      renderUI({glance_ui_game(lost_game_id)}),
      
      footer = tagList(
                fluidRow(
                  column(9, align = "left",
                         helpText("Warning: 'No' will delete the game from the database", 
                                  style = "color: red; display: inline-block; padding: 1.5vh; font-size: 2rem; font-weight: 600;")
                         ),
                  column(1,
                         actionBttn("resume_no",
                                    label = "No",
                                    style = "material-flat",
                                    color = "danger",
                                    size = "md")
                         ),
                  column(1,
                         actionBttn("resume_yes",
                                    label = "Yes",
                                    style = "material-flat", 
                                    color = "warning",
                                    size = "md")
                         )
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
      insertUI(selector = ".buttons-row", ui = team_scoreboard_ui("B", "A"), where = "beforeBegin")
    } else {
      removeUI("#ScoreboardUI", immediate = T)
      insertUI(selector = ".buttons-row", ui = team_scoreboard_ui(), where = "beforeBegin")
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
    
    # Add new players to the players table
    iwalk(snappaneers()$player_name, function(die_thrower, index){
      # If the player is not in the players table
      if(!(die_thrower %in% vals$db_tbls()[["players"]]$player_name)){
        
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
    
    # Check if the last game was finished
    # Switch to the scoreboard
    # Using isFALSE also denies character(0) in the event that we're starting on a fresh table. Nice!
    if (dbGetQuery(con, "SELECT game_complete FROM game_stats WHERE game_id = (SELECT MAX(game_id) FROM game_stats)") %>% 
              pull() %>% 
              isFALSE()) {
      
      # LAST GAME WAS NOT FINISHED
      
        lost_game = dbGetQuery(con, "SELECT MAX(game_id) FROM game_stats") %>% 
          pull()
      
        lost_game_stats = dbGetQuery(con, str_c("SELECT * FROM game_stats WHERE game_id = ", lost_game))
      
        lost_player_stats = dbGetQuery(con, str_c("SELECT * FROM player_stats WHERE game_id = ", lost_game))

        
        # Set the score outputs and shot number to the values from the last game
        vals$current_scores$team_A = dbGetQuery(con, str_c("SELECT SUM(total_points) FROM player_stats WHERE team = 'A' AND game_id = ", lost_game)) %>%
          pull() %>% as.numeric()
        
        
        vals$current_scores$team_B = dbGetQuery(con, str_c("SELECT SUM(total_points) FROM player_stats WHERE team = 'B' AND game_id = ", lost_game)) %>%
          pull() %>% as.numeric()
        
        vals$score_id = dbGetQuery(con, str_c("SELECT MAX(score_id) FROM scores WHERE game_id = ", lost_game)) %>%
          pull() %>% as.numeric()
        

        vals$scores_db = dbGetQuery(con, str_c("SELECT * FROM scores WHERE game_id = ", lost_game))
        vals$game_id = lost_game
        vals$shot_num = parse_round_num(lost_game_stats$last_round)

        vals$game_stats_db = lost_game_stats
        
        # Initialize the current game's player_stats table
        vals$player_stats_db = lost_player_stats
        
    } else {
      
      # LAST GAME WAS FINISHED
      
      # Set the score outputs and shot number to 0
      vals$current_scores$team_A = 0
      vals$current_scores$team_B = 0

      vals$game_id = dbGetQuery(con, "SELECT MAX(game_id)+1 FROM game_stats") %>% 
        as.integer()
      
      # Initialize the current game's game_stats table
      vals$game_stats_db = bind_rows(vals$game_stats_db,
                                     tibble(
                                       game_id = vals$game_id,
                                       num_players = nrow(snappaneers()),
                                       game_start = as.character(now(tzone = "America/Los_Angeles")),
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
                                       game_complete = F,
                                       last_round = "1A",
                                       arena = input$arena_select
                                     ))
      
      dbWriteTable(
        conn = con, 
        name = "game_stats",
        value = vals$game_stats_db,
        append = T
      )
      
      # Initialize the current game's player_stats table
      vals$player_stats_db = app_update_player_stats(vals$scores_db, snappaneers(), game = vals$game_id)
      
      dbWriteTable(
        conn = con, 
        name = "player_stats",
        value = vals$player_stats_db,
        append = T
      )
    }
    
    # Setup a reactive poll for cooldowns to check if any casualty rules are still in effect
    # but have not made their way around the horn yet
    vals$cooldowns = reactivePoll(
      intervalMillis = 100*30,
      session = session,
      checkFunc = function() {dbGetQuery(con, sql(str_c("SELECT COUNT(*) FROM casualties 
                                                        WHERE game_id = ", vals$game_id)))},
      valueFunc = function() {
        map(
          # Map over each type of score-based casualty
          unique(casualty_rules$casualty_title), 
          ~cooldown_check(current_game = vals$game_id, 
                          current_round = round_num(), 
                          casualty_to_check = .x,
                          rounds = rounds)) %>% 
          # Set the names of the list
          set_names(unique(casualty_rules$casualty_title))}
    )
    
    
    
    shinyjs::enable("game_summary")

  })
  

# Halftime ----------------------------------------------------------------

  
  observeEvent(req(sum(vals$scores_db$points_scored) >= score_to()), {
    sendSweetAlert(session, 
                   title = "Halftime", 
                   type = "info",
                   text = HTML(str_c("Change places!", 
                                     "<audio preload='auto' src='change_places.mp3' type='audio/mp3'></audio>")), html = T)

    shinyjs::click("switch_sides")
    
    # In the event that there was a sink which caused this, also popup the sink menu
    last_score = vals$scores_db[ max(vals$scores_db$score_id),]
    
    sink_casualty_popup(session, score_row = last_score, players = snappaneers()$player_name)

  }, once = T, ignoreNULL = T)
  
  observeEvent(c(
    input$ok_A,
    input$ok_B
  ), {
    if(all(input$score == 3, !input$clink)){
      insertUI(selector = "#round_num",
               where = "afterEnd",
               ui = HTML('<audio preload="auto" src="sploosh.mp3" type="audio/mp3" autoplay controls style="display:none;"></audio>'))
      
    }
  },
  ignoreNULL = T, ignoreInit = T)


observeEvent(input$game_summary, {
  showModal(
    tags$div(id= "game_summary", 
             game_summary(replace_na(vals$game_stats_db, list(points_a = vals$current_scores$team_A, 
                                                              points_b = vals$current_scores$team_B)))
    )
  )
  
})
output$scores_tbl = renderReactable({
  filter(vals$scores_db, game_id == vals$game_id) %>% 
    arrange(-score_id) %>% 
    inner_join(snappaneers(), by = "player_id") %>% 
    select(score_id, player_name, round_num, Points = points_scored,
           Paddle = paddle, Clink = clink, Foot = foot) %>% 
    reactable(compact = T,
              columns = list(
                player_name = colDef(name = "Player"),
                round_num = colDef(name = "Round")
              ))
})

  
  
# Restart a game after indicating you would like to do so
  observeEvent(input$resume_yes, {
    
    
    lost_game = dbGetQuery(con, "SELECT MAX(game_id) FROM game_stats") %>% 
      as.integer()
    
    lost_player_stats = dbGetQuery(con, str_c("SELECT * FROM player_stats WHERE game_id = ", lost_game))
    
    players = dbGetQuery(con, "SELECT * FROM players")
  
    lost_players = lost_player_stats %>%
      left_join(players) %>%
      select(player_name, team) %>% 
      group_by(team) %>% 
      mutate(player_input = str_c("name_", team, row_number())) %>% 
      ungroup()
    
    input_list = lost_players %>%
      select(player_input, player_name) %>% 
      deframe()
    
    removeModal()
    #Look at the number of lost players on each team to be certain of the values 
    # that you want
    
    size_A = lost_players %>% filter(team == "A") %>%
      summarize(sum = n()) %>%
        deframe()
    size_B = lost_players %>% filter(team == "B") %>% 
      summarize(sum = n()) %>%
        deframe()
    
    # Check to see if you should be signaling to the app to care about extra
    # players
     if (size_A == 3){
      shinyjs::click("extra_player_A3")
    } else if (size_A == 4){
      shinyjs::click("extra_player_A3")
      shinyjs::click("extra_player_A4")
    } else {
      invisible()
    }
    
    if (size_B == 3){
      shinyjs::click("extra_player_B3")
    } else if (size_B == 4){
      shinyjs::click("extra_player_B3")
      shinyjs::click("extra_player_B4")
    } else {
      invisible()
    }
    
    delay(10, iwalk(input_list, function(name, id){
      
      updateSelectizeInput(session, inputId = id, selected = name)
      
    })
    )
    
    delay(200, shinyjs::click("start_game"))
})
  
# Close the modal dialog if you say no and remove
# the old game from the DB
  
observeEvent(input$resume_no, {
  removeModal()
  
  delete_query = "DELETE FROM game_stats WHERE game_id = (SELECT MAX(game_id) FROM game_stats);"
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
      vals$player_stats_db = app_update_player_stats(vals$scores_db, snappaneers(), game = vals$game_id)    
      #Update the DB with the new player_stats
      db_update_player_stats(vals$player_stats_db)
      
      # Update round in game stats
      db_update_round(round = round_num(), game = vals$game_id)
      
      
    
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
    vals$player_stats_db = app_update_player_stats(vals$scores_db, snappaneers(), game = vals$game_id)    
    #Update the DB with the new player_stats (adds to shots)
    db_update_player_stats(vals$player_stats_db)
    }

    vals$rebuttal = rebuttal_check(a = vals$current_scores$team_A, b = vals$current_scores$team_B,
                                   round = round_num(), points_to_win = score_to())
    
    # Update round in game stats
    db_update_round(round = round_num(), game = vals$game_id)
    
    if (vals$rebuttal == T) {
      vals$rebuttal_tag = T
      
      game_notification(rebuttal = T, 
                        round = round_num(),
                        current_scores = vals$current_scores)
      
    } else {
    }
      
    })
  

# Score notifications -----------------------------------------------------
  
  

  
  observeEvent(input$next_round | input$previous_round | input$ok_A | input$ok_B,
               {
                 validate(
                   need( 
                     vctrs::vec_in(vals$current_scores, 
                                   haystack = casualty_rules[,1:2]), label = "casualty"),
                   # Are any of the cooldowns active?
                   need(!any(flatten_lgl(vals$cooldowns())), label = "Cooldowns")
                 )

                 casualty_popup(session,
                                score = vals$current_scores, 
                                rules = casualty_rules, 
                                players = snappaneers()$player_name)

                 
                 
               }, ignoreInit = T)
  
  observeEvent(input$casualty, {
    # Convert player name to ID
    casualty = select(snappaneers(), starts_with("player")) %>% 
      deframe() %>% 
      pluck(input$casualty)
    
    type = casualty_rules$casualty_title[vctrs::vec_match(vals$current_scores, haystack = casualty_rules[, 1:2])]
    
    # Insert casualty details
    new_casualty = tibble(
      casualty_id = as.numeric(dbGetQuery(con, sql("SELECT MAX(casualty_id)+1 FROM casualties"))),
      game_id = vals$game_id,
      score_id = vals$score_id,
      player_id = casualty,
      casualty_type = type
    )
    dbWriteTable(
      conn = con, 
      name = "casualties", 
      value = new_casualty,
      append=T
    )
    # In the event that there was a sink which caused this, also popup the sink menu
    last_score = vals$scores_db[ max(vals$scores_db$score_id),]
    sink_casualty_popup(session, score_row = last_score, players = snappaneers()$player_name)
  })
  
  observeEvent(input$sink_casualty, {
    # Convert player name to ID
    casualty = select(snappaneers(), starts_with("player")) %>% 
      deframe() %>% 
      pluck(input$sink_casualty)
    
    # Insert casualty details
    new_casualty = tibble(
      casualty_id = as.numeric(dbGetQuery(con, sql("SELECT MAX(casualty_id)+1 FROM casualties"))),
      game_id = vals$game_id,
      score_id = vals$score_id,
      player_id = casualty,
      casualty_type = "Sunk"
    )
    
    dbWriteTable(
      conn = con, 
      name = "casualties", 
      value = new_casualty,
      append=T
    )  
    
    })
  
  observeEvent(input$tifu, {
    showModal(
      tifu_casualty_popup(players = snappaneers()$player_name)
    )
    
  })
  
  observeEvent(input$tifu_confirm, {
    # Convert player name to ID
    casualty = select(snappaneers(), starts_with("player")) %>% 
      deframe() %>% 
      pluck(input$tifu_casualty)
    
    # Insert casualty details
    new_casualty = tibble(
      casualty_id = as.numeric(dbGetQuery(con, sql("SELECT MAX(casualty_id)+1 FROM casualties"))),
      game_id = vals$game_id,
      score_id = vals$score_id,
      player_id = casualty,
      casualty_type = input$casualty_type
    )
    
    dbWriteTable(
      conn = con, 
      name = "casualties", 
      value = new_casualty,
      append=T
    )  
    
    removeModal()
    
    showNotification(str_c("Nothing wrong with just a little bit of horseplay every now and then, ", 
                                input$tifu_casualty), duration = 7, closeButton = F)
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
    val <- paste0("#",getInputs("extra_player_A3"))
    add_player_input("start", val, "A", 3, current_choices(), session)
    
  })
  
  # Remove A3
  #   - Insert add new player action button
  #   - Remove A3 player name input
  observeEvent(input$remove_A3, {
    remove_p3_input("start", "A", session)

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
    
    add_player_input("start", vals, "A", 4, current_choices(), session)
    
  })
  
  # Remove A4
  #   - Insert add new player action button
  #   - Remove A4 player name input
  observeEvent(input$remove_A4, {
    remove_p4_input("start", "A", session)
    
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
    
    add_player_input("start", vals, "B", 3, current_choices(), session)
  })

  # Remove B3
  #   - Insert add new player action button
  #   - Remove B3 player name input
  observeEvent(input$remove_B3, {
    remove_p3_input("start", "B", session)
    
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
    
    add_player_input("start", vals, "B", 4, current_choices(), session)
  })
  

  # Remove B4
  #   - Insert add new player action button
  #   - Remove B4 player name input
  observeEvent(input$remove_B4, {
    remove_p4_input("start", "B", session)
    # Tells later checks to not worry about this
    # empty slot in active_player_names
    vals$want_B4 = F

  })  
  
  

# Dynamic UI for the Edit Teams Tab ---------------------------------------

  # New Player A3
  #   - Add A3 text input
  #   - Remove the add new player action button
  observeEvent(input$edit_add_A3, {
    # Set input want to true
    vals$want_A3 = T
    
    # Get add player button inputs
    val <- paste0("#",getInputs("edit_add_A3"))
    add_player_input("edit", val, "A", 3, current_choices(), session)
    
  })
  
  # Remove A3
  #   - Insert add new player action button
  #   - Remove A3 player name input
  observeEvent(input$edit_remove_A3, {
    remove_p3_input("edit", "A", session)
    
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
  observeEvent(input$edit_add_A4, {
    # Set input want to true
    vals$want_A4 = T
    
    # Get UI inputs for extra player button
    vals <- paste0("#",getInputs("edit_add_A4"))
    
    add_player_input("edit", vals, "A", 4, current_choices(), session)
    
  })
  
  # Remove A4
  #   - Insert add new player action button
  #   - Remove A4 player name input
  observeEvent(input$edit_remove_A4, {
    remove_p4_input("edit", "A", session)
    
    vals$want_A4 = F
    
  })  
  
  
  # New Player B3
  #   - Add B3 text input
  #   - Remove the add new player action button
  observeEvent(input$edit_add_B3, {
    
    # Set want check to true
    vals$want_B3 = T
    
    # Get inputs for add player button
    vals <- paste0("#",getInputs("edit_add_B3"))
    
    add_player_input("edit", vals, "B", 3, current_choices(), session)
  })
  
  # Remove B3
  #   - Insert add new player action button
  #   - Remove B3 player name input
  observeEvent(input$edit_remove_B3, {
    remove_p3_input("edit", "B", session)
    
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
  observeEvent(input$edit_add_B4, {
    # Set want check to true
    vals$want_B4 = T
    
    # Get add player button inputs
    vals <- paste0("#",getInputs("edit_add_B4"))
    
    add_player_input("edit", vals, "B", 4, current_choices(), session)
  })
  
  
  # Remove B4
  #   - Insert add new player action button
  #   - Remove B4 player name input
  observeEvent(input$edit_remove_B4, {
    remove_p4_input("edit", "B", session)
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
                  players = eligible_shooters,
                  round = round_num()))
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
      scorer_pid = pull(filter(vals$db_tbls()[["players"]], player_name == input$scorer), player_id)
      # Were they shooting?
      scorers_team = pull(filter(snappaneers(), player_name == input$scorer), team) # pull the scorer's team from snappaneers
      shooting_team_lgl = all(str_detect(round_num(), "A"), scorers_team == "A") # Are they on team A & did they score for team A?
      
      new_score = tibble(
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
      )
      # Sink notification
      sink_casualty_popup(session, score_row = new_score, players = snappaneers()[snappaneers()$team == "B", "player_name", drop=T])
      
      
      # Add the score to the scores table
      vals$scores_db = bind_rows(vals$scores_db,
                                 new_score)
      #Update the db with the new score
  
      dbWriteTable(con, "scores", anti_join(vals$scores_db, dbGetQuery(con, "SELECT * FROM scores")), append = T)
      
      
      # Update player stats table
      vals$player_stats_db = app_update_player_stats(vals$scores_db, snappaneers(), game = vals$game_id)
      db_update_player_stats(vals$player_stats_db)

      # Congratulate paddlers
      if(input$paddle & str_detect(pull(filter(snappaneers(), player_name == input$scorer), team), "[Aa]") ){
        game_notification()
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
                                   round_num(), score_to())
    

    #    if (!is.null(vals$rebuttal)) {
    if (vals$rebuttal == T & vals$rebuttal_tag == T) {
      game_notification(rebuttal = T, 
                        round = round_num(),
                        current_scores = vals$current_scores)
    } else {
      
    }
    # A fix to issue 45 where games would be prompted to end even though
    # a team has technically left rebuttal (meaning tag needs to be false)
    
    if (vals$rebuttal_tag == T & vals$rebuttal == F){
      vals$rebuttal_tag = F
      team_in_rebuttal = str_sub(round_num(), start = -1)
      text_colour = if_else(team_in_rebuttal == "A", snappa_pal[2], snappa_pal[3])
      showNotification(HTML(str_c("<span style='color:", text_colour, "'>Team ", 
                                  team_in_rebuttal, "</span>",
                                  " has exited rebuttal!")
                            ), 
                       duration = 20, closeButton = F
                      )
    }
    

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
        players = eligible_shooters,
        round = round_num()))
    
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
      scorer_pid = pull(filter(vals$db_tbls()[["players"]], player_name == input$scorer), player_id)
      # Were they shooting?
      scorers_team = pull(filter(snappaneers(), player_name == scorer_pid), team)
      shooting_team_lgl = all(str_detect(round_num(), "[Bb]"), scorers_team == "B")
      
      new_score = tibble(
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
      )
      
      # Sink notification
      sink_casualty_popup(session, score_row = new_score, players = snappaneers()[snappaneers()$team == "A", "player_name", drop=T])
      
      
      # Add the score to the scores table
      vals$scores_db = bind_rows(vals$scores_db,
                                 new_score)
      #Update the server with the new score
      dbWriteTable(con, "scores", 
                   anti_join(vals$scores_db, dbGetQuery(con, "SELECT * FROM scores")), append = T)
      
      
      # Update player stats in the app
      vals$player_stats_db = app_update_player_stats(vals$scores_db, 
                                                     snappaneers(), 
                                                     game = vals$game_id)    
      #Update the DB with the new player_stats
      db_update_player_stats(vals$player_stats_db)
      
      
      # Congratulate paddlers for good offense, chide those who paddled against their own team
      if(input$paddle & str_detect(pull(filter(snappaneers(), player_name == input$scorer), team), "[Bb]") ){
        game_notification()
      }
      if(input$paddle & str_detect(pull(filter(snappaneers(), player_name == input$scorer), team), "A") ){
        showNotification("It's a bold strategy Cotton, let's see if it pays off for them.")
      }
    } else {
      vals$error_msg <- "You did not input anything."
    }
    
    
    # If the game is still in rebuttal in rebuttal, remind players
    # of the points needed to bring it back
    vals$rebuttal = rebuttal_check(vals$current_scores$team_A, 
                                   vals$current_scores$team_B,
                                   round_num(), score_to())
    
    #    if (!is.null(vals$rebuttal)) {
    if (vals$rebuttal == T & vals$rebuttal_tag == T) {
      game_notification(rebuttal = T, 
                        round = round_num(),
                        current_scores = vals$current_scores)
    } else {
      
    }
    
    if (vals$rebuttal_tag == T & vals$rebuttal == F){
      vals$rebuttal_tag = F
      team_in_rebuttal = str_sub(round_num(), start = -1)
      text_colour = if_else(team_in_rebuttal == "A", snappa_pal[2], snappa_pal[3])
      showNotification(HTML(str_c("<span style='color:", text_colour, "'>Team ", 
                                  team_in_rebuttal, "</span>",
                                  " has exited rebuttal!")
      ), 
      duration = 20, closeButton = F
      )
    }
    
    
  })
  
  

# Undo Score --------------------------------------------------------------

  # Undo score consists of:
  # - Observe the press of the undo score button
  # - Present confirmation with details of the last score
  # - Observe the confirmation of the undo score button
  # - Remove score from the database
  
  ## Observe press of undo score button
  # Team A
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
    
    
    
    confirmSweetAlert(
      session,
      type = "warning",
      inputId = "undo_score_A_confirm",
      title = "Remove this score?",
      text = reactableOutput("last_score_A"),
      closeOnClickOutside = T,
      html = T
    )
    
  })
  
  # Team B
  observeEvent(input$undo_score_B, {
    validate(
      need(vals$current_scores$team_B > 0, label = "Team B hasn't scored yet!")
    )
    
    # Select the ID which is the max on Team A
    last_score = filter(vals$scores_db, scoring_team == "B") %>% 
      pull(score_id) %>% 
      max()
    
    # Pull the number of points the last score was worth
    last_score_pts = filter(vals$scores_db, score_id == last_score) %>% 
      pull(points_scored)
    
    
    
    confirmSweetAlert(
      session,
      type = "warning",
      inputId = "undo_score_B_confirm",
      title = "Remove this score?",
      text = reactableOutput("last_score_B"),
      closeOnClickOutside = T,
      html = T
    )
    
  })
  
  ## Last score table outputs
  
  # Team A
  # A Last score table
  output$last_score_A = renderReactable({
    # Check if Team has scored yet
    validate(
      need(vals$current_scores$team_A > 0, label = "Team A hasn't scored yet!")
    )
    
    # Find the highest score on each team and keep only team A
    last_score = filter(group_by(vals$scores_db, scoring_team), scoring_team == "A", score_id == max(score_id)) %>% 
      ungroup() %>% 
      # Join in player names
      inner_join(snappaneers(), by = "player_id")
    
    # Initially I was showing each 'event' (e.g. paddle) column and leaving them blank when
    # the event did not occur. This made the table very wide and it didn't look nice,
    # I tried toying around with minWidth and maxWidth, and making a custom class
    # with a specified with of 'fit-content' etc.
    # 
    # The solution I came up with was to create a list of column definitions and then remove 
    # columns whose values were false
    
    # Create the column list to use for the table display of the last score
    col_list = list(
      player_name = colDef(name = "Player", maxWidth = 100),
      round_num = colDef(name = "Round", maxWidth = 70),
      points_scored = colDef(name = "Pts", maxWidth = 50),
      paddle = colDef(name = "", width = 30, 
                      cell = function(value) {
                        if (value) emo::ji("waving_hand") else ""
                      }),
      clink = colDef(name = "", width = 30, 
                     cell = function(value) {
                       if (value) emo::ji("ear") else ""
                     }),
      foot = colDef(name = "", width = 30, 
                    cell = function(value) {
                      if (value) emo::ji("foot") else ""
                    })
    )
    
    # map over event columns and keep those which are true
    emoji_to_show = map(select(last_score, paddle:foot), isTRUE) %>% 
      keep(isTRUE) %>% 
      names()
    
    # Subset the column definitions
    cols_to_show = col_list[c("player_name", "round_num", "points_scored", emoji_to_show)]
    
    last_score %>% 
      select(player_name, round_num, points_scored, any_of(emoji_to_show)) %>% 
      reactable(compact = T, 
                sortable = F, 
                filterable = F, 
                fullWidth = F, wrap = T, 
                columns = cols_to_show)
  })
  
  # Team B
  # B Last score table
  output$last_score_B = renderReactable({
    # Check if Team has scored yet
    validate(
      need(vals$current_scores$team_B > 0, label = "Team B hasn't scored yet!")
    )
    
    # Find the highest score on each team and keep only team B
    last_score = filter(group_by(vals$scores_db, scoring_team), scoring_team == "B", score_id == max(score_id)) %>% 
      ungroup() %>% 
      # Join in player names
      inner_join(snappaneers(), by = "player_id")
    
    # Initially I was showing each 'event' (e.g. paddle) column and leaving them blank when
    # the event did not occur. This made the table very wide and it didn't look nice,
    # I tried toying around with minWidth and maxWidth, and making a custom class
    # with a specified with of 'fit-content' etc.
    # 
    # The solution I came up with was to create a list of column definitions and then remove 
    # columns whose values were false
    
    # Create the column list to use for the table display of the last score
    col_list = list(
      player_name = colDef(name = "Player", maxWidth = 100),
      round_num = colDef(name = "Round", maxWidth = 70),
      points_scored = colDef(name = "Pts", maxWidth = 50),
      paddle = colDef(name = "", width = 30, 
                      cell = function(value) {
                        if (value) emo::ji("waving_hand") else ""
                      }),
      clink = colDef(name = "", width = 30, 
                     cell = function(value) {
                       if (value) emo::ji("ear") else ""
                     }),
      foot = colDef(name = "", width = 30, 
                    cell = function(value) {
                      if (value) emo::ji("foot") else ""
                    })
    )
    
    # map over event columns and keep those which are true
    emoji_to_show = map(select(last_score, paddle:foot), isTRUE) %>% 
      keep(isTRUE) %>% 
      names()
    
    # Subset the column definitions
    cols_to_show = col_list[c("player_name", "round_num", "points_scored", emoji_to_show)]
    
    last_score %>% 
      select(player_name, round_num, points_scored, any_of(emoji_to_show)) %>% 
      reactable(compact = T, 
                sortable = F, 
                filterable = F, 
                fullWidth = F, wrap = T, 
                columns = cols_to_show)
  })
  
  ## Observe the confirmation of the undo score button
  
  # Team A
  # Remove the last score from the database
  observeEvent(input$undo_score_A_confirm, {
    validate(
      need(isTRUE(input$undo_score_A_confirm), label = "Nothin' to see here..")
    )
    # Select the ID which is the max on Team A
    last_score = filter(vals$scores_db, scoring_team == "A") %>% 
      pull(score_id) %>% 
      max()
    
    # Pull the number of points the last score was worth
    last_score_row = filter(vals$scores_db, score_id == last_score) %>% 
      select(points_scored, clink)
    # Reduce the score ID for any scores which have happened since the score which is being removed
    # Note that score undo-ing is team specific
    vals$scores_db = filter(vals$scores_db, score_id != last_score) %>% 
      mutate(score_id = if_else(score_id > last_score, as.integer(score_id-1), score_id))
    # Reduce the team's score and score_id
    vals$current_scores$team_A = vals$current_scores$team_A - last_score_row$points_scored
    vals$score_id = as.integer(vals$score_id-1)
    
    #Remove the value from the snappaDB
    dbSendQuery(con,
                str_c("DELETE FROM scores WHERE score_id = ", last_score, " AND game_id = ", vals$game_id, ";")
    )
    
    
    # Update player stats table in the app
    vals$player_stats_db = app_update_player_stats(vals$scores_db, snappaneers(), game = vals$game_id)
    #Update the DB with the new player_stats
    db_update_player_stats(vals$player_stats_db)
    
    # Remove any associated sink casualty
    if(vctrs::vec_in(last_score_row, tribble(~points_scored, ~clink, 
                                             3, F,
                                             5, T,
                                             7, T))){
      dbSendQuery(con,
                  str_c("DELETE FROM casualties WHERE score_id = ", last_score, 
                        " AND game_id = ", vals$game_id,
                        " AND casualty_type = 'Sunk'")
      )
    }
      
    
  })
  
  # Team B
  # Undo score B
  observeEvent(input$undo_score_B_confirm, {
    validate(
      need(isTRUE(input$undo_score_B_confirm), label = "Nothin' to see here..")
    )
    
    # Select the ID which is the max on Team B
    last_score = filter(vals$scores_db, scoring_team == "B") %>% 
      pull(score_id) %>% 
      max()
    
    # Pull the number of points the last score was worth
    last_score_row = filter(vals$scores_db, score_id == last_score) %>% 
      select(points_scored, clink)
    
    # Reset any scores which have happened since the score being erased
    vals$scores_db = filter(vals$scores_db, score_id != last_score) %>% 
      mutate(score_id = if_else(score_id > last_score, as.integer(score_id-1), score_id))
    
    vals$current_scores$team_B = vals$current_scores$team_B - last_score_row$points_scored
    vals$score_id = as.integer(vals$score_id-1)
    
    
    #Remove the value from the snappaDB
    dbSendQuery(con,
                str_c("DELETE FROM scores WHERE score_id = ", last_score, 
                      " AND game_id = ", vals$game_id)
    )    
    # Update player_stats 
    vals$player_stats_db = app_update_player_stats(vals$scores_db, snappaneers(), game = vals$game_id)
    #Update the DB with the new player_stats
    db_update_player_stats(vals$player_stats_db)
    
    # Remove any associated sink casualty
    if(vctrs::vec_in(last_score_row, tribble(~points_scored, ~clink, 
                                             3, F,
                                             5, T,
                                             7, T))){
      dbSendQuery(con,
                  str_c("DELETE FROM casualties WHERE score_id = ", last_score, 
                        " AND game_id = ", vals$game_id,
                        " AND casualty_type = 'Sunk'")
      )
    }
    
  })
  
  
  
  
  
  

# Add/Edit Team Events ----------------------------------------------------


observeEvent(input$add_player_A3, {
  
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
    
    vals$game_over = T

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
    # validate(need(any(vals$current_scores$team_a >= score_to(),
    #                   vals$current_scores$team_b >= score_to()),
    #               message = "Your game hasn't ended yet. Please finish the current game or restart before submitting",
    #               label = "check_game_over"))

    # Update Game History
    # Calculate game-level stats from game stats players, and vary it based on whether the game is actually complete or not
    # to test it I use rebuttal since this is the one point in time where we can basically be certain that a game is/isn't over
    # Checking vals$rebuttal here is redundant if we have already clicked next round, but this is necessary in games where
    # players clicked "finish game" since rebuttal is checked on the next round button
    vals$rebuttal = rebuttal_check(a = vals$current_scores$team_A, b = vals$current_scores$team_B,
                                   round = round_num(), points_to_win = score_to())
    
    

    if(vals$rebuttal == T){
    game_stats = vals$player_stats_db %>% 
      group_by(game_id) %>% 
      summarise(points_a = sum((team == "A")*total_points),
                points_b = sum((team == "B")*total_points),
                rounds = as.integer(vals$shot_num),
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
                  rounds = as.integer(vals$shot_num),
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
      select(-contains("_old", ignore.case = F)) %>% 
      # Add quotes around character vars for update query
      mutate(across(where(is_character), ~str_c("'", ., "'")))
    
    
    # Convert tibble to character string in the format: COLNAME = VALUE
    col_updates = t(vals$game_stats_db) %>% 
      str_c(rownames(.), " = ", ., collapse = ", ")
    
    update_game_query = str_c("UPDATE game_stats
                              SET ", col_updates,
                              " WHERE game_id = ", vals$game_id, ";")
    
    dbExecute(con, update_game_query)
  

    
    # Update player stats table one final time
    vals$player_stats_db = app_update_player_stats(vals$scores_db, snappaneers(), game = vals$game_id)
    
    db_update_player_stats(vals$player_stats_db)
    
    
    # Should be made unnecessary by adding
    # players immediately 
    
    # dbAppendTable(
    #   conn = con, 
    #   name = "players",
    #   value = anti_join(vals$players_db, players_tbl))
    
    #Update Scores
    dbWriteTable(
      conn = con,
      name = "scores",
      value = vals$scores_db,
      append = T)
    
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
    

    showModal(
      tags$div(id= "game_summary", 
               game_summary(vals$game_stats_db))
    )
    
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
         function(id, lab) updateSelectizeInput(session, inputId = id, label = lab, c(`Player Name`='', vals$db_tbls()[["players"]]$player_name), 
                                                options = list(create = TRUE)))
    
    # 3. Reset reactive values
    # vals$game_stats_db = game_stats_tbl() %>% slice(0) %>% select(1:5)
    # vals$player_stats_db = player_stats_tbl() %>% slice(0)
    # vals$players_db = dbGetQuery(con, "SELECT * FROM players")
    # vals$scores_db = scores_tbl() %>% slice(0)
    # vals$score_id = as.integer(0)
    # vals$shot_num = as.integer(1)
    

    
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

