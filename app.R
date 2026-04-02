#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
renv::diagnostics()


library(waiter)




# Prior to app startup ----------------------------------------------------

# (rounds, round_labels, casualty_rules, sink_criteria defined in R/_globals.R)

# DB Tables ---------------------------------------------------------------


# Pull db tables for tibble templates
players_tbl = dbGetQuery(con, "SELECT * FROM players")
# scores_tbl = dbGetQuery(con, "SELECT * FROM scores") 
# player_stats_tbl = dbGetQuery(con, "SELECT * FROM player_stats")
# game_stats_tbl = dbGetQuery(con, "SELECT * FROM game_stats") 

# Makea list of table templates
tbls = c("scores", "player_stats", "game_stats", "career_stats")
tbl_templates = map(tbls, function(table){
  dbGetQuery(con, str_c("SELECT * FROM ", table, " LIMIT 0")) 
}) %>% 
  set_names(tbls)





# UI ----------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- dashboardPage(
  header = dashboardHeader(title = tagList(
    # Corner logo
    span(class = "logo-lg", "Snappa Scoreboard"),
    img(class = "logo-mini", src = "die_hex.png", style = "padding:.25vw;")
  ),
                           
    
    # Left side header
    leftUi = tagList(
      dropdownBlock2(
        id = "recent_scores_dropdown",
        title = "Recent Scores",
        icon = "backward",
        badgeStatus = NULL,
        reactableOutput("recent_scores_rt"),
        actionBttn("game_summary",
                   "Detailed Game Summary",
                   style = "material-flat",
                   color = "primary",
                   icon = icon("chart-bar"),
                   size = "sm")
      )
      # Right side header
    ),
    tags$li(class = "dropdown", socialButton(
        href = "https://github.com/mdewey131/Snappa-Scoreboard",
        icon = icon("github")
    ),
    style = "padding-top:9px;"
    )
    ),
  sidebar = dashboardSidebar(
    sidebarMenuOutput("sidebar_menu"),
    collapsed = TRUE
  ),
  controlbar = dashboardControlbar(
    skin = "dark",
    controlbarMenu(
    #   id = 1,
      controlbarItem(
        title = "Game Options",
        sliderInput(
          inputId = "score_to",
          label = "What score are you playing to?",
          min = 11, max = 50, value = 21
        ),
        br(),
        actionBttn("finish_game", "Finish",
                   icon = icon("check"), size = "sm",
                   style = "material-flat", color = "warning"),
        br(),
        actionBttn("debug", label = "debug", icon = icon("bug"), 
                   style = "material-flat", color = "danger")
      ),
      controlbarItem(
        title = "Casualties",
        disabled(actionBttn("tifu", "Friendly Fire",
                            style = "material-flat",
                            size = "sm", color = "danger")),
        br(),
        actionBttn("highnoon_manual", 
                   "High noon", size = "sm",
                   style = "material-flat", color = "success"),
        br(),
        actionBttn("casualty_manual", 
                   "Casualty Check", size = "sm",
                   style = "material-flat", color = "royal")
        
      )
      
    )
    ),
  body = dashboardBody(
    useShinyjs(),
    use_waiter(),
    tabItems(

    ## Player Input ------------------------------------------------------------

      tabItem(
        tabName = "player_input",
        team_input_tab()
      ),

    ## Scoreboard --------------------------------------------------------------

      tabItem(
        tabName = "scoreboard", #icon = icon("window-maximize"), 
        scoreboard_tab()
      ),

    ## Career Stats ------------------------------------------------------------

      tabItem(
        tabName = "career_stats",
        careerStatsUI("career")
      ),

    ## Player Stats ------------------------------------------------------------

      tabItem(
        tabName = "player_stats",
        playerStatsUI("player_stats")
      )
    ),
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "app.css")
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
  
  observeEvent(input$debug, {
    browser()
  })

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
                 icon = icon("chart-column")),
        menuItem("Player Stats", tabName = "player_stats",
                 icon = icon("chart-line"))
      )
      
    } else {
      sidebarMenu(
        menuItem("Player Input", 
                 tabName = "player_input", 
                 icon = icon("users"), selected = T),
        menuItem("Career Stats", 
                 tabName = "career_stats", 
                 icon = icon("chart-column")),
        menuItem("Player Stats", tabName = "player_stats",
                 icon = icon("chart-line"))
      )
      
    }
    
  })
  

  
  
    

# Reactive Values Object ---------------------------------------------------------
  
  # reactivePoll watches for changes in the value of checkFunc at the interval
  #   when it notices changes, it updates using valueFunc
  # This checkFunc should update our tables when a game is complete
  

  # Session identity: game ID, player registry, DB polls
  game_session <- reactiveValues(
    game_id = NULL,
    new_player_id = sum(dbGetQuery(con, "SELECT MAX(player_id) FROM players"), 1),
    players = dbGetQuery(con, sql("SELECT player_id, player_name FROM players")),
    # Live DB snapshots — update on a timer
    db_tbls = reactivePoll(
      intervalMillis = 1000 * 120,
      session = session,
      checkFunc = function() { dbGetQuery(con, sql("SELECT COUNT(*) FROM game_stats where game_complete is true")) },
      valueFunc = function() { db_poll_completed_tables(con, tbls) }
    ),
    recent_scores = reactivePoll(
      intervalMillis = 100 * 30,
      session = session,
      checkFunc = function() { dbGetQuery(con, sql("SELECT COUNT(*) FROM recent_scores")) },
      valueFunc = function() { db_poll_recent_scores(con) }
    )
  )

  # Live game data: scores, rounds, player stats, casualties
  game_state <- reactiveValues(
    score_id = as.integer(0),
    shot_num = as.integer(1),
    game_stats_db = select(tbl_templates$game_stats, 1:5),
    player_stats_db = tbl_templates$player_stats,
    scores_db = tbl_templates$scores,
    casualties = tibble(
      casualty_id = integer(),
      game_id     = integer(),
      score_id    = integer(),
      player_id   = integer(),
      casualty_type = character(),
      reported_player = integer()
    ),
    cooldowns = setNames(list(F, F, F), unique(casualty_rules$casualty_title)),
    current_scores = tibble(team_A = 0, team_B = 0),
    rebuttal = NULL,
    rebuttal_tag = F,
    score = NULL,
    error_msg = NULL,
    print = FALSE,
    score_to = NULL,
    trolls = NULL,
    game_over = F
  )

  # UI-only state: extra player slots and team-switch tracking
  ui_state <- reactiveValues(
    want_A3 = F,
    want_A4 = F,
    want_B3 = F,
    want_B4 = F,
    switch_counter = 1
  )
  
  
  

# Player Inputs, Snappaneers, Other Reactives --------------------------------------

  # A reactive for the current score that we're playing to
  score_to = reactive({
    input$score_to
  })
  
  # Increment round number
  round_num = reactive({
    rounds[game_state$shot_num]
  })
  
  # Active input buttons
  #   - List of player inputs which are not null
  active_player_inputs = reactive({
    list("A1" = input$name_A1, "A2" = input$name_A2, "A3" = input$name_A3, "A4" = input$name_A4, "A5" = input$name_A5, 
         "B1" = input$name_B1, "B2" = input$name_B2, "B3" = input$name_B3, "B4" = input$name_B4, "B5" = input$name_B5) %>% 
      discard(is_null)
  })
  
  player_inputs = reactive({
    tribble(
      ~input, ~team, ~player_name, ~expected,
      "A1", "A", input$name_A1, T,
      "A2", "A", input$name_A2, T,
      "A3", "A", input$name_A3, input$add_player_A3,
      "A4", "A", input$name_A4, input$add_player_A4,
      "A5", "A", input$name_A5, input$add_player_A5,
      "B1", "B", input$name_B1, T,
      "B2", "B", input$name_B2, T, 
      "B3", "B", input$name_B3, input$add_player_B3,
      "B4", "B", input$name_B4, input$add_player_B4,
      "B5", "B", input$name_B5, input$add_player_B5
    )
  })
  
  expected_player_inputs = reactive({
    player_inputs() |> 
      # Keep expected players
      filter(expected)
    
  })
  
  output$expected_inputs = renderReactable({
    reactable(expected_player_inputs())
  })
  
  # Snappaneers - | Team | Player name | Player ID  | Shots
  snappaneers = reactive({
    
    player_inputs() |> 
      filter(player_name !="") |> 
      select(-expected) |> 
      # Remove empty player inputs
      filter(player_name != "") %>% 
      left_join(game_session$players, by = "player_name") %>% 
      # Add shot count
      add_shot_count(shot_num = game_state$shot_num)
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
                        shot = game_state$shot_num, 
                        snappaneers = snappaneers(), 
                        paddle = any(input$paddle, input$foot), 
                        scores_table = game_state$scores_db,
                        rebuttal = game_state$rebuttal_tag) == "valid",
        message = "That entry doesn't make sense for this round/shooter combination"),
      if (snappaneers()[snappaneers()$player_name == input$scorer, "player_id", drop=T] %in%
          game_state$scores_db[game_state$scores_db$round_num == round_num() & game_state$scores_db$paddle == F, "player_id", drop=T]){
        need(
           validate_scores(player = input$scorer,
                               shot = game_state$shot_num, 
                               snappaneers = snappaneers(), 
                               paddle = any(input$paddle, input$foot), 
                               scores_table = game_state$scores_db,
                           rebuttal = game_state$rebuttal_tag) == "valid",
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
                        shot = game_state$shot_num, 
                        snappaneers = snappaneers(), 
                        paddle = any(input$paddle, input$foot), 
                        scores_table = game_state$scores_db,
                        rebuttal = game_state$rebuttal_tag) == "valid",
        message = "That entry doesn't make sense for this round/shooter combination"
      ),
      # Make sure that the last person to score in this round on offense can't paddle
      if (snappaneers()[snappaneers()$player_name == input$scorer, "player_id", drop=T] %in%
          game_state$scores_db[game_state$scores_db$round_num == round_num() & game_state$scores_db$paddle == F, "player_id", drop=T]){
        need(
          validate_scores(player = input$scorer,
                            shot = game_state$shot_num, 
                            snappaneers = snappaneers(), 
                            paddle = any(input$paddle, input$foot), 
                            scores_table = game_state$scores_db,
                          rebuttal = game_state$rebuttal_tag) == "valid",
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
    div(class = "round-control",
      actionBttn("previous_round", 
                 label = "Previous Round", style = "jelly", icon = icon("arrow-left"), color = 
                   team_colours[[str_extract(round_num(), "[AB]+")]], size = "lg"),
      actionBttn("next_round", 
                 label = "Pass the dice", style = "jelly", icon = icon("arrow-right"), 
                 color = team_colours[[str_extract(round_num(), "[AB]+")]], size = "lg")
    )
  })
  
  output$playing_to = renderUI({
    p("Playing to: ", strong(score_to()))
  })
  
  # Die icon indicating the active team
  output$active_die_left = renderUI({
    # switch_counter is a counter for how many times switch_sides 
    # even means that B should be on the left side
    switch_is_even = (ui_state$switch_counter %% 2 == 0)
    
    
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
    switch_is_even = (ui_state$switch_counter %% 2 == 0)
    
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
    game_state$current_scores$team_A
  })
  
  output$score_B = renderText({
    game_state$current_scores$team_B
  })
  

  output$recent_scores_rt = renderReactable({

    # Take the max sentence length to set width of column in col defs
    sentence_width = as.numeric(dbGetQuery(con, sql("SELECT MAX(char_length(what_happened)*6.5) FROM recent_scores")))
    
    column_defs = list(
      scoring_team = colDef(show = F),
      player_name = colDef(
        align = "right",
        minWidth = 80,
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
    reactable(head(game_session$recent_scores(), n = 5),
              compact = T, 
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
    game_state$error_msg
  })
  
  # Download button
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(game_state$scores_db, con)
    }
  )

# Game Summary Stats ------------------------------------------------------
  
  team_a_summary_stats = reactive({
    
    scores = game_session$db_tbls()[["scores"]]
    
    
    if(input$start_game == 0){
      past_games_scores = filter(scores, game_id != max(game_id))
      
      player_performance_summary(game_started = input$start_game, 
                                 player_stats = game_session$db_tbls()[["player_stats"]], 
                                 team_name = "A", 
                                 # current_round, 
                                 past_scores = past_games_scores)
    } else {
      past_games_scores = filter(scores, game_id != game_session$game_id)
      
      player_performance_summary(game_started = input$start_game, 
                                 game_session = game_session, game_state = game_state,
                                 player_stats = game_session$db_tbls()[["player_stats"]], 
                                 team_name = "A", 
                                 current_round = round_num(), 
                                 past_scores = past_games_scores)
    }
  }, label = "Team A Summary")
  
  team_b_summary_stats = reactive({
    
    scores = game_session$db_tbls()[["scores"]]
    
    
    if(input$start_game == 0){
      past_games_scores = filter(scores, game_id != max(game_id))
      
      player_performance_summary(game_started = input$start_game, 
                                 player_stats = game_session$db_tbls()[["player_stats"]], 
                                 team_name = "B", 
                                 # current_round, 
                                 past_scores = past_games_scores)
    } else {
      past_games_scores = filter(scores, game_id != game_session$game_id)
      
      player_performance_summary(game_started = input$start_game, 
                                 game_session = game_session, game_state = game_state,
                                 player_stats = game_session$db_tbls()[["player_stats"]], 
                                 team_name = "B", 
                                 current_round = round_num(), 
                                 past_scores = past_games_scores)
    }
  }, label = "Team A Summary")

  output$a_breakdown = renderPlot({
    
    max_player_points = max(team_b_summary_stats()$total_points, team_a_summary_stats()$total_points)+1
    
    if(input$start_game == 0){
      player_score_breakdown(snappaneers = select(filter(game_session$db_tbls()[["player_stats"]], game_id == max(game_id), team == "A"), player_id, team, shots), 
                             scores = filter(game_session$db_tbls()[["scores"]], game_id == max(game_id)), 
                             ps_players = game_session$players,
                             ps_team = "A",
                             chart_max = max_player_points)
    } else {
      player_score_breakdown(snappaneers = select(filter(game_state$player_stats_db, game_id == game_session$game_id, team == "A"), player_id, team, shots),
                             scores = game_state$scores_db, 
                             ps_players = game_session$players,
                             ps_game = game_session$game_id, 
                             ps_team = "A",
                             chart_max = max_player_points)
      
    }
  }, bg = snappa_pal[1])
  
  output$b_breakdown = renderPlot({
    max_player_points = max(team_b_summary_stats()$total_points, team_a_summary_stats()$total_points)+1
    
    if(input$start_game == 0){
      player_score_breakdown(snappaneers = select(filter(game_session$db_tbls()[["player_stats"]], game_id == max(game_id), team == "B"), player_id, team, shots), 
                             scores = filter(game_session$db_tbls()[["scores"]], game_id == max(game_id)), 
                             ps_players = game_session$players,
                             ps_team = "B",
                             chart_max = max_player_points)
    } else {
      player_score_breakdown(snappaneers = select(filter(game_state$player_stats_db, game_id == game_session$game_id, team == "B"), player_id, team, shots),
                             scores = game_state$scores_db, 
                             ps_players = game_session$players,
                             ps_game = game_session$game_id, 
                             ps_team = "B",
                             chart_max = max_player_points)
      
    }
  }, bg = snappa_pal[1])
  
  output$game_flow = renderPlot({
    if(input$start_game == 0){
      game_flow(player_stats = filter(game_session$db_tbls()[["player_stats"]], game_id == max(game_id)),
                players = game_session$players, 
                scores = filter(game_session$db_tbls()[["scores"]], game_id == max(game_id)),
                game = filter(game_session$db_tbls()[["game_stats"]], game_id == max(game_id))$game_id)
    } else {
      game_flow(player_stats = game_state$player_stats_db,
                players = game_session$players, 
                scores = game_state$scores_db,
                game = game_session$game_id)
      
    }
  })
  

  
  
  
  
  

  output$team_a_summary = renderReactable({

    team_summary_tab_rt(right_join(game_session$players, team_a_summary_stats(), by = "player_id"))
    
  })  
  
  
  
  output$team_b_summary = renderReactable({
    
    team_summary_tab_rt(right_join(game_session$players, team_b_summary_stats(), by = "player_id"))
    
  })  
  
  
  
  
  
  


# Career Stats module ---------------------------------------------------------

  careerStatsServer("career", con)
  
  
  
  
  

# Player Stats module --------------------------------------------------------

  playerStatsServer("player_stats", con)

  
  
  
  



# A little reactive styling for this bar, which looks really bad if it doesn't
# span nearly the entire page
set_plot_width <- function(session, output_width_name){
  function() { 
    session$clientData[[output_width_name]] 
  }
}



  # For debugging
  
  # output$db_output_players = renderTable({
  #   game_session$players
  # })
  # output$db_output_scores = renderTable({
  #   game_session$db_tbls()[["scores"]]
  # })
  # output$db_output_player_stats = renderTable({
  #   game_session$db_tbls()[["player_stats"]]
  # })
  # output$db_output_game_history = renderTable({
  #   game_session$db_tbls()[["game_stats"]]
  # })
  # output$snappaneers = renderTable({
  #   snappaneers()
  # })
  
  


# Events ------------------------------------------------------------------



# Incomplete Game -----------------------------------------------------


# Very start of game: display a popup message
# if the previous game is incomplete
  
observe({
  validate(
    need(
      !dbGetQuery(con, "SELECT game_complete FROM game_stats WHERE game_id = (SELECT MAX(game_id) FROM game_stats)")[1,1],
      message = FALSE
    )
  )
  lost_game_id = dbGetQuery(con, "SELECT game_id FROM game_stats WHERE game_id = (SELECT MAX(game_id) FROM game_stats)")[1,1]
  
  # Pass an additional check to see if the game which is in question is a 0-0 or not. 
  total_lost_game_score = dbGetQuery(con, str_c("SELECT SUM(total_points) FROM player_stats WHERE game_id = ", lost_game_id))[1,1] %>% 
    replace_na(0)
  
  # Discard that game if it's 0-0 and continue on with business as usual, else
  # allow players to restart
  if (total_lost_game_score == 0){
    delete_query = sql("DELETE FROM game_stats WHERE game_id = (SELECT MAX(game_id) FROM game_stats);")
    dbExecute(con, delete_query)
  } else {
    last_game_tbl = tbl(con, "incomplete_game")


    last_game_ps_tbl = last_game_tbl |> 
      select(game_id) |> 
      # Join player stats
      left_join(tbl(con, "player_stats"), by = "game_id") |> 
      inner_join(tbl(con, "players"), by = "player_id") |> 
      select(player_name, team, total_points)
  
    restart_game_popup(last_game_ps_tbl)
  }
  
})


# Game Start Validation ---------------------------------------------------

  
  observeEvent(input$switch_sides, {
    
    ui_state$switch_counter = ui_state$switch_counter+1
    
    switch_is_even = (ui_state$switch_counter %% 2 == 0)
    
    
    if(switch_is_even){
      removeUI("#ScoreboardUI", immediate=T)
      insertUI(selector = "#dice-row", ui = team_scoreboard_ui("B", "A"), where = "afterEnd")
    } else {
      removeUI("#ScoreboardUI", immediate = T)
      insertUI(selector = "#dice-row", ui = team_scoreboard_ui(), where = "afterEnd")
    }
    

  })
  
  need_unique_players = reactive({
    # All player names are unique
    length(unique(expected_player_inputs()$player_name)) == nrow(expected_player_inputs())
  })
  
  need_player_names = reactive({
    # All player names are non-empty
    all(expected_player_inputs()$player_name != "")
  })
  

  # Create a UI output which validates that there are four players and the names are unique
  output$validate_start = reactive({
    # If one of the first two players on each team
    # is removed, disable the button again.
    # This goes above the validate check because 
    # it needs to be updating before the validate
    # check is failed, or else the logic isn't
    # going to pass through
    
    #Record the players that you need to be looking for
    # (i.e., which ui elements are open right now?)
    
    
    # If the number of unique snappaneer names is the same as the number of active player inputs
    #   => enable start button
    # validate(
    #   need(need_unique_players(), label = "Unique players", message = "Player names need to be unique"),
    #   need(need_player_names(), label = "Empty player names", message = "Some player names are empty")
    # )

    
    # If the number of unique snappaneer names is not the same as the number of active player inputs
    #   => disable start button
    if(length(unique(expected_player_inputs()$player_name)) != nrow(expected_player_inputs()) |
       any(expected_player_inputs()$player_name == "")){ 
      
    shinyjs::disable("start_game")
    } else {
      shinyjs::enable("start_game")
    }
    

  })
  # Game Start --------------------------------------------------------------
  
  started = reactiveVal()
  
  # When we click "Start Game", 
  #   - Add new players to the players table
  #   - switch to the scoreboard
  #   - Set the score outputs and shot number to 0
  #   - Record the score we're playing to
  #   - Initialize the current game's player_stats table
  observeEvent(input$start_game, {
    
    if(as.integer(collect(tally(tbl(con, "incomplete_game")))) == 0){
      arena_select_popup()
    }
    
    
    # Setup a reactive poll for cooldowns to check if any casualty rules are still in effect
    # but have not made their way around the horn yet
    game_state$cooldowns = reactivePoll(
      intervalMillis = 100*70,
      session = session,
      # checkFunc = function() {dbGetQuery(con, sql(str_c("SELECT COUNT(*) FROM casualties 
      #                                                   WHERE game_id = ", game_session$game_id)))},
      checkFunc = function() {nrow(game_state$casualties)},
      valueFunc = function() {
        map(
          # Map over each type of score-based casualty
          unique(casualty_rules$casualty_title), 
          ~cooldown_check(casualties = game_state$casualties[game_state$casualties$casualty_type == .x, ], 
                          scores = game_state$scores_db, 
                          current_round = round_num(), 
                          rounds = rounds)) |> 
          # Set the names of the list
          set_names(unique(casualty_rules$casualty_title))}
    )
    
    
    
    shinyjs::enable("tifu")
    
    showNotification(str_c("Game is being played to ", input$score_to, " points!"), type = "message")
    
    started(T)

  })
  
  observeEvent(input$arena_select, {
    # Add new players to the players table
    iwalk(snappaneers()$player_name, function(die_thrower, index){
      # If the player is not in the players table
      if(!(die_thrower %in% game_session$players$player_name)){
        
        # Update the players database right here with the player name
        
        dbAppendTable(con, "players", 
                      tibble(
                        player_id = game_session$new_player_id,
                        player_name = die_thrower
                      )
        )
        
        # game_session$players = dbGetQuery(con, sql("SELECT player_id, player_name FROM players"))
        game_session$players = collect(tbl(con, "players"))
        
        # Increment the ID for the next new player
        game_session$new_player_id = game_session$new_player_id+1
        

      } else {
        invisible()
      }
    })
    
    # Check if the last game was finished
    # Switch to the scoreboard
    # Using isFALSE also denies character(0) in the event that we're starting on a fresh table. Nice!
    # LAST GAME WAS NOT FINISHED
    if (!dbGetQuery(con, "SELECT game_complete FROM game_stats WHERE game_id = (SELECT MAX(game_id) FROM game_stats)")[1,1]) {
      
      
      lost_game = dbGetQuery(con, "SELECT MAX(game_id) FROM game_stats")[1,1]
      
      lost_game_stats = dbGetQuery(con, str_c("SELECT * FROM game_stats WHERE game_id = ", lost_game))
      
      lost_player_stats = dbGetQuery(con, str_c("SELECT * FROM player_stats WHERE game_id = ", lost_game))
      
      
      # Set the score outputs and shot number to the values from the last game
      game_state$current_scores$team_A = dbGetQuery(con, str_c("SELECT SUM(total_points) FROM player_stats WHERE team = 'A' AND game_id = ", lost_game))[1,1] %>% 
        as.numeric()
      
      
      game_state$current_scores$team_B = dbGetQuery(con, str_c("SELECT SUM(total_points) FROM player_stats WHERE team = 'B' AND game_id = ", lost_game))[1,1] %>% 
        as.numeric()
      
      game_state$score_id = dbGetQuery(con, str_c("SELECT MAX(score_id) FROM scores WHERE game_id = ", lost_game))[1,1] %>%
        as.numeric()
      
      
      game_state$scores_db = dbGetQuery(con, str_c("SELECT * FROM scores WHERE game_id = ", lost_game))
      game_session$game_id = lost_game
      game_state$shot_num = parse_round_num(lost_game_stats$last_round)
      
      game_state$game_stats_db = lost_game_stats
      
      # Initialize the current game's player_stats table
      game_state$player_stats_db = lost_player_stats
      
      # Pull in lost game casualties 
      game_state$casualties = as_tibble(dbGetQuery(con, str_c("SELECT * FROM casualties WHERE game_id = ", lost_game)))
      
    } else {
      
      # LAST GAME WAS FINISHED
      
      # Set the score outputs and shot number to 0
      game_state$current_scores$team_A = 0
      game_state$current_scores$team_B = 0
      
      game_session$game_id = as.integer(dbGetQuery(con, "SELECT MAX(game_id)+1 FROM game_stats"))
      # Initialize the current game's game_stats table
      game_state$game_stats_db = bind_rows(game_state$game_stats_db,
                                     tibble(
                                       game_id = game_session$game_id,
                                       num_players = nrow(snappaneers()),
                                       game_start = strtrim(as.character(now(tzone = "America/Los_Angeles")), 19),
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
        value = game_state$game_stats_db,
        append = T
      )
      
      # Initialize the current game's player_stats table
      game_state$player_stats_db = aggregate_player_stats(game_state$scores_db, 
                                                    snappaneers(), 
                                                    game = game_session$game_id)
      
      dbWriteTable(
        conn = con, 
        name = "player_stats",
        value = game_state$player_stats_db,
        append = T
      )
    }
  })
  

# Halftime ----------------------------------------------------------------

  
  observeEvent(req(sum(game_state$scores_db$points_scored) >= score_to()), {
    sendSweetAlert(session, 
                   title = "Halftime", 
                   type = "info", 
                   timer = 2450,
                   timerProgressBar = T,
                   btn_labels = NA,
                   imageUrl = "gifs/futurama-change-places.gif", customClass = "halftime",
                   text = HTML(str_c("Change places!")), html = T)
    
    insertUI(selector = "#switch_sides",
             where = "afterEnd",
             ui = tags$audio(src = "change_places.mp3", type = "audio/mp3", autoplay = NA, controls = NA, class = "sound-effect"))

    shinyjs::click("switch_sides")
    
    # In the event that there was a sink which caused this, also popup the sink menu
    last_score = game_state$scores_db[ max(game_state$scores_db$score_id),]
    
    sink_casualty_popup(session, score_row = last_score, players = snappaneers()[snappaneers()$team != last_score$scoring_team, "player_name", drop=T])

  }, once = T, ignoreNULL = T)
  
  observeEvent(input$highnoon_manual, {
    highnoon_popup(snappaneers()$player_name)
  }, ignoreNULL = T)
  
  observeEvent(req(str_detect(round_num(), "^12[AB]")), {
    delay(runif(n = 1, min = 1000, max = 1200),
          {
            highnoon_popup(snappaneers()$player_name)
            })
    
    
    

    # In the event that there was a sink which caused this, also popup the sink menu
    last_score = game_state$scores_db[ max(game_state$scores_db$score_id),]
    
    sink_casualty_popup(session, score_row = last_score, players = snappaneers()[snappaneers()$team != last_score$scoring_team, "player_name", drop=T])
    
  }, once = T, ignoreNULL = T)
  

game_summary = reactive({
  # If game has not started:
  if (input$start_game == 0 | is_integer(pluck(reactiveValuesToList(session$input), "send_to_db"))){
    # Display the last game in the database
    df = filter(game_session$db_tbls()[["game_stats"]], game_id == max(game_id))
    subtitle_a = if_else(df$points_a > df$points_b, "the winners.", "the losers.")
    subtitle_b = if_else(df$points_a < df$points_b, "the winners.", "the losers.")
    
    # Return a list object with the data and both subtitles
    list(
      df = df,
      subtitle_a = subtitle_a,
      subtitle_b = subtitle_b
    )
    
  } else {
    # If the game HAS started
    # - Use current game data
    df = replace_na(game_state$game_stats_db, list(points_a = game_state$current_scores$team_A, 
                                             points_b = game_state$current_scores$team_B))
    
    # Calculate score difference for the flavour text
    score_difference = abs(df$points_a - df$points_b)
    
    subtitle_a = if_else(df$points_a > df$points_b, 
                         "in the lead.", 
                         str_c("chasing ", score_difference, ".")
    )
    subtitle_b = if_else(df$points_a < df$points_b, 
                         "in the lead.", 
                         str_c("chasing ", score_difference, ".")
    )
    # Return a list object with the data and both subtitles
    list(
      df = df,
      subtitle_a = subtitle_a,
      subtitle_b = subtitle_b
    )
    
  }
  
})



# When game summary input is pressed
observeEvent(input$game_summary, {

  game_summary_modal(game_summary()$df, round_num(), 
                 game_summary()$subtitle_a, game_summary()$subtitle_b)
  


})

  
  
# Restart a game after indicating you would like to do so
  observeEvent(input$resume_yes, {
    
    # lost_game = as.integer(dbGetQuery(con, "SELECT MAX(game_id) FROM game_stats"))
    lost_game = tbl(con, "incomplete_game")
    
    # lost_player_stats = dbGetQuery(con, str_c("SELECT * FROM player_stats WHERE game_id = ", lost_game))
    lost_player_stats = lost_game |> 
      select(game_id) |> 
      left_join(tbl(con, "player_stats"), by = "game_id") |> 
      left_join(tbl(con, "players"), by = "player_id")
      
    lost_players = lost_player_stats |> 
      select(player_name, team) |> 
      group_by(team) |> 
      mutate(player_input = str_c("name_", team, row_number())) |> 
      ungroup() |>  
      collect()
    
    input_list = lost_players |> 
      select(player_input, player_name) |> 
      deframe()
    
    iwalk(input_list, function(name, id){
      
      updateSelectizeInput(session, inputId = id, selected = name)
      
    })
    
    
    
    lost_game_id = collect(lost_game)$game_id
    # Set the score outputs and shot number to the values from the last game
    game_state$current_scores$team_A = dbGetQuery(con, str_c("SELECT SUM(total_points) FROM player_stats WHERE team = 'A' AND game_id = ", lost_game_id))[1,1] %>% 
      as.numeric()
    
    
    game_state$current_scores$team_B = dbGetQuery(con, str_c("SELECT SUM(total_points) FROM player_stats WHERE team = 'B' AND game_id = ", lost_game_id))[1,1] %>% 
      as.numeric()
    
    game_state$score_id = dbGetQuery(con, str_c("SELECT MAX(score_id) FROM scores WHERE game_id = ", lost_game_id))[1,1] %>%
      as.numeric()
    
    
    game_state$scores_db = dbGetQuery(con, str_c("SELECT * FROM scores WHERE game_id = ", lost_game_id))
    game_session$game_id = lost_game_id
    game_state$shot_num = parse_round_num(collect(lost_game)$last_round)
    
    game_state$game_stats_db = collect(lost_game)
    
    # Initialize the current game's player_stats table
    game_state$player_stats_db = collect(lost_player_stats)
    
    # Pull in lost game casualties 
    game_state$casualties = as_tibble(dbGetQuery(con, str_c("SELECT * FROM casualties WHERE game_id = ", lost_game_id)))
    
    removeModal()
    
    delay(500, shinyjs::click("start_game"))
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
      need(game_state$shot_num > 1, label = "Can't go below 0", message = "It's the first round still")
    )
    game_state$shot_num = game_state$shot_num-1
    
    # This is for the case when there hasn't been a scoring point yet, which causes this to fail in the transition
    # between rounds 1A and 1B. Clumsy, perhaps, but it works
      # Update player stats in the app
      game_state$player_stats_db = aggregate_player_stats(game_state$scores_db, snappaneers(), game = game_session$game_id)    
      #Update the DB with the new player_stats
      db_update_player_stats(game_state$player_stats_db, round_button = T)
      
      # Update round in game stats
      db_update_round(round = round_num(), game = game_session$game_id)
      
      
    
  })
  

  
  
  # When next round button is pushed
  observeEvent(input$next_round, {
    
    if (game_state$rebuttal_tag == T){
      if (game_state$rebuttal == T){
        click("finish_game")
        game_state$shot_num = game_state$shot_num - 1
      } else {
        game_state$rebuttal_tag = F
      }
    } else{
    }
    
    game_state$shot_num = game_state$shot_num+1
    if (game_state$current_scores$team_A == 0 &
        game_state$current_scores$team_B == 0){
      invisible()
    } else {
      
    # Update player stats in the app
    game_state$player_stats_db = aggregate_player_stats(game_state$scores_db, snappaneers(), game = game_session$game_id)    
    #Update the DB with the new player_stats (adds to shots)
    db_update_player_stats(game_state$player_stats_db, round_button = T)
    }

    game_state$rebuttal = rebuttal_check(a = game_state$current_scores$team_A, b = game_state$current_scores$team_B,
                                   round = round_num(), points_to_win = score_to())
    
    # Update round in game stats
    db_update_round(round = round_num(), game = game_session$game_id)
    
    if (game_state$rebuttal == T) {
      game_state$rebuttal_tag = T
      
      game_notification(rebuttal = T, 
                        round = round_num(),
                        current_scores = game_state$current_scores)
      
    } else {
    }
      
    })
  

# Score notifications -----------------------------------------------------
  
  observe({
    req(started() == T)
    validate(
      need(vctrs::vec_in(game_state$current_scores,
                         haystack = casualty_rules[,1:2]), label = "casualty score"),
      need(purrr::none(game_state$cooldowns(), rlang::is_true), label = "cooldowns")
    )
    casualty_popup(session,
                   score = game_state$current_scores,
                   rules = casualty_rules,
                   players = snappaneers()$player_name)
  })
  
  observeEvent(input$casualty_manual, {
    casualty_popup(session,
                   score = game_state$current_scores,
                   rules = casualty_rules,
                   players = snappaneers()$player_name)
  })

  observeEvent(ignoreInit = T,
               c(
                 input$casualty,
                 input$highnoon
               ), {
                 # browser()
    if(is.null(input$casualty) && is.character(input$highnoon)){
      
      # Convert player name to ID
      casualty = select(snappaneers(), starts_with("player")) %>% 
        deframe() %>% 
        pluck(input$highnoon)
      
      # Insert casualty details
      new_casualty = tibble(
        casualty_id = as.numeric(dbGetQuery(con, sql("SELECT MAX(casualty_id)+1 FROM casualties"))),
        game_id = game_session$game_id,
        score_id = game_state$score_id,
        player_id = casualty,
        casualty_type = "High noon",
        reported_player = NA_integer_#,
        # round = round_num()
      )
    } else {
      validate(
        need(input$casualty, label = "Casualty")
      )
      # Convert player name to ID
      casualty = select(snappaneers(), starts_with("player")) %>% 
        deframe() %>% 
        pluck(input$casualty)
      
      type = casualty_rules$casualty_title[vctrs::vec_match(game_state$current_scores, haystack = casualty_rules[, 1:2])]
      
      # Insert casualty details
      new_casualty = tibble(
        casualty_id = as.numeric(dbGetQuery(con, sql("SELECT MAX(casualty_id)+1 FROM casualties"))),
        game_id = game_session$game_id,
        score_id = game_state$score_id,
        player_id = casualty,
        casualty_type = type,
        reported_player = NA_integer_#,
        # round = round_num()
      )
    }
    validate(
      need(is_tibble(new_casualty), label = "New casualty")
    )

    # Add to casualties reactive
    game_state$casualties = add_row(game_state$casualties, new_casualty)
    
    # Add to db
    dbWriteTable(
      conn = con, 
      name = "casualties", 
      value = new_casualty,
      append=T
    )
    
    
    # In the event that there was a sink which caused this, also popup the sink menu
    last_score = game_state$scores_db[ max(game_state$scores_db$score_id),]
    sink_casualty_popup(session, score_row = last_score, players = snappaneers()[snappaneers()$team != last_score$scoring_team, "player_name", drop=T])
  })
  
  observeEvent(input$sink_casualty, {
    # Convert player name to ID
    casualty = select(snappaneers(), starts_with("player")) %>% 
      deframe() %>% 
      pluck(input$sink_casualty)
    
    # Insert casualty details
    new_casualty = tibble(
      casualty_id = as.numeric(dbGetQuery(con, sql("SELECT MAX(casualty_id)+1 FROM casualties"))),
      game_id = game_session$game_id,
      score_id = game_state$score_id,
      player_id = casualty,
      casualty_type = "Sunk",
      reported_player = NA_integer_
    )
    
    # Add to casualties reactive
    game_state$casualties = add_row(game_state$casualties, new_casualty)
    
    # Add to db
    dbWriteTable(
      conn = con, 
      name = "casualties", 
      value = new_casualty,
      append=T
    )  
    
    insertUI(selector = "#switch_sides",
             where = "afterEnd",
             ui = tags$audio(src = "sploosh.mp3", type = "audio/mp3", autoplay = NA, controls = NA, class = "sound-effect"))
    
    })
  
  observeEvent(input$tifu, {
    showModal(
      tifu_casualty_popup(players = isolate(snappaneers()))
    )
    
  })
  
  output$casualty_validation = renderUI({
    if(input$casualty_type == "Team sink"){
      sinker_team = snappaneers()[snappaneers()$player_id == input$tifu_accused, "team", drop = T]
      sinkee_team = snappaneers()[snappaneers()$player_id == input$tifu_casualty, "team", drop = T]
      
      # Team sink: 
      validate(
        # both players are on same team
        need(sinker_team == sinkee_team, message = "That's not a team sink!"),
        # players are not the same
        need(input$tifu_accused != input$tifu_casualty, message = "That's a self sink!")
      )
    } else {
      # Self sink: 
      validate(
        # players are the same
        need(input$tifu_accused == input$tifu_casualty, message = "That's not a self sink!")
      )
    }

    actionButton("tifu_confirm", label = "Report", class = "btn-primary", style = "background-color: var(--red);color: var(--bg-col);")
  })
  


  observeEvent(input$tifu_confirm, {

    # Insert casualty details
    new_casualty = tibble(
      casualty_id = as.numeric(dbGetQuery(con, sql("SELECT MAX(casualty_id)+1 FROM casualties"))),
      game_id = game_session$game_id,
      score_id = NA_integer_,
      player_id = as.integer(input$tifu_casualty),
      casualty_type = input$casualty_type,
      reported_player = if_else(input$tifu_casualty == input$tifu_accused, NA_integer_, as.integer(input$tifu_accused))
    )

    # Add to casualties reactive
    game_state$casualties = add_row(game_state$casualties, new_casualty)
    
    # Add to db
    dbWriteTable(
      conn = con, 
      name = "casualties", 
      value = new_casualty,
      append=T
    )  
    
    removeModal()
    
    showNotification(if_else(input$casualty_type == "Team sink", 
                             str_c("Nothing wrong with just a little bit of horseplay every now and then, ", 
                                   players_tbl[match(as.integer(input$tifu_casualty), players_tbl$player_id), 2], "!"),
                             "The good news is that you only get 1 peasant point!"
                             ), duration = 7, closeButton = F)
  })
  

# New Players -------------------------------------------------------------

  # Enable/disable extra player inputs
  # Also reset them upon disabling
  
  ## Team A
  ### Player A3
  observe({
    toggleState(id = "player-input-A3", condition = input$add_player_A3)
  })
  observeEvent(isFALSE(input$add_player_A3), {
    reset("player-input-A3")
  }, ignoreInit = T)
  
  ### Player A4
  observe({
    toggleState(id = "player-input-A4", condition = input$add_player_A4)
  })
  observeEvent(isFALSE(input$add_player_A4), {
    reset("player-input-A4")
  }, ignoreInit = T)
  
  
  ### Player A5
  observe({
    toggleState(id = "player-input-A5", condition = input$add_player_A5)
  })
  observeEvent(isFALSE(input$add_player_A5), {
    reset("player-input-A5")
  }, ignoreInit = T)
  
  ## Team B
  ### Player B3
  observe({
    toggleState(id = "player-input-B3", condition = input$add_player_B3)
  })
  observeEvent(isFALSE(input$add_player_B3), {
    reset("player-input-B3")
  }, ignoreInit = T)
  
  ### Player B4
  observe({
    toggleState(id = "player-input-B4", condition = input$add_player_B4)
  })
  observeEvent(isFALSE(input$add_player_B4), {
    reset("player-input-B4")
  }, ignoreInit = T)
  
  ### Player B5
  observe({
    toggleState(id = "player-input-B5", condition = input$add_player_B5)
  })
  observeEvent(isFALSE(input$add_player_B5), {
    reset("player-input-B5")
  }, ignoreInit = T)

  

# Scoring -----------------------------------------------------------------
  
  #TODO: Fix score_id, game_id, and num_points_scored in scores_db in vals: change from dbl to int


## Team A ------------------------------------------------------------------

  
  observeEvent(input$A_score_button, {
    game_state$error_msg <- NULL
    
    # eligible_shooters = filter(snappaneers(), team == "A") %>% 
    eligible_shooters = snappaneers()[snappaneers()$team == "A", "player_name", drop = T] %>% 
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
    game_state$score <- score
    
    
    # Check score i not null, remove the dialog box
    if (!is.null(game_state$score)) {
      removeModal()
      game_state$print <- TRUE
      
      # Update the team score
      game_state$current_scores$team_A = game_state$current_scores$team_A + game_state$score
      
      # Increment the score_id
      game_state$score_id = as.integer(game_state$score_id+1)
      
      ## Identify scoring characteristics
      # Player ID
      # scorer_pid = pull(filter(game_session$players, player_name == input$scorer), player_id)
      scorer_pid = game_session$players[game_session$players$player_name == input$scorer, "player_id", drop=T]
      
      # Were they shooting?
      # scorers_team = pull(filter(snappaneers(), player_name == input$scorer), team) # pull the scorer's team from snappaneers
      scorers_team = snappaneers()[snappaneers()$player_name == input$scorer, "team", drop=T] # pull the scorer's team from snappaneers
      shooting_team_lgl = all(str_detect(round_num(), "A"), scorers_team == "A") # Are they on team A & did they score for team A?
      
      new_score = tibble(
        score_id = game_state$score_id,
        game_id = game_session$game_id,
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
      game_state$scores_db = bind_rows(game_state$scores_db,
                                 new_score)
      
      #Update the db with the new score
      dbWriteTable(con, "scores", 
                   anti_join(game_state$scores_db, dbGetQuery(con, str_c("SELECT * FROM scores WHERE game_id = ", game_session$game_id)), 
                             by = "score_id"), 
                   append = T)
      
      
      # Update player stats table
      game_state$player_stats_db = aggregate_player_stats(game_state$scores_db, snappaneers(), game = game_session$game_id)
      db_update_player_stats(game_state$player_stats_db, specific_player = scorer_pid)

      # Congratulate paddlers
      # if(input$paddle & str_detect(pull(filter(snappaneers(), player_name == input$scorer), team), "[Aa]") ){
      if(input$paddle & str_detect(snappaneers()[snappaneers()$player_name == input$scorer, "team", drop=T], "[Aa]") ){
        game_notification()
      }
      # if(input$paddle & str_detect(pull(filter(snappaneers(), player_name == input$scorer), team), "[Bb]") ){
      if(input$paddle & str_detect(snappaneers()[snappaneers()$player_name == input$scorer, "team", drop=T], "[Bb]") ){
        showNotification("It's a bold strategy Cotton, let's see if it pays off for them.")
      }
    } else {
      game_state$error_msg <- "You did not input anything."
    }
    
    
    # If the game is in rebuttal, remind players
    # of the points needed to bring it back
    game_state$rebuttal = rebuttal_check(game_state$current_scores$team_A, 
                                   game_state$current_scores$team_B,
                                   round_num(), score_to())
    

    #    if (!is.null(game_state$rebuttal)) {
    if (game_state$rebuttal == T & game_state$rebuttal_tag == T) {
      game_notification(rebuttal = T, 
                        round = round_num(),
                        current_scores = game_state$current_scores)
    } else {
      
    }
    # A fix to issue 45 where games would be prompted to end even though
    # a team has technically left rebuttal (meaning tag needs to be false)
    
    if (game_state$rebuttal_tag == T & game_state$rebuttal == F){
      game_state$rebuttal_tag = F
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
  
  ## Team B ---------------------------------------------------------
  
  
  observeEvent(input$B_score_button, {
    game_state$error_msg <- NULL
    
    eligible_shooters = snappaneers()[snappaneers()$team == "B", "player_name", drop = T] %>% 
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
    game_state$score <- score
    
    if (!is.null(game_state$score)) {
      removeModal()
      game_state$print <- TRUE
      
      # Update Team B's score
      game_state$current_scores$team_B = game_state$current_scores$team_B + game_state$score
      
      # Increment the score_id
      game_state$score_id = as.integer(game_state$score_id+1)
      
      ## Identify scoring characteristics
      # Player ID
      # scorer_pid = pull(filter(game_session$players, player_name == input$scorer), player_id)
      scorer_pid = game_session$players[game_session$players$player_name == input$scorer, "player_id", drop=T]
      # Were they shooting?
      # scorers_team = pull(filter(snappaneers(), player_name == scorer_pid), team)
      scorers_team = snappaneers()[snappaneers()$player_name == input$scorer, "team", drop=T] # pull the scorer's team from snappaneers
      shooting_team_lgl = all(str_detect(round_num(), "[Bb]"), scorers_team == "B")
      
      new_score = tibble(
        score_id = game_state$score_id,
        game_id = game_session$game_id,
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
      game_state$scores_db = bind_rows(game_state$scores_db,
                                 new_score)
      #Update the db with the new score
      dbWriteTable(con, "scores", 
                   anti_join(game_state$scores_db, dbGetQuery(con, str_c("SELECT * FROM scores WHERE game_id = ", game_session$game_id)), 
                             by = "score_id"), 
                   append = T)
      
      
      # Update player stats in the app
      game_state$player_stats_db = aggregate_player_stats(game_state$scores_db, 
                                                     snappaneers(), 
                                                     game = game_session$game_id)    
      #Update the DB with the new player_stats
      db_update_player_stats(game_state$player_stats_db, specific_player = scorer_pid)
      
      
      # Congratulate paddlers for good offense, chide those who paddled against their own team
      if(input$paddle & str_detect(snappaneers()[snappaneers()$player_name == input$scorer, "team", drop=T], "[Bb]") ){
        game_notification()
      }
      if(input$paddle & str_detect(snappaneers()[snappaneers()$player_name == input$scorer, "team", drop=T], "A") ){
        showNotification("It's a bold strategy Cotton, let's see if it pays off for them.")
      }
    } else {
      game_state$error_msg <- "You did not input anything."
    }
    
    
    # If the game is still in rebuttal in rebuttal, remind players
    # of the points needed to bring it back
    game_state$rebuttal = rebuttal_check(game_state$current_scores$team_A, 
                                   game_state$current_scores$team_B,
                                   round_num(), score_to())
    
    #    if (!is.null(game_state$rebuttal)) {
    if (game_state$rebuttal == T & game_state$rebuttal_tag == T) {
      game_notification(rebuttal = T, 
                        round = round_num(),
                        current_scores = game_state$current_scores)
    } else {
      
    }
    
    if (game_state$rebuttal_tag == T & game_state$rebuttal == F){
      game_state$rebuttal_tag = F
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
  
  

## Undo Score --------------------------------------------------------------

  # Undo score consists of:
  # - Observe the press of the undo score button
  # - Present confirmation with details of the last score
  # - Observe the confirmation of the undo score button
  # - Remove score from the database
  
  ## Observe press of undo score button
  # Team A
  observeEvent(input$undo_score_A, {
    validate(
      need(game_state$current_scores$team_A > 0, label = "Team A hasn't scored yet!")
    )

    # Select the ID which is the max on Team A
    last_score = filter(game_state$scores_db, scoring_team == "A") %>% 
      pull(score_id) %>% 
      max()
    
    # Pull the number of points the last score was worth
    last_score_pts = filter(game_state$scores_db, score_id == last_score) %>% 
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
      need(game_state$current_scores$team_B > 0, label = "Team B hasn't scored yet!")
    )
    
    # Select the ID which is the max on Team A
    last_score = filter(game_state$scores_db, scoring_team == "B") %>% 
      pull(score_id) %>% 
      max()
    
    # Pull the number of points the last score was worth
    last_score_pts = filter(game_state$scores_db, score_id == last_score) %>% 
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
      need(game_state$current_scores$team_A > 0, label = "Team A hasn't scored yet!")
    )
    
    # Find the highest score on each team and keep only team A
    last_score = filter(group_by(game_state$scores_db, scoring_team), scoring_team == "A", score_id == max(score_id)) %>% 
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
    col_list = last_score_col_list
    
    # map over event columns and keep those which are true
    emoji_to_show = map(select(last_score, paddle:foot), isTRUE) %>% 
      keep(isTRUE) %>% 
      names()
    
    # Subset the column definitions
    cols_to_show = col_list[c("player_name", "round_num", "points_scored", emoji_to_show)]
    
    select(last_score, player_name, round_num, points_scored, any_of(emoji_to_show)) %>% 
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
      need(game_state$current_scores$team_B > 0, label = "Team B hasn't scored yet!")
    )
    
    # Find the highest score on each team and keep only team B
    last_score = filter(group_by(game_state$scores_db, scoring_team), scoring_team == "B", score_id == max(score_id)) %>% 
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
    
    select(last_score, player_name, round_num, points_scored, any_of(emoji_to_show)) %>% 
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
    last_score = game_state$scores_db[game_state$scores_db$scoring_team == "A", "score_id"] %>% 
      max()
    
    # Pull the number of points the last score was worth
    last_score_row = game_state$scores_db[game_state$scores_db$score_id == last_score, c("points_scored", "clink")]
    # Reduce the score ID for any scores which have happened since the score which is being removed
    # Note that score undo-ing is team specific
    game_state$scores_db = game_state$scores_db[game_state$scores_db$score_id != last_score, ] %>% 
      mutate(score_id = if_else(score_id > last_score, as.integer(score_id-1), score_id))
    # Reduce the team's score and score_id
    game_state$current_scores$team_A = game_state$current_scores$team_A - last_score_row$points_scored
    game_state$score_id = as.integer(game_state$score_id-1)
    
    #Remove the value from the snappaDB
    dbExecute(con,
                str_c("DELETE FROM scores WHERE score_id = ", last_score, " AND game_id = ", game_session$game_id, ";")
    )
    
    
    # Update player stats table in the app
    game_state$player_stats_db = aggregate_player_stats(game_state$scores_db, snappaneers(), game = game_session$game_id)
    #Update the DB with the new player_stats
    db_update_player_stats(game_state$player_stats_db)
    
    # Remove any associated sink casualty
    if(vctrs::vec_in(last_score_row, tribble(~points_scored, ~clink, 
                                             3, F,
                                             5, T,
                                             7, T))){
      # In database
      dbExecute(con,
                  str_c("DELETE FROM casualties WHERE score_id = ", last_score, 
                        " AND game_id = ", game_session$game_id,
                        " AND casualty_type = 'Sunk'")
      )
      # In reactive
      game_state$casualties = game_state$casualties[!((game_state$casualties$score_id == last_score) & game_state$casualties$casualty_type == "Sunk"), ]
    }
      
    
  })
  
  # Team B
  # Undo score B
  observeEvent(input$undo_score_B_confirm, {
    validate(
      need(isTRUE(input$undo_score_B_confirm), label = "Nothin' to see here..")
    )
    
    # Select the ID which is the max on Team B
    last_score = filter(game_state$scores_db, scoring_team == "B") %>% 
      pull(score_id) %>% 
      max()
    
    # Pull the number of points the last score was worth
    last_score_row = filter(game_state$scores_db, score_id == last_score) %>% 
      select(points_scored, clink)
    
    # Reset any scores which have happened since the score being erased
    game_state$scores_db = filter(game_state$scores_db, score_id != last_score) %>% 
      mutate(score_id = if_else(score_id > last_score, as.integer(score_id-1), score_id))
    
    game_state$current_scores$team_B = game_state$current_scores$team_B - last_score_row$points_scored
    game_state$score_id = as.integer(game_state$score_id-1)
    
    
    #Remove the value from the snappaDB
    dbExecute(con,
                str_c("DELETE FROM scores WHERE score_id = ", last_score, 
                      " AND game_id = ", game_session$game_id)
    )    
    # Update player_stats 
    game_state$player_stats_db = aggregate_player_stats(game_state$scores_db, snappaneers(), game = game_session$game_id)
    #Update the DB with the new player_stats
    db_update_player_stats(game_state$player_stats_db)
    
    # Remove any associated sink casualty
    if(vctrs::vec_in(last_score_row, tribble(~points_scored, ~clink, 
                                             3, F,
                                             5, T,
                                             7, T))){
      # In database
      dbExecute(con,
                  str_c("DELETE FROM casualties WHERE score_id = ", last_score, 
                        " AND game_id = ", game_session$game_id,
                        " AND casualty_type = 'Sunk'")
      )
      # In reactive
      game_state$casualties = filter(game_state$casualties, !((score_id == last_score) & casualty_type == "Sunk"))
    }
    
  })
  
  
  

# End of the game ---------------------------------------------------------

  
  
  observeEvent(input$finish_game, {

    shinyWidgets::confirmSweetAlert(
      inputId = "finish_game_sure",
      title = "Finish game",
      text = "Are you sure?",
      type = "warning"
    )

  })
  
  observeEvent(input$finish_game_sure, {
    game_state$game_over = T
    finalize_game(game_session, game_state, con, snappaneers(), score_to(), round_num(), session)
    game_summary_modal(game_summary()$df, round_num(),
                       game_summary()$subtitle_a, game_summary()$subtitle_b)
  })
  

# Send to DB --------------------------------------------------------------

  
  
  observeEvent(input$send_to_db, {
    finalize_game(game_session, game_state, con, snappaneers(), score_to(), round_num(), session)
    game_summary_modal(game_summary()$df, round_num(),
                       game_summary()$subtitle_a, game_summary()$subtitle_b)
  })
  




  
}

# Disconnect from the server at the end  
onStop(function() {
  poolClose(con)
})

# Run the application 
shinyApp(ui = ui, server = server)





# Notes -------------------------------------------------------------------

