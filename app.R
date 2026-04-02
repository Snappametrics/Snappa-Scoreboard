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

  snappa_server(input, output, session, game_session, game_state, ui_state, con)
}

# Disconnect from the server at the end  
onStop(function() {
  poolClose(con)
})

# Run the application 
shinyApp(ui = ui, server = server)





# Notes -------------------------------------------------------------------

