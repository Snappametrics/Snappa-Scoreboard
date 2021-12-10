#' Scoreboard UI
#' Handles all of the UI elements on the scoreboard screen
#' 
#' @param id The namespace of the module

teamUI = function(id, name){
  ns = NS(id)
  team_colours = list("A" = "#e26a6a", "B" = "#2574a9")
  
  wellPanel(
    class = paste0("scoreboard-well ", "well-", str_sub(id, start = -1)), 
    # style = paste(well_panel_style, team_colours[[id]]),
    
    # Header
    h1(class = 'team-name',
       name),
    
    
    # Score
    h2(class = "team-score numbers", 
       textOutput(ns("score"))),
    
    # Score button
    div(class = paste("score-and-undo", id),
        actionBttn(ns("score-input"), 
                   label = "We scored!", color = "danger",
                   size = "lg"),
        actionBttn(
          inputId = ns("undo_score"),
          label = "Undo", style = "unite", color = "danger", icon = icon("undo"), size = "md"
        )
    )
  )
}

scoreboardUI = function(id){
  ns = NS(id)
  
  div(
    # Top row
    fluidRow(
      id = "dice-row", 
       column(4, align = "center", 
              uiOutput("active_die_left")),
      
       column(4, align = "center",
              actionBttn(ns("switch_sides"), 
                         "Switch Sides", style = "material-flat", 
                         color = "primary", 
                         icon = icon("sync"), size = "sm")),
      
       column(4, align = "center", 
              uiOutput("active_die_right"))
    ),
    
    fluidRow(
      # Left Team
      column(width = 4, align = "center",
             
             teamUI(ns("A"), name = "Team A")
             
      ), 
      # Round
      column(width = 4, align = "center", style = 'padding: 0;',
             div(id = 'scoreboard_center_controls',
                 h1("Round", style = "font-size: 5rem; font-weight: 600;"),
                 uiOutput(ns("round_num")),
                 uiOutput(ns("round_control_buttons"))
             )
             
      ),
      # Team B
      column(width = 4, align = "center",
             
             teamUI(ns("B"), name = "Team A")
      )
    )

  )
}