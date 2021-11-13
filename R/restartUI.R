#' Summons the modal dialog to display information about the game which has been lost in the records
#' 
#' @param team_A_points How many points team A had when the scoreboard was halted.
#' @param team_B_points How many points team B had when the scoreboard was halted.
#' @return The UI elements which display the restart game modal



restartUI = function(id, scores) {
  ns = NS(id)
  # Restart game dialog box
  modalDialog(
    title = "Hol' up, did you want to continue the previous game?",
    style = str_c("background-color:", snappa_pal[1], ";"),
    fluidRow(
      style = 'display:flex;',
      
      # Team A information
      column(3,
             align = 'center',
             class = 'col-sm-offset-2',
              h3(class = 'modal_team_title', id = 'modal_team_title_A',
                'Team A'),
              h3(class = 'modal_team_points', id = 'modal_team_points_A',
                # team_A_points)
                scores$team_A)
      ),
      
      # Round information
      column(2,
             align = 'center',
             style = 'align-self: center',
             withSpinner(uiOutput(ns('round_num')), type = 1)
      ),
      
      # Team B information
      column(3,
             align = 'center',
                h3(class = 'modal_team_title', id = 'modal_team_title_B',
                'Team B'),
                h3(class = 'modal_team_points', id = 'modal_team_points_B',
                # team_B_points)
                scores$team_B)
      ),
      column(2)
    ),
    
    
    fluidRow(
      # Team A summary table
      column(6, align = 'center',
             withSpinner(reactableOutput(ns("summary-A")), type = 1)
      ),
      
      # Team B summary table
      column(6, align = 'center', 
             withSpinner(reactableOutput(ns("summary-B")), type = 1)
      )
    ),
    footer = tagList(
      fluidRow(
        div(class = 'restart_game_footer',
            
            # Context of game restart choice
          column(8, align = "left",
                 helpText(
                   h4('Would you like to continue the previous game?',
                      style = "color: black; display: inline-block; padding: 1.5vh; font-size: 2rem; font-weight: 600;"),
                   span("This will delete the game from our database", 
                        style = "color: red; display: inline-block; padding: 1.5vh; font-size: 2rem; font-weight: 600;")
               )
        ),
        # Game restart buttons
        # - Continue
        column(2,
               div(class = 'restart_game_control',
                 actionBttn(ns("continue"),
                            label = "Continue Game",
                            style = "material-flat", 
                            color = "default",
                            size = "md")
               )
        ),
        # Game restart buttons
        # - Delete
        column(2,
               div(class = 'restart_game_control', 
                 actionBttn(ns("delete"),
                            label = "Delete Game",
                            style = "material-flat",
                            color = "danger",
                            size = "md")
               )
        )
        )
      )
    ),
    size = "l",
    easyClose = F,
    fade = F
  )
  
}