#' Summons the modal dialog to display information about the game which has been lost in the records
#' 
#' @param team_A_points How many points team A had when the scoreboard was halted.
#' @param team_B_points How many points team B had when the scoreboard was halted.
#' @return The UI elements which display the restart game modal



restart_game_UI = function(id, team_A_points, team_B_points) {
  ns <- NS(id)
  modalDialog(
    title = "Hol' up, did you want to continue the previous game?",
    style = str_c("background-color:", snappa_pal[1], ";"),
    fluidRow(
      style = 'display:flex;',
      column(3,
             align = 'center',
             class = 'col-sm-offset-2',
              h3(class = 'modal_team_title', id = 'modal_team_title_A',
                'Team A'),
              h3(class = 'modal_team_points', id = 'modal_team_points_A',
                team_A_points)
      ),
      column(2,
             align = 'center',
             style = 'align-self: center',
             withSpinner(uiOutput(ns('round_num')), type = 1),
             br()
      ),
      column(3,
             align = 'center',
                h3(class = 'modal_team_title', id = 'modal_team_title_B',
                'Team B'),
                h3(class = 'modal_team_points', id = 'modal_team_points_B',
                team_B_points)
      ),
      column(2)
    ),
    fluidRow(
      column(6, align = 'center',
             withSpinner(reactableOutput(ns('summary_table_A')),
                  type = 1)
      ),
      column(6, align = 'center', 
             withSpinner(reactableOutput(ns('summary_table_B')),
                  type = 1)
      )
    ),
    footer = tagList(
      fluidRow(
        div(class = 'restart_game_footer',
          column(8, align = "left",
                 helpText(
                   h4('Would you like to continue the previous game?',
                      style = "color: black; display: inline-block; padding: 1.5vh; font-size: 2rem; font-weight: 600;"),
                   span("This will delete the game from our database", 
                        style = "color: red; display: inline-block; padding: 1.5vh; font-size: 2rem; font-weight: 600;")
               )
        ),
        column(2,
               div(class = 'restart_game_control',
                 actionBttn(ns("restart_incomplete_game"),
                            label = "Continue Game",
                            style = "material-flat", 
                            color = "default",
                            size = "md")
               )
        ),
        column(2,
               div(class = 'restart_game_control', 
                 actionBttn(ns("delete_incomplete_game"),
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