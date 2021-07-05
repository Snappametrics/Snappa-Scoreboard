#' Validate start module
#' 
#' This is, admittedly, one part attempt at improving the internal logic of validate
#' start, and one part seeing if the logic of using modules for granular processes
#' like this makes sense. Hopefully yes, but we shall see. 
#' 
#' This module server runs tests for each team's inputs, and determines if they
#' make sense. Outer app logic will run tests for cross-compatability, i.e. whether
#' or not the inputs for both teams mesh with each other. 
#' 
#' @param id The namespace of the module, like 'validate_A' or 'validate_B'
#' @param team_size How many players have been selected for the team. Received from 
#'   input$team_A_size and input$team_B_size in app.R
#' @param player_inputs A reactive vector of the inputs for each team, used for testing. \
#' 
#' @return A list of the results of testing conditions and error messages
#' \describe{
#'     \item 
#' }
validate_start_server = function(id, team_size, player_inputs) {
  moduleServer(id, 
               function(input, output, session) {
                # First test makes sure that players 1 and 2 are not blank
                first_two_players_not_blank <- reactive({
                  player_inputs()[1] != "" & player_inputs()[2] != "" 
                })
                inputs_match_expected_size <- reactive({
                  if (team_size == 3) {
                    player_inputs()[3] != ""
                  } else if (team_size == 4) {
                    player_inputs()[4] != ""
                  }
                })
                
                # This mirrors the existing validate start, but separates out 
                # by teams so that we can color code each output and put it
                # to a side of the center well
                output$message <- reactive({
                  validate(
                    need(player_inputs()[1] != "", label = "Player 1"),
                    need(player_inputs()[2] != "", label = "Player 2")
                  )
                })
               })
}