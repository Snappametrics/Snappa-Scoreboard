#' Player selectize server
#' Holds the reactive player name from the corresponding player selectize UI
#' and receives from restart game ui to update this value 
#' 
#' @param id namespace of the module
#' @param restart_game A boolean passed from restart_game_server
#' @param restart_input Name of the player to be updated into the selectize when the game is restarted
#' 
#' @return 
#' \describe{
#'      \item{player_name} {a reactive value containing the string in the selectize box}
#' }

player_selectize_server = function(id, restart_game, restart_input) {
  moduleServer(id,
               function(input, output, session) {
                 player_name <- reactive({
                   if_else(input$player_name != '', 
                           # If you were to instead pass this as a requirement for 
                           # the overall reactive with req(), then you would need
                           # to make sure that the UI is generated before updating the 
                           # input. I want to avoid that with this if_else
                           if_else(is.null(input$player_name),
                                   '',
                                   input$player_name),
                           restart_input())
                 })
                return(player_name())
               })
}