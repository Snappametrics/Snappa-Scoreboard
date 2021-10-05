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

playerSelectizeServer = function(id, restart_input) {
  moduleServer(id,
               function(input, output, session) {
                 reactive(input$name)

               })
}