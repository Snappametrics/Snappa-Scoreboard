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

player_selectize_server = function(id, restart_input) {
  moduleServer(id,
               function(input, output, session) {
                 # reactivePoll is used here because the implementation of this solution using
                 # a base reactive is sometimes not being updated as the player's input is changing,
                 # even though it is definitely changing and should impact the player_name() output. 
                 
                 # To get around this, the reactivePoll will check every half second to see whether 
                 # or not the input has changed, and this seems to solve it
                 
                 player_name <- reactivePoll(500, session, 
                    checkFunc = function() {return(coalesce(c(input$player_name, ""))[1])},
                    valueFunc = function() {
                   # If you were to instead pass this as a requirement for 
                   # the overall reactive with req(), then you would need
                   # to make sure that the UI is generated before updating the 
                   # input. I want to avoid that with this if_else
                   if_else(coalesce(c(input$player_name, ""))[1] != "",
                           # It's not going to seem like you need to repeat the coalesce
                           # due to the fact that we check for it above. However, if the 
                           # type of this input is null, then it expects restart_input()
                           # to be null as well. Which it never is.
                           
                           # This subscript is necessary because BOTH elements are returned 
                           # if the input is not null
                           ccoalesce(c(input$player_name, ""))[1],
                           restart_input())
                 })
                return(player_name())
               })
}