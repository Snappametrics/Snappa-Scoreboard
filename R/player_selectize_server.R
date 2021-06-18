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
                 ns <- session$ns
                 player_name <- reactive({
                   input$player_name
                 })
                 observeEvent(restart_game, {
                   # Is there a reason to do this over just writing 'restart_game'?
                   req(isTRUE(restart_game()))
                   updateSelectizeInput(session = getDefaultReactiveDomain(),
                                        inputId = 'player_name',
                                        selected = restart_input())
                   
                 },
                 ignoreInit = T,
                 ignoreNULL = T)
                 
                 return(player_name())
               })
}