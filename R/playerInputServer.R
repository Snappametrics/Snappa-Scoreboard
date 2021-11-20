#' Player Input Server
#' Handles all of the server elements outside of the dashboard options when players
#' lauch the app
#' 
#' @param id The namespace of the module
#' 
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

teamInputServer = function(id){
  team = str_sub(id, -1, -1)
  ns = NS(id)
  moduleServer(id,
               
               function(input, output, session) {
                 # Reactive player UI
                 output$input <- renderUI({
                   req(input$size) # Require the team size
                   player_choices = dbGetQuery(con, "SELECT player_name FROM thirstiest_players")[,1]
                   
                   imap(1:input$size, ~{
                     playerSelectizeUI(ns(.y), str_c("Player ", .y), player_choices)
                   })
                   
                 })
                 
                 # Player inputs to output from playerInputServer
                 team_inputs = reactive({
                   imap(1:input$size, ~{
                     playerSelectizeServer(ns(.y))
                   })
                 })
                 

                 list("inputs" = reactive(team_inputs()))
                 
               }
  )
}


playerInputServer = function(id, restart){
  moduleServer(id,
               
               function(input, output, session) {
                 # Start reactive value
                 start = reactiveVal(F)
                 
                 # Observe Start button
                 observeEvent(input$start_game,{
                   start(T)
                 })
                 

                 # Assigns the outputs of teamInputServer for each team
                 team_A = teamInputServer("A")
                 team_B = teamInputServer("B")
                 
                 observeEvent(req(restart() == T), {
                   start(T)
                   return(
                     list("start" = reactive(start()))
                   )
                 })
                 

                 # Create a UI output which validates that there are four players and the names are unique
                 # output$validate_start = reactive({
                 #   # If one of the first two players on each team is removed, disable the button again.
                 #   # This goes above the validate check because it needs to be updating before the validate
                 #   # check is failed, or else the logic isn't going to pass through
                 #   
                 #   if(any(input$`A-1-name` == "",
                 #          input$`A-2-name` == "",
                 #          input$`B-1-name` == "",
                 #          input$`B-2-name` == "")){
                 #     shinyjs::disable("start_game")
                 #   }
                 #   # If a game is being restarted, then this step can be skipped, since we're assuming
                 #   # that the game was validated the first time through. When and how this will come
                 #   # back to bite us, I don't know and I don't want to
                 #   req(!restart)
                 #   validate(
                 #     # Team A
                 #     need(input$`A1-name`, label = "Player A1"),
                 #     need(input$`A2-name`, label = "Player A2"),
                 #     # Extra players A
                 #     need_input(input$`A3-name`, 3, input$team_A_size),
                 #     need_input(input$`A4-name`, 4, input$team_A_size),
                 #     # Team B
                 #     need(input$`B1-name`, label = "Player B1"), 
                 #     need(input$`B2-name`, label = "Player B2"),
                 #     # Extra players B
                 #     need_input(input$`B3-name`, 3, input$team_B_size),
                 #     need_input(input$`B4-name`, 4, input$team_B_size),
                 #     # Unique players
                 #     need(length(unique(snappaneers()$player_name)) == num_players(), 
                 #          message = "Player names need to be unique")
                 #   )
                 #   
                 # })
                 
                 
                 # Outputs ----
                 list(
                   "start" = reactive(start()),
                   "snappaneers" = reactive(
                     list("A" = team_A$inputs,
                          "B" = team_B$inputs)
                   )
                 )
               }
  )
}