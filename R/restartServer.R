#' Handles server-side processing of lost game information
#' 
#' 
#'  @param id namespace of the module
#'  
#'  @return a list with the following components
#'  \describe{
#'       \item{ restart_game } {Passes a boolean to tell the app to restart using past game info}
#'       \item{inputs} {A tibble with the name of the player and the inputID that will contain their name}
#'  }


restartServer = function(id) {
  moduleServer(id,
               
               function(input, output, session) {
                 
                 restart_game <- reactiveVal({F})
                 check_function_return <- reactiveVal({F})
                 
                 # Incomplete game data
                 incomplete_game = dbGetQuery(con, sql("SELECT * FROM incomplete_game"))
                 
                 # Empty lists for potential outputs
                 input_list = list()
                 team_sizes = list()
                 score_inputs = list()
                 
                 
                 # When there is an incomplete game ----
                 if(nrow(incomplete_game) != 0){
                   
                   # This is a reactivePoll because, for some reason, deleting a game does not cause this
                   # to update even though it really, really should. 
                   incomplete_player_summary = dbGetQuery(con, sql("SELECT * FROM ongoing_game_summary"))
                   
                   # Prepare delete query
                   delete_query = sql("DELETE FROM game_stats WHERE game_id = (SELECT MAX(game_id) FROM incomplete_game);")
                   
                   ## If there were no points => DELETE -----
                   if(sum(incomplete_player_summary$points) == 0){
                     
                     
                     dbExecute(con, delete_query)
                     

                   } else {
                     # If there were points ----

                     # => Show game summary
                     #  - Team A
                     # TODO: Functionalize summary table
                     output$summary_table_A <- renderReactable({
                       incomplete_player_summary %>%
                         filter(team == 'A') %>%
                         select(-team, row) %>%
                         arrange(desc(points)) %>%
                         reactable(
                           highlight = T,
                           columns = list(
                             player_name = 
                               colDef(
                                 header = 'Player Name',
                                 style = list(color = '#e26a6a'),
                                 align = 'center',
                                 width = 100) 
                           ),
                           defaultColDef = colDef(
                             header = function(value) {
                               return(str_to_title(gsub('_', ' ', value, fixed = T )))
                             },
                             width = 100,
                             align = 'center'
                           )
                         )
                     })
                     
                     # => Show game summary
                     #  - Team B
                     output$summary_table_B <- renderReactable({
                       incomplete_player_summary %>%
                         filter(team == 'B') %>%
                         select(-team, row) %>%
                         arrange(desc(points)) %>%
                         reactable(
                           highlight = T,
                           columns = list(
                             player_name = 
                               colDef(
                                 header = 'Player Name',
                                 style = list(color = '#2574a9'),
                                 align = 'center',
                                 width = 100)
                           ),
                           defaultColDef = colDef(
                             header = function(value) {
                               return(str_to_title(gsub('_', ' ', value, fixed = T )))
                             },
                             width = 100,
                             align = 'center'
                           )
                         )
                       
                     }
                     )
                     
                     # => Show round num ----
                     output$round_num <- renderUI({
                       team_colours = list("A" = "#e26a6a", "B" = "#2574a9")
                       HTML(str_c('<h1 style = "font-size: 5rem">', 
                                  str_extract(incomplete_game$last_round, "[0-9]+"), # Extract number in round
                                  '<span style="color:', 
                                  team_colours[[str_extract(incomplete_game$last_round, "[AB]+")]], # Colour code shooting team
                                  ';">', 
                                  str_extract(incomplete_game$last_round, "[AB]+"), # Extract team in round
                                  "</span>",
                                  "</h1>"))
                     })
                     
                     # Table of teams and their scores
                     scores = count(incomplete_player_summary, team, wt = points, name = "points")
                     

                     # => Launch the dialog box ----
                     showModal(
                       restartUI('restart', scores)
                       )
                     
                     # Table of player points
                     incomplete_player_summary = dbGetQuery(con, sql("SELECT * FROM ongoing_game_summary"))
                     browser()
                     
                     # Create input ids from player information
                     input_list <- incomplete_player_summary %>%
                       select(player_name, team, row) %>%
                       # Produce a dataframe with an input for all possible player inputs
                       complete(row = 1:4, team = c('A', 'B'), fill = list(player_name = "")) %>%
                       group_by(team) %>% # TODO: Confirm whether we need to group by
                       # Produce inputs in the form: A-1-name
                       mutate(player_input = str_c(team, "-", row, "-name")) %>%
                       ungroup() %>%
                       select(player_input, player_name) %>%
                       # Convert to named vector
                       deframe()
                     
                     # Team size info
                     team_sizes = list('A' = sum(incomplete_player_summary$team == 'A'),
                                       'B' = sum(incomplete_player_summary$team == 'B'))
                     
                     # Create the inputs passed to the score inputs
                     score_inputs = list(
                       "current_scores" = tibble(
                         team_A = sum(incomplete_player_summary[incomplete_player_summary$team == "A", "points"]),
                         team_B = sum(incomplete_player_summary[incomplete_player_summary$team == "B", "points"])
                       ),
                       "score_id" = as.numeric(pull(dbGetQuery(con, sql(str_c("SELECT MAX(score_id) FROM scores WHERE game_id = ", incomplete_game$game_id))))),
                       "scores_db" = dbGetQuery(con, sql(str_c("SELECT * FROM scores WHERE game_id = ", incomplete_game$game_id))),
                       "game_id" = incomplete_game$game_id,
                       "shot_num" = parse_round_num(incomplete_game$last_round),
                       "game_stats_db" = incomplete_game,
                       "snappaneers" = dbGetQuery(con, sql(str_c("SELECT ps.player_id, p.player_name, ps.team FROM player_stats ps JOIN players p USING (player_id) WHERE ps.game_id = ", incomplete_game$game_id))),
                       "player_stats_db" = dbGetQuery(con, sql(str_c("SELECT * FROM player_stats WHERE game_id = ", incomplete_game$game_id))),
                       "casualties" = dbGetQuery(con, sql(str_c("SELECT * FROM casualties WHERE game_id = ", incomplete_game$game_id)))
                     )
                     
                   }

                 }
                 
                 # If player presses restart_game ----
                 # => remove modal and set restart_game to TRUE
                 observeEvent(input$continue, {
                   removeModal()
                   
                   restart_game(T)
                   
                   
                 })
                 
                 # If player presses delete, remove modal and delete game ----
                 observeEvent(input$delete, {
                   removeModal()
                   # This is not my preferred solution, but it covers off some annoying behavior where the 
                   # deletion of the game -> cascade -> updating missing_game_id() to NA, which causes
                   # the app to crash because, for some reason, the observer runs again. Why the deletion of
                   # the game causes missing_game_id() to update, but NOT input_list() for some reason, I'm
                   # sure that I don't know. If it did, then input_list() would not need to be a reactivePoll
                   # req(!is.na(missing_game_id))
                   # This will tell the reactivePoll to trigger, which is much cheaper than querying the server
                   # for the max game id from game stats, both computationally and economically. 
                   # check_function_return(T)
                   dbExecute(con, delete_query)
                 })
                 
                 
                 reactive(list("restart" = restart_game,
                               "input_list" = input_list,
                               "team_sizes" = team_sizes,
                               "score_inputs" = score_inputs))
                 
                 
                 
                 
               })
                 
}
