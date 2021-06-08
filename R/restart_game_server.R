#' Handles server-side processing of lost game information
#' 
#' 
#'  @param id namespace of the application
#'  
#'  @return a list with the following components
#'  \describe{
#'       \item{input_A1} {Name of Player A1 From Game to be Restarted, Else Blank}
#'       \item{input_A2} {Name of Player A2 From Game to be Restarted, Else Blank}
#'       \item{input_A3} {Name of Player A3 From Game to be Restarted, Else Blank}
#'       \item{input_A4} {Name of Player A4 From Game to be Restarted, Else Blank}
#'       \item{input_B1} {Name of Player B1 From Game to be Restarted, Else Blank}
#'       \item{input_B2} {Name of Player B2 From Game to be Restarted, Else Blank}
#'       \item{input_B3} {Name of Player B3 From Game to be Restarted, Else Blank}
#'       \item{input_B4} {Name of Player B4 From Game to be Restarted, Else Blank}
#'       \item{length_A} {Number of slots to fill (for other modules to process w/o checking for blanks)}
#'       \item{length_B} {Number of slots to fill (for other modules to process w/o checking for blanks)}
#'  }


restart_game_server = function(id) {
  moduleServer(id,
               function(input, output, session) {
                 missing_game_id = reactive({
                   pull_game_id = sql('SELECT MAX(game_id) AS game_id FROM game_stats')
                   game_id = dbGetQuery(con, pull_game_id) %>% collect() %>%
                     pull(game_id)
                   return(game_id)
                   
                 })
                 
                 missing_players = reactive({
                   players = dbGetQuery(con,
                                             sql('SELECT
                                                    players.player_id,
                                                    ps.team
                                                 FROM player_stats AS ps
                                                 INNER JOIN players
                                                  ON players.player_id = ps.player_id
                                                 WHERE ps.game_id = (
                                                    SELECT MAX(game_id) 
                                                    FROM player_stats
                                                 )')) %>% collect()
                   return(players)
                 })

                output$incomplete_game_summary = renderReactable({
                  # Maybe this is fast?
                  base_table = dbGetQuery(con, 
                                            sql('SELECT 
                                                  players.player_name AS player,
                                                  scoring_team AS team,
                                                  SUM(points_scored) AS points,
                                                  SUM(CASE 
                                                        WHEN paddle = TRUE THEN 1 
                                                        ELSE 0 END) AS paddle_points,
                                                  SUM(CASE 
                                                        WHEN clink = TRUE THEN 1
                                                        ELSE 0 END) AS clink_points
                                                FROM (SELECT * 
                                                      FROM scores 
                                                      WHERE game_id = (SELECT MAX(game_id)
                                                                       FROM scores)
                                                ) AS scores
                                                LEFT JOIN players ON 
                                                players.player_id = scores.player_id
                                                GROUP BY players.player_name, scoring_team')
                                            ) %>% collect()
                  # Yeah. It's fast. This should probably just be a query to player_stats, 
                  # particularly considering that we need to know the total number 
                  # of players on each team, which has traditionally been stored there
                  
                  return(reactable(base_table))
                  
                
                }
                  
                )
                 
                 observeEvent(input$restart_incomplete_game, {
                   # This needs to get rid of the modal dialog, enter in the player names (bleh)
                   # and begin the game. 
                   removeModal()
                   
                   # This should now return the module output
                   
                   
                   
                 })
                 observeEvent(input$delete_incomplete_game, {
                   removeModal()
                   delete_query = str_c("DELETE FROM game_stats WHERE game_id = ", missing_game_id(), ";")
                   dbExecute(con, delete_query)
                   
                   # This should return some boilerplate list to instruct the module which 
                   # will assemble this information into a game_start screen state
                 }) 
               })
                 
}
