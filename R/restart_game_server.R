#' Handles server-side processing of lost game information
#' 
#' 
#'  @param id namespace of the module
#'  
#'  @return a list with the following components
#'  \describe{
#'       \item{players_A, players_B} {Names of the players from each team  in the game to be restarted}
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
                                                    players.player_name,
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
                                                        ELSE 0 
                                                      END) AS paddle_points,
                                                  SUM(CASE 
                                                        WHEN clink = TRUE THEN 1
                                                        ELSE 0 
                                                      END) AS clink_points
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
                  presentation_table = base_table %>%
                    arrange(team, desc(points)) 
                  return(
                    reactable(presentation_table,
                              highlight = T,
                              rowStyle = function(index) {
                                list(background = if_else(presentation_table$team[index] == 'A',
                                                          'red',
                                                          'blue')
                                     )
                                
                              }
                              )
                         )
                  
                
                }
                  
                )
                 
                 observeEvent(input$restart_incomplete_game, {
                   # This needs to get rid of the modal dialog, enter in the player names (bleh)
                   # and begin the game. 
                   removeModal()
                   # This should now return the module output
                   return(list('players_A' = missing_players()[missing_players()$team == 'A', 'player_name'],
                               'players_B' = missing_players()[missing_players()$team == 'B', 'player_name']
                               )
                          )
                   
                   
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
