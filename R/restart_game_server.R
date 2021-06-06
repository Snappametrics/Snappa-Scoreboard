restart_game_server = function(id) {
  moduleServer(id,
               function(input, output, session) {
                 missing_game_id = reactive({
                   pull_game_id = sql('SELECT MAX(game_id) AS game_id FROM game_stats')
                   game_id = dbGetQuery(con, pull_game_id) %>% collect() %>%
                     pull(game_id)
                   return(game_id)
                   
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
                   
                   
                   
                 })
                 observeEvent(input$delete_incomplete_game, {
                   removeModal()
                   delete_query = str_c("DELETE FROM game_stats WHERE game_id = ", missing_game_id(), ";")
                   dbExecute(con, delete_query)
                 }) 
               })
                 
}
