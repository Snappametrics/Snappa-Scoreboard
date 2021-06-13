#' Handles server-side processing of lost game information
#' 
#' 
#'  @param id namespace of the module
#'  
#'  @return a list with the following components
#'  \describe{
#'       \item{ restart_game } {Passes a boolean to tell the app to restart using past game info}
#'       \item{inputs} {A tibble with the name of the player and the inputID that will contain their name}
#'       \item{size_A, size_B} {Sizes of each team to figure out whether or not to click the buttons to expand team sizes}
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
                 restart_game <- reactiveVal({F})
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
                 
                 input_list <- reactive({
                   player_inputs = missing_players() %>%
                     group_by(team) %>%
                     mutate(player_input = str_c("name_", team, row_number())) %>%
                     ungroup()
                   
                   input_list = player_inputs %>%
                     select(player_input, player_name) %>%
                     deframe()
                   return(input_list)
                 })
                 
                 
                 missing_player_stats <- reactive({
                   dbGetQuery(con, 
                              sql('SELECT * FROM player_stats
                                  WHERE game_id = (
                                    SELECT MAX(game_id)
                                    FROM player_stats
                                  )')) %>% collect()
        
                 })
                
                 team_sizes = reactive({
                   return(
                     missing_players() %>% 
                       group_by(team) %>%
                       summarize(sum = n()) %>%
                       deframe()
                   )
                 })
                 
                output$summary_table_A <- renderReactable({
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
                  presentation_table = missing_players() %>%
                    filter(team == 'A') %>%
                    left_join(base_table, by = c('team' = 'team', 'player_name' = 'player')) %>%
                    replace_na(list(points = 0, paddle_points = 0, clink_points = 0)) %>%
                    arrange(team, desc(points)) 
                  return(
                    reactable(presentation_table,
                              highlight = T,
                              rowStyle = list(background = '#e26a6a' ) 
                              )
                          )
                })
                  
                output$summary_table_B <- renderReactable({
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
                  presentation_table = missing_players() %>%
                    filter(team == 'B') %>%
                    left_join(base_table, by = c('team' = 'team', 'player_name' = 'player')) %>%
                    replace_na(list(points = 0, paddle_points = 0, clink_points = 0)) %>%
                    arrange(team, desc(points)) 
                  return(
                    reactable(presentation_table,
                              highlight = T,
                              rowStyle = list(background = '#2574a9')
                            
                    )
                  )
                }
                )
                 
                 observeEvent(input$restart_incomplete_game, {
                   
                   
                   removeModal()
                   #Look at the number of lost players on each team to be certain of the values 
                   # that you want\
                   size_B = missing_players() %>% filter(team == "B") %>%
                     summarize(sum = n()) %>%
                     deframe()
                   # Check to see if you should be signaling to the app to care about extra
                   # players
                   # if (size_A == 3){
                   #   shinyjs::click("extra_player_A3")
                   # } else if (size_A == 4){
                   #   shinyjs::click("extra_player_A3")
                   #   shinyjs::click("extra_player_A4")
                   # } else {
                   #   invisible()
                   # }
                   # 
                   # if (size_B == 3){
                   #   shinyjs::click("extra_player_B3")
                   # } else if (size_B == 4){
                   #   shinyjs::click("extra_player_B3")
                   #   shinyjs::click("extra_player_B4")
                   # } else {
                   #   invisible()
                   # }
                   
                   # delay(10, iwalk(input_list, function(name, id){
                   #   updateSelectizeInput(session, inputId = id, selected = name)
                   # })
                   # )
                   # 
                   # shinyjs::click("start_game")
                   # This needs to get rid of the modal dialog, enter in the player names (bleh)
                   # and begin the game. 
                   
                   # removeModal()
                   restart_game(T)
                   # # This should now return the module output
                 })
                 observeEvent(input$delete_incomplete_game, {
                   removeModal()
                   delete_query = paste0("DELETE FROM game_stats WHERE game_id = ", missing_game_id(), ";")
                   dbExecute(con, delete_query)
                   # This should return some boilerplate list to instruct the module which 
                   # will assemble this information into a game_start screen state
                 })
                 
                 return(list('restart_game' = restart_game,
                             'inputs' = input_list,
                             'size_A' = team_sizes()[team == 'A',],
                             'size_B' = team_size()[team == 'B', ]))
               })
                 
}
