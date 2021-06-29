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
                   # MARK: may be able to leave out the alias and avoid the need to pull
                   #      but it might just change the colname to max or something
                   pull_game_id = sql('SELECT MAX(game_id) AS game_id FROM game_stats')
                   # MARK: don't need collect after dbGetQuery
                   game_id = dbGetQuery(con, pull_game_id) %>% collect() %>%
                     pull(game_id)
                   
                   return(game_id)
                   
                 })
                 
                 restart_game <- reactiveVal({F})
                 # browser()
                 missing_players = reactive({
                   # MARK: again no need for collect or return
                   dbGetQuery(con,
                              sql('SELECT
                                                    players.player_name,
                                                    ps.team
                                                 FROM player_stats AS ps
                                                 INNER JOIN players
                                                  ON players.player_id = ps.player_id
                                                 WHERE ps.game_id = (
                                                    SELECT MAX(game_id) 
                                                    FROM player_stats
                                                 )'))
                   # players = dbGetQuery(con,
                   #                           sql('SELECT
                   #                                  players.player_name,
                   #                                  ps.team
                   #                               FROM player_stats AS ps
                   #                               INNER JOIN players
                   #                                ON players.player_id = ps.player_id
                   #                               WHERE ps.game_id = (
                   #                                  SELECT MAX(game_id) 
                   #                                  FROM player_stats
                   #                               )')) %>% collect()
                   # return(players)
                 })
                 # MARK: Are we sure last_round is working?
                 last_round <- reactive({
                   query = dbGetQuery(con, 
                              sql('SELECT last_round 
                                  FROM game_stats 
                                  WHERE game_id = (SELECT MAX(game_id) FROM game_stats);')
                              ) %>% collect() %>%
                     pull(last_round)
                   return(query)
                 })
                 
                 input_list <- reactive({
                   # MARK: Is it faster to just do this in a single pipe? 
                   player_inputs = missing_players() %>%
                     group_by(team) %>%
                     mutate(player_input = str_c("name_", team, row_number())) %>%
                     ungroup()
                   
                   # unless you're planning on some intermediary action here
                   
                   input_list = player_inputs %>%
                     select(player_input, player_name) %>%
                     deframe()
                   return(input_list)
                 })
                 
                 # MARK: This seems like it may (in some weird scenario)
                 #      pull different last games than the one in game_stats
                 missing_player_stats <- reactive({
                   dbGetQuery(con, 
                              sql('SELECT * FROM player_stats
                                  WHERE game_id = (
                                    SELECT MAX(game_id)
                                    FROM player_stats
                                  )')) %>% collect()
        
                 })
                
                 team_sizes = reactive({
                   # MARK: these might be faster
                   # length(missing_players()[missing_players()$team == 'A', ])
                   # or
                   # sum(missing_players()$team == 'A')
                     size_A = length(missing_players()[missing_players()$team == 'A'])
                     size_B = length(missing_players()[missing_players()$team == 'B'])
                     
                     return(list('size_A' = size_A,
                                 'size_B' = size_B))
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
                  
                  # MARK: why scores then? was it easier for some reason?
                  presentation_table = missing_players() %>%
                    filter(team == 'A') %>%
                    left_join(base_table, by = c('team' = 'team', 'player_name' = 'player')) %>%
                    replace_na(list(points = 0, paddle_points = 0, clink_points = 0)) %>%
                    select(-team) %>%
                    arrange(desc(points)) 
                  # MARK: No need for return here, you can just pipe the 
                  # presentation_table into reactable()
                  return(
                    reactable(presentation_table,
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
                                  no_dash = gsub('_', ' ', value, fixed = T )
                                  return(str_to_title(no_dash))
                                },
                                width = 100,
                                align = 'center'
                              )
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
                    select(-team) %>%
                    arrange(desc(points)) 
                  return(
                    reactable(presentation_table,
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
                                  no_dash = gsub('_', ' ', value, fixed = T )
                                  return(str_to_title(no_dash))
                                },
                                width = 100,
                                align = 'center'
                              )
                    )
                  )
                }
                )
                
                output$round_num <- renderUI({
                  team_colours = list("A" = "#e26a6a", "B" = "#2574a9")
                  HTML(str_c('<h1 style = "font-size: 5rem">', 
                             str_extract(last_round(), "[0-9]+"), 
                             '<span style="color:', team_colours[[str_extract(last_round(), "[AB]+")]], ';">', str_extract(last_round(), "[AB]+"), "</span>",
                             "</h1>"))
                })
                 
                 observeEvent(input$restart_incomplete_game, {
                   removeModal()
                   restart_game(T)
                 })
                 
                 observeEvent(input$delete_incomplete_game, {
                   removeModal()
                   delete_query = paste0("DELETE FROM game_stats WHERE game_id = ", missing_game_id(), ";")
                   dbExecute(con, delete_query)
                   # This should return some boilerplate list to instruct the module which 
                   # will assemble this information into a game_start screen state
                 })
                 
                 return(list('restart_game' = restart_game,
                             'inputs' = input_list(),
                             'size_A' = team_sizes()[['size_A']],
                             'size_B' = team_sizes()[['size_B']]))
               })
                 
}
