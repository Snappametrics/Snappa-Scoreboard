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
                   # MARK: don't need collect after dbGetQuery
                    dbGetQuery(con, sql('SELECT MAX(game_id) FROM player_stats'))  %>%
                     pull(max)
                 })
                 
                 restart_game <- reactiveVal({F})
                 # browser()
                 missing_players = reactive({
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
                 })
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
                   player_inputs = missing_players() %>%
                     group_by(team) %>%
                     mutate(player_input = str_c("name_", team, row_number())) %>%
                     ungroup() %>%
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
                                  )')) 
        
                 })
                
                 team_sizes = reactive({
                   # MARK: these might be faster
                   # length(missing_players()[missing_players()$team == 'A', ])
                   # or
                   # sum(missing_players()$team == 'A')
                     size_A = sum(missing_players()$team == 'A')
                     size_B = sum(missing_players()$team == 'B')
                     return(list('size_A' = size_A,
                                 'size_B' = size_B))
                 })
                 
                output$summary_table_A <- renderReactable({
                  # Maybe this is fast?
                  missing_player_stats()
                    filter(team == 'A') %>%
                    select(-team) %>%
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
                                  no_dash = gsub('_', ' ', value, fixed = T )
                                  return(str_to_title(no_dash))
                                },
                                width = 100,
                                align = 'center'
                              )
                          )
                })
                  
                output$summary_table_B <- renderReactable({
                  # Maybe this is fast?
                 missing_player_stats() %>%
                    filter(team == 'B') %>%
                    left_join(base_table, by = c('team' = 'team', 'player_name' = 'player')) %>%
                    replace_na(list(points = 0, paddle_points = 0, clink_points = 0)) %>%
                    select(-team) %>%
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
                                  no_dash = gsub('_', ' ', value, fixed = T )
                                  return(str_to_title(no_dash))
                                },
                                width = 100,
                                align = 'center'
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
