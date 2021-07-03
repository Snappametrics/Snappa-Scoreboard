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
                    pull(dbGetQuery(con, sql('SELECT MAX(ps.game_id) FROM player_stats ps JOIN game_stats gs USING (game_id) WHERE game_complete IS FALSE')))
                     
                 })
                 
                 restart_game <- reactiveVal({F})
                 
                 missing_player_summary = reactivePoll(1000, session,
                    checkFunc = function() {dbGetQuery(con,
                                                       sql('SELECT MAX(game_id) 
                                                           FROM game_stats 
                                                           WHERE game_complete IS FALSE'))
                      },
                    valueFunc = function() {
                      dbGetQuery(con,
                              sql('SELECT
                                      players.player_name,
                                      ps.team,
                                      ps.total_points AS points,
                                      ps.paddle_points,
                                      ps.clink_points,
                                      ROW_NUMBER() OVER(
                                        PARTITION BY ps.team
                                      )
                                      AS row
                                    FROM player_stats AS ps
                                    LEFT JOIN players 
                                      ON players.player_id = ps.player_id
                                    LEFT JOIN game_stats
                                      ON game_stats.game_id = ps.game_id
                                    WHERE ps.game_id = (
                                      SELECT MAX(game_id) 
                                      FROM player_stats)
                                    AND game_stats.game_complete IS FALSE'))
                 })
                 
                 last_round <- reactive({
                  pull(dbGetQuery(con, 
                              sql('SELECT last_round 
                                  FROM game_stats 
                                  WHERE game_id = (SELECT MAX(game_id) FROM game_stats)
                                  AND game_complete IS FALSE;')
                              ))
                 })
                 
                 input_list <- reactive({
                   
                    missing_player_summary() %>%
                     select(player_name, team, row) %>%
                     complete(row = 1:4, team = c('A', 'B'), fill = list(player_name = "")) %>%
                     group_by(team) %>%
                     mutate(player_input = str_c("name_", team, row)) %>%
                     ungroup() %>%
                     select(player_input, player_name) %>%
                     deframe()
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

                     size_A = sum(missing_player_summary()$team == 'A')
                     size_B = sum(missing_player_summary()$team == 'B')
                     return(list('size_A' = size_A,
                                 'size_B' = size_B))
                 })
                 
                output$summary_table_A <- renderReactable({
                  missing_player_summary() %>%
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
                  
                output$summary_table_B <- renderReactable({
                   missing_player_summary() %>%
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
                   req(!is.na(missing_game_id()))
                   dbExecute(con, paste0("DELETE FROM game_stats WHERE game_id = ", missing_game_id(), ";"))
                 })
                 
                 return(list('restart_game' = restart_game,
                             'inputs' = input_list(),
                             'size_A' = team_sizes()[['size_A']],
                             'size_B' = team_sizes()[['size_B']]))
               })
                 
}
