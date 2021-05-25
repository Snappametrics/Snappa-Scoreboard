# Restart game module

#showModal(
restart_game_UI = function(id, team_A_points, team_B_points) {
  ns <- NS(id)
  modalDialog(
    title = "Hol' up, did you want to continue the previous game?",
    style = str_c("background-color:", snappa_pal[1], ";"),
    div(
      h3("Summary of the Previous Game", 
         align = 'center')
    ),
    
    # br(),
    
    # br(),
    
    br(),
    ## TODO: Need something here which is created by the module server. Probably
    ## several things
    fluidRow(
      column(4,
             align = 'center',
             h3(class = 'modal_team_title', id = 'modal_team_title_A',
                'Team A'),
             h3(class = 'modal_team_points', id = 'modal_team_points_A',
                team_A_points)
             ),
      column(4, align = 'center'
             ),
      column(4,
             align = 'center',
             h3(class = 'modal_team_title', id = 'modal_team_title_B',
                'Team B'),
             h3(class = 'modal_team_points', id = 'modal_team_points_B',
                team_B_points)
    
             )
    ),
 #   reactableOutput(ns('incomplete_game_summary'))
    
    footer = tagList(
      fluidRow(
        column(8, align = "left",
               helpText(
                 h4('Would you like to continue the previous game?',
                    style = "color: black; display: inline-block; padding: 1.5vh; font-size: 2rem; font-weight: 600;"),
                 span("This will delete the game from our database", 
                        style = "color: red; display: inline-block; padding: 1.5vh; font-size: 2rem; font-weight: 600;")
                 )
        ),
        column(2,
               actionBttn(ns("restart_incomplete_game"),
                          label = "Restart Game",
                          style = "material-flat", 
                          color = "default",
                          size = "md")
        ),
        column(2,
               actionBttn(ns("delete_incomplete_game"),
                          label = "Delete Game",
                          style = "material-flat",
                          color = "danger",
                          size = "md")
        )
      )
    ),
    size = "l",
    easyClose = F,
    fade = F
  )
  
}


restart_game_server = function(id) {
  moduleServer(id,
               function(input, output, session) {
                 missing_game_id = reactive({
                   pull_game_id = sql('SELECT MAX(game_id) AS game_id FROM game_stats')
                   game_id = dbGetQuery(con, pull_game_id) %>% collect() %>%
                     pull(game_id)
                   return(game_id)
                   
                 })

              #   output$incomplete_game_summary = 
                 
                 observeEvent(input$restart_incomplete_game, {
                   # This needs to get rid of the modal dialog, enter in the player names (bleh)
                   # and begin the game. 
                   
                 })
                 observeEvent(input$delete_incomplete_game, {
                   removeModal()
                   delete_query = str_c("DELETE FROM game_stats WHERE game_id = ", missing_game_id(), ";")
                   dbExecute(con, delete_query)
                 }) 
               })
                 
}
