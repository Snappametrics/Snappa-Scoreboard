# Restart game module

#showModal(
restart_game_UI = function(id) {
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
             align = 'left',
             h3(class = 'modal_team_title', id = 'modal_team_title_A',
                'Team A')
             
             ),
      column(2, align = 'center',
             h3('Summary of Previous Game'),
             textOutput(id('incomplete_game_score'))
             ),
      column(4,
             align = 'right',
             h3(class = 'modal_team_title', id = 'modal_team_title_B',
                'Team B')
             )
    ),
 #   reactableOutput(ns('incomplete_game_summary'))
    
    footer = tagList(
      fluidRow(
        column(9, align = "left",
               helpText("This will delete the game from our database", 
                        style = "color: red; display: inline-block; padding: 1.5vh; font-size: 2rem; font-weight: 600;")
        ),
        column(1,
               actionBttn("keep_incomplete_game",
                          label = "Keep Previous Game",
                          style = "material-flat", 
                          color = "default",
                          size = "md")
        ),
        column(1,
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
                 
                 team_scores = reactive({
                   dbGetQuery(con,
                              sql('SELECT '))
                 })
                 game_id = reactive({
                   pull_game_id = sql('SELECT MAX(game_id) FROM game_stats')
                   
                   game_id = dbExecute(con, pull_game_id) %>% collect()
                   return(game_id)
                   
                 })
                 
                 output$incomplete_game_score = renderText(
                   str_c(team_scores()['points_A'],
                         ' - ',
                         team_scores()['points_B']
                   )
                 )
                 
                 
              #   output$incomplete_game_summary = 
                   
                 # I just don't want to believe that this is the best way
                 # to do this
                 observe({
                   browser()
                   showModal(restart_game_UI(id))
                   })
                 
                 observeEvent(input$keep_incomplete_game, {
                   
                 })
                 observeEvent(input$delete_incomplete_game, {
                   
                 }) 
               })
                 
}
