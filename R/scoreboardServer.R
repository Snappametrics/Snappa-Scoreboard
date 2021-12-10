#' Scoreboard Server
#' Handles all of the server elements on the scoreboard screen
#' 
#' @param id The namespace of the module

teamServer = function(id, current_score){

  moduleServer(id,
               
               function(input, output, session) {
                 output$score = renderText({
                   
                   current_score
                 })
                 
               }
               )
}

scoreboardServer = function(id, round_num, current_scores){
  

  moduleServer(id,
               
               function(input, output, session) {
                 
                 
                 output$round_num = renderUI({
                   team_colours = list("A" = "#e26a6a", "B" = "#2574a9")
                   HTML(str_c('<h3 class="numbers">', 
                              str_extract(round_num, "[0-9]+"), 
                              '<span style="color:', team_colours[[str_extract(round_num, "[AB]+")]], ';">', str_extract(round_num, "[AB]+"), "</span>",
                              "</h3>"))
                 })
                 
                 output$round_control_buttons = renderUI({
                   ns = session$ns
                   team_colours = list("A" = "danger", "B" = "primary")
                   column(width=12, align = "center",
                          div( id = 'round_control_buttons',
                               actionBttn(ns("previous_round"), 
                                          label = "Previous Round", style = "jelly", icon = icon("arrow-left"), color = 
                                            team_colours[[str_extract(round_num, "[AB]+")]], size = "lg"),
                               actionBttn(ns("next_round"), 
                                          label = "Pass the dice", style = "jelly", icon = icon("arrow-right"), 
                                          color = team_colours[[str_extract(round_num, "[AB]+")]], size = "lg")
                          )
                   )
                 })
                 
                 # Die icon indicating the active team
                 output$active_die_left = renderUI({
                   # switch_counter is a counter for how many times switch_sides 
                   # even means that B should be on the left side
                   switch_is_even = (switch_counter() %% 2 == 0)
                   
                   
                   if(switch_is_even){
                     # If sides have been switched
                     img(src = "die_hex.png", style = str_c("background: transparent;display: flex;transform: scale(1.25);position: relative;top: -1vh; display:", 
                                                            if_else(str_extract(round_num, "[AB]+") == "B", "block;", "none;")))
                   } else {
                     img(src = "die_hex.png", style = str_c("background: transparent;display: flex;transform: scale(1.25);position: relative;top: -1vh; display:", 
                                                            if_else(str_extract(round_num, "[AB]+") == "A", "block;", "none;")))
                   }
                 })
                 
                 output$active_die_right = renderUI({
                   # switch_counter is a counter for how many times switch_sides 
                   # even means that A should be on the right side
                   switch_is_even = (switch_counter() %% 2 == 0)
                   
                   if(switch_is_even){
                     img(src = "die_hex.png", style = str_c("background: transparent;display: flex;transform: scale(1.25);position: relative;top: -1vh; display:", 
                                                            if_else(str_extract(round_num, "[AB]+") == "A", "block;", "none;")))
                   } else {
                     img(src = "die_hex.png", style = str_c("background: transparent;display: flex;transform: scale(1.25);position: relative;top: -1vh; display:", 
                                                            if_else(str_extract(round_num, "[AB]+") == "B", "block;", "none;")))
                   }
                 })
                 
                 teamServer("A", current_scores$team_A)
                 teamServer("B", current_scores$team_B)

                 switch_counter = reactiveVal(1)
                 
                 observeEvent(input$switch_sides, {
                   # browser()
                   switch_val = switch_counter()+1
                   
                   switch_counter(switch_val)
                   
                   switch_is_even = (switch_counter() %% 2 == 0)
                   
                   
                   if(switch_is_even){
                     removeUI("#ScoreboardUI", immediate=T)
                     insertUI(selector = "#dice-row", ui = scoreboardUI("scoreboard"), where = "afterEnd")
                   } else {
                     removeUI("#ScoreboardUI", immediate = T)
                     insertUI(selector = "#dice-row", ui = scoreboardUI("scoreboard"), where = "afterEnd")
                   }
                   
                   
                 })
                 
                 
                 
                 
               }
  )
}