


# UI functions ------------------------------------------------------------

# Score pop-up dialog box
score_check <- function(team, players) {
  # Identify which team scored
  team_scored = paste("ok", team, sep = "_")
  team_colour = if_else(team_scored == "ok_a", "#e26a6a", "#2574a9")
  score_val = paste(team, "score_val", sep = "_")
  
  # Ask how many points were scored and by whom
  modalDialog(align = "center", easyClose = T, size = "l",
              # Header
              h2(str_c("Team ", str_to_upper(team), " Scored"), style = paste("color:", team_colour)),
              # Who Scored?
              radioGroupButtons(
                inputId = "scorer",
                label = "Who scored?",
                choices = players,
                direction = "horizontal",
                individual = T,
                size = "lg",
                checkIcon = list(
                  yes = tags$i(class = "fa fa-dice", 
                               style = paste("color:", team_colour)))
              ),
              # Number of points
              radioGroupButtons(
                inputId = "score",
                label = "Points",
                choices = c(1, 2, 3, 4, 5, 6, 7),
                size = "lg"
              ),
              
              # Was it a paddle?
              awesomeCheckbox(
                inputId = "paddle", 
                label = tags$div(HTML(str_c('<i id="paddle-icon" class="fas fa-hand-paper" style = "color:', team_colour, ';"></i>  Paddle?'))),#"Was it a paddle?",
                status = "warning"
              ),
              # Was it a clink?
              awesomeCheckbox(
                inputId = "clink", 
                label = tags$div(HTML(str_c('<i id="clink-icon" class="fas fa-assistive-listening-systems" style = "color:', team_colour, ';"></i>  Clink?'))),#"Was it a clink?",
                status = "warning"
              ),
              # feet?
              awesomeCheckbox(
                inputId = "foot", 
                label =tags$div(HTML(str_c('<i id="foot-icon" class="fas fa-shoe-prints" style = "color:', team_colour, ';"></i>  Foot?'))),#"Was it a clink?",
                status = "warning"
              ),
              
              textOutput("skip_error_msg"),
              # Use score_val output to only show score button on valid scoring combinations
              footer = tagList(
                uiOutput(score_val)
              )
  )
  
}



team_input_ui = function(team, player_choices){
  
  players = str_c("#name_", team, 1:4, "-selectized", collapse = ", ")
  player_inputs = str_c("#name_", team, 1:4, collapse = ", ")
  team_colour = if_else(team == "a", "#e26a6a", "#2574a9")
  
  column(4, align = "center",
         
         wellPanel(
           style = paste("opacity: 0.92; background:", team_colour),
           # Header
           h1(strong(paste("Team", toupper(team))), style = "align: center; color: white; font-size: 600%; margin-top:30px;margin-bottom:30px;"),
           # Player 1
           selectizeInput(paste0('name_', team, '1'), 'Player 1', c(`Player Name`='', player_choices),  options = list(create = TRUE), width = "60%"),
           # Player 2
           selectizeInput(paste0('name_', team, '2'), 'Player 2', c(`Player Name`='', player_choices), options = list(create = TRUE), width = "60%"),
           # Add Player 3 button
           actionBttn(paste0("extra_player_", team, "3"), label = "+ Add Player", style = "unite", color = "danger"), 
           
           # CSS: Increase font size, change color to white, add top and bottom margins
           tags$style(type = "text/css", paste(players, "{color: white; margin-top:30px;margin-bottom:30px;}",
                                               player_inputs, "{color: white; margin-top:30px;margin-bottom:30px;}"))
         )
  )
}

team_scoreboard_ui = function(team){
  
  team_colour = if_else(team == "a", "#e26a6a", "#2574a9")
  
  column(width = 4, align = "center",
         
         wellPanel(
           style = paste("opacity: 0.92; background:", team_colour),
           # uiOutput("active_die_a"),
           # Header
           h1(strong(paste("Team", toupper(team))), style = "align: center; color: white; font-size: 600%; margin-top:30px;margin-bottom:30px;"),
           # Score
           h2(textOutput(paste0("score_", team))),
           # Score button
           actionBttn(paste0(team, "_score_button"), 
                      label = "We scored!", color = "danger",
                      size = "lg"),
           br(),
           actionBttn(
             inputId = paste0("undo_score_", team),
             label = "Undo", style = "unite", color = "danger", icon = icon("undo"), size = "md"
           ),
           h3(textOutput(paste0("player_names_", team)))
         )
         
  )
}

# Function for producing extra player UI inputs
extra_player_ui = function(player, player_choices){
  
  # Get the player's team
  player_team = str_extract(player, "[A-z]")
  
  # Get the player number
  player_num = as.numeric(str_extract(player, "[0-9]"))
  div_id = paste0("add_remove_", player)
  
  # Create a div
  tags$div(id = div_id, 
           # Fluid row
               # Add extra player text input 
               selectizeInput(inputId = paste0("name_", player), 
                              label = paste('Player', player_num), c(`Player Name`='', player_choices), options = list(create = TRUE), width = "60%"),
             # Add remove player button outside fluid row
             actionBttn(
               inputId = paste0("remove_", player),  label = "X", style = "jelly", color = "danger", size = "sm"),
             
             # CSS
             tags$style(paste0("#add_remove_", player, " {margin-left:auto; margin-right:auto; position: relative;} #remove_", 
                               player, " {position: relative; top:-9rem; right:-28rem; z-index:1;}")),

           # If the extra player is not the fourth on a team yet, add another add player button
           if(player_num < 4){
             actionBttn(paste0("extra_player_", player_team, player_num+1), 
                        label = "+ Add Player", style = "unite", color = "danger")
           } else{
             invisible()
           }
           
  )
}

add_player_input = function(inputs, team, player, player_choices, session){
  
  
  # Insert extra player UI
  insertUI(
    selector = inputs,
    where = "afterEnd",
    ui = extra_player_ui(paste0(team, player), player_choices)
  )
  
  # Remove add player button       
  removeUI(
    selector = inputs
  )
  
}



remove_p3_input = function(team, session){
  add_p3_button = paste0("#add_remove_", team, "3")
  
  insertUI(selector = add_p3_button,
           where = "afterEnd",
           ui = actionBttn(paste0("extra_player_", team,"3"), label = "+ Add Player", style = "unite", color = "danger")
  )
  
  
  removeUI(selector = add_p3_button)
  
  updateSelectizeInput(session, paste0("name_", team, "3"), selected = character(0))
  updateSelectizeInput(session, paste0("name_", team, "4"), selected = character(0))
}



remove_p4_input = function(team, session){
  
  add_p4_button = paste0("#add_remove_", team, "4")
  # Insert add player button
  insertUI(selector = add_p4_button,
           where = "afterEnd",
           ui = actionBttn(paste0("extra_player_", team, "4"), label = "+ Add Player", style = "unite", color = "danger")
  )
  # Remove player text input
  removeUI(selector = add_p4_button)
  
  # Tells later checks to not worry about this
  # empty slot in active_player_names
  updateSelectizeInput(session, paste0("name_", team, "4"), selected = character(0))
}

recent_score_sentence = function(scores_data){
  scores_data %>% 
    group_by(score_id) %>% 
    transmute(score_sentence = md(str_c("**",player_name, "**",
                                     " scored ",
                                     points_scored,
                                     " point(s)", 
                                     na.omit(if_else(clink, " with a clink", NA_character_)),  
                                     " for Team ", toupper(scoring_team),
                                     " in round ", round_num, ".",
                                     na.omit(if_else(paddle, str_c(" And it was a", na.omit(if_else(foot, "foot", NA_character_)), " paddle!"), NA_character_))))) %>% 
    ungroup() %>% 
    select(-score_id)
  
}
