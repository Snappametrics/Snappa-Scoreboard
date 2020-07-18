


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
           h1(paste("Team", toupper(team)), style = "align: center; color: white; font-size: 600%;"),
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

team_scoreboard_ui = function(left_team = "a", right_team = "b"){
  
  team_colours = list("a" = "#e26a6a", "b" = "#2574a9")
  
  div(id = "ScoreboardUI", 
           
           fluidRow(
             # Left Team
             column(width = 4, align = "center",
                     
                     wellPanel(
                       style = paste("opacity: 0.92; background:", team_colours[[left_team]]),
                       # uiOutput("active_die_a"),
                       # Header
                       h1(paste("Team", toupper(left_team)), style = "align: center; color: white; font-size: 550%;"),
                       # Score
                       h2(textOutput(paste0("score_", left_team))),
                       # Score button
                       actionBttn(paste0(left_team, "_score_button"), 
                                  label = "We scored!", color = "danger",
                                  size = "lg"),
                       br(),
                       actionBttn(
                         inputId = paste0("undo_score_", left_team),
                         label = "Undo", style = "unite", color = "danger", icon = icon("undo"), size = "md"
                       ),
                       h3(textOutput(paste0("player_names_", left_team)))
                     )
              ), 
              # Round
              column(width = 4, align = "center",
                     # materialSwitch(
                     #   inputId = "switch_sides",label = "Switch sides", icon = icon("refresh"), 
                     # ),
                     
                     h1("Round", style = "font-size: 600%;"),
                     h3(textOutput("round_num")),
                     fluidRow(actionBttn("previous_round", 
                                         label = "Previous Round", style = "jelly", icon = icon("arrow-left"), color = "primary", size = "lg"),
                              actionBttn("next_round", 
                                         label = "Pass the dice", style = "jelly", icon = icon("arrow-right"), color = "primary", size = "lg")),
                     br(),
                     # Recent Scores
                     dropdown(
                       gt_output("recent_scores"),
                       style = "unite",
                       size = "lg",
                       label = "Recent Scores",
                       icon = icon("backward"),
                       animate = animateOptions(
                         enter = animations$fading_entrances$fadeInUp,
                         exit = animations$fading_exits$fadeOutDown
                       )
                     ),
              ),
              # Team B
             column(width = 4, align = "center",
                    
                    wellPanel(
                      style = paste("opacity: 0.92; background:", team_colours[[right_team]]),
                      # uiOutput("active_die_a"),
                      # Header
                      h1(paste("Team", toupper(right_team)), style = "align: center; color: white; font-size: 550%;"),
                      # Score
                      h2(textOutput(paste0("score_", right_team))),
                      # Score button
                      actionBttn(paste0(right_team, "_score_button"), 
                                 label = "We scored!", color = "danger",
                                 size = "lg"),
                      br(),
                      actionBttn(
                        inputId = paste0("undo_score_", right_team),
                        label = "Undo", style = "unite", color = "danger", icon = icon("undo"), size = "md"
                      ),
                      h3(textOutput(paste0("player_names_", right_team)))
                    )
             ),
              tags$style(type = "text/css", " #undo_score_a, #undo_score_b {margin-top:2em}")
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
    transmute(score_sentence = str_c(player_name, 
                                     " scored ",
                                     points_scored,
                                     " point(s)", 
                                     na.omit(if_else(clink, " with a clink", NA_character_)),  
                                     " for Team ", toupper(scoring_team),
                                     " in round ", round_num, ".",
                                     na.omit(if_else(paddle, str_c(" And it was a", na.omit(if_else(foot, " foot", NA_character_)), " paddle!"), NA_character_)))) %>% 
    ungroup() %>% 
    select(-score_id)
}


# Stats Output ------------------------------------------------------------

theme_snappa = function(){
  theme_minimal(
    base_family = "Inter",
    base_size = 16
  ) %+replace%
    theme(
      plot.title = element_text(face = "bold", size = rel(1.25)), 
      panel.grid.major = element_blank(),
      panel.grid.minor = element_line(color = "grey", size = 0.1),
      plot.background = element_rect(fill = "#f5f5f5", colour = "transparent")
    ) 
}

leaderboard_table = function(df){
  gt(df, id = "leaderboard") %>% 
    tab_header(title = "Snappa Leaderboard") %>% 
    # Column names
    cols_label(
      rank = "", 
      player_name = "Player",
      games_played = "Games Played",
      points_per_game = "Points per Game\n(PPG)",
      total_points = "Total Points",
      total_shots = "Total Shots",
      off_ppg = "Offensive PPG",
      def_ppg = "Defensive PPG",
      toss_efficiency = "Toss Efficiency"
    ) %>% 
    # Format integers
    fmt_number(
      columns = vars(rank, total_points, total_shots),
      decimals = 0
    ) %>% 
    # Format doubles
    fmt_number(
      columns = vars(points_per_game, off_ppg, def_ppg),
      decimals = 2
    ) %>% 
    # Format percentages
    fmt_percent(
      columns = vars(toss_efficiency),
      decimals = 1
    ) %>% 
    tab_footnote(
      footnote = "Defensive points are scored from paddles.",
      locations = cells_column_labels(columns = vars(def_ppg))
    ) %>% 
    tab_footnote(
      footnote = "% of tosses which are successful.",
      locations = cells_column_labels(columns = vars(toss_efficiency))
    ) %>%
    opt_footnote_marks(marks = "letters") %>% 
    # Styling
    # Title
    tab_style(
      style = list(cell_text(color = "white", weight = "bold", align = "center", size = "x-large", v_align = "middle"), cell_fill(color = "#e26a6a")),
      locations = cells_title(groups = "title")
    ) %>%
    # Rank column
    tab_style(
      style = list(cell_text(color = "white", weight = "bold", align = "center"), cell_fill(color = "#2574a9", alpha = .8)),
      locations = cells_body(
        columns = vars(rank)
      )
    ) %>% 
    # Column header alignment
    tab_style(
      style = cell_text(align = "center", v_align = "middle"),
      locations = cells_column_labels(columns = everything())
    ) %>% 
    # Column widths
    cols_width(
      vars(rank) ~ px(30),
      vars(player_name, points_per_game) ~ px(110),
      vars(total_points, games_played, total_shots) ~ px(60),
      vars(off_ppg, def_ppg, toss_efficiency) ~ px(95)
    ) %>% 
    # Cell alignment in each column
    cols_align(
      align = "center"
    ) %>%
    # Underline dope shit
    tab_style(
      style = list(cell_text(weight = "bold", align = "center"), cell_fill(color = "#FFD600", alpha = .9)),
      locations = list(
        # Most points
        cells_body(
          columns = vars(total_points),
          rows = total_points == max(total_points)
          ),
        # Highest ppg
        cells_body(
          columns = vars(points_per_game),
          rows = points_per_game == max(points_per_game)
        ),
        # Highest off ppg
        cells_body(
          columns = vars(off_ppg),
          rows = off_ppg == max(off_ppg)
        ),
        # Highest def ppg
        cells_body(
          columns = vars(def_ppg),
          rows = def_ppg == max(def_ppg)
        )
      )
    ) %>% 
    tab_options(heading.border.lr.style = "none",
                heading.border.bottom.style = "none",
                table.border.top.style = "none",
                table.border.right.style = "none",
                table.border.left.style = "none",
                table.border.bottom.style = "none",
                column_labels.font.weight = "600")
  
}

score_heatmap = function(df){
  max_score = summarise_at(df, vars(starts_with("score")), max) %>% 
    pull() %>% 
    max()
  
  df %>% 
    ggplot(aes(x = score_b, y = score_a))+
    geom_tile(aes(fill = n), color = "black")+
    # Record the 45 degree line for visual reference
    geom_abline(mapping = NULL, data = NULL, slope = 1, intercept = 0) + 
    scale_fill_gradient(name = "Frequency", low = "#ffeda0", high = "#f03b20")+
    labs(x = "Team B",
         y = "Team A",
         title = "Heatmap of scores in Snappa")+
    coord_cartesian(xlim = c(0.75, max_score - 0.75),
                    ylim = c(0.75, max_score - 0.75)) + 
    scale_x_continuous(breaks = seq.int(from = 0, to = max_score, by = 1)) +
    scale_y_continuous(breaks = seq.int(from = 0, to = max_score, by = 1)) +
    theme_snappa()
  
  
}

