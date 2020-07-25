


# UI functions ------------------------------------------------------------

# Score pop-up dialog box
score_check <- function(team, players) {
  # Identify which team scored
  team_scored = paste("ok", team, sep = "_")
  team_colour = if_else(team_scored == "ok_A", "#e26a6a", "#2574a9")
  score_val = paste(team, "score_val", sep = "_")
  
  # Ask how many points were scored and by whom
  modalDialog(align = "center", easyClose = T, size = "l",
              # Header
              h2(str_c("Team ", str_to_upper(team), " Scored"), style = paste("color:", team_colour)),

              fluidRow(
                column(8,
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
                       )
                       ),
                column(4, 
                       wellPanel(
                         align = "center",
                         h3("Anything cool happen?", style = "font-weight:700; font-size: 2rem;"),
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
                         )
                       )
                       
                       )
              )
              ,
              
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
  team_colour = if_else(team == "A", "#e26a6a", "#2574a9")
  
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

team_scoreboard_ui = function(left_team = "A", right_team = "B"){
  
  team_colours = list("A" = "#e26a6a", "B" = "#2574a9")

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
             )
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

#For the restart game screen, I'm going to make a UI to handle most of the modalDialog
# output. My idea is that if I make a function which can just do this based on the game, 
# then we can also parlay this into other things (e.g. a game history ui) at a later time
glance_table_team = function(game.id, team.id){
  base_table = tbl(con, "player_stats") %>% 
    left_join(tbl(con, "players"), by = "player_id") %>%
    collect() %>% 
    filter(game_id == game.id, team == team.id) %>%
    select(player_name, total_points, team, paddle_points, shots, toss_efficiency)
  return(base_table)
}

glance_ui_team = function(df){
  # Sneakily cheat and change the table one more team because we 
  # need team, but not for the table
  team.id = pull(df, team) %>% unique()
  new_df = df %>% select(-team)
  # gt time
  output_table = new_df %>% 
    gt() %>%
      tab_header(title = str_c("Team ", 
                              str_to_upper(team.id)
                              )) %>%
      cols_label(
        player_name = "Player",
        total_points = "Total Points",
        paddle_points = "Paddle Points",
        shots = "Shots",
        toss_efficiency = "Toss Efficiency") %>% 
    fmt_number(
      columns = vars(toss_efficiency),
      decimals = 3
    ) %>%
    tab_style(style = cell_text(align = 'center'),
              locations = cells_body(
                columns = vars(player_name, total_points, paddle_points, shots, toss_efficiency)
              )) %>%
    tab_style(style = cell_text(align = 'center'),
              locations = cells_column_labels(vars(player_name))) %>% 
    tab_options(heading.border.lr.style = "none",
                heading.border.bottom.style = "none",
                column_labels.border.top.style = "none",
                column_labels.border.bottom.style = "solid",
                column_labels.border.bottom.color = "#7c7c7c",
                column_labels.border.bottom.width = "3px",
                column_labels.border.lr.style = "none",
                table.border.top.style = "none",
                table.border.right.style = "none",
                table.border.left.style = "none",
                table.border.bottom.style = "none",
                column_labels.font.weight = "600")
  
  return(output_table)
}

glance_ui_game = function(game.id){
  # Gather the items that are needed to assemble the UI
  df_a = glance_table_team(game.id, "A") 
  df_b = glance_table_team(game.id, "B") 
  
  score_a = df_a %>% pull(total_points) %>% sum()
  score_b = df_b %>% pull(total_points) %>% sum()  
  
  # Now, set up the UI 
  ui_output = fluidRow(
    column(5, 
           render_gt(glance_ui_team(df_a))),
    column(2,
           h2(str_c(score_a, " - ", score_b), align = 'center')),
    column(5, 
           render_gt(glance_ui_team(df_b)))
    )
 return(ui_output)
}


# Stats Output ------------------------------------------------------------

theme_snappa = function(){
  theme_minimal(
    base_family = "Inter",
    base_size = 18
  ) %+replace%
    theme(
      plot.title = element_text(size = rel(1.25), margin = margin(b = 20)), 
      panel.grid.minor = element_line(color = "grey", size = 0.05),
      panel.grid.major = element_blank(),
      plot.background = element_rect(fill = "#f5f5f5", colour = "#f5f5f5")
    ) 
}

leaderboard_table = function(df){
  df %>% 
    select(rank, player_name, games_played, total_points, total_shots, points_per_game, off_ppg, def_ppg, toss_efficiency) %>% 
    gt(., id = "leaderboard") %>% 
    tab_header(title = "Snappaneers Leaderboard", 
               subtitle = "The Deadliest Die-throwers in all the land.") %>% 
    # Column names
    cols_label(
      rank = "", 
      player_name = "Player",
      games_played = "Games Played",
      total_points = "Total Points",
      total_shots = "Total Shots",
      points_per_game = "Points per Game\n(PPG)",
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
      style = list(cell_text(weight = "bold", size = "x-large")),
      locations = cells_title(groups = "title")
    ) %>%
    tab_style(style = cell_text(align = "left", v_align = "bottom"),
              locations = list(cells_title("title"), cells_title("subtitle"))) %>% 
    # Rank column
    tab_style(
      style = list(cell_text(weight = "bold")),
      locations = cells_body(
        columns = vars(rank)
      )
    ) %>% 
    cols_align(align = "right") %>% 
    cols_align(align = "left", columns = c("player_name", "rank")) %>% 
    # Left Align Player and Rank
    # Column widths
    cols_width(
      vars(rank) ~ px(30),
      vars(player_name) ~ px(220),
      vars(points_per_game) ~ px(110),
      vars(total_points, games_played, total_shots) ~ px(60),
      vars(off_ppg, def_ppg, toss_efficiency) ~ px(95)
    ) %>% 
    # Underline dope shit
    tab_style(
      style = list(cell_text(weight = "bold"), cell_fill(color = "#FFD600", alpha = .8)),
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
                column_labels.border.top.style = "none",
                column_labels.border.bottom.style = "solid",
                column_labels.border.bottom.color = "#7c7c7c",
                column_labels.border.bottom.width = "3px",
                column_labels.border.lr.style = "none",
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
  
  score_labels = seq_len(max_score + 1) - 1 
  score_labels[score_labels %% 2 == 0] = ""
  score_labels = score_labels %>% as.character()
  # Create helpers for the labels in the scale
  
  
  df %>% 
    ggplot(aes(x = score_b, y = score_a))+
    geom_tile(aes(fill = n), color = "black")+
    # Record the 45 degree line for visual reference
    geom_abline(mapping = NULL, data = NULL, slope = 1, intercept = 0) + 
    scale_fill_gradient(name = "Frequency", low = "#ffeda0", high = "#f03b20", na.value = "grey")+
    labs(x = "Team B",
         y = "Team A",
         title = "Heatmap of scores in Snappa")+
    coord_cartesian(xlim = c(1, max_score - 1),
                    ylim = c(1, max_score - 1)) +
    scale_x_continuous(breaks = seq.int(from = 0, to = max_score, by = 1), labels = score_labels) +
    scale_y_continuous(breaks = seq.int(from = 0, to = max_score, by = 1), labels = score_labels) +
    theme_snappa()
  
  
}



