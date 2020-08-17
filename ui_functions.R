


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
           h1(paste("Team", toupper(team)), style = "align: center; color: white; font-size: 7rem;"),
           # Player 1
           selectizeInput(paste0('name_', team, '1'), 'Player 1', c(`Player Name`='', player_choices),  options = list(create = TRUE)),
           # Player 2
           selectizeInput(paste0('name_', team, '2'), 'Player 2', c(`Player Name`='', player_choices), options = list(create = TRUE)),
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
                       h1(paste("Team", toupper(left_team)), style = "color: white; font-size: 7rem;"),
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
                       )#,
                       # h3(textOutput(paste0("player_names_", left_team)))
                     )
              ), 
              # Round
              column(width = 4, align = "center",
                     # materialSwitch(
                     #   inputId = "switch_sides",label = "Switch sides", icon = icon("refresh"), 
                     # ),
                     
                     h1("Round", style = "font-size: 8rem;"),
                     h3(textOutput("round_num")),
                     fluidRow(actionBttn("previous_round", 
                                         label = "Previous Round", style = "jelly", icon = icon("arrow-left"), color = "primary", size = "lg"),
                              actionBttn("next_round", 
                                         label = "Pass the dice", style = "jelly", icon = icon("arrow-right"), color = "primary", size = "lg")),
                     br(),
                     # Recent Scores
                     dropdown(
                       inputId = "recent_scores",
                       gt_output("recent_scores"),
                       style = "unite",
                       size = "lg", 
                       up = T,
                       label = "Recent Scores",
                       icon = icon("backward"),
                       animate = animateOptions(
                         enter = animations$fading_entrances$fadeInUp,
                         exit = animations$fading_exits$fadeOutDown
                       )
                     )
              ),
              # Team B
             column(width = 4, align = "center",
                    
                    wellPanel(
                      style = paste("opacity: 0.92; background:", team_colours[[right_team]]),
                      # uiOutput("active_die_a"),
                      # Header
                      h1(paste("Team", toupper(right_team)), style = "color: white; font-size: 7rem;"),
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
                      )#,
                      # h3(textOutput(paste0("player_names_", right_team)))
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
                              label = paste('Player', player_num), c(`Player Name`='', player_choices), options = list(create = TRUE)),
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

snappa_pal = c("#f5f5ef","#e26a6a","#2574a9","#ffaf47","#67a283","#793e8e","#54b6f2")

theme_snappa = function(title_family = "Inter SemiBold",
                        text_family = "Inter",
                        base_size = 12, 
                        text_color = "gray20",
                        bg_color = snappa_pal[1], line_color = "grey",
                        plot_margin = margin(20,20,20,20),
                        plots_pane = FALSE,
                        md = FALSE){
  
  if (plots_pane == FALSE & md == FALSE) {
    ggplot2::theme_minimal() +
      ggplot2::theme(
        text = element_text(family = text_family,
                            size = base_size,
                            color = text_color),
        title = element_text(family = title_family,
                             color = text_color),
        line = element_line(color = "#DEDDDD"),
        
        plot.title = element_text(face = "bold",
                                  size = base_size * 2,
                                  lineheight = 1.2), 
        plot.title.position = "plot",
        plot.subtitle = element_text(size = base_size * 1.5,
                                     lineheight = 1.1, 
                                     family = text_family,
                                     margin = margin(b=15)),
        plot.margin = plot_margin,
        plot.caption.position = "plot", 
        plot.caption = element_text(hjust = 0, 
                                    size = base_size * 1.25),
        plot.background = element_rect(fill = bg_color,
                                       color = bg_color),
        
        axis.text = element_text(size = base_size * 1.2),
        axis.text.y.left = element_text(hjust = 0),
        axis.title = element_text(size = base_size * 1.6,
                                  hjust = 1, face = "italic"),
        axis.line = element_line(color = line_color),
        
        legend.title = element_text(size = base_size * 1.3),
        legend.text = element_text(size = base_size * 1.1)
      )
  } else if (plots_pane == FALSE & md == TRUE) {
    ggplot2::theme_minimal() +
      ggplot2::theme(
        text = element_text(family = text_family,
                            size = base_size,
                            color = text_color),
        title = ggtext::element_markdown(family = title_family,
                                         color = text_color),
        line = element_line(color = "#DEDDDD"),
        
        plot.title = ggtext::element_markdown(face = "bold",
                                              size = base_size * 2,
                                              lineheight = 1.2),
        plot.title.position = "plot",
        plot.subtitle = ggtext::element_markdown(size = base_size * 1.7,
                                                 lineheight = 1.1, 
                                                 family = text_family,
                                                 margin = margin(b=15)),
        plot.margin = plot_margin,
        plot.caption.position = "plot",
        plot.caption = ggtext::element_markdown(hjust = 0, 
                                                size = base_size * 1.25),
        plot.background = element_rect(fill = bg_color,
                                       color = bg_color),
        
        axis.text = element_text(size = base_size * 1.2),
        axis.text.y.left = element_text(hjust = 0),
        axis.title = ggtext::element_markdown(size = base_size * 1.6,
                                              hjust = 1, face = "italic"),
        axis.line = element_line(color = line_color),
        
        legend.title = ggtext::element_markdown(size = base_size * 1.3),
        legend.text = element_text(size = base_size * 1.1)
      )
  } else if (plots_pane == TRUE && md == TRUE) {
    ggplot2::theme_minimal(base_size = base_size) +
      ggplot2::theme(
        text = element_text(family = text_family,
                            color = text_color),
        title = element_text(family = title_family),
        line = element_line(color = "#DEDDDD"),
        
        plot.title = ggtext::element_markdown(face = "bold",
                                              lineheight = 1.2),
        plot.title.position = "plot",
        plot.subtitle = ggtext::element_markdown(lineheight = 1.1, 
                                                 family = text_family,
                                                 margin = margin(b=15)),
        plot.margin = plot_margin,
        plot.caption.position = "plot",
        plot.caption = ggtext::element_markdown(hjust = 0, 
                                                size = base_size * 1.25),
        plot.background = element_rect(fill = bg_color,
                                       color = bg_color),
        axis.text.y.left = element_text(hjust = 0),
        axis.title = ggtext::element_markdown(hjust = 1, face = "italic"),
        axis.line = element_line(color = line_color)
      )
  } else {
    ggplot2::theme_minimal(base_size = base_size) +
      ggplot2::theme(
        text = element_text(family = text_family,
                            color = text_color),
        title = element_text(family = title_family),
        line = element_line(color = "#DEDDDD"),
        
        plot.title = element_text(face = "bold",
                                  lineheight = 1.2),
        plot.title.position = "plot",
        plot.subtitle = element_text(lineheight = 1.1, 
                                     family = text_family,
                                     margin = margin(b=15)),
        plot.margin = plot_margin,
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, 
                                    size = base_size * 1.25),
        plot.background = element_rect(fill = bg_color,
                                       color = bg_color),
        axis.text.y.left = element_text(hjust = 0),
        axis.title = element_text(hjust = 1, face = "italic"),
        axis.line = element_line(color = line_color)
      )
  }
}

tab_theme_snappa = function(data, 
                            # Container
                            container.width = pct(95), 
                            container.height = pct(50), 
                            container.overflow.x = NULL, 
                            container.overflow.y = NULL, 
                            # Table
                            table.width = NULL, table.layout = NULL, table.align = NULL, 
                            table.margin.left = NULL, table.margin.right = NULL, table.background.color = snappa_pal[1], 
                            table.additional_css = NULL, table.font.names = NULL, table.font.size = NULL, 
                            table.font.weight = NULL, table.font.style = NULL, table.font.color = NULL, 
                            table.font.color.light = NULL, 
                            table.border.top.style = "none",table.border.top.width = NULL, table.border.top.color = NULL, 
                            table.border.right.style = "none", table.border.right.width = NULL,table.border.right.color = NULL, 
                            table.border.bottom.style = "none",table.border.bottom.width = NULL, table.border.bottom.color = NULL, 
                            table.border.left.style = "none", table.border.left.width = NULL,table.border.left.color = NULL, 
                            # Heading
                            heading.background.color = NULL, 
                            heading.align = NULL, 
                            heading.title.font.size = NULL, heading.title.font.weight = NULL, 
                            heading.subtitle.font.size = NULL, heading.subtitle.font.weight = NULL, 
                            heading.border.bottom.style =  "none", heading.border.bottom.width = NULL, heading.border.bottom.color = NULL, 
                            heading.border.lr.style = "none", heading.border.lr.width = NULL, heading.border.lr.color = NULL, 
                            # Column labels
                            column_labels.background.color = NULL, 
                            column_labels.font.size = NULL, column_labels.font.weight = NULL, 
                            column_labels.text_transform = NULL, 
                            column_labels.vlines.style = NULL, column_labels.vlines.width = NULL, column_labels.vlines.color = NULL,
                            column_labels.border.top.style =  "none", column_labels.border.top.width = NULL, column_labels.border.top.color = NULL, 
                            column_labels.border.bottom.style = "solid", column_labels.border.bottom.width = "3px", column_labels.border.bottom.color = "#7c7c7c", 
                            column_labels.border.lr.style = "none", column_labels.border.lr.width = NULL, column_labels.border.lr.color = NULL, 
                            column_labels.hidden = NULL, 
                            # Row group
                            row_group.background.color = NULL, 
                            row_group.font.size = NULL, row_group.font.weight = NULL, 
                            row_group.text_transform = NULL, row_group.padding = NULL, 
                            row_group.border.top.style = NULL, row_group.border.top.width = NULL, 
                            row_group.border.top.color = NULL, row_group.border.bottom.style = NULL, 
                            row_group.border.bottom.width = NULL, row_group.border.bottom.color = NULL, 
                            row_group.border.left.style = NULL, row_group.border.left.width = NULL, 
                            row_group.border.left.color = NULL, row_group.border.right.style = NULL, 
                            row_group.border.right.width = NULL, row_group.border.right.color = NULL, 
                            # Table body
                            table_body.hlines.style = NULL, table_body.hlines.width = NULL, 
                            table_body.hlines.color = NULL, 
                            table_body.vlines.style = NULL, 
                            table_body.vlines.width = NULL, table_body.vlines.color = NULL, 
                            table_body.border.top.style = NULL, table_body.border.top.width = NULL, table_body.border.top.color = "#DEDDDD", 
                            table_body.border.bottom.style = NULL, table_body.border.bottom.width = NULL, table_body.border.bottom.color = "#DEDDDD", 
                            # Stub
                            stub.background.color = NULL, stub.font.size = NULL, stub.font.weight = NULL, 
                            stub.text_transform = NULL, stub.border.style = NULL, stub.border.width = NULL, 
                            stub.border.color = NULL, 
                            data_row.padding = NULL, 
                            # Summary row
                            summary_row.background.color = NULL, 
                            summary_row.text_transform = NULL, summary_row.padding = NULL, 
                            summary_row.border.style = NULL, summary_row.border.width = NULL, 
                            summary_row.border.color = NULL, 
                            # Grand summary row
                            grand_summary_row.background.color = NULL, 
                            grand_summary_row.text_transform = NULL, grand_summary_row.padding = NULL, 
                            grand_summary_row.border.style = NULL, grand_summary_row.border.width = NULL, 
                            grand_summary_row.border.color = NULL, 
                            # Footnotes
                            footnotes.background.color = NULL, 
                            footnotes.font.size = NULL, footnotes.padding = NULL, footnotes.border.bottom.style = NULL, 
                            footnotes.border.bottom.width = NULL, footnotes.border.bottom.color = NULL, 
                            footnotes.border.lr.style = NULL, footnotes.border.lr.width = NULL, 
                            footnotes.border.lr.color = NULL, footnotes.sep = NULL, footnotes.marks = NULL, 
                            # Source notes
                            source_notes.background.color = NULL, source_notes.font.size = NULL, 
                            source_notes.padding = NULL, source_notes.border.bottom.style = NULL, 
                            source_notes.border.bottom.width = NULL, source_notes.border.bottom.color = NULL, 
                            source_notes.border.lr.style = NULL, source_notes.border.lr.width = NULL, 
                            source_notes.border.lr.color = NULL, 
                            # Row striping
                            row.striping.background_color = NULL, 
                            row.striping.include_stub = NULL, row.striping.include_table_body = NULL){
  gt:::stop_if_not_gt(data = data)
  opts_df <- gt:::dt_options_get(data = data)
  arg_names <- formals(tab_options) %>% names() %>% base::setdiff("data")
  arg_vals <- mget(arg_names)
  arg_vals <- arg_vals[!vapply(arg_vals, FUN = is.null, FUN.VALUE = logical(1))]
  arg_vals <- arg_vals %>% gt:::set_super_options()
  
  new_df <- dplyr::tibble(parameter = names(arg_vals) %>% 
                            gt:::tidy_gsub(".", "_", fixed = TRUE), value = unname(arg_vals)) %>%
    dplyr::left_join(opts_df %>% 
                       dplyr::select(parameter, type), by = "parameter") %>% 
    dplyr::mutate(value = mapply(gt:::preprocess_tab_option, option = value, 
                                 var_name = parameter, type = type, SIMPLIFY = FALSE)) %>% 
    dplyr::select(-type)
  
  opts_df <- dplyr::bind_rows(new_df %>% 
                                dplyr::inner_join(opts_df %>% 
                                                    dplyr::select(-value), by = "parameter"), opts_df %>% 
                                dplyr::anti_join(new_df, by = "parameter"))
  
  data <- gt:::dt_options_set(data = data, options = opts_df)
  data
}


team_summary_tab = function(df, team){
  winners = unique(df$winners)
  title_colour = if_else(unique(df$team) == "A", snappa_pal[2], snappa_pal[3])
  
  hide_diff_cols = head(df, 1) %>% # Take the first row of a column
    mutate_all(as.character) %>% # prevents errors in pivot_longer
    # convert to column-value pair dataframe
    pivot_longer(everything(), names_to = "column", values_to = "value") %>% 
    # remove the value
    select(-value) %>% 
    # Craft the desired label
    # Below I remove the year, replace underscores with spaces, and convert to title case
    mutate(label = case_when(
      # column == "player_name" ~ "Player",
      # column == "total_points" ~ "Total Points",
      # column == "paddle_points" ~ "Paddle Points",
      # column == "clink_points" ~ "Clink Points",
      # column == "sinks" ~ "Sinks", # TODO: Fix this 
      # column == "points_per_round" ~ "Points per Round\n(PPR)",
      # column == "off_ppr" ~ "Offensive PPR",
      # column == "def_ppr" ~ "Defensive PPR",
      # column == "toss_efficiency" ~ "Toss Efficiency",
      # T ~ ""
      column == "player_name" ~ "Player",
      str_detect(column, "_diff$") ~ "Diff.",
      T ~ ""
    )) %>% 
    # Deframe to named vector
    deframe()
  
  df %>% 
    arrange(-total_points) %>% 
    gt(.) %>% 
    tab_header(title = str_c("Team ", team),
               subtitle = if_else(winners, "the winners.", "the losers.")) %>% 
    cols_hide(columns = vars(player_id, team, winners)) %>% 
    # Column names
    cols_label(
      # player_name = "Player",
      # total_points = "Total Points",
      # paddle_points = "Paddle Points",
      # clink_points = "Clink Points",
      # sinks_total = "Sinks", # TODO: Fix this
      # points_per_round = "Points per Round\n(PPR)",
      # off_ppr = "Offensive PPR",
      # def_ppr = "Defensive PPR",
      # toss_efficiency = "Toss Efficiency",
      .list = hide_diff_cols
    ) %>%
    # Format integers
    fmt_number(
      columns = vars(total_points, paddle_points, clink_points, sinks),
      decimals = 0
    ) %>% 
    # Format doubles
    fmt_number(
      columns = vars(points_per_round, off_ppr, def_ppr),
      decimals = 2
    ) %>% 
    # Format percentages
    fmt_percent(
      columns = vars(toss_efficiency),
      decimals = 0
    ) %>% 
    # Styling
    # Title
    tab_style(
      style = list(cell_text(weight = "bold", size = px(20), color = title_colour)),
      locations = cells_title(groups = "title")
    ) %>%
    # Subtitle
    tab_style(style = cell_text(align = "left", v_align = "bottom"),
              locations = list(cells_title("title"), cells_title("subtitle"))) %>% 
    # Colour stat differences
    ## Positive
    tab_style(style = cell_text(color = snappa_pal[5]),
              locations = list(
                cells_body(columns = vars(total_points_diff), rows = str_detect(total_points_diff, "\\+")),
                cells_body(columns = vars(paddle_points_diff), rows = str_detect(paddle_points_diff, "\\+")),
                cells_body(columns = vars(clink_points_diff), rows = str_detect(clink_points_diff, "\\+")),
                cells_body(columns = vars(points_per_round_diff), rows = str_detect(points_per_round_diff, "\\+")),
                cells_body(columns = vars(off_ppr_diff), rows = str_detect(off_ppr_diff, "\\+")),
                cells_body(columns = vars(def_ppr_diff), rows = str_detect(def_ppr_diff, "\\+")),
                cells_body(columns = vars(toss_efficiency_diff), rows = str_detect(toss_efficiency_diff, "\\+"))
              )
    ) %>% 
    ## Negative
    tab_style(style = cell_text(color = snappa_pal[2]),
              locations = list(
                cells_body(columns = vars(total_points_diff), rows = str_detect(total_points_diff, "\\-")),
                cells_body(columns = vars(paddle_points_diff), rows = str_detect(paddle_points_diff, "\\-")),
                cells_body(columns = vars(clink_points_diff), rows = str_detect(clink_points_diff, "\\-")),
                cells_body(columns = vars(points_per_round_diff), rows = str_detect(points_per_round_diff, "\\-")),
                cells_body(columns = vars(off_ppr_diff), rows = str_detect(off_ppr_diff, "\\-")),
                cells_body(columns = vars(def_ppr_diff), rows = str_detect(def_ppr_diff, "\\-")),
                cells_body(columns = vars(toss_efficiency_diff), rows = str_detect(toss_efficiency_diff, "\\-"))
              )
    ) %>% 
    # Column spanners
    tab_spanner(
      label = "Total Points",
      columns = contains("total_points")
    ) %>% 
    tab_spanner(
      label = "Paddle Points",
      columns = contains("paddle")
    ) %>% 
    tab_spanner(
      label = "Clink Points",
      columns = contains("clink")
    ) %>% 
    tab_spanner(
      label = "Sinks",
      columns = contains("sink")
    ) %>% 
    tab_spanner(
      label = "Pts per Round",
      columns = contains("per_")
    ) %>% 
    tab_spanner(
      label = "Off. PPR",
      columns = contains("off_")
    ) %>% 
    tab_spanner(
      label = "Def. PPR",
      columns = contains("def_")
    ) %>% 
    tab_spanner(
      label = "Toss Efficiency",
      columns = contains("toss")
    ) %>% 
    tab_source_note("Comparison of current game to career average") %>% 
    # tab_source_note(md(str_c('<span style="font-size: 18px;font-weight: 700;color:', snappa_pal[2], ';">Snappa</span>',
    #                          '<span style="font-size: 18px;font-weight: 700;color:', snappa_pal[4], ';">DB</span>'))) %>% 
    # Footnotes
    # tab_footnote(
    #   footnote = "Defensive points are scored from paddles.",
    #   locations = cells_column_spanners("Def. PPR")#cells_column_labels(columns = vars(def_ppr))
    # ) %>% 
    # tab_footnote(
    #   footnote = "% of tosses which are successful.",
    #   locations = cells_column_spanners("Toss Efficiency")#cells_column_labels(columns = vars(toss_efficiency))
    # ) %>%
    # opt_footnote_marks(marks = "letters") %>% 
    # Styling
  tab_style(
    style = cell_text(weight = 600, size = px(15), v_align = "bottom"),
    locations = map(c("Total Points","Paddle Points","Clink Points","Sinks","Pts per Round","Off. PPR","Def. PPR","Toss Efficiency"),
                    cells_column_spanners)
  ) %>% 
    tab_style(
      style = cell_text(size = px(14)),
      locations = cells_column_labels(everything())
    ) %>% 
    cols_align(align = "right", 
               columns = colnames(df) %>% str_subset("player_name", negate = T)) %>% 
    # Left Align player
    cols_align(align = "left", 
               columns = c("player_name")) %>%
    tab_style(
      style = cell_text(align = "left"),
      locations = cells_column_labels(columns = vars(player_name))
    ) %>% 
    # Column widths
    cols_width(
      vars(player_name) ~pct(7),#px(120),
      vars(total_points, paddle_points, clink_points) ~ pct(3), 
      vars(points_per_round, off_ppr, def_ppr) ~ pct(4),
      vars(toss_efficiency, sinks) ~ pct(5),#px(60),
      ends_with("points_diff") ~ pct(5),#px(50),
      matches("(per_round|ppr|efficiency)_diff$") ~ pct(6)#px(55),
    ) %>% 
    tab_theme_snappa()
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



player_score_breakdown = function(df){
  
  team = unique(df$team)
  
  reverse_legend = (team == "A")
  
  df = df %>% 
    mutate(point_pct = points/sum(points))
  
    df %>%
    ggplot(., aes(y = player_name, x = points, fill = reorder(point_type, desc(point_type))))+
    # Bars
    geom_col(position = "fill", colour = snappa_pal[1], size = 1.5)+
    # Labels
    geom_text(data = filter(df, point_pct > .15), aes(label = scales::percent(point_pct, accuracy = 1)), 
              position = position_fill(vjust = .5), colour = "white", show.legend = F)+
    # Y Axis
    scale_y_discrete(name = NULL, position = if_else(reverse_legend, "left", "right"))+
    # X Axis
    scale_x_continuous(name = NULL, labels = scales::percent)+
    # Colour scale
    scale_fill_manual(name = NULL, 
                      values = c("Normal toss" = "#67A283", "Paddle" = "#793E8E", "Clink" = "#54B6F2", "Sink" = "#FFA630" ),
                      guide = guide_legend(reverse = reverse_legend, label.hjust = 0.5, ncol = 2))+
    # Theme elements
    theme_snappa(md=T, plot_margin = margin(0,0,0,0), base_size = 11)+
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_blank(),
      legend.position = "bottom",
      legend.margin = margin(0,-30,0,-30),
      legend.text.align = .5,
      axis.text.x = element_blank(),
      axis.text.y.left = element_text(margin = margin(l = -5, r = -5), hjust = 1),
      axis.text.y.right = element_text(margin = margin(l = -5, r = -5), hjust = 0)
    )
}

game_flow = function(df){

  # Add zeroes
  player_score_base = df %>% 
    group_by(game_id, player_id, player_name, team) %>% 
    summarise(round = min(round)-1, points_scored = 0, cum_score = 0)
  
  player_label_df = df %>% 
    filter(cum_score == max(cum_score)) %>% 
    select(player_name, player_id, team, round, cum_score)
  
  # Calculate max values for scales
  max_score = max(df$cum_score)
  max_round = max(df$round)

  df %>% 
    bind_rows(player_score_base) %>% 
    ggplot(., aes(x = round, y = cum_score))+
    geom_line(aes(group = player_id, colour = team), size = 1, show.legend = F, alpha = .8)+
    geom_label_repel(data = player_label_df,
                     aes(group = player_id, colour = team, label = player_name),
                     size = 5, label.padding = .15, box.padding = .15, label.size = NA, fill = snappa_pal[1],
                     nudge_x = 1.25, nudge_y = .5, force = .35, show.legend = F, segment.alpha = 0)+
    # geom_image(data = filter(game_flow_df, !is.na(sink_image)))+
    scale_y_continuous(name = "Points", 
                       breaks = scales::breaks_pretty(n = 5), 
                       limits = c(0, max_score+5-(max_score%%5)),
                       expand = expansion())+
    scale_x_continuous(name = "Round", 
                       breaks = scales::breaks_pretty(n =10), 
                       limits = c(0, max_round+5),
                       expand = expansion())+
    scale_colour_manual(values = c("A" = "#e26a6a", "B" = "#2574a9"))+
    labs(title = "How the die flies",
         subtitle = "Players' point progression")+ #<img src = "www/sink.png" width="30px" height="30px">
    theme_snappa(md=T)
}



