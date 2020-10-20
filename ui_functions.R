toss_percent_plus = function(x){
  str_c("(+", round(x*100, 0), "%)")
}
toss_percent_minus = function(x){
  str_c("(", round(x*100, 0), "%)")
}

rounds = str_c(rep(1:100, each = 2), rep(c("A", "B"), 100))

# UI functions ------------------------------------------------------------

# Score pop-up dialog box
score_check <- function(team, players, round) {
  # Identify which team scored
  team_scored = paste("ok", team, sep = "_")
  team_colour = if_else(team_scored == "ok_A", "#e26a6a", "#2574a9")
  score_val = paste(team, "score_val", sep = "_")
  
  # Ask how many points were scored and by whom
  modalDialog(align = "center", easyClose = T, size = "l",
              # Header
              h2(HTML(str_c("Round <span style='font-weight:700'>", round, "</span>", ": ", "<span style='color:", team_colour, "'>", "Team ", str_to_upper(team), " Scored</span>")),
                 style = "margin-bottom: 2vh;"),

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
                      actionBttn("score_breakdown", 
                                 "Check your current score summary",
                                 style = "jelly",
                                 color = "primary",
                                 size = "lg"),
                     h1("Round", style = "font-size: 8rem;"),
                     h3(textOutput("round_num")),
                     column(width=12, align = "center",
                       actionBttn("next_round", 
                                  label = "Pass the dice", style = "jelly", icon = icon("arrow-right"), color = "primary", size = "lg"),
                       actionBttn("previous_round", 
                                  label = "Previous Round", style = "jelly", icon = icon("arrow-left"), color = "primary", size = "lg")
                     ),
                     # fluidRow(,
                     #          ),
                     br()
                     
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
             tags$style(paste0("#add_remove_", player, " {margin-left:auto; margin-right:auto; position: relative;}")),

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

recent_scores_tab = function(scores_data){
  
  scores_data %>% 
    mutate(colour = if_else(scoring_team == "A", snappa_pal[2], snappa_pal[3]),
           # Who scored?
           scorer = str_c("<span style = 'font-weight: 600;color:", 
                          colour, ";'>",
                          player_name, "</span>"), 
           # How many points?
           points = str_c(" scored <span style='font-weight: 600;'>",
                          points_scored,
                          if_else(points_scored > 1, " points", " point"), "</span>",
                          if_else(clink, " with a clink", ""), " for"),
           # In what round?
           when_scored= str_c(" in round <span style = 'font-weight: 600;'>", round_num, "</span>"),
           # For which team?
           for_whom = str_c(" <span style = 'font-weight: 600;color:", 
                            colour, ";'>", "Team ", toupper(scoring_team), "</span>"),
           # Anything special?
           special = str_c(
             if_else(paddle, str_c(" and it was a", 
                                   if_else(foot, " foot", ""), " paddle!"), ""
             )
           ),
           # Put the sentence together
           sentence = str_c(scorer, points, for_whom, when_scored, special)
    ) %>% 
    select(sentence) %>% 
    gt() %>% 
    # Format the sentence with markdown
    fmt_markdown(vars(sentence)) %>% 
    tab_theme_snappa() %>% 
    tab_options(column_labels.hidden = T)
}

#For the restart game screen, I'm going to make a UI to handle most of the modalDialog
# output. My idea is that if I make a function which can just do this based on the game, 
# then we can also parlay this into other things (e.g. a game history ui) at a later time
glance_table_team = function(game.id, team.id){
  base_table = dbGetQuery(con, str_c("SELECT players.player_name, ps.total_points, ps.team, ps.paddle_points, ps.shots, ps.toss_efficiency FROM player_stats AS ps
                                LEFT JOIN players ON players.player_id = ps.player_id
                          WHERE ps.game_id = ", game.id, " AND ps.team = '", team.id, "'"))
  return(base_table)
}

glance_ui_team = function(df, team){
  # Sneakily cheat and change the table one more team because we 
  # need team, but not for the table
  
  title_colour = if_else(!!team == "A", snappa_pal[2], snappa_pal[3])
  new_df = df %>% select(-team)
  # gt time
  output_table = new_df %>% 
    gt() %>%
      tab_header(title = str_c("Team ", 
                              str_to_upper(team)
                              )) %>%
      cols_label(
        player_name = "Player",
        total_points = "Total Points",
        paddle_points = "Paddle Points",
        shots = "Shots",
        toss_efficiency = "Toss Efficiency") %>% 
    fmt_percent(
      columns = vars(toss_efficiency),
      decimals = 0
    ) %>%
    tab_style(style = cell_text(align = 'center'),
              locations = cells_body(
                columns = vars(player_name, total_points, paddle_points, shots, toss_efficiency)
              )) %>%
    tab_style(style = cell_text(align = 'center'),
              locations = cells_column_labels(vars(player_name))) %>% 
    tab_style(style = cell_text(color = title_colour),
              locations = cells_title("title")) %>% 
    tab_theme_snappa()
  
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
           render_gt(glance_ui_team(df_a, team = "A"))),
    column(2,
           h2(str_c(score_a, " - ", score_b), align = 'center')),
    column(5, 
           render_gt(glance_ui_team(df_b, team = "B")))
    )
 return(ui_output)
}


# Stats Output ------------------------------------------------------------

snappa_pal = str_c("#", c("fafaf9","e26a6a","2574a9","ffaf47","67a283","793e8e","54b6f2"))

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

make_summary_table = function(current_player_stats, player_stats, neers, team_name, current_round, past_scores){
  
  # Produce a team's performance summary and comparison to historical performance in equivalent games
  # 1. Get a list of games the player has played in
  # 2. Get a list of scores from historical games at the equivalent point in the game
  # 3. Calculate current game player stats
  # 4. Calculate historical player stats from 1 & 2
  team_size = nrow(neers[neers$team == team_name, ]) # 20x as fast
  opponent_size = nrow(neers[neers$team != team_name, ])
  
  # Make a historical stats table that is only comparing games which are similar
  # to the current one.
  # First, obtain a list of games in which the players on this team were on
  # an equally sized team. This is player specific, so map() is used
  games_list = current_player_stats %>% filter(team == team_name) %>% pull(player_id) %>%
    sort() %>% 
    map(~{
      player_stats %>% group_by(game_id, team) %>% mutate(team_size = n()) %>%
        filter(player_id ==.x,  team_size == team_size) %>% pull(game_id)
    })
  
  
  # make a unique subsection of the scores table which only considers the given player in the
  # given games. round_comparison should only be applied when a game is in progress. 
  # While we're here, also tell the display not to care about winners maybe? I could also set
  # a value here so that I don't have to execute a query later, but the issue becomes 
  
  if (isFALSE(pull(dbGetQuery(con, "SELECT game_complete FROM game_stats WHERE game_id = (SELECT MAX(game_id) FROM game_stats);"), 
                   game_complete))){
    round_comparison = current_round
  } else {
    round_comparison = 999
  }
  
  scores_slice = neers %>% filter(team == team_name) %>% 
    pull(player_id) %>% 
    sort() %>%
    imap( ~{
      past_scores %>% filter(game_id %in% games_list[[.y]], as.numeric(str_sub(round_num, 1, -2)) <= round_comparison, 
                             player_id == .x)
    })
  
  # Bind that list together to make historical scores
  historical_scores = list.rbind(scores_slice)
  
  # Now, this table is going to be plugged in to the pipeline that currently exists in the team summary tab function.
  # That means I have to recreate player_stats using this table 
  
  comparison_player_stats = historical_scores %>% 
    # Join scores to snappaneers to get each player's team
    left_join(neers, by = "player_id") %>% 
    # Group by game and player, (team and shots are held consistent)
    group_by(game_id, player_id, team, shots) %>% 
    # Calculate summary stats
    summarise(total_points = sum(points_scored),
              ones = sum((points_scored == 1)),
              twos = sum((points_scored == 2)),
              threes = sum((points_scored == 3)),
              impossibles = sum((points_scored > 3)),
              paddle_points = sum(points_scored* (paddle | foot)),
              clink_points = sum(points_scored*clink),
              points_per_round = total_points / last(shots),
              off_ppr = sum(points_scored * !(paddle | foot))/ last(shots),
              def_ppr = paddle_points/last(shots),
              toss_efficiency = sum(!(paddle | foot ))/last(shots)) %>% 
    ungroup()
  
  player_info = current_player_stats %>% 
    # Filter player stats
    # filter(game_id == max(game_id)) %>% 
    select(game_id, player_id, team) %>% 
    inner_join(neers) 
  
  player_summary = current_player_stats %>% 
    # Select the last game
    # filter(game_id == max(game_id)) %>% 
    group_by(team) %>% 
    mutate(team_score = sum(total_points)) %>% 
    ungroup() %>% 
    mutate(winning = (team_score == max(team_score))) %>% 
    # Merge in player info
    inner_join(player_info) %>% 
    select(team, winning, player_id, player_name, 
           total_points, paddle_points, clink_points, threes, 
           points_per_round:toss_efficiency)
  
  historical_stats = comparison_player_stats %>% 
    select(-team) %>% 
    # Join in player name
    inner_join(select(player_summary, player_id, team)) %>% 
    select(game_id, player_id, 
           shots, total_points, paddle_points, clink_points, sinks = threes, 
           points_per_round:toss_efficiency) %>%
    arrange(player_id, game_id) %>% 
    group_by(player_id) %>% 
    mutate(game_num = 1:n()) %>% 
    ungroup() %>% 
    group_by(player_id) %>% 
    summarise(
      across(.cols = c(total_points, paddle_points, clink_points), .fns = mean, .names = "{col}_avg"),
      across(.cols = c(sinks), .fns = sum, .names = "{col}_total"),
      across(.cols = c(points_per_round, off_ppr, def_ppr, toss_efficiency),
             .fns = ~weighted.mean(., w = shots), 
             .names = "{col}_wavg")
    )
  
  player_summary_historical = full_join(player_summary, historical_stats, by = "player_id") %>% 
    # Calculate the difference between current game and historical performance
    mutate(total_points_diff = total_points - total_points_avg,
           paddle_points_diff = paddle_points - paddle_points_avg,
           clink_points_diff = clink_points - clink_points_avg,
           points_per_round_diff = points_per_round - points_per_round_wavg,
           off_ppr_diff = off_ppr - off_ppr_wavg,
           def_ppr_diff = def_ppr - def_ppr_wavg,
           toss_efficiency_diff = toss_efficiency - toss_efficiency_wavg,
           # Format each difference for the table
           across(matches("points_diff"), ~str_c("(", if_else(.x >= 0, "+", ""), round(.x, 1), ")")),
           across(matches("(per_round|ppr)_diff$"), ~str_c("(", if_else(.x >= 0, "+", ""), round(.x, 2), ")")),
           toss_efficiency_diff = map_chr(toss_efficiency_diff, ~case_when(. >= 0 ~ toss_percent_plus(.), 
                                                                           . < 0 ~ toss_percent_minus(.)))) %>% 
    # Remove historical stats
    select(-ends_with("_avg"), -ends_with("wavg")) %>% 
    # Order columns
    select(starts_with("player"), team, winning, 
           contains("total_points"), contains("paddle"), contains("clink"), sinks = threes, 
           contains("per_round"), contains("off_"), contains("def_"), contains("toss")) 
  
  df = select(player_summary_historical,
              -contains("clink"), -contains("sink"), -contains("points_per")) %>% 
    filter(team == team_name)
  
  return(df)
}

team_summary_tab = function(df, game_over, score_difference){
  winning = unique(df$winning)
  if (game_over){
    subtitle_name = if_else(winning, "the winners.", "the losers.")
  } else{
    subtitle_name = if_else(winning, 
                            "in the lead.", 
                            str_c("chasing ", 
                                  score_difference,
                                  ".")
                            )
  }
  
  
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
               subtitle = subtitle_name) %>% 
    cols_hide(columns = vars(player_id, team, winning)) %>% 
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
      # columns = vars(total_points, paddle_points, clink_points, sinks),
      columns = vars(total_points, paddle_points),
      decimals = 0
    ) %>% 
    # Format doubles
    fmt_number(
      # columns = vars(points_per_round, off_ppr, def_ppr),
      columns = vars(off_ppr, def_ppr),
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
      style = list(cell_text(align = "left", v_align = "bottom", weight = "bold", size = px(18), color = title_colour)),
      locations = cells_title(groups = "title")
    ) %>%
    # Subtitle
    tab_style(style = cell_text(align = "left", v_align = "bottom"),
              locations = list(cells_title("subtitle"))) %>% 
    # Colour stat differences
    ## Positive
    tab_style(style = cell_text(color = snappa_pal[5]),
              locations = list(
                cells_body(columns = vars(total_points_diff), rows = str_detect(total_points_diff, "\\+")),
                cells_body(columns = vars(paddle_points_diff), rows = str_detect(paddle_points_diff, "\\+")),
                # cells_body(columns = vars(clink_points_diff), rows = str_detect(clink_points_diff, "\\+")),
                # cells_body(columns = vars(points_per_round_diff), rows = str_detect(points_per_round_diff, "\\+")),
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
                # cells_body(columns = vars(clink_points_diff), rows = str_detect(clink_points_diff, "\\-")),
                # cells_body(columns = vars(points_per_round_diff), rows = str_detect(points_per_round_diff, "\\-")),
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
    # tab_spanner(
    #   label = "Clink Points",
    #   columns = contains("clink")
    # ) %>% 
    # tab_spanner(
    #   label = "Sinks",
    #   columns = contains("sink")
    # ) %>% 
    # tab_spanner(
    #   label = "Pts per Round",
    #   columns = contains("per_")
  # ) %>% 
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
    style = cell_text(weight = "normal", size = px(14), v_align = "bottom"),
    # locations = map(c("Total Points","Paddle Points","Clink Points","Sinks","Pts per Round","Off. PPR","Def. PPR","Toss Efficiency"),
    locations = map(c("Total Points","Paddle Points","Off. PPR","Def. PPR","Toss Efficiency"),
                    cells_column_spanners)
  ) %>% 
    # Column Labels
    tab_style(
      style = cell_text(size = px(12)),
      locations = cells_column_labels(everything())
    ) %>% 
    # Table body text
    tab_style(
      style = cell_text(size = px(12)),
      locations = cells_body(everything())
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
      # vars(total_points, paddle_points, clink_points) ~ pct(3), 
      vars(total_points, paddle_points) ~ pct(3), 
      # vars(points_per_round, off_ppr, def_ppr) ~ pct(4),
      vars(off_ppr, def_ppr) ~ pct(4),
      # vars(toss_efficiency, sinks) ~ pct(5),#px(60),
      vars(toss_efficiency) ~ pct(4),#px(60),
      ends_with("points_diff") ~ pct(5),#px(50),
      matches("(per_round|ppr|efficiency)_diff$") ~ pct(6)#px(55),
    ) %>% 
    tab_theme_snappa()
}


leaderboard_table = function(players, player_stats, game_stats){
  # Join players, player_stats, and game_stats
  tab_df = inner_join(players, player_stats, by = "player_id") %>%
    inner_join(game_stats, by = "game_id") %>% 
    # Identify which games were won
    mutate(winning = if_else(points_a > points_b, "A", "B"),
           won_game = (team == winning)) %>% 
    select(-player_id) %>% 
    # Calculate leaderboard
    group_by(player_name) %>% 
    summarise(
      games_played = n(),
      win_pct = sum(won_game, na.rm=T)/games_played,
      points_per_game = mean(total_points),
      total_points = sum(total_points),
      offensive_points = sum(off_ppr*shots),
      defensive_points = sum(def_ppr*shots),
      ones = sum(ones),
      twos = sum(twos),
      threes = sum(threes),
      paddle_points = sum(paddle_points),
      clink_points = sum(clink_points),
      total_shots = sum(shots),
      off_ppg = mean(off_ppr*shots),
      def_ppg = mean(def_ppr*shots),
      toss_efficiency = weighted.mean(toss_efficiency, w = shots)
    ) %>% 
    ungroup() %>% 
    # Remove any NAs
    filter_at(vars(-player_name), any_vars(!is.na(.))) %>% 
    mutate(rank = rank(-total_points)) %>% 
    arrange(rank) %>% 
    select(rank, player_name, games_played, win_pct, total_points, total_shots, points_per_game, toss_efficiency)
  
  dividing_line = tab_df %>% filter(games_played < 5) %>% pull(rank) %>% min()
  
  stats_eligible = tab_df %>% 
    filter(rank < dividing_line)
  
  tab_df %>% 
    gt(., id = "leaderboard") %>% 
    tab_header(title = "Snappaneers Leaderboard", 
               subtitle = "The Deadliest Die-throwers in all the land.") %>% 
    # Column names
    cols_label(
      rank = "", 
      player_name = "Player",
      games_played = "Games Played",
      win_pct = "Win %", 
      total_points = "Total Points",
      total_shots = "Total Shots",
      points_per_game = "Points per Game\n(PPG)",
      # off_ppg = "Offensive PPG",
      # def_ppg = "Defensive PPG",
      toss_efficiency = "Toss Efficiency"
    ) %>% 
    # Format integers
    fmt_number(
      columns = vars(rank, total_points, total_shots),
      decimals = 0
    ) %>% 
    # Format doubles
    fmt_number(
      columns = vars(points_per_game),
      decimals = 2
    ) %>% 
    # Format percentages
    fmt_percent(
      columns = vars(win_pct, toss_efficiency),
      decimals = 0
    ) %>% 
    # tab_footnote(
    #   footnote = "Defensive points are scored from paddles.",
    #   locations = cells_column_labels(columns = vars(def_ppg))
    # ) %>% 
    tab_footnote(
      footnote = "% of tosses which are successful.",
      locations = cells_column_labels(columns = vars(toss_efficiency))
    ) %>%
    opt_footnote_marks(marks = "letters") %>% 
    # Styling
    # Subtitle
    tab_style(style = cell_text(align = "left", v_align = "bottom", size = px(16)),
              locations = list(cells_title("title"), cells_title("subtitle"))) %>% 
    # Title
    tab_style(
      style = list(cell_text(weight = "bold", size = px(18))),
      locations = cells_title(groups = "title")
    ) %>%
    tab_style(
      style = cell_text(v_align = "bottom", weight = "normal", align = "left"),
      locations = cells_column_labels(everything())
    ) %>% 
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
      vars(rank) ~ pct(8),
      vars(player_name) ~ pct(24),
      vars(total_points, games_played, total_shots) ~ pct(19),
      vars(win_pct) ~ pct(14),
      vars(toss_efficiency, points_per_game) ~ pct(28)
    ) %>% 
    # Underline dope shit
    tab_style(
      style = list(cell_text(weight = "bold", color = snappa_pal[2])),
      locations = list(
        # Highest win %
        cells_body(
          columns = vars(win_pct),
          rows = rank == which.max(stats_eligible$win_pct)
        ),
        # Highest Toss efficiency
        cells_body(
          columns = vars(toss_efficiency),
          rows = rank == which.max(stats_eligible$toss_efficiency)
        ),
        # Most points
        cells_body(
          columns = vars(total_points),
          rows = rank == which.max(stats_eligible$total_points)
        ),
        # Highest ppg
        cells_body(
          columns = vars(points_per_game),
          rows = rank == which.max(stats_eligible$points_per_game)
        )
      )
    ) %>% 
    # Fade out the irrelevants
    tab_style(
      style = list(cell_borders(sides = "top", color = snappa_pal[2], weight = px(1), style = "dashed")),
      locations = cells_body(
        columns = everything(),
        rows = rank == dividing_line
      )
    ) %>% 
    tab_style(
      style = list(cell_fill(color = "#E3E3DE", alpha = .25), cell_text(weight = "lighter")),
      locations = cells_body(
        columns = everything(),
        rows = rank >= dividing_line
      )
    ) %>% 
    tab_source_note("Players need to play at least 5 games to be eligible for achievements.") %>% 
    tab_theme_snappa(table.font.size = px(12))
  
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
    scale_fill_gradient(name = "Frequency", low = "#ffeda0", high = "#f03b20", na.value = "grey",
                        guide = guide_colourbar(barheight = .5, barwidth = 15, direction = "horizontal",
                                                title.vjust = 1, title.hjust = 0.5, title.position = "top"))+
    labs(x = "Team B",
         y = "Team A",
         title = "Heatmap of scores in Snappa",
         subtitle = "How often does each score occur?")+
    coord_cartesian(xlim = c(1, max_score - 1),
                    ylim = c(1, max_score - 1)) +
    scale_x_continuous(breaks = scales::breaks_pretty(n = 10), sec.axis = dup_axis(name = NULL)) +
    scale_y_continuous(breaks = scales::breaks_pretty(n = 10), sec.axis = dup_axis(name = NULL)) +
    theme_snappa()+
    theme(panel.grid.major = element_blank(),
          legend.position = "bottom",
          axis.ticks.x.bottom = element_line())
  
  
}



# Summary Visualizations --------------------------------------------------




player_score_breakdown = function(ps_player_stats, ps_players, ps_game, ps_team){

  if(ps_team == "A"){
    team_margin = margin(0,-20,0,0)
  } else {
    team_margin = margin(0,0,0, -20)
  }
  reverse_legend = (!!ps_team == "A")

  df = ps_player_stats %>% 
    filter(game_id == !!ps_game) %>% 
    inner_join(ps_players) %>% 
    filter(team == !!ps_team) %>% 
    select(player_name, team, total_points:clink_points) %>% 
    arrange(-total_points) %>%
    # Calculate sink points and "normal" points
    # NOTE: this is not correct. it currently double counts any paddle clinks/clink sinks/paddle sinks
    mutate(sink = threes*3,
           normal_toss = total_points-(paddle_points+clink_points+sink),
           normal_toss = if_else(normal_toss < 0, 0, normal_toss)) %>% 
    select(player_name, team, `Normal toss` = normal_toss, Paddle = paddle_points, Clink = clink_points, Sink = sink) %>% 
    # Pivot to get point type
    pivot_longer(cols = `Normal toss`:Sink, names_to = "point_type", values_to = "points") %>% 
    # Convert point type to factor
    mutate(point_type = factor(point_type, levels = c("Sink", "Clink", "Paddle", "Normal toss"), ordered = T)) %>% 
    group_by(player_name) %>%
    mutate(point_pct = points/sum(points)) %>% 
    replace_na(list(point_pct = 0))
  
  
  
    df %>%
      ggplot(., aes(y = player_name, x = points, fill = point_type))+
      # Bars
      geom_col(position = "fill", colour = snappa_pal[1], size = 1.5)+
      # Labels
      geom_text(data = filter(df, point_pct > .15), 
                aes(label = scales::percent(point_pct, accuracy = 1)), 
                position = position_fill(vjust = .5), colour = "white", show.legend = F) +
      # Y Axis
      scale_y_discrete(name = NULL, position = if_else(reverse_legend, "left", "right"))+
      # X Axis
      scale_x_continuous(name = NULL, labels = scales::percent)+
      # Colour scale
      scale_fill_manual(name = NULL, drop=F,
                        values = c("Normal toss" = "#67A283", "Paddle" = "#793E8E", "Clink" = "#54B6F2", "Sink" = "#FFA630" ),
                        guide = guide_legend(reverse = T, label.hjust = 0.5, nrow = 2, byrow = T))+
      # Theme elements
      theme_snappa(md=T, plot_margin = team_margin, base_size = 11)+
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
    
    # TODO: Add troll image for the trolls
    # Potentially an if statement and detect if any player trolls
    # plot+
    #   geom_image(data = df %>% distinct(player_name, points, .keep_all = T) %>% mutate(image = 'www/troll_toll.jpg'),
    #              aes(y = player_name, x = points, image = image), size = .35)
}

game_flow = function(player_stats, players, scores, game){
  
  player_info = player_stats %>% 
    # Filter player stats
    filter(game_id == !!game) %>% 
    select(game_id, player_id, team) %>% 
    inner_join(players)
  
  # player_scores = vals$scores_db %>% 
  # inner_join(snappaneers(), by = c("player_id")) %>%
  player_scores = scores %>% 
    # Join scores to the player info table
    inner_join(player_info, by = c("game_id", "player_id")) %>% 
    # Convert round_num into the actual round id
    rowwise() %>% 
    mutate(round = which(rounds == round_num)) %>% 
    ungroup() %>% 
    # Order by game and score id
    arrange(game_id, score_id) %>% 
    group_by(player_id) %>%
    # Calculate players' cumulative score throughout the game
    mutate(cum_score = cumsum(points_scored))

  # Add zeroes
  player_score_base = player_scores %>% 
    # Filter player stats
    filter(game_id == !!game) %>% 
    group_by(game_id, player_id, player_name, team) %>% 
    summarise(round = min(round)-1, 
              points_scored = 0, 
              cum_score = 0)
  
  player_label_df = player_scores %>% 
    filter(game_id == !!game) %>% 
    filter(cum_score == max(cum_score)) %>% 
    select(player_name, player_id, team, round, cum_score)
  
  # Calculate max values for scales
  max_score = max(player_label_df$cum_score)
  max_round = max(player_label_df$round)

  player_scores %>% 
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

game_summary_plot = function(player_stats, players, scores, game){
  # Function which creates the game summary plots and puts them together
  #   - Create a player_score_breakdown for both teams
  #   - Create a game_flow plot

  # Player breakdowns
  a_breakdown = player_score_breakdown(ps_player_stats = player_stats, 
                                       ps_game = game, 
                                       ps_players = players,
                                       ps_team = "A")
  
  b_breakdown = player_score_breakdown(ps_player_stats = player_stats, 
                                       ps_game = game, 
                                       ps_players = players,
                                       ps_team = "B")
    
  game_flow_plot = game_flow(player_stats = player_stats,
                              players = players, 
                              scores = scores,
                              game = game)

  ## Combining plots
  # Team A's breakdown
  (wrap_elements(full = a_breakdown /
                   plot_spacer() / 
                   plot_spacer()*
                   theme(plot.background = element_rect(fill = snappa_pal[1], colour = snappa_pal[1]))*
                   plot_annotation(theme = theme(plot.background = element_rect(fill = snappa_pal[1], colour = snappa_pal[1]))))+
      # Game flow
      game_flow_plot+
      theme_snappa(plots_pane = T, md = T)+
      # Team B breakdown
      wrap_elements(full = b_breakdown /
                      plot_spacer() /
                      plot_spacer()*
                      theme(plot.background = element_rect(fill = snappa_pal[1], colour = snappa_pal[1]))*
                      plot_annotation(theme = theme(plot.background = element_rect(fill = snappa_pal[1], colour = snappa_pal[1])))))+
    plot_layout(widths = c(3,5,3))+
    plot_annotation(caption = str_c('<span style="color:', snappa_pal[2], ';">Snappa</span><span style="color:', snappa_pal[4], ';">DB</span>'), theme = theme_snappa(md=T, plot_margin = margin(5,5,15,5)))
}




  
  