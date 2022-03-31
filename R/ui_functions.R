library(reactable)
library(gt)
library(ggrepel)
library(ggtext)
library(patchwork)
library(ggwaffle)
library(plotly)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(shinycssloaders)
library(htmltools)


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
                 class = "score-header"),

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
                         h4("Anything cool happen?", style = "font-weight:700;"),
                         # Was it a paddle?
                         awesomeCheckbox(
                           inputId = "paddle", 
                           label = tags$div(HTML(str_c('<i id="paddle-icon" class="fas fa-hand-paper score-checkbox" style = "color:', team_colour, ';"></i>Paddle'))),#"Was it a paddle?",
                           status = "warning"
                         ),
                         # Was it a clink?
                         awesomeCheckbox(
                           inputId = "clink", 
                           label = tags$div(HTML(str_c('<i id="clink-icon" class="fas fa-assistive-listening-systems score-checkbox" style = "color:', team_colour, ';"></i>Clink'))),#"Was it a clink?",
                           status = "warning"
                         ),
                         # feet?
                         awesomeCheckbox(
                           inputId = "foot", 
                           label =tags$div(HTML(str_c('<i id="foot-icon" class="fas fa-shoe-prints score-checkbox" style = "color:', team_colour, ';"></i>Foot'))),#"Was it a clink?",
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

dropdownBlock2 = function (..., id, icon = NULL, title = NULL, badgeStatus = "danger") 
{
  if (!is.null(badgeStatus)) 
    validateStatus(badgeStatus)
  items <- c(list(...))
  dropdownClass <- paste0("dropdown")
  numItems <- length(items)
  if (is.null(badgeStatus)) {
    badge <- NULL
  }
  else {
    badge <- dashboardLabel(status = badgeStatus, numItems)
  }
  shiny::tags$li(shiny::singleton(shiny::tags$head(shiny::tags$script(shiny::HTML(paste0("$(document).ready(function(){\n                $('#", 
                                                                                         id, "').find('ul').click(function(e){\n                  e.stopPropagation();\n                });\n              });\n              "))))), 
                 class = dropdownClass, id = id, shiny::tags$a(href = "#", 
                                                               class = "dropdown-toggle", `data-toggle` = "dropdown", 
                                                               shiny::icon(icon), title, badge), 
                 shiny::tags$div(class = "dropdown-menu", 
                                style = "left: 0; right: auto;", 
                                items))
}



game_notification = function(rebuttal = F, round, current_scores){
  if(rebuttal){
    
    team_in_rebuttal = str_extract(round, "[AB]+")
    text_colour = if_else(team_in_rebuttal == "A", snappa_pal[2], snappa_pal[3])
    showNotification(HTML(str_c("Rebuttal: ", "<span style='color:", text_colour, "'>Team ", 
                                team_in_rebuttal, "</span>",
                                " needs ",
                                abs(current_scores$team_A - current_scores$team_B) - 1,
                                " points to bring it back")
    ), duration = 20, closeButton = F)
  } else {
    showNotification(HTML(str_c("<i class='fa fa-sign-language' style='padding: 1vh;'></i><span>That's some hot!</span>")),
                     duration = 10, closeButton = F)
  }
  
}

casualty_popup = function(session, score, rules, players = snappaneers()$player_name){
  # Create a pop up given a score and a set of rules
  if(vctrs::vec_in(score, haystack = rules[,1:2])) {
    current_rule = rules[vctrs::vec_match(score, haystack = rules[,1:2]), ]
    
    inputSweetAlert(session, 
                    inputId = "casualty",
                    title = current_rule$casualty_title,
                    text = current_rule$casualty_text,
                    # Want to add images, but these don't work yet
                    # url = 'https://cdn.britannica.com/96/192896-131-ECCA4FB7/Explosion-USS-Shaw-magazine-attack-Pearl-Harbor-Dec-7-1941.jpg',
                    # imageWidth = 400,
                    # imageHeight = 200,
                    type = "warning",
                    input = "radio",
                    inputOptions = players)
  } else {
    invisible()
  }
  
  
  
}

sink_casualty_popup = function(session, score_row, players = snappaneers()$player_name){
  
  sink_criteria = tribble(~points_scored, ~clink, 
                          3, F,
                          5, T,
                          7, T)
  
  if(vctrs::vec_in(score_row[, c("points_scored", "clink")], sink_criteria)){
    # Create a pop up given a score and a set of rules
    inputSweetAlert(session, 
                    inputId = "sink_casualty",
                    title = "Sploosh! Who got sunk?",
                    text = str_c("Remember: you have a ", scales::percent(1/6), " chance of rolling a one!"),
                    # Want to add images, but these don't work yet
                    # url = 'https://cdn.britannica.com/96/192896-131-ECCA4FB7/Explosion-USS-Shaw-magazine-attack-Pearl-Harbor-Dec-7-1941.jpg',
                    # imageWidth = 400,
                    # imageHeight = 200,
                    type = "warning",
                    input = "radio",
                    inputOptions = players)
  } else {
    invisible()
  }
  

  
  
}



tifu_casualty_popup <- function(players) {
  player_choices = deframe(players[, c("player_name", "player_id")])
  # Ask what happened and to whom
  modalDialog(align = "center", easyClose = T, size = "l", style = "margin:7vh 4vw 0;",
              
              
              fluidRow(style="display: flex;",
                column(8,
                       style="display: flex; flex-flow: column; align-self: center; align-items: flex-start;",
                       h2("Friendly Fire!", class = "h1"),
                       # Header
                       h2(HTML("You <del>love</del> hate to see that!"),
                          style = "margin-bottom: 2vh;")
                       ),
                column(4,
                       tags$img(src = str_c("gifs/", sample(list.files("www/gifs"), 1)), class = "casualty-gif")
                       )
              ),
              fluidRow(
              
                column(8, style = "text-align: left;",
                       # Casualty
                       radioGroupButtons(
                         inputId = "tifu_casualty",
                         label = "Who was the casualty?",
                         choices = player_choices,
                         size = "lg",
                         checkIcon = list(
                           yes = tags$i(class = "fa fa-beer"))
                       ),
                       # Shooter
                       radioGroupButtons(
                         inputId = "tifu_accused",
                         label = "Who was the shooter?",
                         choices = player_choices,
                         size = "lg",
                         checkIcon = list(
                           yes = tags$i(class = "fa fa-trash"))
                       )
                       # radioGroupButtons(
                       #   inputId = "tifu_accused",
                       #   label = "Who was the shooter?",
                       #   choices = players,
                       #   size = "lg",
                       #   checkIcon = list(
                       #     yes = tags$i(class = "fa fa-trash"))
                       # ),
                       # to whom?
                       
                ),
                
                column(4, style = "text-align:left;",
                       # What happened?
                       radioGroupButtons(
                         inputId = "casualty_type",
                         label = "How friendly we talking?",
                         choices = c("Self sink", "Team sink"),
                         direction = "vertical",
                         # individual = T,
                         justified = T,
                         size = "lg",
                         checkIcon = list(
                           yes = tags$i(class = "fa fa-bitbucket"))
                       )
                )
              ),
              
              footer = tagList(
                modalButton("Back"),
                uiOutput("casualty_validation", style = "margin:0px 15px;align-self:center;")
                
              )
  )
  
}




team_input_ui = function(team, player_choices){
  
  players = str_c("#name_", team, 1:4, "-selectized", collapse = ", ")
  player_inputs = str_c("#name_", team, 1:4, collapse = ", ")
  class_selector = paste0('input-well ', if_else(team == 'A', 'well-A', 'well-B'))
  
  column(4, align = "center",
         
         wellPanel(
           class = class_selector,
           # Header
           h1(paste("Team", toupper(team)), class = "team-header"),
           tags$div(class = 'player-input-forms',
                # Player 1
                selectizeInput(paste0('name_', team, '1'), 'Player 1', c(`Player Name`='', player_choices),  
                          options = list(create = TRUE, hideSelected=T), width = "125%"),
                 # Player 2
                selectizeInput(paste0('name_', team, '2'), 'Player 2', c(`Player Name`='', player_choices), 
                               options = list(create = TRUE, hideSelected=T), width = "125%"),
                # Add Player 3 button
                actionBttn(paste0("extra_player_", team, "3"), 
                           label = "+ Add Player", style = "unite", color = "danger", size = "sm")
           )
        )
  )
}


team_edit_column = function(team){
  team_colour = if_else(team == "A", "#e26a6a", "#2574a9")
  
  # Because the ui elements here need to be dynamically generated, I generate them
  # first and then pass it on to the ui nested in the column

  column(4, align = "center",
                wellPanel(
                  style = paste("opacity: 0.92; background:", team_colour),
                  # Header
                  h1(paste("Team", toupper(team)), style = "align: center; color: white; font-size: 7rem;"),
                  uiOutput(
                    outputId = paste0("edit_team_", team)
                  )
                  
                )
        )
}



# This function creates the panels that are used in the add/edit process (the function above).
# This function is called inside the renderUI for each team's add/edit page.

team_edit_ui = function(team, player_choices, active_players){
  # For styling
  team_colour = if_else(team == "A", "#e26a6a", "#2574a9")
  
  current_players = active_players[str_detect(names(active_players), team)]
  add_player_number = length(current_players) + 1
  
  team_list = imap(current_players, ~{
    tagList(
      tags$style(type = "text/css", str_c("#edit_name_", team, str_sub(.y, -1), "-selectized ", "{color: white; margin-top:30px;margin-bottom:30px;} ",
                                          "#edit_name_", team, str_sub(.y, -1), "{color: white; margin-top:30px;margin-bottom:30px;}")),
      selectizeInput(paste0('edit_name_', team, str_sub(.y, -1)), paste0('Player ', str_sub(.y, -1)), c(`Player Name`='', player_choices), 
                     selected = .x, options = list(create = TRUE)
      ),
      br()
      
    )
  })
  if (add_player_number < 5) {
    team_list[[add_player_number]] = tagList(
      actionBttn(paste0("edit_add_", team, add_player_number), label = "+ Add Player", style = "unite", color = "danger")
    )
  } else {
    invisible()
  }
  
  return(team_list)
}

scoreboard_well = function(team){
  wellPanel(
    class = paste0('scoreboard-well ', 'well-', team), 
    # Header
    h1(class = 'team-header',
       paste("Team", toupper(team))),
    # Score
    h2(class = 'team-score numbers', 
       textOutput(paste0("score_", team))),
    # Score button
    div(class = "score-and-undo",
        actionBttn(paste0(team, "_score_button"), 
                   label = "We scored!", color = "danger",
                   size = "lg"),
        actionBttn(
          inputId = paste0("undo_score_", team),
          label = "Undo", style = "unite", color = "danger", icon = icon("undo"), size = "md"
        )#,
    )
  )
}

team_scoreboard_ui = function(left_team = "A", right_team = "B"){
  

  div(id = "ScoreboardUI", class = "scoreboard",
           
             # Left Team
                     scoreboard_well(left_team),
                     
              # Round
              column(width = 4, align = "center", style = 'padding: 0;',
                     div(id = 'scoreboard_center_controls',
                         h1("Round", style = "font-size: 2.5rem;font-weight: 600;"),
                         uiOutput("round_num"),
                         uiOutput("round_control_buttons")
                     )

              ),
              # Team B

                    scoreboard_well(right_team)
  )
}

# Function for producing extra player UI inputs
extra_player_ui = function(current_tab, player, player_choices){
  
  # Get the player's team
  player_team = str_extract(player, "[A-z]")
  
  # Get the player number
  player_num = as.numeric(str_extract(player, "[0-9]"))
  
  # Name the upcoming div based on which screen you're on
  
  if (current_tab == "start") {
    div_id = paste0("add_remove_", player)
    input_type = paste0("name_", player)
    remove_type = paste0("remove_", player)
    extra_type = paste0("extra_player_", player_team, player_num + 1)
  } else {
    div_id = paste0("edit_new_", player)
    input_type = paste0("edit_name_", player)
    remove_type = paste0("edit_remove_", player)
    extra_type = paste0("edit_add_", player_team, player_num + 1)
  }
  # Create a div
  tags$div(id = div_id, 
           # Fluid row
            class = 'additional_players_form',
              div(class = 'input_close_and_name',
                actionBttn(inputId = remove_type,  label = "X", style = "jelly",
                               color = "danger", size = "sm"),
                h1(id = paste0("name_", player_team, player_num),
                   paste0('Player ', player_num)
                )
               ),
               # Add extra player text input 
               selectizeInput(inputId = input_type, 
                              label = NULL, c(`Player Name`='', player_choices), options = list(create = TRUE)),

             # Add remove player button outside fluid row
             
             # CSS
             tags$style(paste0(div_id, " {margin-left:auto; margin-right:auto; position: relative;}")),

           # If the extra player is not the fourth on a team yet, add another add player button
           if(player_num < 4){
             actionBttn(extra_type, 
                        label = "+ Add Player", style = "unite", color = "danger")
           } else{
             invisible()
           }
           
  )
}

add_player_input = function(current_tab, inputs, team, player, player_choices, session){
  
  
  # Insert extra player UI
  insertUI(
    selector = inputs,
    where = "afterEnd",
    ui = extra_player_ui(current_tab, paste0(team, player), player_choices)
  )
  
  # Remove add player button       
  removeUI(
    selector = inputs
  )
  
}



remove_p3_input = function(current_tab, team, session){
 
   if (current_tab == "start") {
     
    add_p3_button = paste0("#add_remove_", team, "3")
    ui_name = paste0("extra_player_", team,"3")
    selectize_name_3 =  paste0("name_", team, "3")
    selectize_name_4 = paste0("name_", team, "4")
    
  }  else if (current_tab == "edit"){
    
    add_p3_button = paste0("#edit_new_", team, "3")
    ui_name = paste0("edit_add_", team,"3")
    selectize_name_3 =  paste0("edit_name_", team, "3")
    selectize_name_4 = paste0("edit_name_", team, "4")
  }
    
  
    insertUI(selector = add_p3_button,
             where = "afterEnd",
             ui = actionBttn(ui_name, label = "+ Add Player", style = "unite", color = "danger")
    )
    
    
    removeUI(selector = add_p3_button)
    
    updateSelectizeInput(session, selectize_name_3, selected = character(0))
    updateSelectizeInput(session, selectize_name_4, selected = character(0))
  
}



remove_p4_input = function(current_tab, team, session){
  if (current_tab == "start") {
    add_p4_button = paste0("#add_remove_", team, "4")
    ui_name = paste0("extra_player_", team, "4")
    selectize_name = paste0("name_", team, "4")
  } else if (current_tab == "edit") {
    add_p4_button = paste0("edit_new_", team, "4")
    ui_name = paste0("edit_add_", team, "4")
    selectize_name = paste0("edit_name_", team, "4")
  }
      
  # Insert add player button
  insertUI(selector = add_p4_button,
           where = "afterEnd",
           ui = actionBttn(ui_name, label = "+ Add Player", style = "unite", color = "danger")
  )
  # Remove player text input
  removeUI(selector = add_p4_button)
  
  # Tells later checks to not worry about this
  # empty slot in active_player_names
  updateSelectizeInput(session, selectize_name, selected = character(0))
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
    tab_options(column_labels.hidden = T,
                heading.align = 'center')
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


make_single_summary_table = function(current_player_stats, player_stats, neers, team_name, current_round, past_scores){
  # Produce a team's performance summary and comparison to historical performance in equivalent games
  # 1. Get a list of games the player has played in
  # 2. Get a list of scores from historical games at the equivalent point in the game
  # 3. Calculate current game player stats
  # 4. Calculate historical player stats from 1 & 2

  # Store current game id and the given team's current player stats
  current_game = unique(current_player_stats$game_id)
  current_shots = unique(current_player_stats[current_player_stats$team == team_name, "shots", drop=T])
  
  # Store players, their current team and opponent sizes
  current_team_sizes = neers %>% 
    mutate(team_size = case_when(team == "A" ~ sum(team == "A"),
                                 team == "B" ~ sum(team == "B")),
           opp_size = case_when(team == "A" ~ sum(team == "B"),
                                team == "B" ~ sum(team == "A"))) %>% 
    distinct(player_id, team_size, opp_size)
  
  # Keep player stats observations for each player where the team and opponent sizes match the current sizes
  equivalent_player_stats = player_stats %>% 
    arrange(game_id) %>% 
    # Calculate the team size for each game
    group_by(game_id) %>% 
    mutate(team_size = case_when(team == "A" ~ sum(team == "A"),
                                 team == "B" ~ sum(team == "B")),
           opp_size = case_when(team == "A" ~ sum(team == "B"),
                                team == "B" ~ sum(team == "A"))) %>% 
    ungroup() %>% 
    # Keep the stats which match on player ID, team size and opponent size in this game
    inner_join(current_team_sizes, by = c("player_id", "team_size", "opp_size"))
  
  
  
  
  # Make a historical stats table that is only comparing games which are similar
  # to the current one.
  # First, obtain a list of games in which the players on this team were on
  # an equally sized team. This is player specific, so map() is used
  
  
  
  # A not particularly elegant but probably working solution to making sure that games
  # are really, truly, apples-to-apples. Create a table of game_id, ally_team_size,
  # and opponent_team_size and merge it to player_stats to be used as a 
  
  
  
  
  
  
  # make a unique subsection of the scores table which only considers the given player in the
  # given games. round_comparison should only be applied when a game is in progress. 
  # While we're here, also tell the display not to care about winners maybe? I could also set
  # a value here so that I don't have to execute a query later, but the issue becomes 
  
  ##
  ## Scenario 1: Game is in progress
  ##
  in_progress = isFALSE(pull(dbGetQuery(con, sql(str_c("SELECT game_complete FROM game_stats WHERE game_id =", current_game, ";"))), 
                             game_complete))
  if (in_progress){
    
    # Filter to scores which are:
    #   - only scored by players on this team
    #   - less than or equal to the current round
    scores_comparison = past_scores %>% 
      filter(player_id %in% neers$player_id, # Only include players on this team
             parse_round_num(round_num) <= parse_round_num(current_round))
    
    ##
    ## Scenario 2: Game is complete
    ##
  } else {
    
    scores_comparison = past_scores
  }
  # List scores which occurred at or before the current game's round
  historical_scores = left_join(equivalent_player_stats, 
                                                    scores_comparison, 
                                                    by = c("game_id", "player_id")) %>% 
    filter(!(game_id %in% 38:48))
  
  # When in progress, keep the shot counter generated from current_shots
  if(in_progress){
    historical_scores = mutate(historical_scores, shots = current_shots)
  }
  
  # Now, this table is going to be plugged in to the pipeline that currently exists in the team summary tab function.
  # That means I have to recreate player_stats using this table 
  
  # Calculate game performance in equivalent games
  comparison_player_stats = historical_scores %>% 
    replace_na(list(points_scored = 0, paddle = F, clink = F, foot = F)) %>% 
    # Group by game and player, (team and shots are held consistent)
    group_by(game_id, player_id, shots) %>% 
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
              toss_efficiency = sum((points_scored>0)*!(paddle | foot ))/last(shots),
              .groups = "drop")
  
  player_info = player_stats %>% 
    # Filter player stats
    select(game_id, player_id, team) %>% 
    inner_join(neers, by = c("player_id", "team")) 
  
  # Calculate current game performance
  #   - Team score
  #   - Which team is winning
  # Then join on player info
  player_summary = current_player_stats %>% 
    group_by(team) %>% 
    mutate(team_score = sum(total_points)) %>% 
    ungroup() %>% 
    mutate(winning = (team_score == max(team_score))) %>% 
    select(team, winning, player_id,  
           total_points, paddle_points, clink_points, threes, 
           points_per_round:toss_efficiency)
  
  historical_stats = comparison_player_stats %>% 
    select(game_id, player_id, 
           shots, total_points, paddle_points, clink_points, sinks = threes, 
           points_per_round:toss_efficiency) %>%
    arrange(player_id, game_id) %>% 
    group_by(player_id) %>% 
    summarise(
      across(.cols = c(total_points, paddle_points, clink_points), .fns = mean, .names = "{col}_avg"),
      across(.cols = c(sinks), .fns = sum, .names = "{col}_total"),
      across(.cols = c(points_per_round, off_ppr, def_ppr, toss_efficiency),
             .fns = ~weighted.mean(., w = shots), 
             .names = "{col}_wavg"),
      .groups = "drop"
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
           across(matches("points_diff"), ~str_c(if_else(.x >= 0, "+", ""), round(.x, 1))),
           across(matches("(per_round|ppr)_diff$"), ~str_c(if_else(.x >= 0, "+", ""), round(.x, 2))),
           toss_efficiency_diff = map_chr(toss_efficiency_diff, 
                                          ~case_when(. >= 0 ~ toss_percent_plus(.), 
                                                     . < 0 ~ toss_percent_minus(.)))) %>% 
    # Remove historical stats
    select(-ends_with("_avg"), -ends_with("wavg")) %>% 
    # Order columns
    select(starts_with("player"), team, winning, 
           contains("total_points"), contains("paddle"), contains("clink"), sinks = threes, 
           contains("per_round"), contains("off_"), contains("def_"), contains("toss"))
  
  inner_join(select(neers, player_id, player_name),
             select(player_summary_historical,
                    -contains("points_per")), by = "player_id")
  
}

make_summary_table = function(current_player_stats, player_stats, neers, team_name, current_round, past_scores){
  # Produce a team's performance summary and comparison to historical performance in equivalent games
  # 1. Get a list of games the player has played in
  # 2. Get a list of scores from historical games at the equivalent point in the game
  # 3. Calculate current game player stats
  # 4. Calculate historical player stats from 1 & 2
  # browser()
  # List players on the given team
  team_players = neers[neers$team == team_name, ]
  # Store current team and opponent sizes
  team_size = nrow(team_players) 
  opponent_size = nrow(neers[neers$team != team_name, ])
  
  # Store current game id and the given team's current player stats
  current_game = unique(current_player_stats$game_id)
  team_player_stats = current_player_stats[current_player_stats$team == team_name, ]
  current_shots = unique(current_player_stats[current_player_stats$team == team_name, "shots", drop=T])
  
  
  
  # Make a historical stats table that is only comparing games which are similar
  # to the current one.
  # First, obtain a list of games in which the players on this team were on
  # an equally sized team. This is player specific, so map() is used

  
  
  # A not particularly elegant but probably working solution to making sure that games
  # are really, truly, apples-to-apples. Create a table of game_id, ally_team_size,
  # and opponent_team_size and merge it to player_stats to be used as a 
  

  # Make a dataframe of team sizes in past games
  team_sizes = player_stats %>% 
    # Count team size for each game
    count(game_id, team, name = "team_size") %>% 
    # Pivot separate columns for team 
    pivot_wider(names_from = team, 
                values_from = team_size, 
                names_glue = "size_{team}")

  equivalent_games_player_stats = team_players$player_id %>%
    map(function(player){
      # Subset each player's stats  to each player's stats and identify equivalent games
      player_stats[player_stats$player_id == player & player_stats$game_id != current_game, ] %>% 
        # Join team sizes to player stats
        inner_join(team_sizes, by = "game_id") %>%
        # Keep cases where the team sizes are equivalent
        filter(if_else(team == "A", size_A, size_B) == team_size,
               if_else(team == "B", size_A, size_B) == opponent_size)

    })
  
  
  # make a unique subsection of the scores table which only considers the given player in the
  # given games. round_comparison should only be applied when a game is in progress. 
  # While we're here, also tell the display not to care about winners maybe? I could also set
  # a value here so that I don't have to execute a query later, but the issue becomes 
  
  ##
  ## Scenario 1: Game is in progress
  ##
  in_progress = isFALSE(pull(dbGetQuery(con, sql(str_c("SELECT game_complete FROM game_stats WHERE game_id =", current_game, ";"))), 
                             game_complete))
  if (in_progress){
    
    # Filter to scores which are:
    #   - only scored by players on this team
    #   - less than or equal to the current round
    scores_comparison = past_scores %>% 
      filter(player_id %in% team_players$player_id, # Only include players on this team
             parse_round_num(round_num) <= parse_round_num(current_round))

    ##
    ## Scenario 2: Game is complete
    ##
  } else {

    scores_comparison = past_scores
  }

  # List scores which occurred at or before the current game's round
  historical_scores = team_players$player_id %>%
    imap_dfr(function(player, index){
      # Join each player's equivalent games to their scores from those games
      left_join(equivalent_games_player_stats[[index]], 
                scores_comparison, 
                by = c("game_id", "player_id")) %>% 
        filter(!(game_id %in% 38:48))
    })
  
  # When in progress, keep the shot counter generated from current_shots
  if(in_progress){
    historical_scores = mutate(historical_scores, shots = current_shots)
  }
  
  # Now, this table is going to be plugged in to the pipeline that currently exists in the team summary tab function.
  # That means I have to recreate player_stats using this table 
  
  # Calculate game performance in equivalent games
  comparison_player_stats = historical_scores %>% 
    replace_na(list(points_scored = 0, paddle = F, clink = F, foot = F)) %>% 
    # Group by game and player, (team and shots are held consistent)
    group_by(game_id, player_id, shots) %>% 
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
              toss_efficiency = sum((points_scored>0)*!(paddle | foot ))/last(shots),
              .groups = "drop")
  
  player_info = team_player_stats %>% 
    # Filter player stats
    select(game_id, player_id, team) %>% 
    inner_join(neers, by = c("player_id", "team")) 
  
  # Calculate current game performance
  #   - Team score
  #   - Which team is winning
  # Then join on player info
  player_summary = current_player_stats %>% 
    group_by(team) %>% 
    mutate(team_score = sum(total_points)) %>% 
    ungroup() %>% 
    mutate(winning = (team_score == max(team_score))) %>% 
    filter(team == team_name) %>% 
    select(team, winning, player_id,  
           total_points, paddle_points, clink_points, threes, 
           points_per_round:toss_efficiency)
  
  historical_stats = comparison_player_stats %>% 
    select(game_id, player_id, 
           shots, total_points, paddle_points, clink_points, sinks = threes, 
           points_per_round:toss_efficiency) %>%
    arrange(player_id, game_id) %>% 
    group_by(player_id) %>% 
    summarise(
      across(.cols = c(total_points, paddle_points, clink_points), .fns = mean, .names = "{col}_avg"),
      across(.cols = c(sinks), .fns = sum, .names = "{col}_total"),
      across(.cols = c(points_per_round, off_ppr, def_ppr, toss_efficiency),
             .fns = ~weighted.mean(., w = shots), 
             .names = "{col}_wavg"),
      .groups = "drop"
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
           across(matches("points_diff"), ~str_c(if_else(.x >= 0, "+", ""), round(.x, 1))),
           across(matches("(per_round|ppr)_diff$"), ~str_c(if_else(.x >= 0, "+", ""), round(.x, 2))),
           toss_efficiency_diff = map_chr(toss_efficiency_diff, 
                                          ~case_when(. >= 0 ~ toss_percent_plus(.), 
                                                     . < 0 ~ toss_percent_minus(.)))) %>% 
    # Remove historical stats
    select(-ends_with("_avg"), -ends_with("wavg")) %>% 
    # Order columns
    select(starts_with("player"), team, winning, 
           contains("total_points"), contains("paddle"), contains("clink"), sinks = threes, 
           contains("per_round"), contains("off_"), contains("def_"), contains("toss"))
  
  inner_join(select(team_players, player_id, player_name),
             select(player_summary_historical,
                    -contains("clink"), -contains("sink"), -contains("points_per")), by = "player_id")
  
}




team_summary_tab = function(df, game_over, score_difference, team){
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
    fmt_missing(columns = contains("diff")) %>% 
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
    when(game_over ~ (.) %>% tab_source_note("Comparison of current game to career average"),
         ~ (.) %>% tab_source_note("Player performance compared to past games at or before the current round")) %>% 
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

game_summary_tab_rt = function(df){
  browser()
  df %>% 
    reactable(defaultSorted = c("winning", "total_points"),
              sortable = F,
              resizable = T,
              style = list(
                fontSize = "12px"
              ),
              defaultColDef = colDef(format = colFormat(digits = 0), 
                                     align = "right", headerStyle = list(textAlign = "right"),
                                     style = JS("function(rowInfo, cellInfo) {
                                            if (rowInfo.row.team == 'A' & rowInfo.level < 1) {
                                                var color =  '#e26a6a80'
                                            } else if (rowInfo.row.team == 'B'& rowInfo.level < 1) {
                                                var color = '#2574a980'
                                            } else {
                                                var color = '#fafaf9'
                                            }
                                            return { backgroundColor: color, padding: '4px 2px'}
                                        }"
                                     )), 
              highlight = F,
              compact = T,
              groupBy = "team", 
              width = "80%", 
              class = "snappaneers-tbl",
              # Column Groups
              columnGroups = list(
                colGroup(name = "Total Points", columns = c("total_points", "total_points_diff")),
                colGroup(name = "Clink Points", columns = c("clink_points", "clink_points_diff")),
                colGroup(name = "Paddle Points", columns = c("paddle_points", "paddle_points_diff")),
                colGroup(name = "Sinks", columns = c("sinks")),
                colGroup(name = "Off. PPR", columns = c("off_ppr", "off_ppr_diff"), headerStyle = list(alignSelf = "flex-end")),
                colGroup(name = "Def. PPR", columns = c("def_ppr", "def_ppr_diff"), headerStyle = list(alignSelf = "flex-end")),
                colGroup(name = "Toss Efficiency", columns = c("toss_efficiency", "toss_efficiency_diff"))
              ),
              # Columns
              columns = list(
                team = colDef(grouped = JS("function(cellInfo) {
                              // Display just the cell value
                              return 'Team '+ cellInfo.value
                            }"), name = "", maxWidth = 80,
                              headerStyle = list(border = "none"),
                              style = rt_team_highlight),
                player_id = colDef(show = F),
                player_name = colDef(name = "Player", 
                                     minWidth = 65, align = "left", 
                                     headerStyle = list(textAlign = "left"),
                                     aggregate = JS("function(values, rows) {
                                                      return [...new Set(rows.team)].join(', ')
                                     }"),
                                     style = rt_team_highlight),
                winning = colDef(show = F),
                total_points = colDef(name = "", 
                                      minWidth = 25, 
                                      defaultSortOrder = "desc"),
                total_points_diff = colDef(name = "Diff", 
                                           minWidth = 30, 
                                           style = JS(game_summary_diff_style("total_points_diff"))),
                paddle_points = colDef(name = "", 
                                       minWidth = 25),
                paddle_points_diff = colDef(name = "Diff", 
                                            minWidth = 30, 
                                            style = JS(game_summary_diff_style("paddle_points_diff"))),
                clink_points = colDef(name = "", 
                                       minWidth = 19),
                clink_points_diff = colDef(name = "Diff", 
                                            minWidth = 30, 
                                            style = JS(game_summary_diff_style("clink_points_diff"))),
                sinks = colDef(name = "", 
                                      minWidth = 30),
                off_ppr = colDef(name = "", 
                                 minWidth = 30, 
                                 format = colFormat(digits = 2)),
                off_ppr_diff = colDef(name = "Diff", 
                                      minWidth = 36, 
                                      style = JS(game_summary_diff_style("off_ppr_diff"))),
                def_ppr = colDef(name = "", 
                                 minWidth = 30, format = colFormat(digits = 2)),
                def_ppr_diff = colDef(name = "Diff", 
                                      minWidth = 36, 
                                      style = JS(game_summary_diff_style("def_ppr_diff"))),
                toss_efficiency = colDef(name = "", 
                                         minWidth = 29, format = colFormat(percent = T, digits = 0)),
                toss_efficiency_diff = colDef(name = "Diff", 
                                              minWidth = 32, 
                                              style = JS(game_summary_diff_style("toss_efficiency_diff")))
              ),
              defaultExpanded = TRUE
    )
}

team_summary_tab_rt = function(df){

  df %>% 
    reactable(defaultSorted = "total_points",
              sortable = F,
              resizable = F,
              style = list(
                fontSize = "12px"
              ),
              defaultColDef = colDef(format = colFormat(digits = 0), 
                                     align = "right", defaultSortOrder = "desc",
                                     style = list(padding = "4px 2px"), headerStyle = list(textAlign = "right")), 
              highlight = F,
              compact = T,
              # width = "400px",
              class = "snappaneers-tbl",
              # Column Groups
              columnGroups = list(
                colGroup(name = "Points", columns = c("total_points", "total_points_diff")),
                # colGroup(name = "Paddle Points", columns = c("paddle_points", "paddle_points_diff")),
                colGroup(name = HTML(str_c('<i id="paddle-icon" class="fas fa-hand-paper"></i>')), html = T, columns = c("paddle_points", "paddle_points_diff")),
                # colGroup(name = "Off. PPR", columns = c("off_ppr", "off_ppr_diff"), headerStyle = list(alignSelf = "flex-end")),
                # colGroup(name = "Def. PPR", columns = c("def_ppr", "def_ppr_diff"), headerStyle = list(alignSelf = "flex-end")),
                colGroup(name = "Toss Efficiency", columns = c("toss_efficiency", "toss_efficiency_diff"))
              ),
              # Columns
              columns = list(
                player_id = colDef(show = F),
                player_name = colDef(name = "Player", maxWidth = 60, align = "left", headerStyle = list(textAlign = "left")),
                team = colDef(show = F),
                winning = colDef(show = F),
                total_points = colDef(name = "", minWidth = 19, maxWidth = 30),
                total_points_diff = colDef(name = "Diff", minWidth = 30, maxWidth = 40, 
                                           style = JS(game_summary_diff_style("total_points_diff"))),
                paddle_points = colDef(name = "", minWidth = 19, maxWidth = 40),
                paddle_points_diff = colDef(name = "Diff", minWidth = 30, maxWidth = 40, 
                                            style = JS(game_summary_diff_style("paddle_points_diff"))),
                off_ppr = colDef(show=F, name = "", minWidth = 30, maxWidth = 50, format = colFormat(digits = 2)),
                off_ppr_diff = colDef(show=F, name = "Diff", minWidth = 36, maxWidth = 60, 
                                      style = JS(game_summary_diff_style("off_ppr_diff"))),
                def_ppr = colDef(show=F, name = "", minWidth = 30, format = colFormat(digits = 2)),
                def_ppr_diff = colDef(show=F, name = "Diff", minWidth = 36, maxWidth = 60, 
                                      style = JS(game_summary_diff_style("def_ppr_diff"))),
                toss_efficiency = colDef(name = "", minWidth = 29, format = colFormat(percent = T, digits = 0)),
                toss_efficiency_diff = colDef(name = "Diff", minWidth = 32, maxWidth = 40, 
                                              style = JS(game_summary_diff_style("toss_efficiency_diff")))
              )
    )
}


leaderboard_table_rt = function(career_stats_data, dividing_line, highlight_colour = snappa_pal[5]){
  stats_eligible = career_stats_data %>% 
    filter(rank < dividing_line)
  
  career_stats_data %>% 
    reactable(
      defaultPageSize = 10, pagination = T, 
      defaultSorted = "rank",
      showSortable = T,
      defaultColDef = colDef(headerStyle = list(minHeight = 51), format = colFormat(digits = 0, separators = T), 
                             align = "left", defaultSortOrder = "desc"),
      highlight = T, 
      # compact = T, 
      width = "100%",
      rowStyle = function(index) {
        if (career_stats_data[index, "games_played"] < 5) {
          list(background = "#E3E3DE",
               fontWeight = 200)
        }
      },
      rowClass = function(index) {
        if (career_stats_data[index, "rank"] == max(stats_eligible$rank)) {
          "dividing-line"
        } else if (career_stats_data[index, "rank"] > max(stats_eligible$rank)) {
          "unranked"
        } else if (career_stats_data[index, "rank"] <= max(stats_eligible$rank)) {
          "ranked"
        }
      },
      columns = list(
        rank = colDef("",
                      align = "left", 
                      minWidth = 35,
                      maxWidth = 45,
                      defaultSortOrder = "asc",
                      headerStyle = list(minHeight = 51, background = snappa_pal[1], position = "sticky", left = 0, zIndex = 1),
                      style = function(value){
                        if(value > max(stats_eligible$rank)){
                          list(background = "#E3E3DE",
                               fontWeight = 200, position = "sticky", left = 0, zIndex = 1)
                        } else {
                          list(background = snappa_pal[1], fontWeight = 700, position = "sticky", left = 0, zIndex = 1)
                        }
                      },
                      class = function(value){
                          if(value > max(stats_eligible$rank)){
                            "unranked"
                          } else {
                          "ranked"
                        }
                        }), 
        player_name = colDef("Player", 
                             sortable = F,
                             minWidth = 100,
                             maxWidth = 200,
                             headerStyle = list(minHeight = 51, background = snappa_pal[1], position = "sticky", left = 35,zIndex = 1),
                             style = function(value){
                               if(value %in% unique(stats_eligible$player_name)){
                                 list(background = snappa_pal[1], position = "sticky", left = 35,zIndex = 1)
                               } else {
                                 list(background = "#E3E3DE", position = "sticky", left = 35,zIndex = 1)
                               }
                             },
                             class = function(value){
                               if(!(value %in% unique(stats_eligible$player_name))){
                                 "unranked"
                               } else {
                               "ranked"
                               }
                             }),
        games_played = colDef("Games Played", 
                              minWidth = 82,
                              maxWidth = 150),
        win_pct = colDef("Win %", 
                         format = colFormat(percent = T, digits = 1), 
                         minWidth = 80,
                         maxWidth = 150,
                         style = function(value) {
                           if(max(stats_eligible[, "win_pct"], na.rm=T) == value){
                             list(color = snappa_pal[5],
                                  fontWeight = 600)
                           }
                         }), 
        total_points = colDef("Total Points", 
                              minWidth = 82,
                              maxWidth = 150),
        offensive_points = colDef("Off. Points", 
                              minWidth = 82,
                              maxWidth = 150),
        defensive_points = colDef("Def. Points", 
                              minWidth = 82,
                              maxWidth = 150),
        paddle_points = colDef("Paddle Points", 
                              minWidth = 82,
                              maxWidth = 150,
                              style = function(value) {
                                if(max(stats_eligible[, "paddle_points"], na.rm=T) == value){
                                  list(color = snappa_pal[5],
                                       fontWeight = 600)
                                }
                              }),
        foot_paddle_points = colDef("Foot Paddle Points", 
                               minWidth = 115,
                               maxWidth = 150,
                               style = function(value) {
                                 if(max(stats_eligible[, "foot_paddle_points"], na.rm=T) == value){
                                   list(color = snappa_pal[5],
                                        fontWeight = 600)
                                 }
                               }),
        clink_points = colDef("Clink Points", 
                               minWidth = 82,
                               maxWidth = 150,
                              style = function(value) {
                                if(max(stats_eligible[, "clink_points"], na.rm=T) == value){
                                  list(color = snappa_pal[5],
                                       fontWeight = 600)
                                }
                              }),
        sinks = colDef("Sinks", 
                               minWidth = 82,
                               maxWidth = 150,
                       style = function(value) {
                         if(max(stats_eligible[, "sinks"], na.rm=T) == value){
                           list(color = snappa_pal[5],
                                fontWeight = 600)
                         }
                       }),
        paddle_sinks = colDef("Paddle Sinks", 
                               minWidth = 82,
                               maxWidth = 150,
                              style = function(value) {
                                if(max(stats_eligible[, "paddle_sinks"], na.rm=T) == value){
                                  list(color = snappa_pal[5],
                                       fontWeight = 600)
                                }
                              }),
        points_per_game = colDef("Points per Game\n(PPG)", 
                                 format = colFormat(digits = 2), 
                                 minWidth = 117,
                                 maxWidth = 200,
                                 style = function(value) {
                                   if(max(stats_eligible[, "points_per_game"], na.rm=T) == value){
                                     list(color = snappa_pal[5],
                                          fontWeight = 600)
                                   }
                                 }),
        off_ppg = colDef("Offensive PPG", 
                         format = colFormat(digits = 2),
                         minWidth = 100,
                         maxWidth = 200,
                         style = function(value) {
                           if(max(stats_eligible[, "off_ppg"], na.rm=T) == value){
                             list(color = snappa_pal[5],
                                  fontWeight = 600)
                           }
                         }),
        def_ppg = colDef("Defensive PPG", 
                         format = colFormat(digits = 2), 
                         minWidth = 105,
                         maxWidth = 200,
                         style = function(value) {
                           if(max(stats_eligible[, "def_ppg"], na.rm=T) == value){
                             list(color = snappa_pal[5],
                                  fontWeight = 600)
                           }
                         }),
        toss_efficiency = colDef("Toss Efficiency", 
                                 format = colFormat(digits = 1, percent = T), 
                                 minWidth = 105,
                                 maxWidth = 200,
                                 style = function(value) {
                                   if(max(stats_eligible[, "toss_efficiency"], na.rm=T) == value){
                                     list(color = snappa_pal[5],
                                          fontWeight = 600)
                                   }
                                 })
      ),
      class = "snappaneers-tbl"
    )
  
}
leaderboard_table = function(career_stats_data, dividing_line, highlight_colour = snappa_pal[5]){

  stats_eligible = career_stats_data %>% 
    filter(rank < dividing_line)
  
  career_stats_data %>% 
    gt(., id = "leaderboard") %>% 
    tab_header(title = "Snappaneers Leaderboard", 
               subtitle = "The Deadliest Die-throwers in all the land.") %>% 
    # Column names
    cols_label(
      rank = "", 
      player_name = "Player",
      games_played = "Games Played",
      win_pct = "Win %", 
      points_per_game = "Points per Game\n(PPG)",
      off_ppg = "Offensive PPG",
      def_ppg = "Defensive PPG",
      toss_efficiency = "Toss Efficiency"
    ) %>% 
    # Format integers
    fmt_number(
      columns = vars(rank),
      decimals = 0
    ) %>% 
    # Format doubles
    fmt_number(
      columns = vars(points_per_game, off_ppg, def_ppg),
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
    cols_align(align = "center") %>% 
    cols_align(align = "left", columns = c("player_name", "rank")) %>% 
    # Left Align Player and Rank
    # Column widths
    cols_width(
      vars(rank) ~ pct(8),
      vars(player_name) ~ pct(24),
      vars(games_played) ~ pct(19),
      vars(win_pct) ~ pct(14),
      vars(toss_efficiency, points_per_game, def_ppg, off_ppg) ~ pct(28)
    ) %>%
    # Highlight dope shit
    tab_style(
      style = list(cell_text(weight = "bold", color = highlight_colour)),
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
          columns = vars(def_ppg),
          rows = rank == which.max(stats_eligible$def_ppg)
        ),
        cells_body(
          columns = vars(off_ppg),
          rows = rank == which.max(stats_eligible$off_ppg)
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

game_summary_dialog = function(df, current_round, a_sub, b_sub){
  showModal(
    modalDialog(
      title = HTML(str_c(if_else(df$game_complete, "Last ", "Current "), "game: <strong>", df$points_a, " - ", df$points_b, "</strong> at ", 
                         coalesce(rounds[df$rounds], current_round))),  
      style = str_c("background-color: ", snappa_pal[1], ";"),
      
      
      
      
      # Tables
      fluidRow(align = "center",
               # reactableOutput("team_a_summary")
               column(6, align = "center",
                      h4("Team A", align = "left", style = str_c("color:", snappa_pal[2])),
                      # h4(a_sub, align = "left"),
                      withSpinner(reactableOutput("team_a_summary"), color = snappa_pal[2], 
                                  proxy.height = "145px", color.background = snappa_pal[1])
               ),
               column(6,align = "center",# offset = 2,
                      h4("Team B", align = "left", style = str_c("color:", snappa_pal[3])),
                      # h4(b_sub, align = "left"),
                      withSpinner(reactableOutput("team_b_summary"), color = snappa_pal[3], 
                                  proxy.height = "145px", color.background = snappa_pal[1])
               )
      ),
      # Summary plot
      fluidRow(align = "center", 
               column(3, style = "padding-right:0;",
                      withSpinner(plotOutput("a_breakdown", width = "100%", height = "44vh"), color = snappa_pal[2], 
                                  proxy.height = "200px", color.background = snappa_pal[1])
               ),
               column(6,style = "padding:0",
                          h4("How the die flies", align = "left", style = "margin:0px 4px; padding:0px 4px;")
                          # h5("Point progression throughout the game", align = "left")
                      ,
                      withSpinner(plotOutput("game_flow", height = "44vh"), color.background = snappa_pal[1],
                                  color = snappa_pal[4])
               ),
               column(3, style = "padding-left:0;",
                      withSpinner(plotOutput("b_breakdown", width = "100%", height = "44vh"), color = snappa_pal[3], 
                                  proxy.height = "200px", color.background = snappa_pal[1])
               )
      ),
      # plotOutput("summary_plot", height = "50vh"),
      # reactableOutput("scores_tbl"),
      
      footer = NULL, 
      easyClose = TRUE,
      size = "l"
    )
  )
}



markov_summary_data = function(simulations){
  # Answer the basic questions
  # Who won more? What's their win rate?
  wins = simulations %>% map_chr( function(element){
    element$won
  })
  A_winrate = length(wins[wins == "A"])/length(wins)
  B_winrate = 1 - A_winrate

  # In the games where the winningest team won, what was the modal score?
  winners = if_else(which(c(A_winrate, B_winrate) == max(c(A_winrate, B_winrate))) == 1,
                    "A",
                    "B")
  if (length(winners) == 2){
    tie = TRUE
    per_game_data = seq(1, length(simulations)) %>% map(function(game_number){
        team_A_score = simulations[[game_number]]$team_A %>% last()
        team_B_score = simulations[[game_number]]$team_B %>% last()
        final_scores = tibble("A" = team_A_score, "B" = team_B_score)
        matrix = matrix(0, nrow = 51, ncol = 51)
        matrix[team_A_score + 1, team_B_score + 1] = 1
        
        return(list("scores" = matrix,
                    "final" = final_scores))
      })
    winrate = 0.50
  } else {
  # Using a matrix here so that I can treat each pairing of points as a unique
  # value, rather than each team's individual total (meaning that I am truly
  # looking for the pair of scores which are modal in the set of games that
  # the winning team won)
    tie = F
    per_game_data = seq(1, length(simulations)) %>% map(function(game_number){
      if (simulations[[game_number]]$won != winners){
        team_A_score = simulations[[game_number]]$team_A %>% last()
        team_B_score = simulations[[game_number]]$team_B %>% last()
        final_scores = tibble("A" = team_A_score, "B" = team_B_score)
        matrix = matrix(0, nrow = 51, ncol = 51)
      } else {
        team_A_score = simulations[[game_number]]$team_A %>% last()
        team_B_score = simulations[[game_number]]$team_B %>% last()
        final_scores = tibble("A" = team_A_score, "B" = team_B_score)
        matrix = matrix(0, nrow = 51, ncol = 51)
        matrix[team_A_score + 1, team_B_score + 1] = 1
      }
      
  
      return(list("scores" = matrix,
                  "final" = final_scores))
    })
    winrate = c("A" = A_winrate, "B" = B_winrate)[winners]
  }
  
  # Initialize an empty matrix
  scores_matrix = matrix(0, nrow = 51, ncol = 51)                     
  for (i in 1:length(simulations)){
    scores_matrix = scores_matrix + per_game_data[[i]]$scores 
  }
        
  modal_score_position = which(scores_matrix == max(scores_matrix), arr.ind = T)
  
  # As one may expect, this sometimes comes up with more than one 
  # modal score! In that case, we have to tread a little more carefully.
  # What I'll do is take advantage of the structure of the matrix to figure
  # out which games look the best for everyone. What I'll do is look at the 
  # score total that's the closest
  if (length(modal_score_position) < 3){
  modal_A_score = modal_score_position[1] - 1
  modal_B_score = modal_score_position[2] - 1
  } else{
    modal_score_position = cbind(modal_score_position, 
                                 abs(modal_score_position[, 1] - modal_score_position[, 2])) 
    modal_row = which(modal_score_position[, 3] == min(modal_score_position[, 3]))
    # If this is more than one, take the one with the highest A value. That will
    # automatically break any ties. There cannot be duplicate A values in this 
    # search already, because only one such A and B score pairing will have
    # the smallest distance between them
    if (length(modal_row) > 1){
      if (all(winners == "A")){
        modal_row = which(
          modal_score_position[modal_row, 1] == max(modal_score_position[modal_row, 1])
        )
      } else {
        #Technically this settles tie games with multiple modals in B's favor,
        # but this is such a ridiculous outcome that I'm not going to worry about
        # doing any more for now
        modal_row = which(
          modal_score_position[modal_row, 2] == max(modal_score_position[modal_row, 2])
        )
      } 
    }
    modal_A_score = modal_score_position[modal_row, 1] - 1
    modal_B_score = modal_score_position[modal_row, 2] - 1
  }
  
  final_scores_table = seq(1, length(per_game_data)) %>% 
    map_df(function(number){
      per_game_data[[number]]$final
    })
  
  modal_frequency = final_scores_table %>%
    filter(A == modal_A_score, B == modal_B_score) %>%
    nrow()
   
  return(list("final_scores" = final_scores_table,
              "wins" = wins,
              "winner" = winners,
              "winrate" = winrate,
              "tie" = tie,
              "modal_A" = modal_A_score,
              "modal_B" = modal_B_score,
              "modal_freq" = modal_frequency))
}

markov_visualizations = function(summary){
  
  # Create a histogram of each team's final score on a common x-axis
  # This should be one of the interactive graphs that allows us to roll our cursor over
 scores_overlap = ggplot(data = summary$final_scores) +
    # Team A
    geom_histogram(aes(x = `A`), fill = snappa_pal[2], alpha = 0.9) +
    geom_histogram(aes(x = `B`), fill = snappa_pal[3], alpha = 0.7) +
   theme_snappa() + 
   xlab("Total Points") + 
   ylab("Number of Games")
  
  # Idea, use the final score summary data to make stacked geom_bars for each
  # game, kind of like what I had in my thesis. Here, I'll separate according 
  # to the margin of victory, so that we look from the most A favored games
  # to the most B favored
  
  score_counts = summary$final_scores %>% 
    mutate(difference = A - B) %>%
    arrange(difference) %>%
    mutate(game_id = row_number()) %>%
    pivot_longer(cols = c(A,B), names_to = "team", values_to = "points")
  score_counts = score_counts[rep(row.names(score_counts), score_counts$points), ]
  # To make this raw data more useful, I expand this out so that my grouping
  # id (game_id) will work
  
  # These geom bars look kind of bad without a line if the number of 
  # obs is less than about 500. Setting the width to 1 fixes this
  
  
  score_shares = ggplot(data = score_counts) + 
      geom_bar(aes(x = game_id, fill = fct_rev(team)),  position = "fill", width = 1) + 
      geom_hline(yintercept = 0.5, color = "white", linetype = "dashed", size = 1) + 
      geom_vline(xintercept = nrow(summary$final_scores)/2, color = "white", linetype = "dashed", size = 1) +
    ylab("Share of Total Points") + 
    xlab("Simulated Game ID - Sorted by Score Difference Between A and B") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
    theme_snappa() +
    scale_fill_manual(values = c(snappa_pal[3], snappa_pal[2])) +
    theme(legend.position = "none")
  
  winners_tbl = tibble(won = summary$wins) %>%
    mutate(game_id = row_number(), team = won) 
  
  winners_tbl = complete(winners_tbl,
                         expand(winners_tbl, game_id, team),
                         fill = list(won = "C")) %>%
    mutate(won = case_when(won == team ~ 1,
                    won != team ~ 0)
    )
  
  
  
  win_probability_bar = winners_tbl %>% 
    filter(won == 1) %>%
    ggplot() + 
    geom_bar(aes(y = won, fill = fct_rev(team)), position = "fill") +
    geom_vline(xintercept = 0.5, color = "white", linetype = "dotted") + 
    ylab("") +
    xlab(" ") + 
    scale_y_continuous(breaks = NULL) +
    scale_x_continuous(breaks = 0.50, labels = scales::percent_format(1)) + 
    scale_fill_manual(values = c(snappa_pal[3], snappa_pal[2])) + 
    coord_fixed(0.01) + 
    theme(panel.background = element_rect(fill = "transparent", colour = NA),
          plot.background = element_rect(fill = "transparent", colour = NA),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          plot.margin = unit(c(0, 0, 0, 0), "null"),
          panel.spacing = unit(c(0, 0, 0, 0), "null"),
          axis.ticks.x = element_line(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(size = 20),
          axis.text.y = element_blank(),
          axis.title = element_blank(),
          axis.line = element_blank(),
          legend.position = "none",
          axis.ticks.length = unit(0, "null"),
          legend.spacing = unit(0, "null"))
  
  return(list("overlap" = scores_overlap,
              "shares" = score_shares,
              "win_probability" = win_probability_bar))
  
}





# Summary Visualizations --------------------------------------------------




player_score_breakdown = function(scores, snappaneers, ps_players, ps_game, ps_team){
  
  # Break down a player's points into their different score types
  # View each score type as a proportion of the total points
  # The issue being that each score type is NOT mutually exclusive

  if(ps_team == "A"){
    team_margin = margin(0,5,0,0)
  } else {
    team_margin = margin(0,0,0,5)
  }
  reverse_legend = (!!ps_team == "A")
  
  
  
  # df = aggregate_player_stats_and_sinks(scores, snappaneers) %>% 
  #   select(player_id, team, total_points, normal_points, sink_points,  paddle_points, clink_points) %>%
  #   mutate(total = total_points) %>% 
  #   # Pivot to get point type
  #   pivot_longer(cols = ends_with("points"), names_to = "type", values_to = "points", names_transform = list(type = ~str_remove(., "_points"))) %>% 
  #   filter(type != "total") %>% 
  #   # Convert point type to factor
  #   mutate(point_type = factor(type, 
  #                              labels = c("Sink", "Clink", "Paddle", "Normal toss"), 
  #                              levels = c("sink", "clink", "paddle", "normal"), ordered = T)) %>% 
  #   group_by(player_id) %>%
  #   mutate(point_pct = points/total) %>% 
  #   replace_na(list(point_pct = 0)) %>% 
  #   left_join(ps_players, by = "player_id") 
  # 
  # 
  # 
  #   df %>%
  #     ggplot(., aes(y = player_name, x = points, fill = point_type))+
  #     # Bars
  #     geom_col(position = "fill", colour = snappa_pal[1], size = 1.5)+
  #     # Labels
  #     geom_text(data = filter(df, point_pct > .15), 
  #               aes(label = scales::percent(point_pct, accuracy = 1)), 
  #               position = position_fill(vjust = .5), colour = "white", show.legend = F) +
  #     # Y Axis
  #     scale_y_discrete(name = NULL, position = if_else(reverse_legend, "left", "right"))+
  #     # X Axis
  #     scale_x_continuous(name = NULL, labels = scales::percent)+
  #     # Colour scale
  #     scale_fill_manual(name = NULL, drop=F,
  #                       values = c("Normal toss" = "#67A283", "Paddle" = "#793E8E", "Clink" = "#54B6F2", "Sink" = "#FFA630" ),
  #                       guide = guide_legend(reverse = T, label.hjust = 0.5, nrow = 2, byrow = T))+
  #     # Theme elements
  #     theme_snappa(md=T, plot_margin = team_margin, base_size = 11)+
  #     theme(
  #       panel.grid.major = element_blank(),
  #       panel.grid.minor = element_blank(),
  #       axis.line = element_blank(),
  #       legend.position = "bottom",
  #       legend.margin = margin(0,-30,0,-30),
  #       legend.text.align = .5,
  #       axis.text.x = element_blank(),
  #       axis.text.y.left = element_text(margin = margin(l = 0, r = -5), hjust = 1),
  #       axis.text.y.right = element_text(margin = margin(l = -5, r = 0), hjust = 0)
  #     )
    
    # Option 1:
    # plot_df = aggregate_player_stats_and_sinks(scores, snappaneers) %>%
    #   select(player_id, team, shots, total_points, normal_points, sink_points, paddle_sinks, paddle_points, foot_points, clink_points) %>%
    #   mutate(total = total_points) %>%
    #   pivot_longer(cols = ends_with("points"), names_to = "type", values_to = "points", names_transform = list(type = ~str_remove(., "_points"))) %>%
    #   arrange(team, desc(total), player_id, desc(points)) %>%
    #   mutate(type = factor(type, levels = c("sink", "foot", "paddle", "clink", "normal", "total"), ordered = T),
    #          bar_width = if_else(type == "total", .1, 10))
    # 
    # filter(plot_df, type !="total") %>%
    #   left_join(ps_players, by = "player_id") %>%
    #   ggplot(., aes(x = type, y = points))+
    #   geom_col(aes(fill = type),
    #            colour = snappa_pal[1])+
    #   geom_text(aes(y = points + .5, label = na_if(points, 0)),
    #             family = "Inter Medium", colour = "black")+
    #   geom_col(data = filter(plot_df, type =="total"),
    #            aes(fill = type), width = .25, position = position_dodge(width = 1), colour = snappa_pal[1])+
    #   scale_fill_manual(name = NULL, drop=F,
    #                     values = c("normal" = "#67A283", "paddle" = "#793E8E", "clink" = "#54B6F2", "sink" = "#FFA630", "foot" = "#011936", "total" = "gray20"),
    #                     guide = guide_legend(direction = "vertical", ncol = 1, reverse = T))+#090C9B
    #   scale_x_discrete(drop=F, position = "top")+
    #   coord_flip()+
    #   facet_wrap(~player_name, ncol = 1, strip.position = "left", as.table = F)+
    #   theme_snappa(md=T, text_family = "Inter Medium")+
    #   theme(axis.text.y = element_blank(), axis.title.y = element_blank(),
    #         strip.text.y.left = element_text(size = 14, angle = 0, face = "bold", margin = margin(0,10,0,0)))
    
    # Option 2: Nightingale/Radar plot
    plot_df = aggregate_player_stats_and_sinks(scores, snappaneers) %>%
      inner_join(ps_players, by = "player_id") %>% 
      select(player_name, team, shots, total_points, normal_points, sink_points, paddle_sinks, paddle_points, foot_points, clink_points) %>%
      # Add total as persistent var
      mutate(total = total_points) %>%
      # Pivot separate variables into an input for fill
      pivot_longer(cols = ends_with("points"), names_to = "type", values_to = "points", names_transform = list(type = ~str_remove(., "_points"))) %>%
      arrange(team, desc(total), player_name, desc(points)) %>%
      # Remove total from our fill var
      filter(type != "total") %>%
      mutate(type = factor(type, levels = c("sink", "foot", "paddle", "clink", "normal"),
                           labels = c("Sink", "Foot", "Paddle", "Clink", "Normal"), ordered = T))
    
    # browser()
    plot_df %>% 
      # Show point type by the number of pts (because factoring above didn't work?)
      ggplot(., aes(x = reorder(type, points), y = points))+
      # Columns
      geom_col(aes(fill = type, y = points + 2),
               colour = snappa_pal[1],
               position = position_dodge(width = 1), width = 1)+
      # Pt labels
      geom_text(aes(y = (points+3)/2, label = na_if(points, 0), 
                    size = 4+sqrt(points)),
                colour = snappa_pal[1],
                family = "Inter Medium", fontface = "bold")+
      # Axes
      scale_x_discrete(drop=T)+
      # Colours
      scale_fill_manual(name = NULL, drop=T,
                        values = c("Normal" = "#67A283", "Clink" = "#54B6F2", "Sink" = "#FFA630", "Paddle" = "#793E8E", "Foot" = "#090C9B"),
                        guide = guide_legend(direction = "horizontal", byrow = T,
                                             ncol = 2, reverse = T))+#090C9B
      scale_size(guide = guide_none())+
      # Make it polar
      coord_polar(start = pi/2, direction = -1)+
      # Facet on player
      facet_wrap(~player_name, ncol=1,
                 strip.position = if_else(reverse_legend, "left", "right"),
                 as.table = F, drop = T)+
      # Theme elements
      theme_snappa(md=T, plot_margin = team_margin)+
      theme(axis.title = element_blank(), # no title
            legend.position = "bottom",# legend on bottom 
            axis.line = element_blank(), # No axis line
            axis.text.y.left = element_blank(), # No axis text
            axis.text.x = element_blank(),
            # Facet labels
            strip.text.y.left = element_text(size = 14, angle = 0, face = "bold", margin = margin(0,0,0,5)),
            strip.text.y.right = element_text(size = 14, angle = 0, face = "bold", margin = margin(0,5,0,0)),
            # No gridlines
            panel.grid.major = element_blank(), 
            panel.spacing = unit(-2/(length(unique(plot_df$player_name))*1.5), "lines"))
    
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
    inner_join(players, by = "player_id")
  
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
              cum_score = 0,
              .groups = "drop")
  
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
                       breaks = breaks_rounds(n =7), 
                       limits = c(0, max_round+5),
                       expand = expansion())+
    scale_colour_manual(values = c("A" = "#e26a6a", "B" = "#2574a9"))+
    # labs(title = "How the die flies",
    #      subtitle = "Players' point progression")+ #<img src = "www/sink.png" width="30px" height="30px">
    theme_snappa(md=T)
}



player_form_plot = function(stat, form_data){
  
  stat_name = str_to_title(str_replace(stat, "_", " "))
  
  # X axis title conditional on number of games chosen
  # plot_title = str_c(stat_name, ": ", 
  #                 if_else(input$sample_select == "All", 
  #                         str_c("All games (n = ", max(pluck(player_form_data(), "x_lims"))-.5, ")"), 
  #                 paste("Last", input$sample_select, "games")))
  plot = pluck(form_data, "data") %>% 
    ggplot(., aes(x = game_num, y = !!sym(stat)))+
    # Bars
    # geom_col(aes(fill = won_game), width = .5)+
    # Testing out line and circle
    geom_line(colour = "gray20", alpha = .3)+
    geom_point(aes(colour = won_game), size = 3, alpha = .8)+
    # Career avg. line
    geom_segment(aes(x = min(game_num)-.45, y = avg_points, 
                     xend = max(game_num)+.45, yend = avg_points), 
                 lty = "dashed", size = .75)+
    # X axis
    scale_x_continuous(limits = pluck(form_data, "x_lims"), 
                       expand = expansion())+
    scale_colour_manual(values = set_names(snappa_pal[c(2, 5)], c("Lost", "Won")), 
                        guide = guide_legend(title = NULL, reverse = T))
  # scale_fill_manual(values = set_names(snappa_pal[c(2, 5)], c("Lost", "Won")), 
  #                   guide = guide_legend(title = NULL, reverse = T))
  
  if(stat == "toss_efficiency"){
    plot = plot +
      labs(x = expression(More ~ Recent ~ Games %->% ""), y = "Toss Efficiency", 
           caption = str_c("- - - -  Career Avg. (", 
                           scales::percent(unique(pluck(form_data, "data")[["avg_points"]])), ")"))+
      scale_y_continuous(breaks = scales::pretty_breaks(), 
                         labels = scales::percent,
                         expand = expansion(),
                         limits = c(0, 1))
    
  } else {
    plot = plot +
      labs(x = expression(More ~ Recent ~ Games %->% ""), y = stat_name, 
           # title = plot_title,
           caption = str_c("- - - -  Career Avg. (", 
                           scales::comma(unique(pluck(form_data, "data")[["avg_points"]]), accuracy = 1), " points)"))+
      scale_y_continuous(breaks = scales::pretty_breaks(), expand = expansion(),
                         limits = c(0, pluck(form_data, "career_high")*1.25))
  }
  plot+
    theme_snappa()+
    theme(axis.text.x = element_blank(),
          legend.position = "bottom",
          legend.key.height = unit(.25, "cm"))
}

breaks_rounds = function (n = 5, ...) {
  scales:::force_all(n, ...)
  n_default <- n
  function(x, n = n_default) {
    breaks <- pretty(x, n, ...)
    names(breaks) <- c("0", rounds)[breaks+1]
    breaks
  }
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



# Visualization themes ----------------------------------------------------


snappa_pal = str_c("#", c("fafaf9","e26a6a","2574a9","ffaf47","67a283","793e8e","54b6f2"))
options(reactable.theme = reactableTheme(
  color = "gray20",
  backgroundColor = snappa_pal[1], 
  tableBodyStyle = list(
    padding = "4px 2px"
  ),
  borderColor = "#DEDDDD",
  headerStyle = list(
    alignSelf = "flex-end",
    fontSize = "13px",
    borderBottom = "3px solid #7c7c7c"
  ))
)


# Render a bar chart with a label on the left
bar_chart <- function(label, width = "100%", height = "14px", fill = "#00bfc4", background = NULL) {
  bar <- div(style = list(background = fill, width = width, height = height))
  chart <- div(style = list(flexGrow = 1, marginLeft = "6px", background = background), bar)
  div(style = list(display = "flex", alignItems = "center"), label, chart)
}

game_summary_diff_style = function(column){
  str_c("
                    function(rowInfo) {
                        var value = rowInfo.row.", column,"
                      // input:
                      //  - rowInfo, an object containing row info
                      //  - colInfo, an object containing column info (optional)
                      //  - state, an object containing the table state (optional)
                      var neg = /\\-/
                      var pos = /\\+/
                      if (neg.test(value)) {
                        var colour = '#e26a6a'
                        var bg_col = '#fafaf9'
                        
                      } 
                      else if (pos.test(value)) {
                        var colour = '#67a283'
                        var bg_col = '#fafaf9'
                        
                      } else if (value == '0') {
                        var colour = 'gray20'
                        var bg_col = '#fafaf9'
                        
                      } else {
                        if (rowInfo.row.team == 'A' & rowInfo.level < 1) {
                          var bgcol =  '#e26a6a80'
                        } else if (rowInfo.row.team == 'B' & rowInfo.level < 1) {
                          var bgcol = '#2574a980'
                        } 
                                     
                      }
                      
                      return { backgroundColor: bgcol, color: colour, padding: '4px 2px'}
                    }
                  ")
}

rt_team_highlight = JS("function(rowInfo) {
                                            if (rowInfo.row.team == 'A' ) {
                                                var color =  '#e26a6a80'
                                            } else if (rowInfo.row.team == 'B') {
                                                var color = '#2574a980'
                                            } 
                                            return { backgroundColor: color, color: '#fafaf9', fontWeight: 600 }
                                        }")

theme_snappa = function(title_family = "Inter SemiBold",
                        text_family = "Inter Medium",
                        base_size = 12, 
                        text_color = "gray20",
                        bg_color = snappa_pal[1], line_colour = "#DEDDDD",
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
        line = element_line(color = line_colour),
        
        plot.title = element_text(face = "bold",
                                  size = base_size * 2,
                                  lineheight = 1.2,
                                  margin = margin(0,5,15,5)), 
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
        axis.line = element_line(color = line_colour),
        
        legend.title = element_text(size = base_size * 1.3),
        legend.text = element_text(size = base_size * 1), 
        legend.key.size = unit(5, "mm")
      )
  } else if (plots_pane == FALSE & md == TRUE) {
    ggplot2::theme_minimal() +
      ggplot2::theme(
        text = element_text(family = text_family,
                            size = base_size,
                            color = text_color),
        title = ggtext::element_markdown(family = title_family,
                                         color = text_color),
        line = element_line(color = line_colour),
        
        plot.title = ggtext::element_markdown(face = "bold",
                                              size = base_size * 2,
                                              lineheight = 1.2,
                                              margin = margin(0,5,15,5)),
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
        axis.line = element_line(color = line_colour),
        
        legend.title = ggtext::element_markdown(size = base_size * 1.3),
        legend.text = element_text(size = base_size * 1), 
        legend.key.size = unit(5, "mm")
      )
  } else if (plots_pane == TRUE && md == TRUE) {
    ggplot2::theme_minimal(base_size = base_size) +
      ggplot2::theme(
        text = element_text(family = text_family,
                            color = text_color),
        title = element_text(family = title_family),
        line = element_line(color = line_colour),
        
        plot.title = ggtext::element_markdown(face = "bold",
                                              lineheight = 1.2,
                                              margin = margin(0,5,15,5)),
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
        axis.line = element_line(color = line_colour), 
        legend.key.size = unit(5, "mm")
      )
  } else {
    ggplot2::theme_minimal(base_size = base_size) +
      ggplot2::theme(
        text = element_text(family = text_family,
                            color = text_color),
        title = element_text(family = title_family),
        line = element_line(color = line_colour),
        
        plot.title = element_text(face = "bold",
                                  lineheight = 1.2,
                                  margin = margin(0,5,15,5)),
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
        axis.line = element_line(color = line_colour), 
        legend.key.size = unit(5, "mm")
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
                            row_group.default_label = NULL, 
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

