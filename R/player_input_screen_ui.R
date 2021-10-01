#' Player Input Screen UI
#' Handles all of the UI elements outside of the dashboard options when players
#' lauch the app
#' 
#' @param id The namespace of the module

player_input_screen_ui <- function(id) {
  ns <- ns(id)
  # This isn't my favorite way to do it since we're sourcing the functions in the
  # module, but it will at least take these away from UI functions.
  # Maybe it's better for these to be their own modules if we want to get 
  # crazy with it
  
  # It looks like we can assign functions outside the module and then call them
  # inside the module:
  # https://mastering-shiny.org/scaling-modules.html#modules-inside-of-modules
  team_input_ui = function(team, player_choices){
    players = str_c("#name_", team, 1:4, "-selectized", collapse = ", ")
    player_inputs = str_c("#name_", team, 1:4, collapse = ", ")
    team_colour = if_else(team == "A", "#e26a6a", "#2574a9")
    
    well_selector = str_c('input_well_', team)
    div_selector = str_c('input_forms_', team)
    class_selector = paste0('input_well well_', team)
    player_ids = str_c(team, 1:2)
    
    column(4, align = "center",
           
           wellPanel(
             class = class_selector,
             id = well_selector,
             style = paste("background:", team_colour),
             # Header
             h1(paste("Team", toupper(team)), style = "text-align: center; color: white; font-size: 400%; width: fit-content; align-self:center; margin-bottom:10%;"),
             tags$div( id = div_selector,
                       class = 'player_input_forms',
                       # Player 1
                       # player_selectize_UI(player_ids[1], 'Player 1', player_choices),
                       # Player 2
                       # player_selectize_UI(player_ids[2], 'Player 2', player_choices),
                       uiOutput(ns(str_c("team_", team, "_input")), class = "player-inputs"),
                       h3(class = 'team-size-selector-title', 'How many players?'),
                       radioGroupButtons(ns(str_c("team_", team, "_size")),
                                         label = "",
                                         choices = 2:4,selected = 2,
                                         size = "lg"),
                       
                       # CSS: Increase font size, change color to white, add top and bottom margins
                       tags$style(type = "text/css", paste(players, "{color: white; margin-top:30px;margin-bottom:30px;}"))
             )
           )
    )
  }
  
  fluidRow(
    team_input_ui("A"),
    
    
    # Column 2 - empty
    column(4,  align = "center",
           pickerInput(
             inputId = "arena_select",
             label = "Arena",
             selected = "Greenhaus 2: Electric Boogaloo",
             choices = c("Greenhaus", "Ventura", "Greenhaus 2: Electric Boogaloo"),
             options = pickerOptions(
               mobile = T,
               showTick = T
             )
           ),
           disabled(
             actionBttn("start_game", 
                        label = "Throw dice?", style = "pill", color = "primary", 
                        icon = icon("dice"), size = "sm")),
           uiOutput("validate_start"),
           
           helpText("Note: All players must enter their name before the game can begin")
           # 
           # awesomeRadio(inputId = "play_to", 
           #              label = "What score are you playing to?", 
           #              choices = list("21" = 1, "32" = 2), 
           #              selected = 1, inline = T)
    ),
    
    
    # Column 3 - Team B
    team_input_ui("B")
  )
 
   
}