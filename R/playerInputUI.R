#' Player Input Screen UI
#' Handles all of the UI elements outside of the dashboard options when players
#' lauch the app
#' 
#' @param id The namespace of the module

teamInputUI = function(id){
  ns = NS(id)
  team = str_sub(id, -1, -1)
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
           # style = paste("background:", team_colour),
           # Header
           h1(paste("Team", toupper(team)), class=str_c("team-head ", team)),
           tags$div( id = div_selector,
                     # How many players
                     h3(class = 'team-size-selector-title', 'How many players?'),
                     radioGroupButtons(ns("size"),
                                       label = NULL,
                                       choices = 2:4, selected = 2,
                                       size = "lg", status = str_c("size btn-", team)),
                     # Player inputs
                     uiOutput(ns("input"), class = "player-inputs"),
                     
                     
                     # CSS: Increase font size, change color to white, add top and bottom margins
                     tags$style(type = "text/css", paste(players, "{color: white; margin-top:30px;margin-bottom:30px;}"))
           )
         ),
         tags$link(rel = 'stylesheet', type = 'text/css', href = 'teamInputUI.css')
  )
}



playerInputUI <- function(id) {
  # This isn't my favorite way to do it since we're sourcing the functions in the
  # module, but it will at least take these away from UI functions.
  # Maybe it's better for these to be their own modules if we want to get 
  # crazy with it
  
  # It looks like we can assign functions outside the module and then call them
  # inside the module:
  # https://mastering-shiny.org/scaling-modules.html#modules-inside-of-modules
  ns = NS(id)
  
  fluidRow(class = id,
    teamInputUI(ns("A")),
    
    
    # Column 2 - empty
    column(4,  align = "center", class = ns("center"),
           pickerInput(
             inputId = ns("arena_select"),
             label = "Arena",
             selected = "Greenhaus 2: Electric Boogaloo",
             choices = c("Greenhaus", "Ventura", "Greenhaus 2: Electric Boogaloo"),
             options = pickerOptions(
               mobile = T,
               showTick = T
             )
           ),
           disabled(
             actionButton(ns("start_game"), 
                        label = "Throw dice?", class = "start",
                        icon = icon("dice"))),
           uiOutput("validate_start"),
           
           helpText("Note: All players must enter their name before the game can begin")
    ),
    
    
    # Column 3 - Team B
    teamInputUI(ns("B")),
    tags$link(rel = 'stylesheet', type = 'text/css', href = 'playerInputUI.css')
  )
 
   
}