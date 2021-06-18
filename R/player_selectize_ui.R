#' Player Selectize UI 
#' Adds selectize inputs for players on both teams 
#' @param id The namespace of the app, as well as the positional ID of the player (e.g. 'A1')
#' @param header The header to go over the selectize input, usually "Player __"
#' @param replacement_name A name which is handed off from 'game_restart_outputs' in the event that the game is restarted
#' 

player_selectize_UI = function(id, header, replacement_name) {
  ns <- NS(id)
  selectizeInput(ns('player_input'), header, c(`Player Name`='', player_choices),  
                 options = list(create = TRUE, hideSelected=T), width = "125%")
}