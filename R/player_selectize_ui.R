#' Player Selectize UI 
#' Adds selectize inputs for players on both teams 
#' @param id The namespace of the app, as well as the positional ID of the player (e.g. 'A1')
#' @param header The header to go over the selectize input, usually "Player __"
#' @param player_choices The group of players to be sourced as options for the selectize

player_selectize_UI = function(id, header, player_choices) {
  ns <- NS(id)
  selectizeInput(ns('player_name'), header, c(`Player Name`='', player_choices),  
                 options = list(create = TRUE, hideSelected=T), width = "125%")
}