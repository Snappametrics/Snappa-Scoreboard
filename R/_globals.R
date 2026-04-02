library(tidyverse)

# Round labels used throughout the app (sourced first due to underscore prefix)
rounds = paste0(rep(1:100, each = 2), rep(c("A", "B"), 100))
round_labels = rep(c("Pass the dice", "Next round"), 100)

# Score combinations that trigger casualty events
casualty_rules = tribble(~team_A, ~team_B, ~casualty_title, ~casualty_text, ~image,
                         12, 7,  "12-7",       "Roll off to see who is taking the kamikaze to the face", 'https://upload.wikimedia.org/wikipedia/commons/thumb/c/c7/Attack_on_Pearl_Harbor_Japanese_planes_view.jpg/1280px-Attack_on_Pearl_Harbor_Japanese_planes_view.jpg',
                         7,  12, "12-7",       "Roll off to see who is taking the kamikaze to the face", 'https://upload.wikimedia.org/wikipedia/commons/thumb/c/c7/Attack_on_Pearl_Harbor_Japanese_planes_view.jpg/1280px-Attack_on_Pearl_Harbor_Japanese_planes_view.jpg',
                         18, 12, "War of 1812","Everyone roll a die, the lowest roll takes a shot.", NULL,
                         12, 18, "War of 1812","Everyone roll a die, the lowest roll takes a shot.", NULL,
                         20, 03, "2003",        "Nevar forget: a 9/11 consists of a shot of fireball into a Sam Adams", NULL,
                         03, 20, "2003",        "Nevar forget: a 9/11 consists of a shot of fireball into a Sam Adams", NULL)

# Point + clink combinations that qualify as sinks
sink_criteria = tribble(~points_scored, ~clink,
                        3, FALSE,
                        5, TRUE,
                        7, TRUE)
