# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Is

A Shiny web application for tracking and displaying scoreboard statistics for a dice game called Snappa. Deployed to Posit Connect (shinyapps.io), backed by a PostgreSQL database on AWS RDS.

## Common Commands

```r
# Restore dependencies (run once after cloning)
renv::restore()

# Run the app locally
shiny::runApp()

# Deploy to Posit Connect
source("documentation/deployapp.R")

# Lint code
lintr::lint_dir("R/")

# Format code
styler::style_dir("R/")
```

There is no automated test suite. Manual testing protocols are in `documentation/testing_protocol`.

## Architecture

### File Layout

- **`app.R`** — Monolithic main file (~2,800 lines): global setup, full UI definition, all server logic. This is intentionally large (refactoring is in progress; see `docs/MODULE_ARCHITECTURE.md`).
- **`R/ui_functions.R`** — ~47 functions that return UI components: tabs, modals, tables, visualizations.
- **`R/server_functions.R`** — ~31 helper functions: game logic (`validate_scores`, `rebuttal_check`, `detect_sink`), stats aggregation, DB write operations.
- **`R/dbconnect.R`** — Database connection pool setup (reads from env vars).
- **`database/`** — SQL schema files and query templates (not sourced by the app; reference only).

### Data Flow

1. `app.R` sources `R/dbconnect.R` at startup, creating global `con` (a `pool::dbPool` connection).
2. Global constants are defined before UI/server: `rounds`, `casualty_rules`, `snappa_pal`.
3. Server uses `reactiveValues(...)` as `vals` — a god object holding all live game state (current round, scores, player names, etc.).
4. 16+ reactive expressions derive computed state from `vals`; 39 `observeEvent`/`observe` handlers drive state changes.
5. UI functions from `R/ui_functions.R` are called inside the UI definition to build each tab/modal.

### Database

- **PostgreSQL** via `RPostgres` + `pool` (connection pooling).
- Credentials come exclusively from `.Renviron` environment variables: `DB_HOST`, `DB_USER`, `DB_PASSWORD`, `DB_PORT`, `DB_NAME`.
- A test database is available via `DB_HOST_TEST`.
- Key tables: `players`, `scores`, `player_stats`, `game_stats`, `career_stats`.

### UI Structure

The dashboard has 4 main tabs:
1. **Team Input** — Enter player names, configure game settings.
2. **Scoreboard** — Live scoring interface during a game.
3. **Player Stats** — Per-game stats and visualizations.
4. **Career Stats** — Aggregated career statistics.

Visualizations use ggplot2 with custom `snappa_pal` theme colors; interactive tables use `reactable` and `gt`.

## Key Patterns

- **`vals` reactiveValues** is the central state object. Most server-side logic reads from or writes to `vals`.
- **`pool::poolWithTransaction()`** wraps multi-step DB writes to ensure atomicity.
- UI component functions in `R/ui_functions.R` reference global constants (`rounds`, `snappa_pal`) directly — they are not pure functions.
- The `casualty_rules` tribble encodes special scoring rules (e.g., "War of 1812", "2003/9-11") that trigger bonus points.
- Font loading via `extrafont` may require `font_import()` on first use on a new machine.

## Branch Context

The current branch `refactor-modularize` is an in-progress refactor. `docs/MODULE_ARCHITECTURE.md` describes the planned modular structure. The master branch reflects production state.

## Git Conventions

When exploring or modifying the project, commit meaningful progress incrementally rather than batching all changes.