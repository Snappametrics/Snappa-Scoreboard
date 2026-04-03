# Snappa Scoreboard - Module Architecture Analysis

## Overview

This document analyzes the current codebase architecture and proposes a modularized structure based on the Single Responsibility Principle (SRP).

---

## 1. Current File Structure & Sizes

| File | Lines | Purpose |
|------|-------|---------|
| `app.R` | 2,807 | Main app (UI + server + everything) |
| `R/ui_functions.R` | 2,506 | UI components + visualizations |
| `R/server_functions.R` | 793 | Game logic + stats + DB helpers |
| `R/dbconnect.R` | 34 | Database connection |

**Total: ~6,140 lines** with most logic concentrated in `app.R`

---

## 2. Current Dependency Structure

```
┌─────────────────────────────────────────────────────────────────────┐
│                            app.R (2,807 lines)                      │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────────────────┐  │
│  │ UI Definition│  │ 16 Reactives │  │ 39 Event Handlers        │  │
│  │ (lines 51-   │  │ (342-1200)   │  │ (781-2785)               │  │
│  │  165)        │  │              │  │                          │  │
│  └──────┬───────┘  └──────┬───────┘  └────────────┬─────────────┘  │
│         │                 │                       │                 │
│         │     ┌───────────┴───────────┐          │                 │
│         │     │   vals (reactiveValues)│          │                 │
│         │     │   - game_id, score_id  │◄─────────┤                 │
│         │     │   - player_stats_db    │          │                 │
│         │     │   - casualties, etc.   │          │                 │
│         │     └───────────┬───────────┘          │                 │
│         │                 │                       │                 │
└─────────┼─────────────────┼───────────────────────┼─────────────────┘
          │                 │                       │
          ▼                 ▼                       ▼
┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐
│ R/ui_functions.R│  │R/server_functions│  │  R/dbconnect.R  │
│   (2,506 lines) │  │   (793 lines)    │  │   (34 lines)    │
│                 │  │                  │  │                 │
│ • 47 functions  │  │ • 31 functions   │  │ • DB connection │
│ • Tab builders  │  │ • validate_scores│  │ • con object    │
│ • Modals        │  │ • aggregate_stats│  │                 │
│ • Tables        │  │ • db_update_*    │  │                 │
│ • Themes        │  │ • leaderboard    │  │                 │
└────────┬────────┘  └────────┬─────────┘  └────────┬────────┘
         │                    │                     │
         └────────────────────┼─────────────────────┘
                              │
                    ┌─────────▼─────────┐
                    │  Global Variables │
                    │  • rounds         │
                    │  • casualty_rules │
                    │  • snappa_pal     │
                    │  • con (DB)       │
                    └───────────────────┘
```

---

## 3. Function Catalog

### R/ui_functions.R (47 Functions)

#### UI Layout Functions
- `team_input_tab()` - Team setup interface
- `scoreboard_tab()` - Active game scoreboard display
- `career_stats_tab()` - Career statistics view
- `player_stats_tab()` - Individual player statistics
- `team_input_ui()` - Team roster input form
- `team_scoreboard_ui()` - Scoreboard layout
- `team_edit_ui()` - Edit team configuration
- `glance_ui_game()` - Game summary glance
- `glance_ui_team()` - Team summary glance

#### Pop-up/Modal Functions
- `score_check()` - Modal for score entry validation
- `casualty_popup()` - Special event notification (e.g., "12-7", "War of 1812")
- `sink_casualty_popup()` - Sink/casualty selection
- `tifu_casualty_popup()` - Friendly fire incident handler
- `highnoon_popup()` - "High noon" event trigger
- `restart_game_popup()` - Resume unfinished game
- `game_summary_modal()` - Game completion summary
- `arena_select_popup()` - Arena selection dialog

#### Component Functions
- `player_input()` - Single player input field
- `extra_player_input()` - Dynamic extra player fields
- `dropdownBlock2()` - Custom dropdown header component
- `game_notification()` - Toast notification for scoring
- `add_player_input()` - Add 3rd/4th player dynamically
- `remove_p3_input()` / `remove_p4_input()` - Remove extra players
- `extra_player_ui()` - Extra player UI injection

#### Table/Display Functions
- `recent_scores_tab()` - Recent scoring events display
- `glance_table_team()` - Team summary table
- `game_summary_tab_rt()` - Game results table
- `restart_summary_tab_rt()` - Resume game summary
- `team_summary_tab_rt()` - Team stats comparison table
- `leaderboard_table_rt()` - Career stats leaderboard

#### Visualization Functions
- `score_heatmap()` - 2D heatmap of final scores
- `player_score_breakdown()` - Score type breakdown (partial)

#### Styling/Theme Functions
- `theme_snappa()` - Custom ggplot theme
- `tab_theme_snappa()` - GT table styling
- `last_score_col_list` - Reactable column definitions

---

### R/server_functions.R (31 Functions)

#### Game Logic & Validation
- `validate_scores()` - Check if a score entry is valid (rules enforcement)
- `rebuttal_check()` - Determine if team is in "rebuttal" (comeback) state
- `cooldown_check()` - Check if casualty rules have "cooled down"
- `detect_sink()` - Identify if a score was a "sink" (specific points+clink combos)

#### Scoring & Shot Tracking
- `add_shot_count()` - Calculate shots per player based on team size
- `parse_round_num()` - Convert round labels ("1A", "1B") to numeric

#### Player Statistics Aggregation
- `aggregate_player_stats()` - Calculate per-game player stats from raw scores
- `aggregate_player_stats_and_sinks()` - Enhanced stats with sink tracking
- `make_summary_table()` - Historical comparison table (current vs. past games)
- `player_performance_summary()` - Comparative performance analysis
- `find_similar_games()` - Find historically "similar" games by team size

#### Leaderboard & Career Stats
- `calculate_leaderboard_stats()` - Aggregate career-long statistics

#### Formatting
- `toss_percent_plus()` - Format positive percentages
- `toss_percent_minus()` - Format negative percentages

#### Database Operations
- `db_update_player_stats()` - UPDATE player_stats table
- `db_update_round()` - UPDATE current round in game_stats

#### Data Utilities
- `rowAny()` - Helper for row-wise logical operations

---

### app.R Server Logic

#### Reactive Values & Initialization
- `vals` (reactiveValues) - Central state management object containing:
  - `game_id`, `new_player_id`, `score_id`, `shot_num`
  - `game_stats_db`, `player_stats_db`, `scores_db`
  - `players`, `db_tbls` (reactive poll), `recent_scores` (reactive poll)
  - `casualties`, `cooldowns`, `current_scores`, `rebuttal`, `error_msg`

#### Reactive Computations (16 major reactives)
- `score_to()` - Game target score (11-50)
- `round_num()` - Current round ("1A", "1B", etc.)
- `active_player_inputs()` - List of non-null player inputs
- `player_inputs()` - All player input values
- `expected_player_inputs()` - Only required players (A1, A2, B1, B2)
- `snappaneers()` - Current game roster with team/shot info
- `current_choices()` - Available players not yet selected
- `num_players()` - Count of active players
- `team_a_summary_stats()` / `team_b_summary_stats()` - Real-time team performance
- `player_game_stats()` - Per-player current game stats
- `player_form_data()` - Historical form data
- `teammate_stats()` - Teammate performance metrics
- `player_game_history()` - Player game history query
- `overall_player_stats()` - Career player statistics
- `casualty_stats()` - Casualty frequency by player
- `game_summary()` - Game completion summary

#### Major Event Handlers (39 observeEvent blocks)
- `input$start_game` - Initialize new game
- `input$next_round` - Advance to next round (with rebuttal check)
- `input$previous_round` - Go back one round
- `input$ok_A` / `input$ok_B` - Score submission
- `input$casualty_manual` - Manual casualty trigger
- `input$sink_casualty` - Handle sink casualties
- `input$tifu` / `input$tifu_confirm` - Friendly fire events
- `input$highnoon_manual` - High noon event
- `input$resume_yes` / `input$resume_no` - Resume game logic
- `input$finish_game` - Complete the game
- `input$send_to_db` - Persist game to database
- Dynamic player add/remove handlers

#### Output Renderers (15+ major outputs)
- `output$sidebar_menu` - Dynamic sidebar
- `output$round_num` - Current round display
- `output$score_A` / `output$score_B` - Team scores
- `output$recent_scores_rt` - Recent scoring table
- `output$leaderboard_rt` - Career stats leaderboard
- `output$general_stats` / `output$paddle_stats` - Player stats tables
- `output$player_form` - Player form line chart
- `output$casualty_stats_plot` - Casualty bar chart
- `output$scoring_heatmap` - Score distribution heatmap

---

## 4. Core Functional Areas

### A. UI Components & Presentation Layer

**Files:** `R/ui_functions.R` (primary), `app.R` (secondary)

**Responsibilities:**
- Building HTML structure for 4 main tabs
- Creating modal dialogs for user interactions
- Rendering data tables (reactable, gt) with custom styling
- Generating visualizations (ggplot2 themes, heatmaps)
- Handling dynamic UI updates

**Coupling Issues:**
- UI functions hardcode color palette and game rules
- Direct reference to global variables in component functions
- Tight coupling to specific data structures

---

### B. Game Logic & Rules

**Files:** `R/server_functions.R` (primary), `app.R` (secondary)

**Responsibilities:**
- Score validation (checking rules)
- Casualty rule application
- Round/shot tracking and conversion
- Game state transitions (rebuttal detection)
- Rule cooldowns

**Coupling Issues:**
- Rules embedded in function logic (not data-driven)
- Heavy reliance on global `vals`, `rounds`, `casualty_rules`
- Complex business logic mixed with data transformations

---

### C. Database Operations

**Files:** `R/dbconnect.R`, `R/server_functions.R`, `app.R`

**Responsibilities:**
- Database connection management (PostgreSQL)
- CRUD operations for games, players, scores, statistics
- Query execution for historical data retrieval
- Data persistence for game completion

**Coupling Issues:**
- Raw SQL string construction scattered throughout
- No parameterized queries (SQL injection risk)
- Database access scattered across multiple files
- Connection object (`con`) is global and implicit

---

### D. Statistics & Analytics

**Files:** `R/server_functions.R` (primary), `R/ui_functions.R` (secondary), `app.R` (tertiary)

**Responsibilities:**
- Per-game player statistics calculation
- Historical performance aggregation
- Leaderboard ranking and date-filtered stats
- Form/trend analysis
- Casualty frequency analytics

**Coupling Issues:**
- Heavy interdependencies with snappaneers data
- Complex nested transformations
- Statistics calculations use environment variable lookups

---

### E. State Management & Reactivity

**File:** `app.R` (lines 248-340, 342-415)

**Responsibilities:**
- Central reactive values object for game state
- Multiple reactive computations for derived values
- Reactive polling for database updates
- Event triggering and observation

**Coupling Issues:**
- Monolithic `vals` object with 15+ properties
- State mutations scattered across 39 observeEvent handlers
- Implicit dependencies between reactives
- No clear separation of concerns

---

## 5. Coupling & Interdependency Analysis

### Tightly Coupled Components

1. **Game Logic ↔ Data Structures**
   - `validate_scores()` requires exact structure of `snappaneers`, `scores_table`
   - Hard to test in isolation

2. **UI ↔ Global State**
   - UI functions reference global `snappa_pal`, `casualty_rules`, `rounds`
   - Cannot reuse UI components in different contexts

3. **Statistics ↔ Data Fetch Logic**
   - `aggregate_player_stats()` tightly coupled to score structure
   - Historical comparison requires specific column naming

4. **Database ↔ Business Logic**
   - Raw SQL queries throughout app.R
   - Database structure assumed in statistics calculations

5. **Server Logic ↔ Reactives**
   - 16 major reactives depend on `vals` state
   - Circular dependencies possible

### Loosely Coupled Components

1. **Visualization Functions** - Generally standalone
2. **Formatting Functions** - Pure functions with no dependencies
3. **DB Connection** - Cleanly isolated in dbconnect.R

---

## 6. app.R Analysis

**Size:** 2,807 lines - Significantly oversized for a single file

### Responsibility Breakdown

| Category | Lines | Responsibility |
|----------|-------|----------------|
| Imports & Setup | 1-45 | Load libraries, connect to DB |
| Global Data | 14-29 | Define `rounds`, `casualty_rules`, `snappa_pal` |
| UI Definition | 51-165 | Build entire UI structure |
| Server Entry | 191-234 | Start server, initialize sidebar |
| Reactive Values | 248-340 | Central state object (15+ properties) |
| Reactives | 342-1200 | 16 major reactive expressions |
| Output Renderers | 421-810 | 15+ output$ definitions |
| Event Handlers | 781-2785 | 39 observeEvent() blocks |
| Game End Logic | 2602-2785 | Game completion and database persistence |

### Problems Identified

1. **God Object Pattern** - `vals` contains unrelated game state, UI state, reactives
2. **Mixed Concerns** - App logic, UI rendering, database queries all in one file
3. **Code Duplication** - Game completion logic repeated in `finish_game` and `send_to_db` handlers
4. **Deep Nesting** - ObserveEvent handlers contain 10-20+ lines of complex logic
5. **No Clear Separation** - Game rules, database operations, UI rendering all mixed

---

## 7. Proposed Module Architecture (SRP-Based)

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                        PROPOSED ARCHITECTURE                                │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│   ┌────────────────────────────────────────────────────────────────────┐   │
│   │                         app.R (SIMPLIFIED)                         │   │
│   │                    UI definition + shinyApp() only                 │   │
│   └───────────────────────────────┬────────────────────────────────────┘   │
│                                   │                                         │
│   ┌───────────────────────────────┴────────────────────────────────────┐   │
│   │                      R/server_main.R                               │   │
│   │              Event handlers only (observeEvent)                    │   │
│   └───────────────────────────────┬────────────────────────────────────┘   │
│                                   │                                         │
│   ┌───────────────────────────────┼────────────────────────────────────┐   │
│   │                               │                                     │   │
│   ▼                               ▼                                     ▼   │
│ ┌──────────────┐  ┌───────────────────────────┐  ┌───────────────────┐     │
│ │R/globals.R   │  │    R/game_state.R         │  │ R/reactive_logic.R│     │
│ │              │  │                           │  │                   │     │
│ │ • rounds     │  │  GameState R6 class:      │  │ • Organized       │     │
│ │ • casualty_  │  │  • current_round          │  │   reactives       │     │
│ │   rules      │  │  • team_scores            │  │ • By dependency   │     │
│ │ • snappa_pal │  │  • player_stats           │  │   group           │     │
│ │ • score_to   │  │  • casualties             │  │                   │     │
│ └──────────────┘  │  • advance_round()        │  └───────────────────┘     │
│                   │  • submit_score()         │                             │
│                   │  • is_game_over()         │                             │
│                   └───────────────────────────┘                             │
│                                                                             │
├─────────────────────────────────────────────────────────────────────────────┤
│                           DOMAIN LAYER                                      │
│                                                                             │
│ ┌─────────────────┐  ┌─────────────────┐  ┌─────────────────────────────┐  │
│ │ R/game_logic.R  │  │ R/statistics.R  │  │ R/database.R                │  │
│ │                 │  │                 │  │                             │  │
│ │ • validate_     │  │ • aggregate_    │  │ • db_get_players()          │  │
│ │   scores()      │  │   player_stats()│  │ • db_get_games()            │  │
│ │ • rebuttal_     │  │ • calculate_    │  │ • db_save_game()            │  │
│ │   check()       │  │   leaderboard() │  │ • db_save_player_stats()    │  │
│ │ • detect_sink() │  │ • find_similar_ │  │ • db_update_round()         │  │
│ │ • cooldown_     │  │   games()       │  │                             │  │
│ │   check()       │  │ • player_       │  │ (All SQL centralized here)  │  │
│ │ • add_shot_     │  │   performance() │  │                             │  │
│ │   count()       │  │                 │  │                             │  │
│ └─────────────────┘  └─────────────────┘  └─────────────────────────────┘  │
│                                                                             │
├─────────────────────────────────────────────────────────────────────────────┤
│                          PRESENTATION LAYER                                 │
│                                                                             │
│ ┌────────────────┐ ┌────────────────┐ ┌────────────────┐ ┌───────────────┐ │
│ │R/ui_tabs.R     │ │R/ui_modals.R   │ │R/ui_tables.R   │ │R/ui_themes.R  │ │
│ │                │ │                │ │                │ │               │ │
│ │• team_input_   │ │• score_check() │ │• leaderboard_  │ │• theme_       │ │
│ │  tab()         │ │• casualty_     │ │  table_rt()    │ │  snappa()     │ │
│ │• scoreboard_   │ │  popup()       │ │• game_summary_ │ │• tab_theme_   │ │
│ │  tab()         │ │• game_summary_ │ │  tab_rt()      │ │  snappa()     │ │
│ │• career_stats_ │ │  modal()       │ │• recent_scores │ │• snappa_pal   │ │
│ │  tab()         │ │• highnoon_     │ │  _tab()        │ │               │ │
│ │• player_stats_ │ │  popup()       │ │                │ │               │ │
│ │  tab()         │ │                │ │                │ │               │ │
│ └────────────────┘ └────────────────┘ └────────────────┘ └───────────────┘ │
│                                                                             │
│ ┌─────────────────────────────────────────────────────────────────────────┐│
│ │                        R/visualizations.R                               ││
│ │  • score_heatmap()  • player_score_breakdown()  • form_chart()          ││
│ └─────────────────────────────────────────────────────────────────────────┘│
└─────────────────────────────────────────────────────────────────────────────┘
```

---

## 8. Proposed File Structure

```
snappa-scoreboard/
├── app.R                    # SIMPLIFIED: UI + shinyApp() only (~200 lines)
├── R/
│   ├── _globals.R           # Constants: rounds, casualty_rules, snappa_pal
│   │
│   ├── database.R           # ALL database operations (centralized SQL)
│   │   └── db_get_*, db_save_*, db_update_*
│   │
│   ├── game_logic.R         # Pure game rules (no reactivity)
│   │   └── validate_scores, rebuttal_check, detect_sink, cooldown_check
│   │
│   ├── game_state.R         # GameState R6 class (replaces vals)
│   │   └── Encapsulates all mutable game state
│   │
│   ├── statistics.R         # All stats calculations
│   │   └── aggregate_player_stats, calculate_leaderboard, find_similar_games
│   │
│   ├── server_main.R        # Server function with event handlers
│   │   └── All observeEvent() blocks, organized by feature
│   │
│   ├── reactive_logic.R     # Reactive expressions (organized by dependency)
│   │   └── 16 reactives, grouped logically
│   │
│   ├── ui_tabs.R            # Tab layout builders
│   │   └── team_input_tab, scoreboard_tab, career_stats_tab, player_stats_tab
│   │
│   ├── ui_modals.R          # Modal/popup functions
│   │   └── score_check, casualty_popup, game_summary_modal
│   │
│   ├── ui_tables.R          # Table rendering
│   │   └── leaderboard_table_rt, game_summary_tab_rt, recent_scores_tab
│   │
│   ├── ui_themes.R          # Styling
│   │   └── theme_snappa, tab_theme_snappa, color definitions
│   │
│   └── visualizations.R     # Charts and plots
│       └── score_heatmap, player_score_breakdown
│
└── database/
    └── (existing SQL files)
```

---

## 9. Key Issues Summary

| Aspect | Current State | Risk Level |
|--------|---------------|------------|
| **Code Organization** | All logic in 1-2 files | HIGH |
| **Testability** | Functions tightly coupled to globals | HIGH |
| **Maintainability** | Duplicated game completion logic | MEDIUM |
| **Database Separation** | SQL mixed throughout | HIGH |
| **UI Reusability** | Hardcoded dependencies | MEDIUM |
| **Game Logic Clarity** | Scattered across app.R & server_functions.R | HIGH |
| **State Management** | Monolithic `vals` object | MEDIUM |
| **Reactive Complexity** | 16 interdependent reactives | HIGH |

---

## 10. Refactoring Priority

| Priority | Module | Effort | Impact |
|----------|--------|--------|--------|
| 1 | `_globals.R` | Low | Removes global variable coupling |
| 2 | `database.R` | Medium | Centralizes SQL, improves security |
| 3 | `game_logic.R` | Low | Already mostly done in server_functions |
| 4 | `statistics.R` | Medium | Extract from server_functions |
| 5 | `ui_modals.R` | Low | Easy split from ui_functions |
| 6 | `game_state.R` | High | Major refactor of vals object |
| 7 | `server_main.R` | High | Extract from app.R |

---

## 11. Specific Refactoring Opportunities

### Quick Wins (Low Effort, High Impact)

1. Extract duplicate game completion logic into `finalize_game()` function
2. Move all `output$` definitions to a separate file
3. Create `_globals.R` with `rounds`, `casualty_rules`, `snappa_pal`
4. Extract all SQL queries to `database.R` module

### Medium Effort

1. Separate `score_check()` logic from UI rendering (data vs. presentation)
2. Create pure game logic functions (non-reactive) that take data as parameters
3. Organize reactives by dependency groups in `reactive_logic.R`
4. Create a "GameState" object to replace scattered `vals` properties

### Major Refactoring

1. Implement repository pattern for database access
2. Create a "Game" R6 object to encapsulate game state and logic
3. Separate concerns into feature-based modules
4. Implement dependency injection for testability
