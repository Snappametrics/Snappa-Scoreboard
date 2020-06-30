# Database connection -----------------------------------------------------

con <- dbConnect(RPostgres::Postgres(),
                 user = "username",
                 password = rstudioapi::askForPassword("connection password"),
                 host = "database.hostname",
                 port = 5432,
                 dbname = "Snappa Scoreboard"
)
