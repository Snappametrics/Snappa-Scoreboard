
# Libraries ---------------------------------------------------------------

library(DBI)
library(RPostgres)
library(pool)

# Load Environment vars ---------------------------------------------------

host = Sys.getenv("DB_HOST") # "DB_HOST_TEST"
user = Sys.getenv("DB_USER")
password = Sys.getenv("DB_PASSWORD")
dbname = Sys.getenv("DB_NAME")
port = Sys.getenv("DB_PORT")

# con = dbConnect(
#   RPostgres::Postgres(),
#   user = user,
#   password = password,
#   host = host,
#   port = port,
#   dbname = dbname,
#   bigint = "integer"
# )

con = dbPool(
  RPostgres::Postgres(),
  host = host,
  user = user,
  password = password,
  port = port,
  dbname = dbname,
  bigint = "integer"
)