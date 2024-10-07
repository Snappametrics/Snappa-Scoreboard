rsconnect::deployApp(
  appName = "SnappaScoreboardBeta",
  appFiles = list.files(recursive = T) |> 
    stringr::str_subset("(database|r\\-lib|renv|documentation|testing|analysis)/*", negate=T)
)
