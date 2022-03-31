rsconnect::deployApp(
  appName = "SnappaScoreboard2",
  appFiles = stringr::str_subset(list.files(recursive = T), 
                        "(documentation|testing|analysis)/*", negate=T)
)
