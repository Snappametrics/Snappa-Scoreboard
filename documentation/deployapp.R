rsconnect::deployApp(
  appName = "APP_NAME_HERE",
  appFiles = list.files(recursive = T) %>% 
    str_subset("(database|r\\-lib|renv|documentation|testing|analysis)/*", negate=T)
)
