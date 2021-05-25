rsconnect::deployApp(
  appName = "APP_NAME_HERE",
  appFiles = list.files(recursive = T) %>% 
    str_subset("(documentation|testing|analysis)/*", negate=T)
)
