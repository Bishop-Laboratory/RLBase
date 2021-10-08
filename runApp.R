args <- commandArgs(trailingOnly = TRUE)

shiny::runApp("rlbase/", port = as.numeric(args[1]), launch.browser = FALSE, host = "0.0.0.0")
