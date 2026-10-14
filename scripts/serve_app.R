if (!requireNamespace("shiny", quietly = TRUE)) stop("Restore the project environment first.")
shiny::runApp("app", host = Sys.getenv("HOST", "127.0.0.1"), port = 3838, launch.browser = FALSE)
