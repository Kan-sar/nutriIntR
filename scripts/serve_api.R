if (!requireNamespace("plumber", quietly = TRUE)) stop("Run scripts/setup.R first.")
plumber::plumb("api/plumber.R")$run(host = Sys.getenv("HOST", "127.0.0.1"), port = 8000)
