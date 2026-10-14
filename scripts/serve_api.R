if (!requireNamespace("plumber", quietly = TRUE)) stop("Run scripts/setup.R first.")
plumber::plumb("api/plumber.R")$run(host = "127.0.0.1", port = 8000)
