options(timeout = max(300, getOption("timeout")))
if (!requireNamespace("renv", quietly = TRUE))
  utils::install.packages("renv", repos = "https://cloud.r-project.org")
renv::restore(prompt = FALSE)
