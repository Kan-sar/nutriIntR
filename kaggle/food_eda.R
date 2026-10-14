# R notebook-ready script; attach the dataset in Kaggle first.
files <- list.files("/kaggle/input", pattern = "^nutrition\\.csv$", recursive = TRUE, full.names = TRUE)
stopifnot(length(files) == 1L)
foods <- read.csv(files, stringsAsFactors = FALSE, check.names = FALSE)
cat(nrow(foods), "foods;", ncol(foods), "columns\n")
print(head(foods[c("name", "serving_size", "iron", "calcium", "vitamin_c")]))
cat("These are food composition observations, not human absorption outcomes.\n")
