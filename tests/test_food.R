source("scripts/load_project.R")
catalog <- nutrient_catalog()
labels <- nutrient_display_names(catalog)
stopifnot(length(labels) == 477L, !anyDuplicated(labels), labels[catalog$key == "iron"] == "Iron")
stopifnot(isTRUE(all.equal(parse_food_amount(c("1 g", "500 mg", "25 mcg", "0", "bad", "3 IU"), "mg"),
  c(1000, 500, .025, 0, NA_real_, NA_real_))))
stopifnot(identical(parse_food_amount(c("8 IU", "8 mg", "0"), "iu"), c(8, NA_real_, 0)))
stopifnot(identical(parse_food_amount(c("12", "unknown"), "kcal", bare = TRUE), c(12, NA_real_)))
payload <- jsonlite::fromJSON("inst/extdata/example-clock.json", simplifyVector = FALSE)
events <- normalize_events(payload$events)
raw <- data.frame(serving_size = rep("100 g", 4), vitamin_c = c("1 mg", "2 mg", "3 mg", "4 mg"),
  irom = c("4 mg", "3 mg", "2 mg", "1 mg"), protein = c("1 g", "2 g", "3 g", "4 g"),
  vitamin_a = c("1 IU", "2 IU", "3 IU", "4 IU"))
data <- selected_food_correlations(events, raw)
stopifnot(length(data$labels) == 4L, nrow(data$pairs) == 6L,
  data$correlations["Iron", "Vitamin C"] == -1, all(data$pair_counts == 4L))
repeated <- selected_food_correlations(rbind(events, events[1, ]), raw)
stopifnot(identical(data$correlations, repeated$correlations))
raw$irom[1] <- "missing"
stopifnot(selected_food_correlations(events, raw)$pair_counts["Iron", "Vitamin C"] == 3L)
raw$irom <- "0"
stopifnot(is.na(selected_food_correlations(events, raw)$correlations["Iron", "Vitamin C"]))
unknown <- events[1, ]; unknown$usda_nutrient_id <- 1042L; unknown$nutrient_name <- "Phytic acid"
absent <- selected_food_correlations(unknown, raw)
stopifnot(length(absent$labels) == 0L, absent$unavailable == "Phytic acid")
stopifnot(length(selected_food_correlations(events[1, ], raw)$labels) == 1L)
raw$serving_size[1] <- "1 cup"
stopifnot(selected_food_correlations(events, raw)$rows == 3L)

# Optional real-data crosswalk integration; CI need not download the dataset.
if (file.exists("data-raw/kaggle-manifest.json")) {
  manifest <- jsonlite::fromJSON("data-raw/kaggle-manifest.json")
  if (file.exists(manifest$csv_path)) {
    raw <- utils::read.csv(manifest$csv_path, check.names = FALSE)
    crosswalk <- food_nutrient_columns()
    stopifnot(all(unname(crosswalk) %in% names(raw)), all(as.integer(names(crosswalk)) %in% catalog$id))
    data <- selected_food_correlations(events, raw)
    stopifnot(length(data$labels) == 4L, data$rows == 8789L, all(data$pair_counts > 0L))
  }
}
cat("PASS: nutrient labels, unit parsing, selected-only correlations, duplicates, missing data, constant columns and serving basis\n")
