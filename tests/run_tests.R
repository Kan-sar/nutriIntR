# Small deterministic test runner: no extra test framework required.
source("scripts/load_project.R")
checks <- 0L
check <- function(condition, label) {
  if (!isTRUE(condition)) stop("FAIL: ", label)
  checks <<- checks + 1L
  cat("PASS:", label, "\n")
}
fails <- function(expr) inherits(tryCatch(force(expr), error = identity), "error")
input <- jsonlite::fromJSON("inst/extdata/example.json", simplifyVector = FALSE)
events <- normalize_events(input$events)
result <- analyze_interactions(events)
find <- function(r, s, t) Filter(function(x) x$source_id == s && x$target_id == t, r$interactions)[[1]]
check(nrow(events) == 4L, "example normalization")
check(length(result$interactions) == 12L, "all directed non-self pairs are accounted for")
check(find(result, "vitamin_c_lunch", "iron_lunch")$direction == "enhances", "vitamin C direction")
check(find(result, "phytate_lunch", "iron_lunch")$study_reference$relative_absorption == .36, "phytate anchor")
check(find(result, "calcium_evening", "iron_lunch")$status == "timing_unresolved", "later calcium not inferred from earlier-calcium study")
check(is.null(result$targets[[1]]$adjusted_bioavailability), "no invented absolute prediction")
check(identical(result_json(result), result_json(analyze_interactions(events[4:1, ]))), "input row order does not change result")
converted <- events; converted$unit[1] <- "g"; converted$amount[1] <- converted$amount[1] / 1000
check(identical(events$amount_mg, normalize_events(converted)$amount_mg), "g/mg equivalence")
converted <- events; converted$unit[1] <- "mcg"; converted$amount[1] <- converted$amount[1] * 1000
check(identical(events$amount_mg, normalize_events(converted)$amount_mg), "mcg/mg equivalence")
bad <- events; bad$amount[1] <- -1
check(fails(analyze_interactions(bad)), "negative amount rejected")
bad <- events; bad$amount[1] <- Inf
check(fails(analyze_interactions(bad)), "nonfinite amount rejected")
bad <- events; bad$id[1] <- bad$id[2]
check(fails(analyze_interactions(bad)), "duplicate IDs rejected")
bad <- events; bad$unit[1] <- "iu"
check(fails(analyze_interactions(bad)), "ambiguous IU rejected")
bad <- events; bad$form[bad$nutrient == "iron"] <- "heme"
check(find(analyze_interactions(bad), "vitamin_c_lunch", "iron_lunch")$status == "unsupported_form", "heme iron is not treated as non-heme")
check(find(analyze_interactions(events, list(population = "child", meal_state = "fed")), "vitamin_c_lunch", "iron_lunch")$status == "unsupported_context", "population context enforced")
check(find(analyze_interactions(events, list(population = "adult", meal_state = "fasted")), "vitamin_c_lunch", "iron_lunch")$status == "unsupported_context", "meal context enforced")
bad <- events; bad$time_minutes[bad$nutrient == "vitamin_c"] <- 5
check(find(analyze_interactions(bad), "vitamin_c_lunch", "iron_lunch")$status == "within_meal_timing_unresolved", "within-meal chronology uncertainty preserved")
bad <- events; bad$amount[bad$nutrient == "vitamin_c"] <- 0
check(find(analyze_interactions(bad), "vitamin_c_lunch", "iron_lunch")$status == "zero_dose", "zero dose does not activate interaction")
check(is.null(phytate_reference(10)$relative_absorption), "interpolation is opt-in")
check(phytate_reference(10, TRUE)$relative_absorption < .82 && phytate_reference(10, TRUE)$relative_absorption > .36, "interpolation bounded by neighboring anchors")
check(is.null(phytate_reference(500, TRUE)$relative_absorption), "no dose extrapolation")
for (minutes in c(120, 240)) {
  timing <- events[events$nutrient %in% c("calcium", "iron"), ]
  timing$time_minutes[timing$nutrient == "calcium"] <- 0
  timing$time_minutes[timing$nutrient == "iron"] <- minutes
  check(find(analyze_interactions(timing), "calcium_evening", "iron_lunch")$direction == "no_detected_effect", paste("calcium timing study", minutes))
}
timing$amount[timing$nutrient == "calcium"] <- 500
check(find(analyze_interactions(timing), "calcium_evening", "iron_lunch")$status == "timing_unresolved", "timing study dose not generalized")
check(jsonlite::validate(result_json(result)), "valid JSON serialization")
roundtrip <- jsonlite::fromJSON(result_json(result), simplifyVector = FALSE)
check(is.null(roundtrip$targets[[1]]$adjusted_bioavailability), "unknown remains JSON null")
check(length(analyze_interactions(events[1, ])$interactions) == 0L, "single event supported")
check(fails(analyze_interactions(events, interpolate = NA)), "invalid interpolation flag rejected")
check(fails(analyze_interactions(events, context = list())), "missing context rejected")
check(fails(analyze_interactions(events[rep(1, 101), ])), "event count capped")
cat("\n", checks, "checks passed.\n")
