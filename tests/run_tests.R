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
catalog <- nutrient_catalog()
check(nrow(catalog) == 477L && !anyDuplicated(catalog$id), "all 477 unique USDA definitions bundled")
check(unname(tools::md5sum(engine_file("nutrient.csv"))) == jsonlite::fromJSON("data-raw/usda-manifest.json")$csv_md5,
  "USDA snapshot matches source manifest checksum")
all_events <- data.frame(id = paste0("event_", catalog$id), nutrient = paste0("usda_", catalog$id),
  form = catalog$default_form, amount = 1, unit = catalog$unit_name, time_minutes = 0, meal_id = "meal")
for (rows in split(seq_len(nrow(all_events)), ceiling(seq_len(nrow(all_events)) / 100))) {
  all_normalized <- normalize_events(all_events[rows, ])
  expected <- all_events[rows, ]; expected <- expected[order(expected$id, method = "radix"), ]
  check(identical(all_normalized$usda_nutrient_id, as.integer(sub("usda_", "", expected$nutrient))),
    paste("USDA IDs and native units round trip, batch", ceiling(rows[1] / 100)))
  check(all(all_normalized$amount_canonical == 1), "all native-unit values preserved")
}
usda_event <- function(id, amount = 1, unit = NULL) {
  row <- catalog[catalog$id == id, ]
  data.frame(id = paste0("test_", id), nutrient = paste0("usda_", id), form = row$default_form,
    amount = amount, unit = if (is.null(unit)) row$unit_name else unit, time_minutes = 0, meal_id = "meal")
}
check(normalize_events(usda_event(1003, 2))$amount_mg == 2000, "protein g converted to mass correctly")
check(normalize_events(usda_event(1008, 4.184, "kJ"))$amount_canonical == 1, "kJ to kcal conversion")
check(normalize_events(usda_event(1062, 1, "kcal"))$amount_canonical == 4.184, "kcal to kJ conversion")
check(is.na(normalize_events(usda_event(1104, 800))$amount_mg), "IU never treated as mg")
check(fails(normalize_events(usda_event(1104, 1, "mg"))), "IU-to-mass inference rejected")
check(fails(normalize_events(usda_event(1008, 1, "g"))), "energy-to-mass conversion rejected")
bad <- usda_event(1008); bad$nutrient <- "Energy"
check(fails(normalize_events(bad)), "duplicate USDA names require IDs")
bad <- usda_event(1089); bad$nutrient <- "Iron, Fe"
check(normalize_events(bad)$usda_nutrient_id == 1089L, "exact USDA name resolution")
bad <- usda_event(1042)
check(normalize_events(bad)$nutrient != "phytate", "phytic acid distinct from phytate phosphorus")
for (id in catalog$id[!catalog$unit_family %in% c("mass", "energy")]) {
  check(is.na(normalize_events(usda_event(id))$amount_mg), paste("special unit retained without mass inference", id))
}
clock <- jsonlite::fromJSON("inst/extdata/example-clock.json", simplifyVector = FALSE)
clock_result <- analyze_interactions(clock$events, clock$context, medications = clock$medications)
check(clock_result$chronology$mode == "consumption_times", "clock chronology enabled")
check(identical(clock_result$events$time_minutes, c(0, 0, 330, 330)), "actual consumption times derive chronological gaps")
check(clock_result$events$consumed_at_local[1] == "08:00:00", "selected local time retained")
check(identical(result_json(clock_result), result_json(analyze_interactions(rev(clock$events), clock$context,
  medications = clock$medications))),
  "timestamp sorting independent of input order")
check(all(vapply(Filter(function(f) f$target_nutrient == "usda_1104", clock_result$interactions),
  function(f) f$status == "unsupported_pair", logical(1))), "new USDA pairs explicitly unknown")
check(is.null(jsonlite::fromJSON(result_json(clock_result), simplifyVector = FALSE)$events[[4]]$amount_mg),
  "non-mass amounts serialize to JSON null")
med_find <- function(r, medication_id, nutrient_id) Filter(function(x)
  x$medication_id == medication_id && x$nutrient_event_id == nutrient_id,
  r$medication_interactions)[[1]]
cipro_iron <- med_find(clock_result, "ciprofloxacin_morning", "iron_breakfast")
check(cipro_iron$rule_id == "ciprofloxacin_iron", "ciprofloxacin and iron rule matched")
check(cipro_iron$status == "potential_timing_conflict", "asymmetric ciprofloxacin timing window applied")
check(cipro_iron$nutrient_minus_medication_minutes == -30, "medication and nutrient clocks share one origin")
drug_find <- function(r, first_id, second_id) Filter(function(x)
  all(c(x$drug_a_id, x$drug_b_id) %in% c(first_id, second_id)), r$drug_interactions)[[1]]
cipro_warfarin <- drug_find(clock_result, "ciprofloxacin_morning", "warfarin_morning")
check(cipro_warfarin$rule_id == "ciprofloxacin_warfarin",
  "ciprofloxacin and warfarin drug rule matched")
check(cipro_warfarin$status == "label_monitoring_interaction",
  "drug-drug label monitoring status applied")
check(cipro_warfarin$time_gap_minutes == 15,
  "drug-drug chronology shares the scenario clock")
analgesic_medications <- append(clock$medications, list(list(id = "paracetamol_dose",
  medication = "paracetamol", route = "oral", consumed_at = "09:00")))
analgesic_result <- analyze_interactions(clock$events, clock$context,
  medications = analgesic_medications)
check(resolve_medication("paracetamol")$key == "acetaminophen",
  "paracetamol alias resolves to acetaminophen")
check(drug_find(analgesic_result, "paracetamol_dose", "warfarin_morning")$rule_id ==
  "acetaminophen_warfarin", "acetaminophen and warfarin rule matched")
potassium_event <- usda_event(1092, 500, "mg")
lisinopril <- list(list(id = "lisinopril_dose", medication = "lisinopril",
  route = "oral", time_minutes = 0))
lisinopril_result <- analyze_interactions(potassium_event, medications = lisinopril)
check(med_find(lisinopril_result, "lisinopril_dose", "test_1092")$rule_id ==
  "lisinopril_potassium", "lisinopril and potassium supplement rule matched")
separated_medication <- clock$medications
separated_medication[[1]]$consumed_at <- "14:30"
separated_result <- analyze_interactions(clock$events, clock$context, medications = separated_medication)
check(med_find(separated_result, "ciprofloxacin_morning", "iron_breakfast")$status == "separated_per_label",
  "label-consistent ciprofloxacin separation recognized")
levothyroxine <- list(list(id = "thyroid", medication = "Synthroid", route = "oral", consumed_at = "07:00"))
thyroid_result <- analyze_interactions(clock$events, clock$context, medications = levothyroxine)
check(med_find(thyroid_result, "thyroid", "iron_breakfast")$status == "potential_timing_conflict",
  "medication aliases and symmetric separation rule work")
nonoral <- levothyroxine; nonoral[[1]]$route <- "intravenous"
check(med_find(analyze_interactions(clock$events, clock$context, medications = nonoral),
  "thyroid", "iron_breakfast")$status == "unsupported_route", "non-oral route is not generalized")
check(nrow(medication_catalog()) == 16L && nrow(read_medication_evidence()) == 14L,
  "expanded medication catalog and label rules loaded")
check(nrow(read_drug_evidence()) == 8L, "expanded drug-drug label rules loaded")
payload <- explainability_payload(clock_result)
check(length(payload$medication_findings) == 1L &&
  payload$medication_findings[[1]]$rule_id == "ciprofloxacin_iron",
  "LLM payload contains only grounded medication matches")
check(length(payload$drug_findings) == 1L &&
  payload$drug_findings[[1]]$rule_id == "ciprofloxacin_warfarin",
  "LLM payload contains only grounded drug-drug matches")
check(strip_reasoning_trace("<think>private</think>Visible") == "Visible",
  "reasoning trace is removed from displayed model output")
formatted_llm <- format_llm_markdown("```markdown\n### **Key findings:**\n- Grounded result.\n## Safety boundary\nIgnore this.\n```")
check(grepl("^## Bottom line", formatted_llm) &&
  !grepl("Safety boundary", formatted_llm, fixed = TRUE) && !grepl("```", formatted_llm, fixed = TRUE),
  "LLM output is normalized into the requested report structure")
generation_payload <- llm_generation_payload("Explain the evidence.")
check(identical(generation_payload$think, FALSE) && identical(generation_payload$stream, FALSE),
  "Ollama generation disables private reasoning output")
check(parse_consumed_at("08:00") == parse_consumed_at("08:00:00"), "HH:MM and HH:MM:SS agree")
check(diff(parse_consumed_at(c("2026-10-14T23:50:00", "2026-10-15T00:10:00"))) == 1200,
  "dated local times support midnight crossings")
check(fails(parse_consumed_at("2026-02-30T12:00:00")), "invalid calendar day rejected")
check(fails(parse_consumed_at("25:00")), "invalid clock time rejected")
check(fails(parse_consumed_at("12:60")), "invalid minute rejected")
check(fails(parse_consumed_at(c("08:00", "2026-10-14T08:00"))), "mixed dated and undated clocks rejected")
mixed <- clock$events; mixed[[1]]$consumed_at <- NULL; mixed[[1]]$time_minutes <- 0
check(fails(analyze_interactions(mixed)), "mixed absolute and relative times rejected")
provided <- clock$events; provided[[1]]$time_minutes <- 999
check(identical(clock_result$events$time_minutes, analyze_interactions(provided)$events$time_minutes),
  "timestamps authoritative over stale offsets")
styles <- event_plot_styles(clock_result)
check(!anyDuplicated(nutrient_plot_colors(c(catalog$key, "phytate"))), "unique identity colors for every USDA definition and phytate extension")
check(identical(nutrient_plot_colors(c("iron", "vitamin_c")), rev(nutrient_plot_colors(c("vitamin_c", "iron")))),
  "nutrient colors stable across ordering and views")
check(styles$alpha[1] == .25 && styles$alpha[nrow(styles)] == 1, "oldest and newest intake opacity endpoints")
check(styles$alpha[1] == styles$alpha[2], "equal intake times share opacity")
same_time <- clock_result; same_time$events$time_minutes[] <- 0
check(all(event_plot_styles(same_time)$alpha == 1), "simultaneous scenario stays fully opaque")
repeated <- clock_result; repeated$events$nutrient[2] <- repeated$events$nutrient[1]
check(event_plot_styles(repeated)$color[1] == event_plot_styles(repeated)$color[2], "repeated nutrient retains identity color")
check(all(diff(styles$alpha) >= 0), "recency opacity increases chronologically")
check(styles$brightness[1] == .45 && styles$brightness[nrow(styles)] == 1,
  "oldest and latest brightness endpoints")
check(all(diff(styles$brightness) >= 0), "brightness increases with recency")
check(all(event_plot_styles(same_time)$brightness == 1), "simultaneous scenario uses full brightness")
check(identical(styles$display_color[nrow(styles)], styles$color[nrow(styles)]), "latest intake retains full identity color")
recency_table <- intake_recency_table(clock_result)
check(identical(recency_table$age_minutes, c(0, 0, 330, 330)), "recency table shows newest first and exact intake ages")
check(recency_table$consumed[1] == "13:30" && recency_table$opacity_percent[1] == 100 &&
  recency_table$brightness_percent[1] == 100, "table matches selected time and display scaling")
check(all(intake_recency_table(same_time)$age_minutes == 0), "equal-time table ages are zero")
cat("\n", checks, "checks passed.\n")
