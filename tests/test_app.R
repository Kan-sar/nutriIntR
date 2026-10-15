# Server-level checks for the scenario draft workflow (no browser required).
app_env <- new.env(parent = globalenv())
sys.source("app/app.R", envir = app_env)
shiny::testServer(app_env$server, {
  session$setInputs(scenario = app_env$example_text, catalog_search = "", nutrient = "iron")
  session$flushReact()
  stopifnot(nrow(current()$events) == 4L, nrow(current()$medications) == 2L,
    length(current()$medication_interactions) == 8L,
    length(current()$drug_interactions) == 1L, is.null(error()))
  session$setInputs(timing_mode = "clock", consumed_time = "09:45", amount = 6,
    unit = "mg", form = "unspecified", meal = "lunch", add = 1)
  session$flushReact()
  # Browser applies updateTextAreaInput; simulate its round trip here.
  draft <- jsonlite::fromJSON(app_env$example_text, simplifyVector = FALSE)
  draft$events <- append(draft$events, list(list(id = "event_5", nutrient = "iron",
    form = "unspecified", amount = 6, unit = "mg", meal_id = "lunch", consumed_at = "09:45")))
  session$setInputs(scenario = as.character(app_env$result_json(draft)))
  stopifnot(any(grepl("Draft changed", output$status)), nrow(current()$events) == 4L)
  session$setInputs(analyze = 1)
  stopifnot(nrow(current()$events) == 5L, nrow(current()$medications) == 2L,
    any(grepl("Analysis ready", output$status)),
    any(grepl("09:45", output$dose_recency)), grepl("5", output$draft_summary),
    any(grepl("ciprofloxacin", output$medication_findings, ignore.case = TRUE)),
    any(grepl("warfarin", output$drug_findings, ignore.case = TRUE)))
  session$setInputs(scenario = "{invalid", analyze = 2)
  stopifnot(is.null(current()), !is.null(error()), any(grepl("Check scenario", output$status)))
  session$setInputs(reset = 1)
  stopifnot(nrow(current()$events) == 4L, nrow(current()$medications) == 2L, is.null(error()))
  session$setInputs(catalog_search = "1089")
  stopifnot(nrow(catalog_filtered()) == 1L)
})
cat("PASS: dashboard nutrient, drug, pair-analysis, recency, invalid-input, reset and catalog workflows\n")
