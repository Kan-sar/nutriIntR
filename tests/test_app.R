# Server-level checks for empty startup and nutrient/medication editing.
app_env <- new.env(parent = globalenv())
sys.source("app/app.R", envir = app_env)
shiny::testServer(app_env$server, {
  session$setInputs(scenario = app_env$empty_text, catalog_search = "", nutrient = "", drug = "")
  session$flushReact()
  stopifnot(is.null(current()), is.null(error()), length(draft_entries()) == 0L,
    grepl("0 nutrients / 0 drugs", output$draft_summary),
    any(grepl("No intakes yet", output$draft_manager)))
  session$setInputs(analyze = 1)
  stopifnot(is.null(current()), is.null(error()))
  session$setInputs(drug = "warfarin", medication_route = "oral", timing_mode = "clock",
    medication_time = "08:30", add_medication = 1)
  stopifnot(length(draft_payload()$medications) == 1L, length(draft_payload()$events) == 0L)
  session$setInputs(nutrient = "iron", form = "non_heme_food", amount = 6,
    unit = "mg", meal = "breakfast", consumed_time = "08:00", add = 1)
  stopifnot(length(draft_payload()$events) == 1L, length(draft_entries()) == 2L)
  session$setInputs(analyze = 2)
  stopifnot(nrow(current()$events) == 1L, nrow(current()$medications) == 1L)
  session$setInputs(draft_item = "events:event_1", edit_item = 1)
  session$setInputs(edit_nutrient = "calcium", edit_form = "food", edit_amount = 100,
    edit_unit = "mg", edit_meal = "lunch", edit_date = "", edit_time = "12:15", save_item = 1)
  item <- draft_payload()$events[[1]]
  stopifnot(item$id == "event_1", item$nutrient == "calcium", item$amount == 100,
    item$consumed_at == "12:15", is.null(current()))
  session$setInputs(analyze = 3)
  stopifnot(current()$events$nutrient == "calcium")
  session$setInputs(draft_item = "medications:medication_1", edit_item = 2)
  session$setInputs(edit_drug = "ciprofloxacin", edit_route = "oral", edit_date = "",
    edit_time = "13:30", save_item = 2)
  stopifnot(draft_payload()$medications[[1]]$medication == "ciprofloxacin",
    draft_payload()$medications[[1]]$consumed_at == "13:30", is.null(current()))
  session$setInputs(analyze = 4)
  stopifnot(current()$medications$medication_key == "ciprofloxacin")
  # Invalid edits must not mutate the draft.
  session$setInputs(draft_item = "events:event_1", edit_item = 3)
  previous <- draft_text()
  session$setInputs(edit_nutrient = "calcium", edit_form = "food", edit_amount = -1,
    edit_unit = "mg", edit_meal = "lunch", edit_date = "", edit_time = "12:15", save_item = 3)
  stopifnot(identical(previous, draft_text()))
  # A modal cannot overwrite an intake changed in raw JSON meanwhile.
  altered <- draft_payload(); altered$events[[1]]$amount <- 200
  session$setInputs(scenario = as.character(app_env$result_json(altered)), edit_amount = 300, save_item = 4)
  stopifnot(draft_payload()$events[[1]]$amount == 200)
  session$setInputs(draft_item = "events:event_1", remove_item = 1)
  stopifnot(length(draft_payload()$events) == 0L, length(draft_payload()$medications) == 1L,
    is.null(current()))
  session$setInputs(draft_item = "medications:medication_1", remove_item = 2)
  stopifnot(length(draft_entries()) == 0L, is.null(current()))
  session$setInputs(scenario = app_env$example_text, analyze = 5)
  stopifnot(nrow(current()$events) == 4L, nrow(current()$medications) == 2L,
    length(current()$medication_interactions) == 8L, length(current()$drug_interactions) == 1L)
  session$setInputs(scenario = "{invalid", analyze = 6)
  stopifnot(is.null(current()), !is.null(error()), any(grepl("Check scenario", output$status)))
  session$setInputs(reset = 1)
  stopifnot(length(draft_entries()) == 0L, is.null(current()), is.null(error()))
  session$setInputs(catalog_search = "1089")
  stopifnot(nrow(catalog_filtered()) == 1L)
})
shiny::testServer(app_env$server, {
  session$setInputs(scenario = app_env$empty_text, timing_mode = "offset", drug = "warfarin",
    medication_route = "oral", medication_minute = 30, add_medication = 1)
  stopifnot(draft_payload()$medications[[1]]$time_minutes == 30)
  session$setInputs(nutrient = "iron", form = "non_heme_food", amount = 5, unit = "mg",
    meal = "breakfast", minute = 0, add = 1)
  session$setInputs(draft_item = "events:event_1", edit_item = 1,
    edit_nutrient = "iron", edit_form = "non_heme_food", edit_amount = 10,
    edit_unit = "mg", edit_meal = "breakfast", edit_minute = 15, save_item = 1)
  stopifnot(draft_payload()$events[[1]]$time_minutes == 15)
})
cat("PASS: empty startup, medication-first draft, nutrient/drug edits, removal, invalid/stale edits, reset and offset timing\n")
