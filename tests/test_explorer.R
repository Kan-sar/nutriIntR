# Interaction adapter and Shiny lifecycle regressions for the linked workspace.
app_env <- new.env(parent = globalenv())
sys.source("app/app.R", envir = app_env)
payload <- jsonlite::fromJSON(app_env$example_text, simplifyVector = FALSE)
result <- app_env$analyze_interactions(payload$events, payload$context, medications = payload$medications)
n <- app_env$explorer_nodes(result); f <- app_env$explorer_findings(result)
stopifnot(nrow(n) == 6L, !anyDuplicated(n$id), nrow(f) == 21L,
  all(f$source %in% n$id), all(f$target %in% n$id), !anyDuplicated(f$id))
# Two intakes of the same nutrient remain independently selectable.
repeated <- payload; repeated$events[[2]] <- repeated$events[[1]]; repeated$events[[2]]$id <- "iron_again"
repeated$events[[2]]$consumed_at <- "09:00"
r <- app_env$analyze_interactions(repeated$events, repeated$context, medications = repeated$medications)
rn <- app_env$explorer_nodes(r)
stopifnot(sum(grepl("Iron", rn$name)) == 2L, !anyDuplicated(rn$id), !anyDuplicated(rn$label))
# Medication and nutrient collections may have the same raw ID: namespace them.
collision <- payload; collision$medications[[1]]$id <- collision$events[[1]]$id
cr <- app_env$analyze_interactions(collision$events, collision$context, medications = collision$medications)
stopifnot(!anyDuplicated(app_env$explorer_nodes(cr)$id))
network <- app_env$explorer_relationship(n, f)
matrix <- app_env$explorer_relationship(n, f, matrix = TRUE)
stopifnot(length(network$x$opts$series[[1]]$links) == nrow(f),
  length(matrix$x$opts$series[[1]]$data) == nrow(f) + sum(f$type == "dd"),
  identical(network$x$opts$series[[1]]$links[[which(f$type == "dd")]]$symbol, list("none", "none")))
stopifnot(length(app_env$explorer_timeline(n)$x$opts$series[[1]]$data) == 6L)
shiny::testServer(app_env$server, {
  session$setInputs(scenario = app_env$empty_text, catalog_search = "", nutrient = "", drug = "", pair_type = "all")
  session$flushReact()
  # Same flush: analysis must use the latest JSON, not the previous reactive draft.
  session$setInputs(scenario = app_env$example_text, analyze = 1)
  stopifnot(!is.null(current()), nrow(all_findings()) == 21L)
  token <- nodes()$id[1]
  session$setInputs(timeline_selection = token)
  stopifnot(identical(focused_intake(), token),
    all(filtered_findings()$source == token | filtered_findings()$target == token))
  id <- filtered_findings()$id[1]
  session$setInputs(relationship_selection = id)
  stopifnot(identical(selected_finding(), id), any(grepl("Applicability", output$finding_detail)))
  session$setInputs(pair_type = "dd")
  stopifnot(is.null(selected_finding()), all(filtered_findings()$type == "dd"))
  session$setInputs(clear_focus = 1)
  stopifnot(nrow(filtered_findings()) == 1L)
  session$setInputs(choose_finding = filtered_findings()$id[1])
  stopifnot(!is.null(selected_finding()))
  # New row actions must open the correct editor, preserve ID, and clear results.
  session$setInputs(edit_intake = "events:iron_breakfast")
  stopifnot(editing()$id == "iron_breakfast")
  session$setInputs(edit_nutrient = "iron", edit_form = "non_heme_food", edit_amount = 10,
    edit_unit = "mg", edit_meal = "breakfast", edit_date = "", edit_time = "08:00", save_item = 1)
  stopifnot(is.null(current()), is.null(selected_finding()), identical(focused_intake(), ""),
    draft_payload()$events[[1]]$id == "iron_breakfast", draft_payload()$events[[1]]$amount == 10)
  session$setInputs(remove_intake = "medications:warfarin_morning")
  stopifnot(length(draft_payload()$medications) == 1L)
  session$setInputs(reset = 1)
  stopifnot(is.null(analyzed_text()), is.null(current()), length(draft_entries()) == 0L)
})
cat("PASS: linked views, typed IDs, repeated intakes, matrix direction, rapid analysis, row actions and stale selection resets\n")
shiny::testServer(app_env$server, {
  session$setInputs(scenario = app_env$empty_text, catalog_search = "", nutrient = "iron", drug = "",
    form = "non_heme_food", amount = 6, unit = "mg", meal = "", timing_mode = "clock", consumed_time = "08:00")
  session$flushReact()
  session$setInputs(add = 1)
  session$setInputs(add = 2)
  stopifnot(length(draft_payload()$events) == 2L,
    draft_payload()$events[[1]]$meal_id != draft_payload()$events[[2]]$meal_id)
})
cat("PASS: optional meal labels never invent a shared meal\n")
