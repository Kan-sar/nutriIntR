# No model download or live service is needed for these failure-path regressions.
app_env <- new.env(parent = globalenv())
sys.source("app/app.R", envir = app_env)
p <- jsonlite::fromJSON(app_env$example_text, simplifyVector = FALSE)
r <- app_env$analyze_interactions(p$events, p$context, medications = p$medications)
e <- app_env$explainability_payload(r)
findings <- app_env$llm_finding_rows(e)
gaps <- app_env$llm_gap_rows(e)
stopifnot(length(findings) == 2L)
fails <- function(expr) inherits(tryCatch(force(expr), error = identity), "error")
stopifnot(length(gaps) == length(e$unaccounted_pairs), all(nzchar(vapply(gaps, `[[`, character(1), "gap_id"))))
priorities <- list(lead_finding_id = findings[[2]]$finding_id, lead_gap_id = gaps[[2]]$gap_id)
stopifnot(identical(app_env$validate_llm_priorities(priorities, findings, gaps), priorities))
bad_priorities <- priorities; bad_priorities$lead_gap_id <- "invented_gap"
stopifnot(fails(app_env$validate_llm_priorities(bad_priorities, findings, gaps)))
bad_priorities <- priorities; bad_priorities$lead_finding_id <- NULL
stopifnot(fails(app_env$validate_llm_priorities(bad_priorities, findings, gaps)))
report <- app_env$format_grounded_explanation(e, findings, priorities)
lead_pair <- if (identical(findings[[2]]$category, "drug_findings"))
  paste(findings[[2]]$drug_a, "and", findings[[2]]$drug_b) else findings[[2]]$scenario_pair
med_gap_evidence <- e
med_gap_evidence$unaccounted_pairs[[length(med_gap_evidence$unaccounted_pairs) + 1L]] <- list(
  category = "medication–medication", pair = "A and B", status = "unsupported_pair", reason = "No matching rule.")
med_gap_report <- app_env$format_grounded_explanation(med_gap_evidence, findings, priorities)
stopifnot(all(vapply(findings, function(f) grepl(f$source_url, report, fixed = TRUE), logical(1))),
  grepl("fixed node positions", e$visualization_context$interaction_map, fixed = TRUE),
  grepl("AI's take", report, fixed = TRUE),
  grepl("clearest supported interaction", report, fixed = TRUE),
  grepl(paste0("My read: **", lead_pair), report, fixed = TRUE),
  grepl(paste("I would investigate", gaps[[2]]$pair, "first."), report, fixed = TRUE),
  grepl("Nutrient + medication:", report, fixed = TRUE),
  grepl("Medication + medication: A and B", med_gap_report, fixed = TRUE),
  all(vapply(gaps, function(g) grepl(g$pair, report, fixed = TRUE), logical(1))))

mode <- "ok"; requests <- 0L
app_env$ollama_request <- function(path, payload = NULL, timeout_seconds = 120) {
  if (path == "/api/tags") {
    if (mode == "unavailable") stop("Ollama is unavailable")
    return(list(models = if (mode == "missing") list() else list(list(name = app_env$llm_model_name()))))
  }
  requests <<- requests + 1L
  if (mode == "empty") return(list(response = ""))
  if (mode == "truncated") return(list(response = "{}", done_reason = "length"))
  if (mode == "malformed") return(list(response = "not json", done = TRUE))
  if (mode == "timeout") stop("Request timed out")
  selected <- list()
  if ("lead_finding_id" %in% names(payload$format$properties))
    selected$lead_finding_id <- payload$format$properties$lead_finding_id$enum[[1]]
  if ("lead_gap_id" %in% names(payload$format$properties))
    selected$lead_gap_id <- payload$format$properties$lead_gap_id$enum[[1]]
  list(response = as.character(jsonlite::toJSON(selected, auto_unbox = TRUE)), done = TRUE)
}
generated <- app_env$generate_llm_explanation(r)
stopifnot(requests == 1L, grepl("AI's take", generated$text, fixed = TRUE),
  identical(generated$grounded_rule_ids, unique(vapply(findings, function(f) f$rule_id, character(1)))))
for (failure in c("empty", "truncated", "malformed", "timeout", "unavailable", "missing")) {
  mode <- failure
  stopifnot(fails(app_env$generate_llm_explanation(r)))
}
mode <- "ok"; requests <- 0L
solo <- app_env$analyze_interactions(p$events[1], p$context)
stopifnot(app_env$generate_llm_explanation(solo)$generation_mode == "evidence_only", requests == 0L)

coverage_only <- r
coverage_only$interactions <- lapply(coverage_only$interactions, function(x) { x$status <- "uncataloged"; x })
coverage_only$medication_interactions <- list()
coverage_only$drug_interactions <- list()
requests <- 0L
coverage_generated <- app_env$generate_llm_explanation(coverage_only)
stopifnot(coverage_generated$generation_mode == "ai_coverage_only", requests == 1L,
  grepl("I would investigate", coverage_generated$text, fixed = TRUE))

shiny::testServer(app_env$server, {
  session$setInputs(scenario = app_env$example_text, catalog_search = "", nutrient = "", drug = "", pair_type = "all")
  session$flushReact(); session$setInputs(analyze = 1, explain = 1)
  # Analysis runs after input synchronization; request explanation after it completes.
  session$setInputs(explain = 2)
  stopifnot(!is.null(explanation()), is.null(explanation_error()), any(grepl("llm-report", output$llm_explanation)))
  mode <<- "timeout"; session$setInputs(explain = 3)
  stopifnot(is.null(explanation()), grepl("timed out", explanation_error()), any(grepl("alert", output$llm_error)))
  mode <<- "ok"; session$setInputs(explain = 4)
  stopifnot(!is.null(explanation()), is.null(explanation_error()))
  session$setInputs(scenario = app_env$empty_text)
  stopifnot(is.null(explanation()), is.null(current()))
  mode <<- "unavailable"; session$setInputs(refresh_llm = 1)
  stopifnot(any(grepl("Local AI unavailable", output$llm_status)))
  mode <<- "ok"; session$setInputs(refresh_llm = 2)
  stopifnot(any(grepl("Local AI ready", output$llm_status)))
})
cat("PASS: AI-prioritized supported and unanswered interactions, complete gap coverage, sources, invalid/truncated/empty responses, timeout, retry, status refresh and stale explanation clearing\n")
