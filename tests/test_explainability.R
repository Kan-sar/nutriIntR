# No model download or live service is needed for these failure-path regressions.
app_env <- new.env(parent = globalenv())
sys.source("app/app.R", envir = app_env)
p <- jsonlite::fromJSON(app_env$example_text, simplifyVector = FALSE)
r <- app_env$analyze_interactions(p$events, p$context, medications = p$medications)
e <- app_env$explainability_payload(r)
findings <- app_env$llm_finding_rows(e)
stopifnot(length(findings) == 2L)
answer <- list(findings = lapply(findings, function(f) list(finding_id = f$finding_id,
  status = f$status, explanation = app_env$llm_explanation_choices(f)[1])))
fails <- function(expr) inherits(tryCatch(force(expr), error = identity), "error")
stopifnot(length(app_env$validate_llm_findings(answer, findings)) == 2L)
bad <- answer; bad$findings <- bad$findings[1]
stopifnot(fails(app_env$validate_llm_findings(bad, findings)))
bad <- answer; bad$findings[[2]] <- bad$findings[[1]]
stopifnot(fails(app_env$validate_llm_findings(bad, findings)))
bad <- answer; bad$findings[[1]]$status <- "separated_per_label"
stopifnot(fails(app_env$validate_llm_findings(bad, findings)))
bad <- answer; bad$findings[[1]]$explanation <- answer$findings[[2]]$explanation
stopifnot(fails(app_env$validate_llm_findings(bad, findings)))
bad <- answer; bad$findings[[1]]$explanation <- "Invented medical advice."
stopifnot(fails(app_env$validate_llm_findings(bad, findings)))
report <- app_env$format_grounded_explanation(e, findings, answer$findings)
stopifnot(grepl("Iron-containing products", report, fixed = TRUE), grepl("Scope:", report, fixed = TRUE),
  all(vapply(findings, function(f) grepl(f$source_url, report, fixed = TRUE), logical(1))),
  grepl("fixed node positions", e$visualization_context$interaction_map, fixed = TRUE))

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
  id <- payload$format$properties$findings$items$properties$finding_id$enum[[1]]
  f <- findings[[match(id, vapply(findings, function(x) x$finding_id, character(1)))]]
  list(response = as.character(jsonlite::toJSON(list(findings = list(list(finding_id = id,
    status = f$status, explanation = app_env$llm_explanation_choices(f)[1]))), auto_unbox = TRUE)), done = TRUE)
}
generated <- app_env$generate_llm_explanation(r)
stopifnot(requests == 2L, identical(generated$grounded_rule_ids, unique(vapply(findings, function(f) f$rule_id, character(1)))))
for (failure in c("empty", "truncated", "malformed", "timeout", "unavailable", "missing")) {
  mode <- failure
  stopifnot(fails(app_env$generate_llm_explanation(r)))
}
mode <- "ok"; requests <- 0L
solo <- app_env$analyze_interactions(p$events[1], p$context)
stopifnot(app_env$generate_llm_explanation(solo)$generation_mode == "evidence_only", requests == 0L)

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
cat("PASS: grounded coverage, no cross-pair substitutions, statuses, sources, text integrity, missing model, invalid/truncated/empty responses, timeout, retry, status refresh and stale explanation clearing\n")
