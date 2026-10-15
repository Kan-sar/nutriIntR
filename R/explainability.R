llm_model_name <- function() {
  value <- Sys.getenv("NUTRIINT_LLM_MODEL", "deepseek-r1:1.5b")
  if (!nzchar(trimws(value))) "deepseek-r1:1.5b" else trimws(value)
}

llm_endpoint <- function() {
  sub("/+$", "", Sys.getenv("NUTRIINT_OLLAMA_URL", "http://127.0.0.1:11434"))
}

ollama_request <- function(path, payload = NULL, timeout_seconds = 120) {
  handle <- curl::new_handle()
  curl::handle_setheaders(handle, "Content-Type" = "application/json")
  curl::handle_setopt(handle, timeout_ms = timeout_seconds * 1000)
  if (!is.null(payload)) {
    body <- jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null", na = "null")
    curl::handle_setopt(handle, postfields = charToRaw(body))
  }
  response <- curl::curl_fetch_memory(paste0(llm_endpoint(), path), handle = handle)
  if (response$status_code < 200L || response$status_code >= 300L) {
    detail <- tryCatch(jsonlite::fromJSON(rawToChar(response$content))$error,
      error = function(e) rawToChar(response$content))
    if (is.null(detail) || !nzchar(trimws(detail))) detail <- "No error detail returned."
    stop("Ollama returned HTTP ", response$status_code, ": ", detail)
  }
  jsonlite::fromJSON(rawToChar(response$content), simplifyVector = FALSE)
}

llm_status <- function() {
  tryCatch({
    response <- ollama_request("/api/tags", timeout_seconds = 3)
    installed <- vapply(response$models, function(x) x$name, character(1))
    list(available = llm_model_name() %in% installed, model = llm_model_name(),
      endpoint = llm_endpoint(), installed = installed,
      message = if (llm_model_name() %in% installed) "Model is ready."
        else paste("Model is not installed. Run: ollama pull", llm_model_name()))
  }, error = function(e) list(available = FALSE, model = llm_model_name(),
    endpoint = llm_endpoint(), installed = character(),
    message = paste("Ollama is unavailable:", conditionMessage(e))))
}

explainability_payload <- function(result) {
  nutrient_matches <- Filter(function(x) x$status == "qualitative_evidence", result$interactions)
  medication_matches <- Filter(function(x) !x$status %in% c("unsupported_pair", "unsupported_form",
    "unsupported_route", "zero_dose"), result$medication_interactions)
  drug_matches <- Filter(function(x) x$status == "label_monitoring_interaction",
    result$drug_interactions)
  event_rows <- lapply(seq_len(nrow(result$events)), function(i) {
    event <- result$events[i, ]
    list(id = event$id, nutrient = event$nutrient_name, amount = event$amount,
      unit = event$unit, time_minutes = event$time_minutes,
      consumed_at = if (is.na(event$consumed_at)) NULL else event$consumed_at)
  })
  medication_rows <- if (!nrow(result$medications)) list() else
    lapply(seq_len(nrow(result$medications)), function(i) {
      item <- result$medications[i, ]
      list(id = item$id, medication = item$medication_name, route = item$route,
        time_minutes = item$time_minutes,
        consumed_at = if (is.na(item$consumed_at)) NULL else item$consumed_at)
    })
  nutrient_rows <- lapply(nutrient_matches, function(x) list(rule_id = x$rule_id,
    source = x$source_id, target = x$target_id, direction = x$direction,
    status = x$status, applicability = x$applicability,
    source_url = if (is.null(x$citation)) NULL else x$citation$url))
  medication_rows_found <- lapply(medication_matches, function(x) list(rule_id = x$rule_id,
    medication = x$medication_name, nutrient_event = x$nutrient_event_id,
    gap_minutes = x$nutrient_minus_medication_minutes, status = x$status,
    direction = x$direction, effect = x$effect, mechanism = x$mechanism,
    guidance = x$guidance, limitation = x$applicability,
    source_url = if (is.null(x$citation)) NULL else x$citation$url))
  drug_rows_found <- lapply(drug_matches, function(x) list(rule_id = x$rule_id,
    drug_a = x$drug_a_name, drug_b = x$drug_b_name, status = x$status,
    direction = x$direction, effect = x$effect, mechanism = x$mechanism,
    guidance = x$guidance, limitation = x$applicability,
    source_url = if (is.null(x$citation)) NULL else x$citation$url))
  list(
    scope = "Evidence explorer. No diagnosis, dose change, exact absorption estimate or individualized treatment advice.",
    events = event_rows, medications = medication_rows,
    nutrient_findings = nutrient_rows, medication_findings = medication_rows_found,
    drug_findings = drug_rows_found,
    unresolved_nutrient_pairs = length(result$interactions) - length(nutrient_matches),
    unresolved_medication_pairs = length(result$medication_interactions) - length(medication_matches),
    unresolved_drug_pairs = length(result$drug_interactions) - length(drug_matches),
    visualization_context = list(
      timeline = "Events are ordered by entered time; position is chronology and does not imply absorption kinetics.",
      interaction_map = "Directed solid arrows represent applicable nutrient evidence. Dashed arrows represent unresolved applicability. Node color identifies a nutrient; brightness and opacity encode recency only.",
      dose_reference = "Hollow phytate points are published study anchors, not patient predictions. Entered intake recency appears in a separate table.",
      medication_map = "Arrows point from nutrient events to medication events. Color distinguishes potential timing conflict, label-consistent separation, non-timing guidance, unresolved timing and chronic monitoring context. Missing arrows are catalog gaps.",
      medication_table = "Status distinguishes a potential timing conflict, label-consistent separation, non-timing guidance, chronic monitoring context and unsupported pairs.",
      drug_table = "Drug-drug rows are unordered selected pairs. A label-monitoring interaction is not a dose or timing recommendation; unsupported means uncataloged, not safe."),
    required_caveats = result$warnings)
}

strip_reasoning_trace <- function(text) {
  text <- gsub("(?is)<think>.*?</think>", "", text, perl = TRUE)
  trimws(text)
}

format_llm_markdown <- function(text) {
  text <- strip_reasoning_trace(text)
  text <- gsub("\r\n?", "\n", text)
  text <- gsub("(?m)^\\s*```(?:markdown|md|text)?\\s*$", "", text, perl = TRUE)
  headings <- c(
    "Key findings" = "Key findings",
    "What the visualizations show" = "What the visualizations show",
    "Unknowns and limitations" = "Unknowns and limitations",
    "Safety boundary" = "Safety boundary"
  )
  for (label in names(headings)) {
    pattern <- paste0("(?im)^\\s*(?:\\*\\*)?", label,
      "\\s*:?(?:\\*\\*)?\\s*:?\\s*$")
    text <- gsub(pattern, paste0("## ", headings[[label]]), text, perl = TRUE)
  }
  text <- sub("(?is)^\\s*(?:here(?:'s| is) (?:a |the )?(?:grounded )?(?:analysis|explanation|summary)[.:]?\\s*)",
    "", text, perl = TRUE)
  text <- gsub("\n[ \t]+\n", "\n\n", text)
  text <- gsub("\n{3,}", "\n\n", text)
  if (!grepl("(?m)^##\\s+", text, perl = TRUE)) text <- paste("## Explanation", text, sep = "\n\n")
  if (!grepl("(?im)^##\\s+Safety boundary\\s*$", text, perl = TRUE)) {
    text <- paste(text,
      "## Safety boundary\n\nThis is an evidence-exploration summary, not medical advice. Confirm medication changes with a pharmacist or prescriber.",
      sep = "\n\n")
  }
  trimws(text)
}

llm_generation_payload <- function(prompt) {
  list(model = llm_model_name(), prompt = prompt, stream = FALSE, think = FALSE,
    options = list(temperature = 0.1, num_predict = 700))
}

generate_llm_explanation <- function(result, timeout_seconds = 120) {
  status <- llm_status()
  if (!status$available) stop(status$message)
  evidence <- explainability_payload(result)
  prompt <- paste(
    "Explain this NutriInt-R analysis to a research user using only the supplied JSON evidence.",
    "Return concise Markdown with exactly these four level-2 headings in this order:",
    "## Key findings",
    "## What the visualizations show",
    "## Unknowns and limitations",
    "## Safety boundary",
    "Under each heading use short bullet points. Use at most two sentences per bullet.",
    "Start Key findings with the most important matched nutrient-nutrient, nutrient-drug or drug-drug finding.",
    "Explain what the timeline, interaction maps and dose-reference view do and do not show.",
    "Mention rule IDs and source URLs for every clinical interaction claim.",
    "Clearly separate potential timing conflicts from label-consistent separation, chronic monitoring relationships and unknown pairs.",
    "Use the supplied counts exactly. Unsupported or unresolved pairs are unknown, not resolved and not evidence of safety.",
    "The dose-reference view contains only phytate study anchors; never describe a drug as a dose-reference anchor.",
    "Do not diagnose, prescribe, recommend a dose, invent a threshold, infer an interaction from an unsupported pair or expose private chain-of-thought.",
    "In Safety boundary, direct medication changes to a pharmacist or prescriber.",
    "Do not add a title, preamble, table, code fence or conclusion outside those four sections.",
    "Evidence JSON:",
    as.character(jsonlite::toJSON(evidence, auto_unbox = TRUE, null = "null", na = "null")),
    sep = "\n\n")
  response <- ollama_request("/api/generate", llm_generation_payload(prompt), timeout_seconds)
  if (is.null(response$response) || !nzchar(trimws(response$response)))
    stop("The model returned an empty explanation.")
  list(text = format_llm_markdown(response$response), model = llm_model_name(),
    endpoint = llm_endpoint(), generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    grounded_rule_ids = unique(c(vapply(evidence$nutrient_findings, function(x) x$rule_id, character(1)),
      vapply(evidence$medication_findings, function(x) x$rule_id, character(1)),
      vapply(evidence$drug_findings, function(x) x$rule_id, character(1)))))
}
