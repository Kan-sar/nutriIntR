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
    status = x$status, order = x$order, minutes_between = abs(x$source_minus_target_minutes),
    same_meal = x$same_meal, source_amount = x$source_amount, source_unit = x$source_unit,
    target_amount = x$target_amount, target_unit = x$target_unit,
    study_reference = x$study_reference, applicability = x$applicability,
    citation_design = if (is.null(x$citation)) NULL else x$citation$design,
    citation_population = if (is.null(x$citation)) NULL else x$citation$population,
    evidence_grade = if (is.null(x$citation)) NULL else x$citation$evidence_grade,
    source_url = if (is.null(x$citation)) NULL else x$citation$url))
  medication_rows_found <- lapply(medication_matches, function(x) list(rule_id = x$rule_id,
    medication = x$medication_name, medication_event = x$medication_id, route = x$route,
    nutrient_event = x$nutrient_event_id,
    gap_minutes = x$nutrient_minus_medication_minutes, status = x$status,
    direction = x$direction, effect = x$effect, mechanism = x$mechanism,
    guidance = x$guidance, limitation = x$applicability,
    source_url = if (is.null(x$citation)) NULL else x$citation$url))
  drug_rows_found <- lapply(drug_matches, function(x) list(rule_id = x$rule_id,
    drug_a = x$drug_a_name, drug_a_event = x$drug_a_id,
    drug_b = x$drug_b_name, drug_b_event = x$drug_b_id, time_gap_minutes = x$time_gap_minutes,
    status = x$status,
    direction = x$direction, effect = x$effect, mechanism = x$mechanism,
    guidance = x$guidance, limitation = x$applicability,
    source_url = if (is.null(x$citation)) NULL else x$citation$url))
  gap_rows <- list()
  add_gaps <- function(items, category, pair, covered) {
    for (item in items) if (!covered(item)) gap_rows[[length(gap_rows) + 1L]] <<- list(
      category = category, pair = pair(item), status = item$status,
      reason = item$applicability)
  }
  add_gaps(result$interactions, "nutrient–nutrient",
    function(x) {
      source <- result$events$nutrient_name[match(x$source_id, result$events$id)]
      target <- result$events$nutrient_name[match(x$target_id, result$events$id)]
      paste(source, "and", target)
    },
    function(x) x$status == "qualitative_evidence")
  add_gaps(result$medication_interactions, "nutrient–medication",
    function(x) paste(x$nutrient_name, "and", x$medication_name),
    function(x) !x$status %in% c("unsupported_pair", "unsupported_form", "unsupported_route", "zero_dose"))
  add_gaps(result$drug_interactions, "medication–medication",
    function(x) paste(x$drug_a_name, "and", x$drug_b_name),
    function(x) x$status == "label_monitoring_interaction")
  list(
    scope = "Evidence explorer. No diagnosis, dose change, exact absorption estimate or individualized treatment advice.",
    events = event_rows, medications = medication_rows,
    nutrient_findings = nutrient_rows, medication_findings = medication_rows_found,
    drug_findings = drug_rows_found,
    unaccounted_pairs = gap_rows,
    unresolved_nutrient_pairs = length(result$interactions) - length(nutrient_matches),
    unresolved_medication_pairs = length(result$medication_interactions) - length(medication_matches),
    unresolved_drug_pairs = length(result$drug_interactions) - length(drug_matches),
    visualization_context = list(
      timeline = "Events are ordered by entered time; position is chronology and does not imply absorption kinetics.",
      interaction_map = "The optional network uses fixed node positions. Circles are nutrient intakes and squares are medications. Colors indicate relationship status, not strength or safety. Dashed links are uncataloged; drug-drug links have no direction. The matrix shows source columns and target rows; blank cells are self-pairs or outside the selected filters.",
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
    "Bottom line" = "Bottom line",
    "Key findings" = "Bottom line",
    "What the visualizations show" = "What the visualizations show",
    "Unknowns and limitations" = "Unknowns and limitations"
  )
  for (label in names(headings)) {
    pattern <- paste0("(?im)^\\s*(?:#{1,6}\\s*)?(?:\\*\\*)?", label,
      "\\s*:?(?:\\*\\*)?\\s*:?\\s*$")
    text <- gsub(pattern, paste0("## ", headings[[label]]), text, perl = TRUE)
  }
  text <- sub("(?is)^\\s*(?:here(?:'s| is) (?:a |the )?(?:grounded )?(?:analysis|explanation|summary)[.:]?\\s*)",
    "", text, perl = TRUE)
  text <- gsub("\n[ \t]+\n", "\n\n", text)
  text <- gsub("\n{3,}", "\n\n", text)
  if (!grepl("(?m)^##\\s+", text, perl = TRUE)) text <- paste("## Explanation", text, sep = "\n\n")
  text <- sub("(?is)\\n*##\\s+Safety boundary\\s*.*$", "", text, perl = TRUE)
  trimws(text)
}

llm_generation_payload <- function(prompt) {
  list(model = llm_model_name(), prompt = prompt, stream = FALSE, think = FALSE,
    options = list(temperature = 0.1, num_predict = 700))
}

llm_finding_rows <- function(evidence) {
  rows <- list()
  event_by_id <- function(id) {
    if (is.null(id)) return(NULL)
    matches <- Filter(function(x) identical(x$id, id), evidence$events)
    if (length(matches)) matches[[1]] else NULL
  }
  medication_by_id <- function(id) {
    if (is.null(id)) return(NULL)
    matches <- Filter(function(x) identical(x$id, id), evidence$medications)
    if (length(matches)) matches[[1]] else NULL
  }
  event_label <- function(item) {
    if (is.null(item)) return(NULL)
    paste(item$nutrient, item$amount, item$unit, "at",
      if (is.null(item$consumed_at)) paste0("+", item$time_minutes, " min") else item$consumed_at)
  }
  medication_label <- function(item) {
    if (is.null(item)) return(NULL)
    paste(item$medication, item$route, "at",
      if (is.null(item$consumed_at)) paste0("+", item$time_minutes, " min") else item$consumed_at)
  }
  for (category in c("nutrient_findings", "medication_findings", "drug_findings")) {
    for (item in evidence[[category]]) {
      item$finding_id <- paste0("finding_", length(rows) + 1L)
      item$category <- category
      if (identical(category, "nutrient_findings")) {
        source <- event_by_id(item$source)
        target <- event_by_id(item$target)
        item$scenario_pair <- paste(event_label(source), "and", event_label(target))
      } else if (identical(category, "medication_findings")) {
        nutrient <- event_by_id(item$nutrient_event)
        medication <- medication_by_id(item$medication_event)
        item$scenario_pair <- paste(event_label(nutrient), "and", medication_label(medication))
      } else {
        first <- medication_by_id(item$drug_a_event)
        second <- medication_by_id(item$drug_b_event)
        item$scenario_pair <- paste(medication_label(first), "and", medication_label(second))
      }
      rows[[length(rows) + 1L]] <- item
    }
  }
  rows
}

llm_gap_rows <- function(evidence) {
  lapply(seq_along(evidence$unaccounted_pairs), function(i) {
    item <- evidence$unaccounted_pairs[[i]]
    item$gap_id <- paste0("gap_", i)
    item
  })
}

validate_llm_priorities <- function(response, findings, gaps) {
  read_id <- function(value, items, field) {
    if (!length(items)) {
      if (!is.null(value)) stop("The AI returned an unexpected ", field, ". Please try again.")
      return(NULL)
    }
    if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value))
      stop("The AI did not choose a ", field, ". Please try again.")
    expected <- vapply(items, function(x) if (!is.null(x$finding_id)) x$finding_id else x$gap_id, character(1))
    if (!value %in% expected) stop("The AI selected an item outside the scenario. Please try again.")
    value
  }
  list(
    lead_finding_id = read_id(response$lead_finding_id, findings, "supported interaction"),
    lead_gap_id = read_id(response$lead_gap_id, gaps, "unanswered interaction")
  )
}

format_grounded_explanation <- function(evidence, findings, priorities = NULL) {
  human <- function(x) gsub("_", " ", x)
  plain <- function(x) gsub("[\\r\\n]+", " ", gsub("[`*#<>\\[\\]]", "", x, perl = TRUE), perl = TRUE)
  intake_label <- function(id) {
    matches <- Filter(function(x) identical(x$id, id), evidence$events)
    if (!length(matches)) return(id)
    item <- matches[[1]]
    paste(item$nutrient, "at", if (is.null(item$consumed_at)) paste0("+", item$time_minutes, " min") else item$consumed_at)
  }
  finding_pair <- function(f) switch(f$category,
    nutrient_findings = paste(intake_label(f$source), "to", intake_label(f$target)),
    medication_findings = paste(intake_label(f$nutrient_event), "and", f$medication),
    drug_findings = paste(f$drug_a, "and", f$drug_b))
  finding_detail <- function(f) {
    if (identical(f$category, "nutrient_findings")) {
      direction <- switch(f$direction, enhances = "enhancing relationship",
        inhibits = "inhibitory relationship", no_detected_effect = "no detected effect within the study conditions",
        human(f$direction))
      detail <- paste0("The catalog describes an ", direction, ".")
      reference_scope <- f$study_reference$scope
      if (!is.null(reference_scope) && nzchar(reference_scope)) detail <- paste(detail, reference_scope)
      return(detail)
    }
    pieces <- c(if (!is.null(f$effect) && nzchar(f$effect)) f$effect,
      if (!is.null(f$guidance) && nzchar(f$guidance)) paste("Catalog guidance:", f$guidance),
      if (!is.null(f$limitation) && nzchar(f$limitation)) paste("Scope:", f$limitation))
    paste(pieces, collapse = " ")
  }
  if (length(findings)) {
    ids <- vapply(findings, function(x) x$finding_id, character(1))
    lead_finding_id <- if (is.null(priorities)) NULL else priorities$lead_finding_id
    if (is.null(lead_finding_id) || !lead_finding_id %in% ids) lead_finding_id <- ids[[1]]
    findings <- c(findings[match(lead_finding_id, ids)], findings[ids != lead_finding_id])
  }
  interaction_bullets <- if (!length(findings)) "- No catalog-supported interaction matched this scenario." else
    vapply(findings, function(f) paste0("- **", plain(f$scenario_pair), " (", human(f$status), ")**: ",
      plain(finding_detail(f)), " (Rule `", f$rule_id, "`; [source](", f$source_url, "))."), character(1))
  gaps <- llm_gap_rows(evidence)
  if (length(gaps)) {
    gap_ids <- vapply(gaps, function(x) x$gap_id, character(1))
    lead_gap_id <- if (is.null(priorities)) NULL else priorities$lead_gap_id
    if (is.null(lead_gap_id) || !lead_gap_id %in% gap_ids) lead_gap_id <- gap_ids[[1]]
    gaps <- c(gaps[match(lead_gap_id, gap_ids)], gaps[gap_ids != lead_gap_id])
  }
  gap_bullets <- if (!length(gaps)) "- No unanswered pairs were identified for this scenario." else
    vapply(seq_along(gaps), function(i) {
      x <- gaps[[i]]
      category <- if (grepl("medication", x$category, ignore.case = TRUE)) {
        if (grepl("nutrient", x$category, ignore.case = TRUE)) "Nutrient + medication" else "Medication + medication"
      } else "Nutrient + nutrient"
      paste0(i, ". **", category, ": ", plain(x$pair), " (", human(x$status), ")**: ", plain(x$reason))
    }, character(1))
  take <- character()
  if (length(findings)) {
    lead <- findings[[1]]
    relation <- if (identical(lead$category, "nutrient_findings"))
      paste("the catalog describes an", switch(lead$direction, enhances = "enhancing relationship",
        inhibits = "inhibitory relationship", human(lead$direction))) else
        if (identical(lead$status, "potential_timing_conflict")) "the catalog flags a potential timing conflict" else
          if (identical(lead$status, "separated_per_label")) "the entered interval meets the catalog's separation rule" else
            paste("the catalog records", human(lead$status))
    take <- paste0("My read: **", plain(finding_pair(lead)), "** is the clearest supported interaction to focus on; ",
      plain(relation), ".")
  } else if (length(gaps)) {
    take <- "My read: unanswered catalog coverage is the main issue in this scenario."
  }
  if (length(gaps)) take <- paste(take, "I would investigate", plain(gaps[[1]]$pair), "first.")
  paste(c("## AI's take", take, "", "## Interaction assessments", interaction_bullets,
    "", "## Unanswered interactions", gap_bullets), collapse = "\n")
}

generate_llm_explanation <- function(result, timeout_seconds = 120) {
  status <- llm_status()
  if (!status$available) stop(status$message)
  evidence <- explainability_payload(result)
  findings <- llm_finding_rows(evidence)
  gaps <- llm_gap_rows(evidence)
  if (!length(findings) && !length(gaps)) {
    return(list(text = format_grounded_explanation(evidence, findings, list()),
      model = llm_model_name(), endpoint = llm_endpoint(),
      generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
      generation_mode = "evidence_only", explanation_method = "evidence_selection",
      grounded_rule_ids = character()))
  }
  priorities <- NULL
  started <- proc.time()[["elapsed"]]
  if (length(findings) || length(gaps)) {
    prompt <- paste(
      "Choose an opinionated lead interaction and lead coverage gap for this scenario.",
      "Select the supported relationship that deserves the most attention based on its evidence, scope, entered amounts, route and chronology.",
      "Also select the unanswered pair that should be investigated first, considering its category and scenario context.",
      "This is prioritization, not an effect-size or safety ranking. Do not infer effects for unanswered pairs.",
      "Return JSON containing exactly the requested ID fields, using only supplied IDs. Do not write prose.",
      as.character(jsonlite::toJSON(list(events = evidence$events, medications = evidence$medications,
        supported_interactions = findings, unanswered_interactions = gaps), auto_unbox = TRUE, null = "null", na = "null")),
      sep = "\n")
    payload <- llm_generation_payload(prompt)
    payload$options$num_predict <- 100L
    payload$options$num_ctx <- 8192L
    properties <- list()
    required <- list()
    if (length(findings)) {
      required <- c(required, list("lead_finding_id"))
      properties$lead_finding_id <- list(type = "string", enum = lapply(findings, function(x) x$finding_id))
    }
    if (length(gaps)) {
      required <- c(required, list("lead_gap_id"))
      properties$lead_gap_id <- list(type = "string", enum = lapply(gaps, function(x) x$gap_id))
    }
    payload$format <- list(type = "object", required = required, additionalProperties = FALSE,
      properties = properties)
    remaining <- timeout_seconds - (proc.time()[["elapsed"]] - started)
    if (remaining <= 0) stop("Explanation timed out. Try a smaller scenario.")
    response <- ollama_request("/api/generate", payload, remaining)
    if (is.null(response$response) || !nzchar(trimws(response$response))) stop("The model returned an empty explanation.")
    if (identical(response$done, FALSE) || identical(response$done_reason, "length"))
      stop("The model stopped before completing the explanation. Try a smaller scenario.")
    decoded <- tryCatch(jsonlite::fromJSON(strip_reasoning_trace(response$response), simplifyVector = FALSE),
      error = function(e) stop("The model returned an unreadable explanation. Please try again."))
    priorities <- validate_llm_priorities(decoded, findings, gaps)
  }
  list(text = format_grounded_explanation(evidence, findings, priorities), model = llm_model_name(),
    endpoint = llm_endpoint(), generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    generation_mode = if (length(findings)) "ai" else "ai_coverage_only",
    explanation_method = "model_prioritization",
    grounded_rule_ids = unique(vapply(findings, function(x) x$rule_id, character(1))))
}
