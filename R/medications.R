read_medication_evidence <- function() {
  rules <- utils::read.csv(engine_file("medication_interactions.csv"),
    stringsAsFactors = FALSE, na.strings = c("", "NA"))
  rules$window_before_minutes <- as.numeric(rules$window_before_minutes)
  rules$window_after_minutes <- as.numeric(rules$window_after_minutes)
  rules
}

read_drug_evidence <- function() {
  utils::read.csv(engine_file("drug_interactions.csv"), stringsAsFactors = FALSE,
    na.strings = c("", "NA"))
}

medication_catalog <- function() {
  rules <- read_medication_evidence()
  keys <- unique(rules$medication_key)
  do.call(rbind, lapply(keys, function(key) {
    rows <- rules[rules$medication_key == key, , drop = FALSE]
    data.frame(key = key, name = rows$medication_name[1], aliases = rows$aliases[1],
      rule_count = nrow(rows), stringsAsFactors = FALSE)
  }))
}

resolve_medication <- function(value) {
  if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(trimws(value)))
    stop("medication must be a nonempty string.")
  needle <- gsub("[ -]", "_", tolower(trimws(value)))
  catalog <- medication_catalog()
  hits <- vapply(seq_len(nrow(catalog)), function(i) {
    aliases <- strsplit(catalog$aliases[i], "|", fixed = TRUE)[[1]]
    needle %in% c(catalog$key[i], gsub("[ -]", "_", tolower(catalog$name[i])), aliases)
  }, logical(1))
  if (sum(hits) > 1L) stop("Ambiguous medication name: ", value)
  if (any(hits)) return(catalog[hits, , drop = FALSE])
  data.frame(key = needle, name = trimws(value), aliases = "", rule_count = 0L,
    stringsAsFactors = FALSE)
}

normalize_medications <- function(medications) {
  if (is.null(medications) || !length(medications)) return(data.frame())
  if (is.list(medications) && !is.data.frame(medications)) {
    fields <- unique(unlist(lapply(medications, names)))
    medications <- do.call(rbind, lapply(medications, function(x) {
      if (!is.list(x) || is.null(names(x))) stop("Each medication must be an object.")
      if (any(vapply(x, length, integer(1)) != 1L)) stop("Medication fields must be scalar values.")
      x[setdiff(fields, names(x))] <- NA
      as.data.frame(x[fields], stringsAsFactors = FALSE)
    }))
  }
  if (!is.data.frame(medications) || nrow(medications) < 1L || nrow(medications) > 50L)
    stop("medications must contain between 1 and 50 rows.")
  required <- c("id", "medication", "route")
  if (!all(required %in% names(medications)))
    stop("Required medication fields: ", paste(required, collapse = ", "))
  medications <- normalize_chronology(medications)
  keep <- c(required, "time_minutes", "consumed_at", "consumed_at_local")
  medications <- medications[, keep, drop = FALSE]
  for (name in required) {
    if (!is.character(medications[[name]]) || anyNA(medications[[name]]) ||
        any(!nzchar(trimws(medications[[name]])))) stop(name, " must contain nonempty strings.")
    medications[[name]] <- trimws(medications[[name]])
  }
  if (anyDuplicated(medications$id)) stop("Medication IDs must be unique.")
  if (!is.numeric(medications$time_minutes) || anyNA(medications$time_minutes) ||
      any(!is.finite(medications$time_minutes)) || any(medications$time_minutes < 0))
    stop("Medication time_minutes must contain finite nonnegative numbers.")
  resolved <- lapply(medications$medication, resolve_medication)
  medications$medication_key <- vapply(resolved, function(x) x$key[1], character(1))
  medications$medication_name <- vapply(resolved, function(x) x$name[1], character(1))
  medications$route <- tolower(medications$route)
  medications <- medications[order(medications$time_minutes, medications$id, method = "radix"), , drop = FALSE]
  rownames(medications) <- NULL
  medications
}

align_medication_chronology <- function(events, medications) {
  if (!nrow(medications)) return(list(events = events, medications = medications))
  event_clock <- all(!is.na(events$consumed_at) & nzchar(events$consumed_at))
  medication_clock <- all(!is.na(medications$consumed_at) & nzchar(medications$consumed_at))
  if (event_clock != medication_clock)
    stop("Use the same chronology mode for nutrient and medication events.")
  if (event_clock) {
    event_seconds <- parse_consumed_at(events$consumed_at)
    medication_seconds <- parse_consumed_at(medications$consumed_at)
    origin <- min(c(event_seconds, medication_seconds))
    events$time_minutes <- (event_seconds - origin) / 60
    medications$time_minutes <- (medication_seconds - origin) / 60
  }
  list(events = events, medications = medications)
}

medication_interactions <- function(events, medications) {
  if (is.null(medications) || !nrow(medications)) return(list())
  rules <- read_medication_evidence()
  findings <- list()
  for (m_i in seq_len(nrow(medications))) {
    medication <- medications[m_i, ]
    for (n_i in seq_len(nrow(events))) {
      nutrient <- events[n_i, ]
      matched <- rules[rules$medication_key == medication$medication_key &
        rules$nutrient_key == nutrient$evidence_key, , drop = FALSE]
      gap <- nutrient$time_minutes - medication$time_minutes
      item <- list(medication_id = medication$id, medication = medication$medication_key,
        medication_name = medication$medication_name, route = medication$route,
        nutrient_event_id = nutrient$id, nutrient = nutrient$nutrient,
        nutrient_name = nutrient$nutrient_name, nutrient_minus_medication_minutes = gap,
        status = "unsupported_pair", direction = "unknown", rule_id = NULL,
        clinical_significance = NULL, effect = NULL, mechanism = NULL, guidance = NULL,
        citation = NULL, applicability = "No matching medication-nutrient rule in this version's catalog.")
      if (nrow(matched)) {
        valid_form <- vapply(seq_len(nrow(matched)), function(i)
          nutrient$form %in% strsplit(matched$nutrient_forms[i], "|", fixed = TRUE)[[1]], logical(1))
        candidates <- matched[valid_form, , drop = FALSE]
        if (!nrow(candidates)) {
          item$status <- "unsupported_form"
          item$applicability <- "The pair is cataloged, but this nutrient form is outside the rule scope."
        } else if (medication$route != "oral") {
          item$status <- "unsupported_route"
          item$applicability <- "The cataloged rule applies to oral medication; this route is not covered."
        } else {
          rule <- candidates[1, ]
          item$rule_id <- rule$rule_id
          item$direction <- rule$direction
          item$clinical_significance <- rule$clinical_significance
          item$effect <- rule$effect
          item$mechanism <- rule$mechanism
          item$guidance <- rule$guidance
          item$citation <- list(source = rule$evidence_source, url = rule$url,
            reviewed_on = rule$reviewed_on)
          item$applicability <- rule$limitation
          if (nutrient$amount == 0) {
            item$status <- "zero_dose"
          } else if (rule$scope == "separate_symmetric") {
            item$status <- if (abs(gap) < rule$window_after_minutes) "potential_timing_conflict" else "separated_per_label"
          } else if (rule$scope == "separate_asymmetric") {
            safely_separated <- gap >= rule$window_after_minutes || gap <= -rule$window_before_minutes
            item$status <- if (safely_separated) "separated_per_label" else "potential_timing_conflict"
          } else if (rule$scope == "consistent_intake") {
            item$status <- "evidence_match_non_timing"
          } else if (rule$scope == "chronic_monitoring") {
            item$status <- "long_term_context_required"
          } else {
            item$status <- "evidence_match_timing_unresolved"
          }
        }
      }
      findings[[length(findings) + 1L]] <- item
    }
  }
  findings
}

drug_interactions <- function(medications) {
  if (is.null(medications) || nrow(medications) < 2L) return(list())
  rules <- read_drug_evidence()
  pairs <- utils::combn(seq_len(nrow(medications)), 2L)
  findings <- vector("list", ncol(pairs))
  for (i in seq_len(ncol(pairs))) {
    first <- medications[pairs[1, i], ]
    second <- medications[pairs[2, i], ]
    matched <- rules[(rules$drug_a_key == first$medication_key &
      rules$drug_b_key == second$medication_key) |
      (rules$drug_a_key == second$medication_key &
        rules$drug_b_key == first$medication_key), , drop = FALSE]
    item <- list(drug_a_id = first$id, drug_a = first$medication_key,
      drug_a_name = first$medication_name, drug_b_id = second$id,
      drug_b = second$medication_key, drug_b_name = second$medication_name,
      time_gap_minutes = second$time_minutes - first$time_minutes,
      status = "unsupported_pair", direction = "unknown", rule_id = NULL,
      clinical_significance = NULL, effect = NULL, mechanism = NULL,
      guidance = NULL, citation = NULL,
      applicability = "No matching drug-drug rule in this version's selected label catalog.")
    if (nrow(matched)) {
      rule <- matched[1, ]
      item$rule_id <- rule$rule_id
      item$direction <- rule$direction
      item$clinical_significance <- rule$clinical_significance
      item$effect <- rule$effect
      item$mechanism <- rule$mechanism
      item$guidance <- rule$guidance
      item$citation <- list(source = rule$evidence_source, url = rule$url,
        reviewed_on = rule$reviewed_on)
      item$applicability <- rule$limitation
      item$status <- if (first$route == "oral" && second$route == "oral")
        "label_monitoring_interaction" else "unsupported_route"
    }
    findings[[i]] <- item
  }
  findings
}
