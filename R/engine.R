# Helpers are internal; public functions are documented in man/ and docs/.
engine_file <- function(name) {
  installed <- system.file("extdata", name, package = "nutrientengine")
  if (nzchar(installed)) return(installed)
  local <- file.path("inst", "extdata", name)
  if (!file.exists(local)) stop("Run from the project root or install nutrientengine.")
  local
}

read_evidence <- function() {
  utils::read.csv(engine_file("evidence.csv"), stringsAsFactors = FALSE,
                  na.strings = c("", "NA"))
}

phytate_reference <- function(amount_mg, interpolate = FALSE) {
  if (!is.numeric(amount_mg) || length(amount_mg) != 1L ||
      !is.finite(amount_mg) || amount_mg < 0) stop("amount_mg must be finite and nonnegative.")
  if (!is.logical(interpolate) || length(interpolate) != 1L || is.na(interpolate))
    stop("interpolate must be true or false.")
  anchors <- utils::read.csv(engine_file("phytate_reference.csv"))
  exact <- which(abs(anchors$phytate_phosphorus_mg - amount_mg) < 1e-9)
  if (length(exact)) return(list(relative_absorption = anchors$relative_absorption[exact],
    method = "reported_study_anchor", lower = NULL, upper = NULL))
  if (!interpolate || amount_mg < 2 || amount_mg > 250)
    return(list(relative_absorption = NULL, method = "not_estimated", lower = NULL, upper = NULL))
  value <- stats::approx(log(anchors$phytate_phosphorus_mg),
    anchors$relative_absorption, xout = log(amount_mg))$y
  list(relative_absorption = value, method = "exploratory_log_dose_interpolation",
       lower = NULL, upper = NULL)
}

normalize_events <- function(events) {
  if (is.list(events) && !is.data.frame(events)) {
    if (!length(events)) stop("Supply at least one event.")
    fields <- unique(unlist(lapply(events, names)))
    events <- do.call(rbind, lapply(events, function(x) {
      if (!is.list(x) || is.null(names(x))) stop("Each event must be an object.")
      if (any(vapply(x, length, integer(1)) != 1L)) stop("Event fields must be scalar values.")
      x[setdiff(fields, names(x))] <- NA
      x <- x[fields]
      as.data.frame(x, stringsAsFactors = FALSE)
    }))
  }
  if (!is.data.frame(events) || nrow(events) < 1L || nrow(events) > 100L)
    stop("events must contain between 1 and 100 rows.")
  required <- c("id", "nutrient", "form", "amount", "unit", "meal_id")
  if (!all(required %in% names(events)))
    stop("Required event fields: ", paste(required, collapse = ", "))
  events <- normalize_chronology(events)
  events <- events[, c(required, "time_minutes", "consumed_at", "consumed_at_local"), drop = FALSE]
  for (name in c("id", "nutrient", "form", "unit", "meal_id")) {
    if (!is.character(events[[name]]) || anyNA(events[[name]]) ||
        any(!nzchar(trimws(events[[name]])))) stop(name, " must contain nonempty strings.")
    events[[name]] <- trimws(events[[name]])
  }
  if (anyDuplicated(events$id)) stop("Event IDs must be unique.")
  for (name in c("amount", "time_minutes")) {
    if (!is.numeric(events[[name]]) || anyNA(events[[name]]) ||
        any(!is.finite(events[[name]])) || any(events[[name]] < 0))
      stop(name, " must contain finite nonnegative numbers.")
  }
  catalog <- nutrient_catalog()
  resolved <- do.call(rbind, lapply(events$nutrient, resolve_nutrient, catalog = catalog))
  events$nutrient <- resolved$key
  events$usda_nutrient_id <- resolved$id
  events$nutrient_name <- resolved$name
  events$usda_unit <- resolved$unit_name
  events$nutrient_origin <- resolved$origin
  events$evidence_key <- resolved$evidence_key
  events$form <- tolower(events$form)
  events$unit <- normalize_unit(events$unit)
  events$canonical_unit <- resolved$canonical_unit
  events$unit_family <- resolved$unit_family
  factor <- c(g = 1000, mg = 1, mcg = .001, ug = .001)
  mass <- resolved$unit_family == "mass"
  energy <- resolved$unit_family == "energy"
  if (any(mass & !events$unit %in% names(factor)) ||
      any(energy & !events$unit %in% c("kcal", "kj")) ||
      any(!mass & !energy & events$unit != resolved$canonical_unit))
    stop("Unit is incompatible with this nutrient's USDA measure. Activity/equivalent units cannot be converted to mass.")
  events$amount_mg <- NA_real_
  events$amount_mg[mass] <- events$amount[mass] * unname(factor[events$unit[mass]])
  events$amount_canonical <- events$amount
  events$amount_canonical[mass] <- events$amount_mg[mass] / unname(factor[resolved$canonical_unit[mass]])
  events$amount_canonical[energy & events$unit == "kj" & resolved$canonical_unit == "kcal"] <-
    events$amount[energy & events$unit == "kj" & resolved$canonical_unit == "kcal"] / 4.184
  events$amount_canonical[energy & events$unit == "kcal" & resolved$canonical_unit == "kj"] <-
    events$amount[energy & events$unit == "kcal" & resolved$canonical_unit == "kj"] * 4.184
  if (any(!is.finite(events$amount_canonical)) || any(!is.finite(events$amount_mg[mass])))
    stop("Converted amounts exceed numeric limits.")
  events <- events[order(events$time_minutes, events$id, method = "radix"), , drop = FALSE]
  rownames(events) <- NULL
  events
}

analyze_interactions <- function(events, context = list(population = "adult", meal_state = "fed"),
                                 interpolate = FALSE) {
  if (!is.logical(interpolate) || length(interpolate) != 1L || is.na(interpolate))
    stop("interpolate must be true or false.")
  if (!is.list(context) || is.null(context$population) || is.null(context$meal_state) ||
      !is.character(context$population) || length(context$population) != 1L ||
      is.na(context$population) || !nzchar(context$population) ||
      !is.character(context$meal_state) || length(context$meal_state) != 1L ||
      is.na(context$meal_state) || !nzchar(context$meal_state))
    stop("context requires scalar population and meal_state strings.")
  events <- normalize_events(events)
  evidence <- read_evidence()
  findings <- list()
  for (target_i in seq_len(nrow(events))) {
    for (source_i in setdiff(seq_len(nrow(events)), target_i)) {
      source <- events[source_i, ]; target <- events[target_i, ]
      gap <- source$time_minutes - target$time_minutes
      same_meal <- source$meal_id == target$meal_id
      rules <- evidence[evidence$source == source$evidence_key & evidence$target == target$evidence_key, , drop = FALSE]
      base <- list(source_id = source$id, target_id = target$id,
        source_nutrient = source$nutrient, target_nutrient = target$nutrient,
        source_usda_nutrient_id = source$usda_nutrient_id, target_usda_nutrient_id = target$usda_nutrient_id,
        source_amount = source$amount_canonical, source_unit = source$canonical_unit,
        target_amount = target$amount_canonical, target_unit = target$canonical_unit,
        source_amount_mg = source$amount_mg, target_amount_mg = target$amount_mg,
        source_minus_target_minutes = gap,
        order = if (gap < 0) "source_before_target" else if (gap > 0) "source_after_target" else "simultaneous",
        same_meal = same_meal, status = "unsupported_pair", direction = "unknown",
        rule_id = NULL, reference_direction = NULL, study_reference = NULL,
        citation = NULL, applicability = "No matching evidence in this version's catalog.")
      if (nrow(rules)) {
        valid_form <- vapply(seq_len(nrow(rules)), function(i)
          source$form %in% strsplit(rules$source_forms[i], "|", fixed = TRUE)[[1]] &&
          target$form %in% strsplit(rules$target_forms[i], "|", fixed = TRUE)[[1]], logical(1))
        candidates <- rules[valid_form, , drop = FALSE]
        if (!nrow(candidates)) {
          base$status <- "unsupported_form"
          base$applicability <- "Pair exists but these chemical forms are not covered."
        } else {
          timing_match <- (candidates$scope == "same_meal" & same_meal) |
            (candidates$scope == "prior_separate_meal" & !same_meal &
              gap %in% c(-120, -240) & abs(source$amount_mg - 340) < 1e-9)
          if (!any(timing_match)) {
            base$status <- "timing_unresolved"
            base$applicability <- "This order, time gap or separate-meal dose is outside the cataloged timing evidence."
          } else {
            rule <- candidates[which(timing_match)[1], ]
            base$rule_id <- rule$rule_id
            base$reference_direction <- rule$direction
            base$citation <- list(pmid = rule$pmid, url = rule$url,
              design = rule$study_design, population = rule$population,
              sample_size = if (is.na(rule$sample_size)) NULL else rule$sample_size,
              evidence_grade = "not_formally_graded")
            base$applicability <- rule$limitation
            if (context$population != "adult" || context$meal_state != "fed") {
              base$status <- "unsupported_context"
              base$applicability <- paste(base$applicability, "Catalog requires adult, fed context.")
            } else if (source$amount == 0 || target$amount == 0) {
              base$status <- "zero_dose"
              base$applicability <- "At least one event has zero dose; no interaction applied."
            } else {
              base$status <- "qualitative_evidence"
              base$direction <- rule$direction
              if (same_meal && gap != 0) {
                base$status <- "within_meal_timing_unresolved"
                base$direction <- "unknown"
                base$applicability <- paste(base$applicability,
                  "Meal association is known; this exact within-meal time gap is unquantified.")
              }
              if (rule$rule_id == "phytate_iron" && gap == 0) {
                base$study_reference <- phytate_reference(source$amount_mg, interpolate)
                base$study_reference$dose_basis <- "mg phytate phosphorus, not mg phytate compound"
                base$study_reference$scope <- "wheat-roll experiment reference; not a prediction for the entered meal"
              }
            }
          }
        }
      }
      findings[[length(findings) + 1L]] <- base
    }
  }
  target_summaries <- lapply(seq_len(nrow(events)), function(i) {
    incoming <- Filter(function(x) x$target_id == events$id[i], findings)
    supported <- Filter(function(x) x$status == "qualitative_evidence", incoming)
    directions <- unique(vapply(supported, function(x) x$direction, character(1)))
    list(event_id = events$id[i], nutrient = events$nutrient[i],
      usda_nutrient_id = events$usda_nutrient_id[i], nutrient_name = events$nutrient_name[i],
      amount = events$amount_canonical[i], unit = events$canonical_unit[i], amount_mg = events$amount_mg[i],
      consumed_at = events$consumed_at[i], consumed_at_local = events$consumed_at_local[i],
      evidence_directions = as.list(unname(directions)), matched_findings = length(supported),
      baseline_bioavailability = NULL, adjusted_bioavailability = NULL,
      combined_relative_effect = NULL,
      explanation = "Absolute and combined effects require a validated model; pairwise findings are reported separately.")
  })
  list(schema_version = "1.1.0", engine_version = "0.2.0", mode = "evidence_explorer",
    chronology = list(mode = if (all(is.na(events$consumed_at))) "minute_offsets" else "consumption_times",
      origin_local = if (all(is.na(events$consumed_at_local))) NULL else events$consumed_at_local[1]),
    context = context, interpolate = interpolate, events = events,
    targets = target_summaries, interactions = findings,
    warnings = c("Catalog is incomplete: unknown does not mean no biological interaction.",
      "Study references are not personalized absorption predictions.",
      "No universal timing decay or combined-effect multiplier has been assumed.",
      if (interpolate) "Dose interpolation is exploratory and has no estimated confidence interval."),
    provenance = list(evidence_catalog = "human-meal-studies-v0.1", reviewed_on = "2026-10-14",
      nutrient_catalog = list(source = "USDA FoodData Central nutrient.csv", release = "2026-04-30",
        definitions = nrow(nutrient_catalog()), source_url = "https://fdc.nal.usda.gov/download-datasets/"),
      extraction = "abstract-level; full-text appraisal and independent review pending"))
}

result_json <- function(result, pretty = TRUE) {
  jsonlite::toJSON(result, auto_unbox = TRUE, pretty = pretty, null = "null",
                   na = "null", digits = 10, dataframe = "rows")
}
