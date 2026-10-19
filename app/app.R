library(shiny)
project_root <- if (file.exists("scripts/load_project.R")) "." else ".."
for (file in list.files(file.path(project_root, "R"), pattern = "\\.R$", full.names = TRUE)) source(file, local = TRUE)
# Development source lookup is relative to the app working directory.
engine_file <- function(name) file.path(project_root, "inst", "extdata", name)
example_text <- paste(readLines(engine_file("example-clock.json")), collapse = "\n")
empty_text <- as.character(jsonlite::toJSON(list(context = list(population = "adult", meal_state = "fed"),
  events = list(), medications = list()), auto_unbox = TRUE, pretty = TRUE))
catalog <- nutrient_catalog()
choice_labels <- nutrient_display_names(catalog)
choice_order <- order(tolower(choice_labels))
nutrient_choices <- stats::setNames(catalog$key[choice_order], choice_labels[choice_order])
nutrient_choices <- c(nutrient_choices, "Phytate phosphorus" = "phytate")
medication_data <- medication_catalog()
medication_choices <- stats::setNames(medication_data$key, medication_data$name)
clock_default <- ""

# An omitted meal must not imply that separate intakes belonged to the same meal.
meal_label <- function(value, id) {
  if (is.null(value) || !nzchar(trimws(value))) paste0("unassigned_", id) else trimws(value)
}

clock_input <- function(local) {
  if (is.null(local) || !nzchar(local)) stop("Select a consumption time.")
  parse_consumed_at(local)
  local
}

prepare_timing <- function(payload, mode, origin) {
  collections <- c("events", "medications")
  if (mode == "clock") {
    base <- parse_consumed_at(origin)
    for (collection in collections) {
      items <- payload[[collection]]
      if (is.null(items) || !length(items)) next
      has_clock <- vapply(items, function(e) !is.null(e$consumed_at) && nzchar(e$consumed_at), logical(1))
      if (all(has_clock)) next
      if (any(has_clock)) stop("Mixed consumption timestamps and offsets: supply a timestamp for every item.")
      payload[[collection]] <- lapply(items, function(e) {
        if (is.null(e$time_minutes) || !is.numeric(e$time_minutes) || !is.finite(e$time_minutes) || e$time_minutes < 0)
          stop("Every offset item requires nonnegative time_minutes.")
        seconds <- base + e$time_minutes * 60
        if (seconds >= 86400 || seconds %% 1 != 0)
          stop("Time-only conversion requires same-day, whole-second values. Use dated local times in JSON for overnight scenarios.")
        e$consumed_at <- format_local_time(seconds)
        e$time_minutes <- NULL
        e
      })
    }
  } else {
    normalized <- if (length(payload$events)) normalize_events(payload$events) else data.frame()
    normalized_medications <- normalize_medications(payload$medications)
    aligned <- if (nrow(normalized)) align_medication_chronology(normalized, normalized_medications)
      else list(events = normalized, medications = normalized_medications)
    normalized <- aligned$events
    normalized_medications <- aligned$medications
    payload$events <- lapply(payload$events, function(e) {
      e$time_minutes <- normalized$time_minutes[match(e$id, normalized$id)]
      e$consumed_at <- NULL
      e
    })
    if (nrow(normalized_medications)) {
      payload$medications <- lapply(payload$medications, function(e) {
        e$time_minutes <- normalized_medications$time_minutes[match(e$id, normalized_medications$id)]
        e$consumed_at <- NULL
        e
      })
    }
  }
  payload
}

view_heading <- function(title, description) div(class = "view-heading", h2(title), p(description))

source(file.path(project_root, "app", "explorer.R"), local = TRUE, encoding = "UTF-8")
source(file.path(project_root, "app", "workspace-ui.R"), local = TRUE, encoding = "UTF-8")

server <- function(input, output, session) {
  observeEvent(input$nutrient, {
    if (!nzchar(input$nutrient)) {
      updateSelectInput(session, "form", choices = character())
      updateSelectInput(session, "unit", choices = character())
      return()
    }
    selected <- resolve_nutrient(input$nutrient, catalog)
    forms <- nutrient_forms(selected$key, selected$default_form)
    updateSelectInput(session, "form", choices = forms, selected = forms[1])
    updateSelectInput(session, "unit", choices = nutrient_units(selected$canonical_unit, selected$unit_family),
      selected = selected$canonical_unit)
  })
  output$nutrient_hint <- renderText({
    req(input$nutrient); selected <- resolve_nutrient(input$nutrient, catalog)
    paste("Native unit:", selected$canonical_unit,
      if (!is.na(selected$id)) paste0(" / USDA ", selected$id) else "")
  })
  catalog_filtered <- reactive({
    query <- tolower(trimws(input$catalog_search))
    catalog[grepl(query, tolower(paste(catalog$id, catalog$name, catalog$unit_name)), fixed = TRUE), ]
  })
  output$catalog_count <- renderText(paste(nrow(catalog_filtered()), "of", nrow(catalog), "definitions match."))
  output$catalog_table <- renderTable(utils::head(catalog_filtered()[, c("id", "name", "unit_name", "nutrient_nbr", "definition_status")], 50))
  current <- reactiveVal(NULL)
  error <- reactiveVal(NULL)
  explanation <- reactiveVal(NULL)
  explanation_error <- reactiveVal(NULL)
  analyzed_text <- reactiveVal(NULL)
  draft_text <- reactiveVal(empty_text)
  observeEvent(input$scenario, {
    if (!identical(input$scenario, draft_text())) {
      draft_text(input$scenario)
      current(NULL); error(NULL); explanation(NULL); explanation_error(NULL)
    }
  })
  draft_payload <- reactive({
    payload <- jsonlite::fromJSON(draft_text(), simplifyVector = FALSE)
    if (is.null(payload$context) || is.null(payload$events)) stop("Supply context and events.")
    if (is.null(payload$medications)) payload$medications <- list()
    payload
  })
  validate_draft <- function(payload) {
    nutrients <- if (length(payload$events)) normalize_events(payload$events) else data.frame()
    drugs <- normalize_medications(payload$medications)
    if (nrow(nutrients)) align_medication_chronology(nutrients, drugs)
    invisible(payload)
  }
  publish_draft <- function(payload) {
    validate_draft(payload)
    text <- as.character(result_json(payload))
    draft_text(text)
    updateTextAreaInput(session, "scenario", value = text)
    current(NULL); error(NULL); explanation(NULL); explanation_error(NULL)
  }
  draft_entries <- reactive({
    payload <- draft_payload()
    entries <- list()
    for (collection in c("events", "medications")) for (item in payload[[collection]]) {
      nutrient <- collection == "events"
      label <- if (nutrient) {
        resolved <- resolve_nutrient(item$nutrient, catalog)
        friendly <- nutrient_display_names(catalog)[match(resolved$key, catalog$key)]
        if (is.na(friendly)) resolved$name else friendly
      } else resolve_medication(item$medication)$name
      time <- if (!is.null(item$consumed_at)) item$consumed_at else paste0("+", item$time_minutes, " min")
      entries[[length(entries) + 1L]] <- list(token = paste(collection, item$id, sep = ":"),
        collection = collection, id = item$id, item = item,
        label = paste(if (nutrient) "Nutrient:" else "Medication:", label,
          if (nutrient) paste(item$amount, item$unit) else item$route, " / ", time))
    }
    entries
  })
  output$draft_manager <- renderUI({
    tryCatch({
      entries <- draft_entries()
      if (!length(entries)) return(div(class = "intake-manager", h3("Your intakes"),
        p(class = "note", "No intakes yet. Add a nutrient or medication to begin.")))
      div(class = "intake-manager", h3("Your intakes"),
        tags$ul(class = "intake-list", lapply(entries, function(e) tags$li(
          span(class = "intake-label", e$label),
          div(class = "intake-actions",
            tags$button(type = "button", class = "btn btn-default intake-action", `data-token` = e$token,
              `data-action` = "edit", `aria-label` = paste("Edit", e$label), "Edit"),
            tags$button(type = "button", class = "btn btn-default btn-remove intake-action", `data-token` = e$token,
              `data-action` = "remove", `aria-label` = paste("Remove", e$label), "Remove"))))))
    }, error = function(e) div(class = "intake-manager", p("Correct the scenario JSON to manage intakes.")))
  })
  selected_entry <- function(token = input$draft_item) {
    entries <- draft_entries()
    matches <- Filter(function(e) identical(e$token, token), entries)
    if (length(matches) != 1L) stop("Select an intake first.")
    matches[[1]]
  }
  editing <- reactiveVal(NULL)
  observeEvent(list(input$remove_item, input$remove_intake), {
    tryCatch({
      entry <- selected_entry(if (!is.null(input$remove_intake)) input$remove_intake else input$draft_item); payload <- draft_payload()
      payload[[entry$collection]] <- Filter(function(e) e$id != entry$id, payload[[entry$collection]])
      publish_draft(payload)
      showNotification("Intake removed. Analyze to refresh results.", type = "message")
    }, error = function(e) showNotification(conditionMessage(e), type = "error"))
  }, ignoreInit = TRUE)
  observeEvent(list(input$edit_item, input$edit_intake), {
    tryCatch({
      entry <- selected_entry(if (!is.null(input$edit_intake)) input$edit_intake else input$draft_item); item <- entry$item; nutrient <- entry$collection == "events"
      editing(entry)
      fields <- if (nutrient) {
        resolved <- resolve_nutrient(item$nutrient, catalog)
        tagList(selectizeInput("edit_nutrient", "Nutrient", nutrient_choices, selected = resolved$key),
          selectInput("edit_form", "Chemical form", unique(c(item$form, nutrient_forms(resolved$key, resolved$default_form))), selected = item$form),
          numericInput("edit_amount", "Amount", item$amount, min = 0),
          selectInput("edit_unit", "Unit", nutrient_units(resolved$canonical_unit, resolved$unit_family), selected = item$unit),
          textInput("edit_meal", "Meal label (optional)",
            if (identical(item$meal_id, paste0("unassigned_", item$id))) "" else item$meal_id))
      } else {
        resolved <- resolve_medication(item$medication)
        choices <- medication_choices
        if (!resolved$key %in% choices) choices <- c(choices, stats::setNames(resolved$key, resolved$name))
        tagList(selectizeInput("edit_drug", "Medication", choices, selected = resolved$key),
          selectInput("edit_route", "Route", unique(c(item$route, "oral")), selected = item$route))
      }
      clock <- !is.null(item$consumed_at) && nzchar(item$consumed_at)
      timing <- if (clock) tagList(
        textInput("edit_date", "Date (optional, YYYY-MM-DD)", if (grepl("T", item$consumed_at, fixed = TRUE)) substr(item$consumed_at, 1, 10) else ""),
        tags$label(`for` = "edit_time", "Time consumed"),
        tags$input(id = "edit_time", type = "time", class = "form-control", step = "1",
          value = sub("^.*T", "", item$consumed_at),
          onchange = "Shiny.setInputValue('edit_time', this.value, {priority:'event'});"))
      else numericInput("edit_minute", "Minutes from scenario start", item$time_minutes, min = 0)
      showModal(modalDialog(title = "Edit intake", fields, timing, easyClose = FALSE,
        footer = tagList(modalButton("Cancel"), actionButton("save_item", "Save changes", class = "btn-primary",
          onclick = "syncIntakeFields(['edit_nutrient','edit_form','edit_amount','edit_unit','edit_meal','edit_drug','edit_route','edit_date','edit_time','edit_minute']);"))))
    }, error = function(e) showNotification(conditionMessage(e), type = "error"))
  }, ignoreInit = TRUE)
  observeEvent(input$edit_nutrient, {
    req(editing(), input$edit_nutrient)
    resolved <- resolve_nutrient(input$edit_nutrient, catalog)
    original <- editing()$item
    same <- identical(resolved$key, resolve_nutrient(original$nutrient, catalog)$key)
    forms <- unique(c(if (same) original$form, nutrient_forms(resolved$key, resolved$default_form)))
    updateSelectInput(session, "edit_form", choices = forms, selected = forms[1])
    updateSelectInput(session, "edit_unit", choices = nutrient_units(resolved$canonical_unit, resolved$unit_family),
      selected = if (same) original$unit else resolved$canonical_unit)
  })
  observeEvent(input$save_item, {
    tryCatch({
      entry <- editing(); if (is.null(entry)) stop("Reopen the intake editor.")
      payload <- draft_payload()
      index <- which(vapply(payload[[entry$collection]], function(e) identical(e$id, entry$id), logical(1)))
      if (length(index) != 1L || !identical(payload[[entry$collection]][[index]], entry$item))
        stop("This intake changed while the editor was open. Cancel and reopen it.")
      item <- entry$item
      if (entry$collection == "events") {
        item$nutrient <- input$edit_nutrient; item$form <- input$edit_form
        item$amount <- input$edit_amount; item$unit <- input$edit_unit; item$meal_id <- meal_label(input$edit_meal, item$id)
      } else {item$medication <- input$edit_drug; item$route <- input$edit_route}
      if (!is.null(entry$item$consumed_at) && nzchar(entry$item$consumed_at)) {
        time <- if (is.null(input$edit_time)) sub("^.*T", "", entry$item$consumed_at) else clock_input(input$edit_time)
        date <- trimws(input$edit_date)
        item$consumed_at <- paste0(if (nzchar(date)) paste0(date, "T") else "", time)
        item$time_minutes <- NULL
      } else item$time_minutes <- input$edit_minute
      payload[[entry$collection]][[index]] <- item
      publish_draft(payload); removeModal(); editing(NULL)
      showNotification("Intake updated. Analyze to refresh results.", type = "message")
    }, error = function(e) showNotification(conditionMessage(e), type = "error"))
  })
  output$draft_summary <- renderText({
    tryCatch({
      draft <- draft_payload()
      paste(length(draft$events), "nutrients /", length(draft$medications), "drugs in draft")
    }, error = function(e) "Check draft JSON")
  })
  run <- function(text) {
    tryCatch({
      if (nchar(text, type = "bytes") > 1024 * 1024) stop("Scenario exceeds 1 MB.")
      payload <- jsonlite::fromJSON(text, simplifyVector = FALSE)
      if (is.null(payload$context) || is.null(payload$events)) stop("Supply context and events.")
      if (!length(payload$events)) {
        current(NULL); error(NULL); explanation(NULL); explanation_error(NULL); return(invisible(NULL))
      }
      flag <- if (is.null(payload$interpolate)) FALSE else payload$interpolate
      current(analyze_interactions(payload$events, payload$context, flag, payload$medications));
      error(NULL); analyzed_text(text); explanation(NULL); explanation_error(NULL)
    }, error = function(e) {current(NULL); error(conditionMessage(e))})
  }
  # Apply the flushed JSON field before analysis, including rapid edit-and-click.
  observeEvent(input$analyze, run(draft_text()), priority = -1)
  observeEvent(input$reset, {
    publish_draft(jsonlite::fromJSON(empty_text, simplifyVector = FALSE)); editing(NULL); removeModal()
    analyzed_text(NULL)
    updateSelectizeInput(session, "nutrient", selected = "")
    updateSelectizeInput(session, "drug", selected = "")
    updateNumericInput(session, "amount", value = NA_real_)
    updateNumericInput(session, "minute", value = NA_real_)
    updateNumericInput(session, "medication_minute", value = NA_real_)
    updateTextInput(session, "meal", value = "")
    session$sendCustomMessage("clear-intake-times", list())
  })
  observeEvent(input$add, {
    tryCatch({
      payload <- draft_payload()
      if (is.null(input$nutrient) || !nzchar(input$nutrient)) stop("Choose a nutrient.")
      existing <- vapply(payload$events, function(e) e$id, character(1))
      index <- length(existing) + 1L
      while (paste0("event_", index) %in% existing) index <- index + 1L
      origin <- if (input$timing_mode == "clock") clock_input(input$consumed_time) else NULL
      payload <- prepare_timing(payload, input$timing_mode, origin)
      event <- list(id = paste0("event_", index), nutrient = input$nutrient,
        form = input$form, amount = input$amount, unit = input$unit,
        meal_id = meal_label(input$meal, paste0("event_", index)))
      if (input$timing_mode == "clock") event$consumed_at <- origin else event$time_minutes <- input$minute
      payload$events <- append(payload$events, list(event))
      publish_draft(payload)
      showNotification("Added to draft. Click Analyze scenario to refresh results.", type = "message")
    }, error = function(e) showNotification(conditionMessage(e), type = "error"))
  })
  observeEvent(input$add_medication, {
    tryCatch({
      payload <- draft_payload()
      if (is.null(input$drug) || !nzchar(input$drug)) stop("Choose a medication.")
      if (is.null(payload$medications)) payload$medications <- list()
      existing <- vapply(payload$medications, function(e) e$id, character(1))
      index <- length(existing) + 1L
      while (paste0("medication_", index) %in% existing) index <- index + 1L
      origin <- if (input$timing_mode == "clock") clock_input(input$medication_time) else NULL
      payload <- prepare_timing(payload, input$timing_mode, origin)
      item <- list(id = paste0("medication_", index), medication = input$drug,
        route = input$medication_route)
      if (input$timing_mode == "clock") item$consumed_at <- origin else item$time_minutes <- input$medication_minute
      payload$medications <- append(payload$medications, list(item))
      publish_draft(payload)
      showNotification("Medication added to the draft. Analyze to refresh results.", type = "message")
    }, error = function(e) showNotification(conditionMessage(e), type = "error"))
  })
  output$status <- renderUI({
    if (!is.null(error())) return(div(class = "status status-error", role = "alert", strong("Check scenario: "), error()))
    if (is.null(current())) return(div(class = "status", role = "status", `aria-live` = "polite",
      strong(if (is.null(analyzed_text())) "Build your scenario" else "Draft changed"),
      span(if (is.null(analyzed_text())) "Add your intakes, then analyze. Analysis requires at least one nutrient."
        else "Previous results were cleared. Analyze again to refresh findings and export.")))
    result <- current()
    changed <- !identical(draft_text(), analyzed_text())
    div(class = if (changed) "status status-draft" else "status", role = "status", `aria-live` = "polite",
      strong(if (changed) "Draft changed" else "Analysis ready"),
      span(if (changed) "Analyze to apply your edits. Results and export still reflect the last analyzed scenario."
        else "Results match your analyzed scenario."))
  })
  output$metrics <- renderUI({
    req(current()); result <- current()
    matched <- sum(vapply(result$interactions, function(f) f$status == "qualitative_evidence", logical(1)))
    metric <- function(value, label, detail) div(class = "metric", span(class = "metric-label", label),
      strong(class = "metric-value", value), span(class = "metric-detail", detail))
    medication_matches <- sum(vapply(result$medication_interactions,
      function(f) !f$status %in% c("unsupported_pair", "unsupported_form", "unsupported_route", "zero_dose"), logical(1)))
    drug_matches <- sum(vapply(result$drug_interactions,
      function(f) f$status == "label_monitoring_interaction", logical(1)))
    div(class = "metrics",
      metric(nrow(result$events), "Nutrients", "In the analyzed scenario"),
      metric(nrow(result$medications), "Drugs", "In the analyzed scenario"),
      metric(matched, "Nutrient\u2013nutrient", "Evidence matches"),
      metric(medication_matches, "Nutrient\u2013drug", "Evidence matches"),
      metric(drug_matches, "Drug\u2013drug", "Evidence matches"))
  })
  output$timeline <- renderPlot({req(current()); plot_timeline(current())})
  output$network <- renderPlot({req(current()); plot_network(current())})
  output$medication_network <- renderPlot({req(current()); plot_medication_network(current())})
  output$drug_network <- renderPlot({req(current()); plot_drug_network(current())})
  output$curve <- renderPlot({req(current()); plot_phytate_curve(current()$interpolate)})
  output$dose_recency <- renderUI({
    req(current()); data <- intake_recency_table(current())
    tagList(h4("Intake recency"),
      p(class = "note", "Newest first. Age is minutes before the latest entered intake (not the current clock)."),
      tags$table(class = "recency-table",
        tags$thead(tags$tr(lapply(c("Nutrient / event", "Amount", "Consumed", "Age (min)", "Opacity", "Brightness"), tags$th))),
        tags$tbody(lapply(seq_len(nrow(data)), function(i) tags$tr(
          tags$td(tags$span(style = paste0("display:inline-block;width:13px;height:13px;border-radius:50%;margin-right:6px;background:",
            data$display_color[i], ";opacity:", data$alpha[i], ";")),
            data$nutrient[i], tags$br(), tags$small(data$event_id[i])),
          tags$td(paste(data$amount[i], data$unit[i])), tags$td(data$consumed[i]),
          tags$td(format(round(data$age_minutes[i], 2), trim = TRUE)),
          tags$td(paste0(data$opacity_percent[i], "%")), tags$td(paste0(data$brightness_percent[i], "%"))
        )))))
  })
  nutrient_key <- reactive({
    req(current()); events <- current()$events; styles <- event_plot_styles(current())
    rows <- which(!duplicated(events$nutrient))
    tagList(
      div(style = "display:flex;flex-wrap:wrap;gap:8px 18px;margin:12px 0;",
        lapply(rows, function(i) tags$span(
          tags$span(style = paste0("display:inline-block;width:12px;height:12px;border-radius:50%;margin-right:6px;background:", styles$color[i], ";")),
          events$nutrient_name[i]))),
      p(class = "note", "Hue identifies the nutrient. Recency scales opacity from 25% to 100% and brightness from 45% to 100%, relative to the latest intake."))
  })
  output$network_key <- renderUI(nutrient_key())
  output$dose_key <- renderUI(tagList(nutrient_key(),
    p(class = "note", tags$span(style = paste0("display:inline-block;width:12px;height:12px;border-radius:50%;margin-right:6px;border:2px solid ", nutrient_plot_colors("phytate"), ";")),
      "Phytate phosphorus: hollow study-reference dots have no intake time and are not faded.")))
  output$events <- renderTable({
    req(current()); e <- current()$events
    data.frame(Intake = e$id, Nutrient = e$nutrient_name, Amount = paste(e$amount, e$unit),
      Consumed = ifelse(is.na(e$consumed_at) | !nzchar(e$consumed_at), paste0("+", e$time_minutes, " min"), e$consumed_at),
      Meal = e$meal_id, check.names = FALSE)
  }, striped = FALSE, spacing = "m")
  output$findings <- renderTable({
    req(current()); findings <- current()$interactions
    if (!length(findings)) return(data.frame(message = "Add another event to explore interactions."))
    do.call(rbind, lapply(findings, function(f) data.frame(
      source = f$source_id, target = f$target_id, gap_minutes = f$source_minus_target_minutes,
      status = f$status, direction = f$direction, details = f$applicability,
      citation = if (is.null(f$citation)) "" else f$citation$url)))
  }, striped = TRUE, bordered = FALSE, spacing = "s")
  output$medications <- renderTable({
    req(current()); items <- current()$medications
    if (!nrow(items)) return(data.frame(message = "Add a medication to run medication-nutrient checks."))
    data.frame(ID = items$id, Medication = items$medication_name, Route = items$route,
      Consumed = ifelse(is.na(items$consumed_at) | !nzchar(items$consumed_at),
        paste0("+", items$time_minutes, " min"), items$consumed_at), check.names = FALSE)
  }, striped = FALSE, spacing = "m")
  output$medication_findings <- renderTable({
    req(current()); findings <- current()$medication_interactions
    if (!length(findings)) return(data.frame(message = "Add a medication to run medication-nutrient checks."))
    do.call(rbind, lapply(findings, function(f) data.frame(
      medication = f$medication_name, nutrient = f$nutrient_name,
      gap_minutes = f$nutrient_minus_medication_minutes, status = f$status,
      direction = f$direction, rule = if (is.null(f$rule_id)) "" else f$rule_id,
      guidance = if (is.null(f$guidance)) f$applicability else f$guidance,
      source = if (is.null(f$citation)) "" else f$citation$url)))
  }, striped = TRUE, bordered = FALSE, spacing = "s")
  output$drug_findings <- renderTable({
    req(current()); findings <- current()$drug_interactions
    if (!length(findings)) return(data.frame(message = "Add at least two drugs to run drug-drug checks."))
    do.call(rbind, lapply(findings, function(f) data.frame(
      drug_a = f$drug_a_name, drug_b = f$drug_b_name,
      gap_minutes = f$time_gap_minutes, status = f$status, direction = f$direction,
      rule = if (is.null(f$rule_id)) "" else f$rule_id,
      guidance = if (is.null(f$guidance)) f$applicability else f$guidance,
      source = if (is.null(f$citation)) "" else f$citation$url)))
  }, striped = TRUE, bordered = FALSE, spacing = "s")
  output$sources <- renderTable(read_evidence()[, c("rule_id", "dose_basis", "scope", "sample_size", "url", "limitation")])
  output$medication_sources <- renderTable(read_medication_evidence()[,
    c("rule_id", "medication_name", "nutrient_key", "scope", "clinical_significance", "url", "limitation")])
  output$drug_sources <- renderTable(read_drug_evidence()[,
    c("rule_id", "drug_a_name", "drug_b_name", "clinical_significance", "url", "limitation")])
  output$llm_status <- renderUI({
    input$refresh_llm
    input$explain
    status <- llm_status()
    div(class = if (status$available) "status" else "status status-draft",
      role = "status", `aria-live` = "polite",
      strong(if (status$available) "Local AI ready" else "Local AI unavailable"),
      span(if (status$available) paste("Using", status$model) else status$message))
  })
  observeEvent(input$explain, {
    req(current()); explanation_error(NULL); explanation(NULL)
    session$sendCustomMessage("ai-busy", TRUE)
    tryCatch(withProgress(message = "Generating a local explanation", detail = "Checking each matched finding against the analysis...", value = .15, {
      item <- generate_llm_explanation(current())
      incProgress(.85); explanation(item)
    }), error = function(e) explanation_error(conditionMessage(e)),
      finally = session$sendCustomMessage("ai-busy", FALSE))
  })
  output$llm_error <- renderUI({
    if (is.null(explanation_error())) return(NULL)
    div(class = "status status-error", role = "alert", explanation_error())
  })
  output$llm_explanation <- renderUI({
    item <- explanation()
    if (is.null(item)) return(div(class = "llm-placeholder",
      "Select Generate explanation after analyzing a scenario."))
    safe_markdown <- htmltools::htmlEscape(item$text)
    tagList(
      div(class = "llm-report", shiny::markdown(safe_markdown)),
      div(class = "llm-meta",
        span(class = "llm-meta-label", if (identical(item$generation_mode, "evidence_only")) "Evidence summary (no AI findings to explain)" else "Generated locally"),
        span(class = "llm-meta-value", item$model),
        span(class = "llm-meta-label", "Grounded rules"),
        span(class = "llm-meta-value",
          if (length(item$grounded_rule_ids)) paste(item$grounded_rule_ids, collapse = " \u00b7 ") else "none"))
    )
  })
  output$json <- renderText({req(current()); as.character(result_json(current()))})
  output$download <- downloadHandler(filename = function() "nutrient-interactions.json",
    content = function(file) {req(current()); writeLines(result_json(current()), file)},
    contentType = "application/json")
  food_raw <- reactive({
    path <- file.path(project_root, "data-raw", "kaggle-manifest.json")
    validate(need(file.exists(path), "Food dataset unavailable. Run scripts/acquire_kaggle.R."))
    manifest <- jsonlite::fromJSON(path)
    csv <- file.path(project_root, manifest$csv_path)
    validate(need(file.exists(csv), "Food dataset missing. Run scripts/acquire_kaggle.R."))
    validate(need(unname(tools::md5sum(csv)) == manifest$csv_md5, "Food dataset checksum mismatch."))
    utils::read.csv(csv, check.names = FALSE, stringsAsFactors = FALSE)
  })
  food_selected <- reactive({req(current()); selected_food_correlations(current()$events, food_raw(), catalog)})
  output$food <- renderUI({
    data <- food_selected()
    tagList(p(paste(length(data$labels), "of", data$selected_count, "selected nutrients available across",
      format(data$rows, big.mark = ","), "foods / per 100 g / Spearman correlation")),
      if (length(data$unavailable)) p(class = "note", "Not available in this food dataset: ", paste(data$unavailable, collapse = ", ")),
      if (length(data$labels) < 2L) p("Add at least two nutrients with food data, then analyze your scenario.")
      else tagList(echarts4r::echarts4rOutput("food_heatmap", height = paste0(max(400, length(data$labels) * 24), "px")),
        p(class = "note", "Blank cells have insufficient or constant data; they do not mean zero correlation. Exact coefficients and sample counts are in the table."), tableOutput("food_pairs")))
  })
  output$food_heatmap <- echarts4r::renderEcharts4r({
    data <- food_selected(); req(length(data$labels) >= 2L)
    cells <- list()
    for (i in seq_along(data$labels)) for (j in seq_along(data$labels)) {
      value <- data$correlations[i, j]
      if (!is.na(value)) cells[[length(cells) + 1L]] <- list(value = list(i - 1L, j - 1L, value),
        name = paste(data$labels[i], "/", data$labels[j], "\u2014 rho:", round(value, 3), "/ foods:", data$pair_counts[i, j]))
    }
    explorer_widget(list(grid = list(left = 15, right = 30, top = 20, bottom = 100, containLabel = TRUE),
      xAxis = list(type = "category", data = as.list(data$labels), axisLabel = list(rotate = 35)),
      yAxis = list(type = "category", data = as.list(data$labels)),
      visualMap = list(min = -1, max = 1, orient = "horizontal", left = "center", bottom = 0,
        inRange = list(color = list("#b55b45", "#f6f7f3", "#17694f"))),
      series = list(list(type = "heatmap", data = cells))))
  })
  output$food_pairs <- renderTable({
    data <- food_selected(); req(length(data$labels) >= 2L)
    data$pairs
  }, digits = 3, spacing = "m")
  source(file.path(project_root, "app", "explorer-server.R"), local = TRUE, encoding = "UTF-8")
}

shinyApp(ui, server)
