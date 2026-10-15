library(shiny)
project_root <- if (file.exists("scripts/load_project.R")) "." else ".."
for (file in list.files(file.path(project_root, "R"), pattern = "\\.R$", full.names = TRUE)) source(file, local = TRUE)
# Development source lookup is relative to the app working directory.
engine_file <- function(name) file.path(project_root, "inst", "extdata", name)
example_text <- paste(readLines(engine_file("example-clock.json")), collapse = "\n")
catalog <- nutrient_catalog()
choice_labels <- nutrient_display_names(catalog)
choice_order <- order(tolower(choice_labels))
nutrient_choices <- stats::setNames(catalog$key[choice_order], choice_labels[choice_order])
nutrient_choices <- c(nutrient_choices, "Phytate phosphorus" = "phytate")
medication_data <- medication_catalog()
medication_choices <- stats::setNames(medication_data$key, medication_data$name)
clock_default <- "08:00"

clock_input <- function(local) {
  if (is.null(local) || !nzchar(local)) local <- clock_default
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
    normalized <- normalize_events(payload$events)
    normalized_medications <- normalize_medications(payload$medications)
    aligned <- align_medication_chronology(normalized, normalized_medications)
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

ui <- fluidPage(
  title = "Nutrient Lab | Interaction engine",
  tags$head(tags$link(rel = "stylesheet", href = "style.css"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")),
  tags$a(class = "skip-link", href = "#workspace", "Skip to results"),
  tags$header(class = "app-header",
    div(class = "brand", span(class = "brand-mark", "n"),
      div(strong("Nutrient Lab"))),
    div(class = "header-meta", span("477 nutrients / 6 drugs"))),
  div(class = "page-intro", div(div(class = "eyebrow", "Explore / compare / understand"),
    h1("One timeline for nutrients and drugs."),
    p("Compare nutrient–nutrient, nutrient–drug and drug–drug evidence in one place."))),
  div(class = "workspace-grid",
    tags$aside(class = "composer", `aria-label` = "Scenario builder",
      div(class = "section-title", span(class = "step-label", "01"), h2("Build your scenario")),
      p(class = "note", "Build one timeline with separate nutrient and drug selectors, then analyze all three pair types."),
      div(class = "composer-group nutrient-builder", h3("Add a nutrient"),
        selectizeInput("nutrient", "Nutrient dropdown", nutrient_choices, selected = "iron"),
        div(class = "field-hint", textOutput("nutrient_hint")),
        selectInput("form", "Chemical form", nutrient_forms("iron")),
        fluidRow(column(6, numericInput("amount", "Amount", 6, min = 0)),
                 column(6, selectInput("unit", "Unit", c("mg", "g", "mcg")))),
        textInput("meal", "Meal label", "lunch"),
        actionButton("add", "+ Add nutrient to draft", class = "btn-add")),
      div(class = "field-divider", "Shared timing"),
      radioButtons("timing_mode", "Timing", c("Clock time" = "clock", "Minutes from start" = "offset"), selected = "clock", inline = TRUE),
      conditionalPanel("input.timing_mode == 'offset'", numericInput("minute", "Minute", 0, min = 0)),
      conditionalPanel("input.timing_mode == 'clock'",
        tags$div(class = "form-group", tags$label(`for` = "consumed_time", "Time consumed"),
          tags$input(id = "consumed_time", type = "time", class = "form-control",
            value = clock_default, step = "60",
            onchange = "Shiny.setInputValue('consumed_time', this.value, {priority:'event'});")),
        p(class = "field-hint", "Local time. All clock entries refer to the same day.")),
      div(class = "composer-group drug-builder", h3("Add a drug"),
        p(class = "note", "Drug checks use selected official-label rules and the same timeline."),
        selectizeInput("drug", "Drug dropdown", medication_choices),
        selectInput("medication_route", "Route", c("oral"), selected = "oral"),
        conditionalPanel("input.timing_mode == 'offset'", numericInput("medication_minute", "Medication minute", 0, min = 0)),
        conditionalPanel("input.timing_mode == 'clock'",
          tags$div(class = "form-group", tags$label(`for` = "medication_time", "Medication time"),
            tags$input(id = "medication_time", type = "time", class = "form-control",
              value = "08:30", step = "60",
              onchange = "Shiny.setInputValue('medication_time', this.value, {priority:'event'});"))),
        actionButton("add_medication", "+ Add drug to draft", class = "btn-add")),
      tags$details(class = "json-editor", tags$summary("Advanced / edit scenario JSON"),
        p(class = "note", "Edit or remove events and change study context here. Use dated local times for overnight scenarios."),
        textAreaInput("scenario", "Scenario JSON", example_text, height = "260px")),
      div(class = "composer-actions", textOutput("draft_summary"),
        actionButton("analyze", "Analyze scenario", class = "btn-primary"),
        actionButton("reset", "Restore example", class = "btn-reset"))
    ),
    tags$main(id = "workspace", tabindex = "-1", class = "results-workspace",
      div(class = "workspace-toolbar", div(class = "section-title", span(class = "step-label", "02"), h2("Explore results")),
        downloadButton("download", "Export JSON")),
      uiOutput("status"),
      uiOutput("metrics"),
      div(class = "analysis-card",
      tabsetPanel(
        id = "analysis_view",
        tabPanel("Timeline", view_heading("Intake timeline", "Every intake in order, with amounts and local consumption times."),
          plotOutput("timeline", height = "340px"), h3("Analyzed intakes"), tableOutput("events")),
        tabPanel("Nutrient–nutrient", view_heading("Nutrient–nutrient interactions", "Explore every directed nutrient pair, with matched evidence and explicit unknowns."),
          plotOutput("network", height = "460px"), uiOutput("network_key"),
          h3("Directed nutrient pairs"), tableOutput("findings"),
          p(class = "note", "Arrows show matched evidence. Dashed gray arrows have unresolved applicability. Missing arrows indicate catalog gaps.")),
        tabPanel("Nutrient–drug", view_heading("Nutrient–drug interactions", "Selected label-based interactions with timing and scope kept explicit."),
          plotOutput("medication_network", height = "440px"), tableOutput("medications"),
          h3("Matched and unresolved pairs"), tableOutput("medication_findings"),
          p(class = "note", "This research prototype does not replace a pharmacist, prescriber or product-specific medication instructions.")),
        tabPanel("Drug–drug", view_heading("Drug–drug interactions", "Compare every selected drug pair against the small official-label catalog."),
          plotOutput("drug_network", height = "420px"), h3("Selected drug pairs"), tableOutput("drug_findings"),
          p(class = "note", "Uncataloged means unknown in this version, not safe. Never change medication without a qualified clinician.")),
        tabPanel("Dose reference", view_heading("Dose in context", "Study observations and the recency of your intakes, kept distinct."),
          plotOutput("curve", height = "360px"), uiOutput("dose_recency"), uiOutput("dose_key"),
          p(class = "note", "Relative iron absorption in wheat-roll experiments. Enable interpolation in scenario JSON to connect the study observations.")),
        tabPanel("Sources", view_heading("Trace the evidence", "Study scope, dose basis and source details."),
          h3("Nutrient evidence"), tableOutput("sources"),
          h3("Medication label evidence"), tableOutput("medication_sources"),
          h3("Drug-drug label evidence"), tableOutput("drug_sources")),
        tabPanel("USDA catalog", view_heading("The nutrient library", "All 477 definitions from USDA nutrient.csv, April 2026."),
          textInput("catalog_search", "Filter definitions by name, ID or unit", ""),
          textOutput("catalog_count"), tableOutput("catalog_table"),
          p(class = "note", "First 50 matches shown. Refine the filter to find more.")),
        tabPanel("Food correlations", view_heading("Your nutrients across foods", "A custom correlation matrix for the nutrients in your analyzed scenario."), uiOutput("food")),
        tabPanel("AI explanation", view_heading("Grounded explanation", "DeepSeek explains the deterministic findings and visual encodings without creating new interaction claims."),
          uiOutput("llm_status"), actionButton("explain", "Generate explanation", class = "btn-primary"),
          uiOutput("llm_error"), verbatimTextOutput("llm_explanation"),
          p(class = "note", "Model output can be wrong. Verify medication decisions against the linked label and a qualified clinician.")),
        tabPanel("JSON", view_heading("Structured result", "The same analysis, ready for reproducible workflows and downstream tools."), verbatimTextOutput("json"))
      ))
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$nutrient, {
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
  analyzed_text <- reactiveVal(example_text)
  output$draft_summary <- renderText({
    tryCatch({
      draft <- jsonlite::fromJSON(input$scenario, simplifyVector = FALSE)
      paste(length(draft$events), "nutrients /", length(draft$medications), "drugs in draft")
    }, error = function(e) "Check draft JSON")
  })
  run <- function(text) {
    tryCatch({
      if (nchar(text, type = "bytes") > 1024 * 1024) stop("Scenario exceeds 1 MB.")
      payload <- jsonlite::fromJSON(text, simplifyVector = FALSE)
      if (is.null(payload$context) || is.null(payload$events)) stop("Supply context and events.")
      flag <- if (is.null(payload$interpolate)) FALSE else payload$interpolate
      current(analyze_interactions(payload$events, payload$context, flag, payload$medications));
      error(NULL); analyzed_text(text); explanation(NULL); explanation_error(NULL)
    }, error = function(e) {current(NULL); error(conditionMessage(e))})
  }
  observeEvent(TRUE, run(example_text), once = TRUE)
  observeEvent(input$analyze, run(input$scenario))
  observeEvent(input$reset, {updateTextAreaInput(session, "scenario", value = example_text); run(example_text)})
  observeEvent(input$add, {
    tryCatch({
      payload <- jsonlite::fromJSON(input$scenario, simplifyVector = FALSE)
      existing <- vapply(payload$events, function(e) e$id, character(1))
      index <- length(existing) + 1L
      while (paste0("event_", index) %in% existing) index <- index + 1L
      origin <- if (input$timing_mode == "clock") clock_input(input$consumed_time) else NULL
      payload <- prepare_timing(payload, input$timing_mode, origin)
      event <- list(id = paste0("event_", index), nutrient = input$nutrient,
        form = input$form, amount = input$amount, unit = input$unit,
        meal_id = input$meal)
      if (input$timing_mode == "clock") event$consumed_at <- origin else event$time_minutes <- input$minute
      payload$events <- append(payload$events, list(event))
      normalize_events(payload$events)
      updateTextAreaInput(session, "scenario", value = as.character(result_json(payload)))
      showNotification("Added to draft. Click Analyze scenario to refresh results.", type = "message")
    }, error = function(e) showNotification(conditionMessage(e), type = "error"))
  })
  observeEvent(input$add_medication, {
    tryCatch({
      payload <- jsonlite::fromJSON(input$scenario, simplifyVector = FALSE)
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
      aligned <- align_medication_chronology(normalize_events(payload$events), normalize_medications(payload$medications))
      updateTextAreaInput(session, "scenario", value = as.character(result_json(payload)))
      showNotification("Medication added to the draft. Analyze to refresh results.", type = "message")
    }, error = function(e) showNotification(conditionMessage(e), type = "error"))
  })
  output$status <- renderUI({
    if (!is.null(error())) return(div(class = "status status-error", role = "alert", strong("Check scenario: "), error()))
    req(current()); result <- current()
    changed <- !is.null(input$scenario) && !identical(input$scenario, analyzed_text())
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
      metric(matched, "Nutrient–nutrient", "Evidence matches"),
      metric(medication_matches, "Nutrient–drug", "Evidence matches"),
      metric(drug_matches, "Drug–drug", "Evidence matches"))
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
    status <- llm_status()
    div(class = if (status$available) "status" else "status status-draft",
      strong(if (status$available) "DeepSeek ready" else "DeepSeek unavailable"),
      span(paste(status$model, "at", status$endpoint, "-", status$message)))
  })
  observeEvent(input$explain, {
    req(current()); explanation_error(NULL)
    tryCatch(explanation(generate_llm_explanation(current())),
      error = function(e) { explanation(NULL); explanation_error(conditionMessage(e)) })
  })
  output$llm_error <- renderUI({
    if (is.null(explanation_error())) return(NULL)
    div(class = "status status-error", role = "alert", explanation_error())
  })
  output$llm_explanation <- renderText({
    item <- explanation()
    if (is.null(item)) return("Select Generate explanation after analyzing a scenario.")
    paste0(item$text, "\n\nModel: ", item$model, "\nGrounded rule IDs: ",
      if (length(item$grounded_rule_ids)) paste(item$grounded_rule_ids, collapse = ", ") else "none")
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
      else tagList(plotOutput("food_heatmap", height = paste0(max(500, length(data$labels) * 24), "px")), tableOutput("food_pairs")))
  })
  output$food_heatmap <- renderPlot({
    data <- food_selected(); req(length(data$labels) >= 2L)
    plot_food_correlations(data)
  })
  output$food_pairs <- renderTable({
    data <- food_selected(); req(length(data$labels) >= 2L)
    data$pairs
  }, digits = 3, spacing = "m")
}

shinyApp(ui, server)
