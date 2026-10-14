library(shiny)
project_root <- if (file.exists("scripts/load_project.R")) "." else ".."
for (file in list.files(file.path(project_root, "R"), pattern = "\\.R$", full.names = TRUE)) source(file, local = TRUE)
# Development source lookup is relative to the app working directory.
engine_file <- function(name) file.path(project_root, "inst", "extdata", name)
example_text <- paste(readLines(engine_file("example-clock.json")), collapse = "\n")
catalog <- nutrient_catalog()
nutrient_choices <- stats::setNames(catalog$key,
  paste0(catalog$name, " | USDA ", catalog$id, " | ", catalog$unit_name))
nutrient_choices <- c(nutrient_choices, "Phytate phosphorus | study extension | mg" = "phytate")
clock_default <- "08:00"

clock_input <- function(local) {
  if (is.null(local) || !nzchar(local)) local <- clock_default
  parse_consumed_at(local)
  local
}

prepare_timing <- function(payload, mode, origin) {
  if (!length(payload$events)) return(payload)
  if (mode == "clock") {
    has_clock <- vapply(payload$events, function(e) !is.null(e$consumed_at) && nzchar(e$consumed_at), logical(1))
    if (all(has_clock)) return(payload)
    if (any(has_clock)) stop("Mixed consumption timestamps and offsets: supply a timestamp for every event.")
    base <- parse_consumed_at(origin)
    payload$events <- lapply(payload$events, function(e) {
      if (is.null(e$time_minutes) || !is.numeric(e$time_minutes) || !is.finite(e$time_minutes) || e$time_minutes < 0)
        stop("Every offset event requires nonnegative time_minutes.")
      seconds <- base + e$time_minutes * 60
      if (seconds >= 86400 || seconds %% 1 != 0)
        stop("Time-only conversion requires same-day, whole-second values. Use dated local times in JSON for overnight scenarios.")
      e$consumed_at <- format_local_time(seconds)
      e$time_minutes <- NULL
      e
    })
  } else {
    normalized <- normalize_events(payload$events)
    payload$events <- lapply(payload$events, function(e) {
      e$time_minutes <- normalized$time_minutes[match(e$id, normalized$id)]
      e$consumed_at <- NULL
      e
    })
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
      div(strong("Nutrient Lab"), span(class = "brand-subtitle", "Interaction research workspace"))),
    div(class = "header-meta", span(class = "preview-badge", "Research preview"),
      span("USDA catalog / 477 definitions"))),
  div(class = "page-intro", div(div(class = "eyebrow", "Explore / compare / understand"),
    h1("A clearer view of nutrient interactions."),
    p("Connect what you consumed with when you consumed it. Explore the evidence, not assumptions."))),
  div(class = "workspace-grid",
    tags$aside(class = "composer", `aria-label` = "Scenario builder",
      div(class = "section-title", span(class = "step-label", "01"), h2("Build your scenario")),
      p(class = "note", "Add an intake, then analyze your draft."),
      selectizeInput("nutrient", "Nutrient", nutrient_choices, selected = "iron"),
      div(class = "field-hint", textOutput("nutrient_hint")),
      selectInput("form", "Chemical form", nutrient_forms("iron")),
      fluidRow(column(6, numericInput("amount", "Amount", 6, min = 0)),
               column(6, selectInput("unit", "Unit", c("mg", "g", "mcg")))),
      div(class = "field-divider", "Intake details"),
      radioButtons("timing_mode", "Timing", c("Clock time" = "clock", "Minutes from start" = "offset"), selected = "clock", inline = TRUE),
      conditionalPanel("input.timing_mode == 'offset'", numericInput("minute", "Minute", 0, min = 0)),
      conditionalPanel("input.timing_mode == 'clock'",
        tags$div(class = "form-group", tags$label(`for` = "consumed_time", "Time consumed"),
          tags$input(id = "consumed_time", type = "time", class = "form-control",
            value = clock_default, step = "60",
            onchange = "Shiny.setInputValue('consumed_time', this.value, {priority:'event'});")),
        p(class = "field-hint", "Local time. All clock entries refer to the same day.")),
      textInput("meal", "Meal label", "lunch"),
      actionButton("add", "+ Add intake to draft", class = "btn-add"),
      tags$details(class = "json-editor", tags$summary("Advanced / edit scenario JSON"),
        p(class = "note", "Edit or remove events and change study context here. Use dated local times for overnight scenarios."),
        textAreaInput("scenario", "Scenario JSON", example_text, height = "260px")),
      div(class = "composer-actions", textOutput("draft_summary"),
        actionButton("analyze", "Analyze scenario", class = "btn-primary"),
        actionButton("reset", "Restore example", class = "btn-reset")),
      p(class = "field-hint", "Catalog coverage is not evidence coverage. Most nutrient pairs have no curated interaction evidence.")
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
        tabPanel("Interaction map", view_heading("How the nutrients connect", "Directed links show evidence matches, not a combined absorption prediction."),
          plotOutput("network", height = "460px"), uiOutput("network_key"),
          p(class = "note", "Arrows show matched evidence. Dashed gray arrows have unresolved applicability. Missing arrows indicate catalog gaps.")),
        tabPanel("Findings", view_heading("Evidence & applicability", "Inspect each directed pair, including unresolved cases and source citations."), tableOutput("findings")),
        tabPanel("Dose reference", view_heading("Dose in context", "Study observations and the recency of your intakes, kept distinct."),
          plotOutput("curve", height = "360px"), uiOutput("dose_recency"), uiOutput("dose_key"),
          p(class = "note", "Study observations describe relative iron absorption in wheat-roll experiments. They are not a prediction for your entered meal. Dashed interpolation, when enabled in JSON, is exploratory.")),
        tabPanel("Sources", view_heading("Trace the evidence", "Review study scope, dose basis and limitations before interpreting a finding."), tableOutput("sources"),
          p(class = "note", "Abstract-level extraction; formal evidence grading and independent validation are pending. No numerical aggregate or absolute bioavailability is inferred.")),
        tabPanel("USDA catalog", view_heading("The nutrient library", "All 477 definitions from USDA nutrient.csv, April 2026."),
          textInput("catalog_search", "Filter definitions by name, ID or unit", ""),
          textOutput("catalog_count"), tableOutput("catalog_table"),
          p(class = "note", "First 50 matching definitions are shown. Refine the filter to inspect others. Catalog membership does not imply interaction evidence.")),
        tabPanel("Food correlations", view_heading("Patterns across foods", "Nutrient co-occurrence is not evidence of absorption effects."), uiOutput("food")),
        tabPanel("JSON", view_heading("Structured result", "The same analysis, ready for reproducible workflows and downstream tools."), verbatimTextOutput("json"))
      ))
    )
  ),
  tags$footer(class = "app-footer", span("Nutrient Lab / Evidence before inference"),
    span("Research tool. Not a clinical or personalized nutrition recommendation."))
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
    paste(selected$name, if (!is.na(selected$id)) paste0("(USDA ", selected$id, ")") else "",
      "- native unit:", selected$unit_name)
  })
  catalog_filtered <- reactive({
    query <- tolower(trimws(input$catalog_search))
    catalog[grepl(query, tolower(paste(catalog$id, catalog$name, catalog$unit_name)), fixed = TRUE), ]
  })
  output$catalog_count <- renderText(paste(nrow(catalog_filtered()), "of", nrow(catalog), "definitions match."))
  output$catalog_table <- renderTable(utils::head(catalog_filtered()[, c("id", "name", "unit_name", "nutrient_nbr", "definition_status")], 50))
  current <- reactiveVal(NULL)
  error <- reactiveVal(NULL)
  analyzed_text <- reactiveVal(example_text)
  output$draft_summary <- renderText({
    tryCatch({
      draft <- jsonlite::fromJSON(input$scenario, simplifyVector = FALSE)
      paste(length(draft$events), "intakes in draft")
    }, error = function(e) "Check draft JSON")
  })
  run <- function(text) {
    tryCatch({
      if (nchar(text, type = "bytes") > 1024 * 1024) stop("Scenario exceeds 1 MB.")
      payload <- jsonlite::fromJSON(text, simplifyVector = FALSE)
      if (is.null(payload$context) || is.null(payload$events)) stop("Supply context and events.")
      flag <- if (is.null(payload$interpolate)) FALSE else payload$interpolate
      current(analyze_interactions(payload$events, payload$context, flag)); error(NULL); analyzed_text(text)
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
  output$status <- renderUI({
    if (!is.null(error())) return(div(class = "status status-error", role = "alert", strong("Check scenario: "), error()))
    req(current()); result <- current()
    changed <- !is.null(input$scenario) && !identical(input$scenario, analyzed_text())
    div(class = if (changed) "status status-draft" else "status", role = "status", `aria-live` = "polite",
      strong(if (changed) "Draft changed" else "Analysis ready"),
      span(if (changed) "Analyze to apply your edits. Results and export still reflect the last analyzed scenario."
        else "Study evidence only. Absolute bioavailability is not estimated."))
  })
  output$metrics <- renderUI({
    req(current()); result <- current()
    matched <- sum(vapply(result$interactions, function(f) f$status == "qualitative_evidence", logical(1)))
    metric <- function(value, label, detail) div(class = "metric", span(class = "metric-label", label),
      strong(class = "metric-value", value), span(class = "metric-detail", detail))
    div(class = "metrics",
      metric(nrow(result$events), "Intake events", "In the analyzed scenario"),
      metric(matched, "Evidence matches", "Applicable qualitative findings"),
      metric(length(result$interactions) - matched, "Unresolved pairs", "No applicable qualitative finding"))
  })
  output$timeline <- renderPlot({req(current()); plot_timeline(current())})
  output$network <- renderPlot({req(current()); plot_network(current())})
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
      p(class = "note", "Dot hue identifies the nutrient. Relative to the latest entered intake, opacity increases from 25% to 100% and color brightness from 45% to 100%. Equal times share both values. These are display settings, not absorption estimates."))
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
  output$sources <- renderTable(read_evidence()[, c("rule_id", "dose_basis", "scope", "sample_size", "url", "limitation")])
  output$json <- renderText({req(current()); as.character(result_json(current()))})
  output$download <- downloadHandler(filename = function() "nutrient-interactions.json",
    content = function(file) {req(current()); writeLines(result_json(current()), file)},
    contentType = "application/json")
  output$food <- renderUI({
    path <- file.path(project_root, "reports", "generated", "food-analysis.json")
    if (!file.exists(path)) return(p("Run scripts/acquire_kaggle.R and scripts/analyze_foods.R to generate the food analysis."))
    summary <- jsonlite::fromJSON(path)
    tagList(p(paste(format(summary$rows, big.mark = ","), "Kaggle food records; nutrient composition per 100 g.")),
      p(class = "note", "Spearman correlations describe co-occurrence among foods. They do not measure absorption or establish nutrient interactions."),
      imageOutput("heatmap", height = "600px"))
  })
  output$heatmap <- renderImage({
    path <- file.path(project_root, "reports", "generated", "food-correlation.png")
    req(file.exists(path)); list(src = normalizePath(path), contentType = "image/png", width = "100%")
  }, deleteFile = FALSE)
}

shinyApp(ui, server)
