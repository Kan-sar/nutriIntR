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

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body {background:#f2f6f8;color:#163d4f;font-family:'Segoe UI',sans-serif}
    .container-fluid {max-width:1500px;padding:24px}
    .hero {background:#163d4f;color:white;padding:26px 32px;border-radius:18px;margin-bottom:22px}
    .hero h1 {font-size:32px;margin:6px 0 10px;font-weight:650}
    .eyebrow {color:#7addce;font-size:12px;letter-spacing:2px;text-transform:uppercase}
    .well {background:white;border:1px solid #dce6ec;border-radius:14px;box-shadow:none}
    .panel-body {background:white;border-radius:12px}
    .btn-primary {background:#128b7c;border-color:#128b7c}
    .nav-tabs>li.active>a {color:#128b7c;font-weight:600}
    textarea {font-family:Consolas,monospace!important;font-size:12px!important}
    .note {font-size:13px;color:#526b78;line-height:1.6}
    .status {padding:14px 18px;background:#e4f4f0;border-radius:10px;margin-bottom:16px}
    table {font-size:12px} .tab-content {padding-top:18px}
    #events, #findings, #sources {overflow-x:auto}
  "))),
  div(class = "hero", div(class = "eyebrow", "Nutrient interaction engine / research preview"),
    h1("See how nutrients meet."),
    p("Explore amounts, chemical forms and chronology through human-study evidence.")),
  sidebarLayout(
    sidebarPanel(width = 4,
      h4("Build a scenario"),
      selectizeInput("nutrient", "Nutrient — search name or USDA ID", nutrient_choices, selected = "iron"),
      textOutput("nutrient_hint"),
      selectInput("form", "Chemical form", nutrient_forms("iron")),
      fluidRow(column(6, numericInput("amount", "Amount", 6, min = 0)),
               column(6, selectInput("unit", "Unit", c("mg", "g", "mcg")))),
      radioButtons("timing_mode", "When was it consumed?", c("Time consumed" = "clock", "Minutes from scenario start" = "offset"), selected = "clock"),
      conditionalPanel("input.timing_mode == 'offset'", numericInput("minute", "Minute", 0, min = 0)),
      conditionalPanel("input.timing_mode == 'clock'",
        tags$div(class = "form-group", tags$label(`for` = "consumed_time", "Time consumed"),
          tags$input(id = "consumed_time", type = "time", class = "form-control",
            value = clock_default, step = "60",
            onchange = "Shiny.setInputValue('consumed_time', this.value, {priority:'event'});")),
        p(class = "note", "Select the time on the clock. Times in one scenario refer to the same day.")),
      textInput("meal", "Meal ID", "lunch"),
      actionButton("add", "Add event to draft"),
      hr(),
      textAreaInput("scenario", "Scenario JSON", example_text, height = "260px"),
      actionButton("analyze", "Analyze scenario", class = "btn-primary"),
      actionButton("reset", "Reset example"),
      br(), br(), downloadButton("download", "Download result JSON"),
      p(class = "note", "477 USDA definitions are available. Use each measure's native unit; activity units and equivalents are retained. Most pairs do not yet have curated interaction evidence. Edit JSON to remove events or change context.")
    ),
    mainPanel(width = 8,
      uiOutput("status"),
      tabsetPanel(
        tabPanel("Timeline", plotOutput("timeline", height = "390px"), tableOutput("events")),
        tabPanel("Interaction map", plotOutput("network", height = "500px"), uiOutput("network_key"),
          p(class = "note", "Arrows show matched evidence. Dashed gray arrows have unresolved applicability. Missing arrows indicate catalog gaps.")),
        tabPanel("Findings", tableOutput("findings")),
        tabPanel("Dose reference", plotOutput("curve", height = "650px"), uiOutput("dose_key"),
          p(class = "note", "Study observations describe relative iron absorption in wheat-roll experiments. They are not a prediction for your entered meal. Dashed interpolation, when enabled in JSON, is exploratory.")),
        tabPanel("Sources", tableOutput("sources"),
          p(class = "note", "Abstract-level extraction; formal evidence grading and independent validation are pending. No numerical aggregate or absolute bioavailability is inferred.")),
        tabPanel("USDA catalog", p("All 477 definitions from USDA nutrient.csv, April 2026."),
          textInput("catalog_search", "Filter definitions by name, ID or unit", ""),
          textOutput("catalog_count"), tableOutput("catalog_table"),
          p(class = "note", "First 50 matching definitions are shown. Refine the filter to inspect others. Catalog membership does not imply interaction evidence.")),
        tabPanel("Food correlations", uiOutput("food")),
        tabPanel("JSON", verbatimTextOutput("json"))
      )
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
  run <- function(text) {
    tryCatch({
      if (nchar(text, type = "bytes") > 1024 * 1024) stop("Scenario exceeds 1 MB.")
      payload <- jsonlite::fromJSON(text, simplifyVector = FALSE)
      if (is.null(payload$context) || is.null(payload$events)) stop("Supply context and events.")
      flag <- if (is.null(payload$interpolate)) FALSE else payload$interpolate
      current(analyze_interactions(payload$events, payload$context, flag)); error(NULL)
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
    if (!is.null(error())) return(div(class = "status", strong("Check scenario: "), error()))
    req(current()); result <- current()
    count <- sum(vapply(result$interactions, function(f) f$status == "qualitative_evidence", logical(1)))
    div(class = "status", strong(paste(nrow(result$events), "events /", count, "matched findings")),
      br(), "Absolute bioavailability: unavailable. Findings below show study evidence and unresolved cases.")
  })
  output$timeline <- renderPlot({req(current()); plot_timeline(current())})
  output$network <- renderPlot({req(current()); plot_network(current())})
  output$curve <- renderPlot({req(current()); plot_phytate_curve(current()$interpolate, result = current())})
  nutrient_key <- reactive({
    req(current()); events <- current()$events; styles <- event_plot_styles(current())
    rows <- which(!duplicated(events$nutrient))
    tagList(
      div(style = "display:flex;flex-wrap:wrap;gap:8px 18px;margin:12px 0;",
        lapply(rows, function(i) tags$span(
          tags$span(style = paste0("display:inline-block;width:12px;height:12px;border-radius:50%;margin-right:6px;background:", styles$color[i], ";")),
          events$nutrient_name[i]))),
      p(class = "note", "Dot color identifies the nutrient in both views. Opacity is relative to the latest entered intake: oldest 25%, latest 100%; equal times share opacity. This is a recency display, not an absorption estimate."))
  })
  output$network_key <- renderUI(nutrient_key())
  output$dose_key <- renderUI(tagList(nutrient_key(),
    p(class = "note", tags$span(style = paste0("display:inline-block;width:12px;height:12px;border-radius:50%;margin-right:6px;border:2px solid ", nutrient_plot_colors("phytate"), ";")),
      "Phytate phosphorus: hollow study-reference dots have no intake time and are not faded.")))
  output$events <- renderTable({req(current()); current()$events[, c("id", "consumed_at", "nutrient_name", "amount", "unit", "usda_nutrient_id", "time_minutes", "meal_id")]})
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
