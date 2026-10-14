library(shiny)
project_root <- if (file.exists("scripts/load_project.R")) "." else ".."
for (file in list.files(file.path(project_root, "R"), pattern = "\\.R$", full.names = TRUE)) source(file, local = TRUE)
# Development source lookup is relative to the app working directory.
engine_file <- function(name) file.path(project_root, "inst", "extdata", name)
example_text <- paste(readLines(engine_file("example.json")), collapse = "\n")

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
      selectInput("nutrient", "Nutrient", c("iron", "vitamin_c", "calcium", "phytate", "zinc", "copper")),
      selectInput("form", "Chemical form", "non_heme_food"),
      fluidRow(column(6, numericInput("amount", "Amount", 6, min = 0)),
               column(6, selectInput("unit", "Unit", c("mg", "g", "mcg")))),
      fluidRow(column(6, numericInput("minute", "Minute", 0, min = 0)),
               column(6, textInput("meal", "Meal ID", "lunch"))),
      actionButton("add", "Add event to draft"),
      hr(),
      textAreaInput("scenario", "Scenario JSON", example_text, height = "260px"),
      actionButton("analyze", "Analyze scenario", class = "btn-primary"),
      actionButton("reset", "Reset example"),
      br(), br(), downloadButton("download", "Download result JSON"),
      p(class = "note", "Amounts for minerals are elemental amounts. The phytate reference uses mg phytate phosphorus. Edit the JSON to remove events or change the population, meal state or interpolation option.")
    ),
    mainPanel(width = 8,
      uiOutput("status"),
      tabsetPanel(
        tabPanel("Timeline", plotOutput("timeline", height = "390px"), tableOutput("events")),
        tabPanel("Interaction map", plotOutput("network", height = "500px"),
          p(class = "note", "Arrows show matched evidence. Dashed gray arrows have unresolved applicability. Missing arrows indicate catalog gaps.")),
        tabPanel("Findings", tableOutput("findings")),
        tabPanel("Dose reference", plotOutput("curve", height = "430px"),
          p(class = "note", "Study observations describe relative iron absorption in wheat-roll experiments. They are not a prediction for your entered meal. Dashed interpolation, when enabled in JSON, is exploratory.")),
        tabPanel("Sources", tableOutput("sources"),
          p(class = "note", "Abstract-level extraction; formal evidence grading and independent validation are pending. No numerical aggregate or absolute bioavailability is inferred.")),
        tabPanel("Food correlations", uiOutput("food")),
        tabPanel("JSON", verbatimTextOutput("json"))
      )
    )
  )
)

server <- function(input, output, session) {
  forms <- list(iron = c("non_heme_food", "ferrous_sulfate", "heme"),
    vitamin_c = c("ascorbic_acid", "food"),
    calcium = c("food", "calcium_carbonate", "calcium_citrate", "calcium_phosphate"),
    phytate = c("sodium_phytate_phosphorus", "phytic_acid"),
    zinc = "elemental", copper = "elemental")
  observeEvent(input$nutrient, updateSelectInput(session, "form", choices = forms[[input$nutrient]]))
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
      event <- list(id = paste0("event_", index), nutrient = input$nutrient,
        form = input$form, amount = input$amount, unit = input$unit,
        time_minutes = input$minute, meal_id = input$meal)
      payload$events <- append(payload$events, list(event))
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
  output$curve <- renderPlot({req(current()); plot_phytate_curve(current()$interpolate)})
  output$events <- renderTable({req(current()); current()$events})
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
