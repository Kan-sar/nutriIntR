output$analysis_available <- reactive(!is.null(current()))
outputOptions(output, "analysis_available", suspendWhenHidden = FALSE)
output$draft_timeline <- echarts4r::renderEcharts4r({
  payload <- tryCatch(draft_payload(), error = function(e) NULL)
  validate(need(!is.null(payload), "Correct the scenario JSON to preview the timeline."))
  validate(need(length(payload$events) + length(payload$medications) > 0, "Add an intake to preview its time."))
  preview <- tryCatch({
    e <- if (length(payload$events)) normalize_events(payload$events) else data.frame(
      id = character(), nutrient_name = character(), time_minutes = numeric(), consumed_at = character(), amount = numeric(), unit = character())
    m <- normalize_medications(payload$medications)
    if (nrow(e)) align_medication_chronology(e, m) else list(events = e, medications = m)
  }, error = function(e) NULL)
  validate(need(!is.null(preview), "Check intake values and timing to preview this draft."))
  explorer_timeline(explorer_nodes(preview), input_id = NULL)
})
output$analysis_empty <- renderUI({
  if (!is.null(current())) return(NULL)
  div(class = "empty-state", h2("Your findings will appear here"),
    p("Add your intakes and analyze a scenario to explore relationships and their evidence."),
    actionButton("back_to_intakes", "Go to your intakes", class = "btn-primary"))
})
observeEvent(input$back_to_intakes, updateTabsetPanel(session, "analysis_view", "intakes"))
nodes <- reactive({req(current()); explorer_nodes(current())})
all_findings <- reactive({req(current()); explorer_findings(current())})
focused_intake <- reactiveVal("")
selected_finding <- reactiveVal(NULL)
observeEvent(current(), {
  focused_intake(""); selected_finding(NULL)
  choices <- if (is.null(current())) c("All intakes" = "") else
    c("All intakes" = "", stats::setNames(nodes()$id, nodes()$label))
  updateSelectInput(session, "intake_focus", choices = choices, selected = "")
  if (!is.null(current())) updateTabsetPanel(session, "analysis_view", "findings")
}, ignoreNULL = FALSE)
set_focus <- function(token) {
  if (!is.null(current()) && (identical(token, "") || token %in% nodes()$id)) {
    focused_intake(token); selected_finding(NULL)
    updateSelectInput(session, "intake_focus", selected = token)
  }
}
observeEvent(input$timeline_selection, set_focus(input$timeline_selection))
observeEvent(input$intake_focus, set_focus(input$intake_focus))
observeEvent(input$clear_focus, set_focus(""))
filtered_findings <- reactive({
  f <- all_findings()
  if (!is.null(input$pair_type) && input$pair_type != "all") f <- f[f$type == input$pair_type, , drop = FALSE]
  token <- focused_intake()
  if (nzchar(token)) f <- f[f$source == token | f$target == token, , drop = FALSE]
  f
})
observeEvent(input$pair_type, selected_finding(NULL))
choose_finding <- function(id) {
  if (!is.null(current()) && id %in% filtered_findings()$id) selected_finding(id)
}
observeEvent(input$choose_finding, choose_finding(input$choose_finding))
observeEvent(input$relationship_selection, {
  req(current())
  if (input$relationship_selection %in% nodes()$id) set_focus(input$relationship_selection)
  else choose_finding(input$relationship_selection)
})
observeEvent(selected_finding(), {
  session$sendCustomMessage("selected-finding", list(id = selected_finding()))
}, ignoreNULL = FALSE)
output$intake_timeline <- echarts4r::renderEcharts4r({ explorer_timeline(nodes()) })
output$relationship_chart <- echarts4r::renderEcharts4r({
  f <- filtered_findings()
  validate(need(nrow(f), "No relationships in this selection. Add another intake or clear the filters."))
  explorer_relationship(nodes(), f, identical(input$relationship_view, "matrix"))
})
output$finding_list <- renderUI({
  f <- filtered_findings(); n <- nodes()
  if (!nrow(f)) return(div(class = "empty-state", h3("No relationships to show"),
    p("Add another intake or clear the filters. An empty list does not establish safety.")))
  # Show actionable matched evidence before unresolved or uncataloged pairs.
  f <- f[order(!nzchar(f$rule), f$id), , drop = FALSE]
  tagList(h3(paste(nrow(f), "relationships")),
    p(class = "note", "Select a relationship to read its scope and evidence."),
    tags$ul(class = "finding-list", lapply(seq_len(nrow(f)), function(i) tags$li(
      tags$button(type = "button", class = "finding-choice", `data-finding` = f$id[i],
        `aria-pressed` = if (identical(isolate(selected_finding()), f$id[i])) "true" else "false",
        strong(n$label[match(f$source[i], n$id)], if (f$type[i] == "dd") " \u2194 " else " \u2192 ", n$label[match(f$target[i], n$id)]),
        span(class = "finding-status", finding_label(f$status[i])),
        span(class = "note", gsub("_", " ", f$direction[i])))))))
})
output$finding_detail <- renderUI({
  f <- filtered_findings(); n <- nodes(); id <- selected_finding()
  row <- if (is.null(id)) integer() else which(f$id == id)
  if (!length(row)) return(tagList(h3("Evidence for your selection"), p("Choose a relationship from the list, network, or matrix.")))
  f <- f[row[1], , drop = FALSE]
  tagList(h3("Selected relationship"),
    p(strong(n$label[match(f$source, n$id)]), if (f$type == "dd") " \u2194 " else " \u2192 ", strong(n$label[match(f$target, n$id)])),
    p(class = "finding-status", finding_label(f$status)),
    p(gsub("_", " ", f$direction)),
    h4("Applicability"), p(f$applicability),
    if (nzchar(f$guidance)) tagList(h4("Label guidance"), p(f$guidance)),
    if (nzchar(f$rule)) p(class = "note", "Evidence rule: ", f$rule),
    if (grepl("^https?://", f$url)) tags$a(href = f$url, target = "_blank", rel = "noopener noreferrer", "Open original source \u2197")
    else p(class = "note", "No source is linked for this pair in the current catalog."),
    p(class = "note", "Uncataloged or unresolved relationships do not establish safety. Check medication decisions with a qualified clinician."))
})
