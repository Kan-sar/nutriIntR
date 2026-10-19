# Presentation adapters only: engine results remain the source of every finding.
explorer_nodes <- function(result) {
  e <- result$events; m <- result$medications
  clock_label <- function(x) ifelse(is.na(x$consumed_at) | !nzchar(x$consumed_at),
    paste0("+", x$time_minutes, " min"), x$consumed_at)
  nodes <- data.frame(id = paste0("events:", e$id), name = e$nutrient_name,
    time = e$time_minutes, consumed = clock_label(e), type = rep("Nutrient", nrow(e)),
    amount = paste(e$amount, e$unit), stringsAsFactors = FALSE)
  if (nrow(e) && !is.null(e$nutrient)) {
    catalog <- nutrient_catalog()
    names <- nutrient_display_names(catalog)[match(e$nutrient, catalog$key)]
    nodes$name[!is.na(names)] <- names[!is.na(names)]
  }
  if (nrow(m)) nodes <- rbind(nodes, data.frame(id = paste0("medications:", m$id),
    name = m$medication_name, time = m$time_minutes, consumed = clock_label(m),
    type = rep("Medication", nrow(m)), amount = m$route, stringsAsFactors = FALSE))
  nodes$label <- paste0(nodes$name, " \u00b7 ", nodes$consumed)
  repeated <- duplicated(nodes$label) | duplicated(nodes$label, fromLast = TRUE)
  nodes$label[repeated] <- paste0(nodes$label[repeated], " (intake ", which(repeated), ")")
  nodes[order(nodes$time, nodes$id), , drop = FALSE]
}

explorer_findings <- function(result) {
  rows <- list()
  append_row <- function(f, type, source, target) {
    empty <- function(x) if (is.null(x)) "" else as.character(x)
    rows[[length(rows) + 1L]] <<- data.frame(id = paste0(type, ":", length(rows) + 1L),
      type = type, source = source, target = target, status = f$status,
      direction = empty(f$direction), rule = empty(f$rule_id),
      applicability = empty(f$applicability), guidance = empty(f$guidance),
      url = if (is.null(f$citation)) "" else empty(f$citation$url), stringsAsFactors = FALSE)
  }
  for (f in result$interactions) append_row(f, "nn", paste0("events:", f$source_id), paste0("events:", f$target_id))
  for (f in result$medication_interactions) append_row(f, "nd", paste0("events:", f$nutrient_event_id), paste0("medications:", f$medication_id))
  for (f in result$drug_interactions) append_row(f, "dd", paste0("medications:", f$drug_a_id), paste0("medications:", f$drug_b_id))
  if (!length(rows)) return(data.frame(id = character(), type = character(), source = character(), target = character(),
    status = character(), direction = character(), rule = character(), applicability = character(), guidance = character(), url = character()))
  do.call(rbind, rows)
}

finding_label <- function(status) {
  labels <- c(qualitative_evidence = "Evidence match", unsupported_pair = "Uncataloged \u2014 unknown",
    timing_unresolved = "Timing unresolved", within_meal_timing_unresolved = "Within-meal timing unresolved",
    unsupported_context = "Context unsupported",
    potential_timing_conflict = "Potential timing conflict", separated_per_label = "Separated per label",
    evidence_match_non_timing = "Non-timing guidance", evidence_match_timing_unresolved = "Timing unresolved",
    long_term_context_required = "Long-term context needed", unsupported_form = "Form unsupported",
    unsupported_route = "Route unsupported", zero_dose = "Zero dose", label_monitoring_interaction = "Label monitoring interaction")
  value <- unname(labels[status]); missing <- is.na(value)
  value[missing] <- gsub("_", " ", status[missing]); value
}

finding_color <- function(status) {
  colors <- c(qualitative_evidence = "#17694f", potential_timing_conflict = "#a33c2c",
    separated_per_label = "#356c88", evidence_match_non_timing = "#735099",
    label_monitoring_interaction = "#a33c2c")
  value <- unname(colors[status]); value[is.na(value)] <- "#717b83"; value
}

explorer_widget <- function(options, input_id = NULL) {
  widget <- echarts4r::e_charts()
  widget$x$opts <- modifyList(list(animation = FALSE, aria = list(enabled = TRUE),
    textStyle = list(fontFamily = "Segoe UI, Arial, sans-serif", color = "#203630"),
    tooltip = list(trigger = "item", renderMode = "richText", confine = TRUE)), options)
  if (!is.null(input_id)) widget <- htmlwidgets::onRender(widget, sprintf(
    "function(el) { var c = echarts.getInstanceByDom(el); if(el.explorerClick) c.off('click', el.explorerClick); el.explorerClick = function(p) { if(p.data && p.data.token && window.Shiny) Shiny.setInputValue('%s', p.data.token, {priority:'event'}); }; c.on('click', el.explorerClick); }", input_id))
  widget
}

explorer_timeline <- function(nodes, input_id = "timeline_selection") {
  points <- lapply(seq_len(nrow(nodes)), function(i) list(value = list(nodes$time[i], nodes$label[i]),
    name = paste(nodes$label[i], nodes$amount[i]), token = nodes$id[i],
    symbol = if (nodes$type[i] == "Medication") "rect" else "circle",
    itemStyle = list(color = if (nodes$type[i] == "Medication") "#356c88" else "#17694f")))
  explorer_widget(list(grid = list(left = 12, right = 25, top = 15, bottom = 65, containLabel = TRUE),
    xAxis = list(type = "value", name = "Minutes from first intake", nameLocation = "middle", nameGap = 30),
    yAxis = list(type = "category", data = as.list(nodes$label), inverse = TRUE,
      axisLabel = list(width = 180, overflow = "truncate")),
    dataZoom = list(list(type = "slider", xAxisIndex = 0, bottom = 0)),
    series = list(list(type = "scatter", symbolSize = 15, data = points))), input_id)
}

explorer_relationship <- function(nodes, findings, matrix = FALSE) {
  if (matrix) {
    cells <- lapply(seq_len(nrow(findings)), function(i) list(
      value = list(match(findings$source[i], nodes$id) - 1L, match(findings$target[i], nodes$id) - 1L, 1),
      name = paste(nodes$label[match(findings$source[i], nodes$id)], "\u2192", nodes$label[match(findings$target[i], nodes$id)], finding_label(findings$status[i])),
      token = findings$id[i], itemStyle = list(color = finding_color(findings$status[i]))))
    # Drug\u2013drug checks are undirected; mirror them without inventing another finding.
    for (i in which(findings$type == "dd")) {
      mirror <- cells[[i]]; mirror$value[1:2] <- rev(mirror$value[1:2]); cells <- append(cells, list(mirror))
    }
    return(explorer_widget(list(aria = list(label = list(description = "Pairwise interaction matrix. Columns are sources and rows are targets. Use the findings list for keyboard-accessible statuses and evidence. Blank cells are self-pairs or outside the selected relationship types.")),
      grid = list(left = 15, right = 30, top = 35, bottom = 110, containLabel = TRUE),
      xAxis = list(type = "category", data = as.list(nodes$label), axisLabel = list(rotate = 45, width = 120, overflow = "truncate")),
      yAxis = list(type = "category", name = "Target", data = as.list(nodes$label), axisLabel = list(width = 160, overflow = "truncate")),
      series = list(list(type = "heatmap", data = cells))), "relationship_selection"))
  }
  n <- nrow(nodes); theta <- seq(0, 2*pi, length.out = n + 1L)[seq_len(n)]
  vertices <- lapply(seq_len(n), function(i) list(id = nodes$id[i], name = nodes$label[i], token = nodes$id[i],
    x = cos(theta[i]) * 180, y = sin(theta[i]) * 150, symbolSize = 22,
    symbol = if (nodes$type[i] == "Medication") "rect" else "circle",
    itemStyle = list(color = if (nodes$type[i] == "Medication") "#356c88" else "#17694f")))
  edges <- lapply(seq_len(nrow(findings)), function(i) list(source = findings$source[i], target = findings$target[i],
    token = findings$id[i], name = paste(finding_label(findings$status[i]), gsub("_", " ", findings$direction[i])),
    symbol = list("none", if (findings$type[i] == "dd") "none" else "arrow"),
    lineStyle = list(color = finding_color(findings$status[i]), width = 2, curveness = .16,
      type = if (nzchar(findings$rule[i])) "solid" else "dashed")))
  explorer_widget(list(aria = list(label = list(description = "Interaction network. Nodes represent individual intakes. Use the findings list for keyboard-accessible relationship statuses and evidence.")),
    series = list(list(type = "graph", layout = "none", roam = TRUE,
    data = vertices, links = edges, label = list(show = TRUE, position = "bottom", width = 130, overflow = "truncate"),
    emphasis = list(focus = "adjacency")))), "relationship_selection")
}
