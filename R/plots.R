direction_color <- function(direction) {
  palette <- c(enhances = "#129B88", inhibits = "#D76543",
               no_detected_effect = "#597DB5", unknown = "#A5AEB8")
  unname(palette[direction])
}

plot_timeline <- function(result) {
  events <- result$events
  old <- graphics::par(mar = c(5, 11, 3, 2), bg = "white")
  on.exit(graphics::par(old))
  y <- rev(seq_len(nrow(events)))
  span <- range(events$time_minutes)
  if (diff(span) == 0) span <- span + c(-10, 10)
  clock <- !is.null(result$chronology$origin_local)
  graphics::plot(events$time_minutes, y, pch = 21, bg = "#129B88", col = "#129B88",
    cex = 1.6, yaxt = "n", xaxt = if (clock) "n" else "s", ylab = "",
    xlab = if (clock) "Time consumed" else "Minutes from scenario start",
    xlim = span + c(-1, 1) * diff(span) * .1, ylim = c(.4, nrow(events) + .6),
    main = "Chronology of intake events", bty = "n")
  if (clock) {
    ticks <- pretty(span, n = 5)
    tick_time <- parse_consumed_at(result$chronology$origin_local) + ticks * 60
    labels <- format_local_time(tick_time, grepl("T", result$chronology$origin_local, fixed = TRUE))
    labels <- gsub("T", "\n", sub(":[0-9]{2}$", "", labels), fixed = TRUE)
    graphics::axis(1, at = ticks, labels = labels, cex.axis = .8)
    graphics::mtext("Local clock times", side = 3, cex = .65)
  }
  graphics::axis(2, at = y, labels = paste(events$id, events$amount, events$unit), las = 1, cex.axis = .8)
  graphics::abline(v = unique(events$time_minutes), col = "#E7EDF1", lty = 3)
  graphics::points(events$time_minutes, y, pch = 21, bg = "#129B88", col = "#129B88", cex = 1.6)
}

plot_network <- function(result) {
  events <- result$events
  styles <- event_plot_styles(result)
  old <- graphics::par(mar = c(3, 1, 4, 1), bg = "#D9D9D9")
  on.exit(graphics::par(old))
  n <- nrow(events); theta <- seq(0, 2 * pi, length.out = n + 1L)[- (n + 1L)]
  x <- cos(theta); y <- sin(theta)
  graphics::plot(x, y, type = "n", axes = FALSE, xlab = "", ylab = "", asp = 1,
    xlim = c(-1.7, 1.7), ylim = c(-1.5, 1.5), main = "Directed evidence map")
  for (f in result$interactions) {
    if (is.null(f$rule_id)) next
    a <- match(f$source_id, events$id); b <- match(f$target_id, events$id)
    graphics::arrows(.87*x[a], .87*y[a], .87*x[b], .87*y[b], length = .12,
      col = direction_color(f$direction), lwd = 2,
      lty = if (f$status == "qualitative_evidence") 1 else 2)
  }
  graphics::points(x, y, pch = 19, col = styles$rgba, cex = 3.2)
  for (i in seq_len(n)) {
    pos <- if (x[i] > .5) 4 else if (x[i] < -.5) 2 else if (y[i] >= 0) 3 else 1
    graphics::text(x[i], y[i], labels = events$id[i], pos = pos, offset = 1.1, cex = .8)
  }
  graphics::legend("bottom", c("Enhancing", "Inhibiting", "No detected effect", "Unresolved"),
    col = direction_color(c("enhances", "inhibits", "no_detected_effect", "unknown")),
    lty = 1, bty = "n", cex = .7, horiz = TRUE)
  graphics::mtext("Dot hue = nutrient | opacity + brightness = intake recency", side = 3, cex = .75)
}

medication_status_color <- function(status) {
  palette <- c(potential_timing_conflict = "#C94C3A", separated_per_label = "#2B7A5C",
    evidence_match_non_timing = "#7A5AA6", evidence_match_timing_unresolved = "#C07A2B",
    long_term_context_required = "#4D6FA9", unsupported_pair = "#A5AEB8",
    unsupported_form = "#A5AEB8", unsupported_route = "#A5AEB8", zero_dose = "#A5AEB8")
  unname(palette[status])
}

plot_medication_network <- function(result) {
  medications <- result$medications
  events <- result$events
  old <- graphics::par(mar = c(2, 2, 4, 2), bg = "#F2F3F1")
  on.exit(graphics::par(old))
  if (!nrow(medications)) {
    graphics::plot.new(); graphics::title("Medication and nutrient evidence map")
    graphics::text(.5, .5, "Add a medication to visualize label-backed checks.", col = "#62716B")
    return(invisible(NULL))
  }
  medication_y <- seq(.85, .15, length.out = nrow(medications))
  nutrient_y <- seq(.9, .1, length.out = nrow(events))
  graphics::plot(c(0, 1), c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "",
    main = "Medication and nutrient evidence map")
  matched <- Filter(function(x) !is.null(x$rule_id), result$medication_interactions)
  for (finding in matched) {
    m <- match(finding$medication_id, medications$id)
    n <- match(finding$nutrient_event_id, events$id)
    graphics::arrows(.83, nutrient_y[n], .18, medication_y[m], length = .08,
      col = medication_status_color(finding$status), lwd = 2,
      lty = if (finding$status %in% c("potential_timing_conflict", "separated_per_label")) 1 else 2)
  }
  styles <- event_plot_styles(result)
  graphics::points(rep(.9, nrow(events)), nutrient_y, pch = 19, col = styles$rgba, cex = 2.5)
  graphics::points(rep(.1, nrow(medications)), medication_y, pch = 22, bg = "#203630", col = "#203630", cex = 2.5)
  graphics::text(rep(.94, nrow(events)), nutrient_y, events$id, pos = 4, cex = .75, xpd = NA)
  graphics::text(rep(.06, nrow(medications)), medication_y, medications$medication_name,
    pos = 2, cex = .8, font = 2, xpd = NA)
  graphics::mtext("Arrows point from nutrient events to medication events; color encodes rule status", side = 1, cex = .7)
  graphics::legend("bottom", c("Potential timing conflict", "Separated per label", "Non-timing guidance", "Timing unresolved", "Chronic context"),
    col = medication_status_color(c("potential_timing_conflict", "separated_per_label",
      "evidence_match_non_timing", "evidence_match_timing_unresolved", "long_term_context_required")),
    lty = c(1, 1, 2, 2, 2), bty = "n", cex = .65, horiz = TRUE, xpd = NA)
  invisible(NULL)
}

plot_drug_network <- function(result) {
  medications <- result$medications
  old <- graphics::par(mar = c(2, 2, 4, 2), bg = "#F2F3F1")
  on.exit(graphics::par(old))
  if (nrow(medications) < 2L) {
    graphics::plot.new(); graphics::title("Drug-drug evidence map")
    graphics::text(.5, .5, "Add at least two drugs to compare selected label rules.", col = "#62716B")
    return(invisible(NULL))
  }
  theta <- seq(0, 2 * pi, length.out = nrow(medications) + 1L)[-(nrow(medications) + 1L)]
  x <- cos(theta); y <- sin(theta)
  graphics::plot(x, y, type = "n", axes = FALSE, xlab = "", ylab = "", asp = 1,
    xlim = c(-1.55, 1.55), ylim = c(-1.35, 1.35), main = "Selected drug-drug label checks")
  for (finding in result$drug_interactions) {
    a <- match(finding$drug_a_id, medications$id)
    b <- match(finding$drug_b_id, medications$id)
    supported <- finding$status == "label_monitoring_interaction"
    graphics::segments(.84 * x[a], .84 * y[a], .84 * x[b], .84 * y[b],
      col = if (supported) "#C94C3A" else "#A5AEB8", lwd = if (supported) 2.5 else 1.2,
      lty = if (supported) 1 else 2)
  }
  graphics::points(x, y, pch = 22, bg = "#203630", col = "#203630", cex = 3)
  for (i in seq_len(nrow(medications))) {
    pos <- if (x[i] > .4) 4 else if (x[i] < -.4) 2 else if (y[i] >= 0) 3 else 1
    graphics::text(x[i], y[i], medications$medication_name[i], pos = pos, offset = 1.1,
      cex = .82, font = 2)
  }
  graphics::legend("bottom", c("Label monitoring interaction", "Uncataloged pair"),
    col = c("#C94C3A", "#A5AEB8"), lty = c(1, 2), lwd = c(2.5, 1.2), bty = "n",
    horiz = TRUE, cex = .72)
  invisible(NULL)
}

plot_phytate_curve <- function(interpolate = FALSE, result = NULL) {
  anchors <- utils::read.csv(engine_file("phytate_reference.csv"))
  # result is retained for callers of the previous interface. Recency is now a
  # native dashboard table rather than a second plot panel.
  old <- graphics::par(mar = c(4, 5, 4, 2), bg = "#D9D9D9")
  on.exit(graphics::par(old))
  phytate_color <- nutrient_plot_colors("phytate")
  graphics::plot(anchors$phytate_phosphorus_mg, anchors$relative_absorption,
    log = "x", ylim = c(0, 1), pch = 1, col = phytate_color, cex = 1.5, lwd = 2,
    xlab = "Added phytate phosphorus (mg, log scale)",
    ylab = "Absorption / study control absorption", bty = "n",
    main = "Study reference: phytate and non-heme iron")
  if (interpolate) graphics::lines(anchors$phytate_phosphorus_mg,
    anchors$relative_absorption, col = phytate_color, lty = 2)
  graphics::mtext("PMID 2911999 | Hollow dots: study observations, without intake times", cex = .7)
}
