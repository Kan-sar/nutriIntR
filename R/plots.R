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
