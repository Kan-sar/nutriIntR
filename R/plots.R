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
  graphics::plot(events$time_minutes, y, pch = 21, bg = "#129B88", col = "#129B88",
    cex = 1.6, yaxt = "n", ylab = "", xlab = "Minutes from scenario start",
    xlim = span + c(-1, 1) * diff(span) * .1, ylim = c(.4, nrow(events) + .6),
    main = "Chronology of intake events", bty = "n")
  graphics::axis(2, at = y, labels = paste(events$id, events$amount, events$unit), las = 1, cex.axis = .8)
  graphics::abline(v = unique(events$time_minutes), col = "#E7EDF1", lty = 3)
  graphics::points(events$time_minutes, y, pch = 21, bg = "#129B88", col = "#129B88", cex = 1.6)
}

plot_network <- function(result) {
  events <- result$events
  old <- graphics::par(mar = c(1, 1, 3, 1), bg = "white")
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
  graphics::points(x, y, pch = 21, bg = "#163D4F", col = "white", cex = 3)
  for (i in seq_len(n)) {
    pos <- if (x[i] > .5) 4 else if (x[i] < -.5) 2 else if (y[i] >= 0) 3 else 1
    graphics::text(x[i], y[i], labels = events$id[i], pos = pos, offset = 1.1, cex = .8)
  }
  graphics::legend("bottom", c("Enhancing", "Inhibiting", "No detected effect", "Unresolved"),
    col = direction_color(c("enhances", "inhibits", "no_detected_effect", "unknown")),
    lty = 1, bty = "n", cex = .7, horiz = TRUE)
}

plot_phytate_curve <- function(interpolate = FALSE) {
  anchors <- utils::read.csv(engine_file("phytate_reference.csv"))
  old <- graphics::par(mar = c(5, 5, 4, 2), bg = "white")
  on.exit(graphics::par(old))
  graphics::plot(anchors$phytate_phosphorus_mg, anchors$relative_absorption,
    log = "x", ylim = c(0, 1), pch = 19, col = "#D76543", cex = 1.5,
    xlab = "Added phytate phosphorus (mg, log scale)",
    ylab = "Absorption / study control absorption", bty = "n",
    main = "Study reference: phytate and non-heme iron")
  if (interpolate) graphics::lines(anchors$phytate_phosphorus_mg,
    anchors$relative_absorption, col = "#D76543", lty = 2)
  graphics::mtext("PMID 2911999; wheat rolls; points are observations, dashed line is exploratory", cex = .7)
}
