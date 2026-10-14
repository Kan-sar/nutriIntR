# Stable nutrient identity colors, shared by the evidence map and dose view.
nutrient_plot_colors <- function(keys) {
  catalog <- nutrient_catalog()
  registered <- c(catalog$key[order(catalog$id)], "phytate")
  index <- seq_along(registered)
  colors <- grDevices::hcl(h = (index * 137.508) %% 360,
    c = 65, l = rep(c(45, 57, 67), length.out = length(index)))
  names(colors) <- registered
  familiar <- c(iron = "#D55E00", vitamin_c = "#009E73", calcium = "#0072B2",
    zinc = "#CC79A7", copper = "#A66B00", phytate = "#7B3FB9",
    usda_1003 = "#278EC4", usda_1104 = "#B84773")
  colors[names(familiar)] <- familiar
  used <- unname(familiar)
  for (key in setdiff(registered, names(familiar))) {
    value <- strtoi(sub("#", "", colors[key], fixed = TRUE), base = 16L)
    candidate <- colors[key]
    while (candidate %in% used) {
      value <- (value + 1) %% 16777216
      candidate <- sprintf("#%06X", as.integer(value))
    }
    colors[key] <- candidate
    used <- c(used, candidate)
  }
  result <- unname(colors[keys])
  # Unregistered extension names also receive a deterministic identity color.
  for (i in which(is.na(result))) {
    hash <- 0
    for (code in utf8ToInt(keys[i])) hash <- (hash * 131 + code) %% 16777213
    result[i] <- grDevices::hcl(h = hash %% 360, c = 65, l = 45 + hash %% 23)
  }
  result
}

event_plot_styles <- function(result) {
  events <- result$events
  span <- diff(range(events$time_minutes))
  alpha <- if (span == 0) rep(1, nrow(events)) else
    .25 + .75 * (events$time_minutes - min(events$time_minutes)) / span
  colors <- nutrient_plot_colors(events$nutrient)
  rgba <- vapply(seq_along(colors), function(i)
    grDevices::adjustcolor(colors[i], alpha.f = alpha[i]), character(1))
  data.frame(event_id = events$id, nutrient = events$nutrient, color = colors,
    alpha = alpha, rgba = rgba, stringsAsFactors = FALSE)
}
