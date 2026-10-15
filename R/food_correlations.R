
food_nutrient_columns <- function() {
  c(`1003` = "protein", `1004` = "fat", `1005` = "carbohydrate", `1007` = "ash",
    `1008` = "calories", `1010` = "sucrose", `1011` = "glucose", `1012` = "fructose",
    `1013` = "lactose", `1014` = "maltose", `1018` = "alcohol", `1051` = "water",
    `1057` = "caffeine", `1058` = "theobromine", `1063` = "sugars", `1075` = "galactose",
    `1079` = "fiber", `1087` = "calcium", `1089` = "irom", `1090` = "magnesium",
    `1091` = "phosphorous", `1092` = "potassium", `1093` = "sodium", `1095` = "zink",
    `1098` = "copper", `1101` = "manganese", `1103` = "selenium", `1104` = "vitamin_a",
    `1106` = "vitamin_a_rae", `1107` = "carotene_beta", `1108` = "carotene_alpha",
    `1109` = "vitamin_e", `1110` = "vitamin_d", `1120` = "cryptoxanthin_beta",
    `1122` = "lucopene", `1123` = "lutein_zeaxanthin", `1162` = "vitamin_c",
    `1165` = "thiamin", `1166` = "riboflavin", `1167` = "niacin", `1170` = "pantothenic_acid",
    `1175` = "vitamin_b6", `1177` = "folate", `1178` = "vitamin_b12", `1180` = "choline",
    `1185` = "vitamin_k", `1186` = "folic_acid", `1210` = "tryptophan", `1211` = "threonine",
    `1212` = "isoleucine", `1213` = "leucine", `1214` = "lysine", `1215` = "methionine",
    `1216` = "cystine", `1217` = "phenylalanine", `1218` = "tyrosine", `1219` = "valine",
    `1220` = "arginine", `1221` = "histidine", `1222` = "alanine", `1223` = "aspartic_acid",
    `1224` = "glutamic_acid", `1225` = "glycine", `1226` = "proline", `1227` = "serine",
    `1228` = "hydroxyproline", `1253` = "cholesterol", `1257` = "fatty_acids_total_trans",
    `1258` = "saturated_fatty_acids", `1292` = "monounsaturated_fatty_acids",
    `1293` = "polyunsaturated_fatty_acids")
}

nutrient_display_names <- function(catalog) {
  labels <- catalog$name
  simple <- c(`1087` = "Calcium", `1089` = "Iron", `1090` = "Magnesium", `1091` = "Phosphorus",
    `1092` = "Potassium", `1093` = "Sodium", `1095` = "Zinc", `1098` = "Copper",
    `1101` = "Manganese", `1103` = "Selenium", `1162` = "Vitamin C",
    `1165` = "Vitamin B1 (thiamin)", `1166` = "Vitamin B2 (riboflavin)",
    `1167` = "Vitamin B3 (niacin)", `1170` = "Vitamin B5 (pantothenic acid)",
    `1175` = "Vitamin B6", `1178` = "Vitamin B12", `1004` = "Total fat",
    `1005` = "Carbohydrates (by difference)", `1079` = "Dietary fiber",
    `1104` = "Vitamin A (IU)", `1106` = "Vitamin A (RAE)",
    `1110` = "Vitamin D (D2 + D3, IU)", `1114` = "Vitamin D (D2 + D3, mass)",
    `1180` = "Choline (total)", `1177` = "Folate (total)")
  found <- as.character(catalog$id) %in% names(simple)
  labels[found] <- unname(simple[as.character(catalog$id[found])])
  repeated <- duplicated(labels) | duplicated(labels, fromLast = TRUE)
  labels[repeated] <- paste0(labels[repeated], " [", catalog$id[repeated], "]")
  labels
}

parse_food_amount <- function(x, unit, bare = FALSE) {
  x <- tolower(trimws(as.character(x)))
  unit <- normalize_unit(unit)
  mass <- c(g = 1, mg = .001, ug = .000001, mcg = .000001)
  value <- rep(NA_real_, length(x))
  value[!is.na(x) & x == "0"] <- 0
  if (bare) {
    valid <- !is.na(x) & grepl("^[0-9]+(\\.[0-9]+)?$", x)
    value[valid] <- as.numeric(x[valid])
  } else {
    valid <- !is.na(x) & grepl("^[0-9]+(\\.[0-9]+)?[[:space:]]*(g|mg|mcg|ug|iu)$", x)
    source_unit <- sub("^[0-9.]+[[:space:]]*", "", x)
    number <- suppressWarnings(as.numeric(sub("[[:space:]]*[a-z]+$", "", x)))
    if (unit %in% names(mass)) {
      valid <- valid & source_unit %in% names(mass)
      value[valid] <- number[valid] * mass[source_unit[valid]] / mass[unit]
    } else {
      valid <- valid & source_unit == unit
      value[valid] <- number[valid]
    }
  }
  value[!is.finite(value) | value < 0] <- NA_real_
  value
}

selected_food_correlations <- function(events, raw, catalog = nutrient_catalog()) {
  ids <- unique(events$usda_nutrient_id)
  selected <- catalog[match(ids[!is.na(ids)], catalog$id), , drop = FALSE]
  map <- food_nutrient_columns()
  columns <- unname(map[as.character(selected$id)])
  available <- !is.na(columns) & columns %in% names(raw)
  unavailable <- selected$name[!available]
  if (any(is.na(ids))) unavailable <- c(unavailable, unique(events$nutrient_name[is.na(events$usda_nutrient_id)]))
  selected <- selected[available, , drop = FALSE]; columns <- columns[available]
  raw <- raw[!is.na(raw$serving_size) & trimws(raw$serving_size) == "100 g", , drop = FALSE]
  clean <- matrix(NA_real_, nrow(raw), nrow(selected))
  for (i in seq_len(nrow(selected))) clean[, i] <- parse_food_amount(raw[[columns[i]]],
    selected$canonical_unit[i], bare = columns[i] == "calories")
  labels <- nutrient_display_names(selected)
  dimnames(clean) <- list(NULL, labels)
  n <- ncol(clean)
  rho <- matrix(NA_real_, n, n, dimnames = list(labels, labels))
  counts <- crossprod(!is.na(clean))
  for (i in seq_len(n)) for (j in seq_len(n)) {
    complete <- !is.na(clean[, i]) & !is.na(clean[, j])
    if (sum(complete) >= 3L && length(unique(clean[complete, i])) > 1L && length(unique(clean[complete, j])) > 1L)
      rho[i, j] <- stats::cor(clean[complete, i], clean[complete, j], method = "spearman")
  }
  pairs <- which(upper.tri(rho), arr.ind = TRUE)
  table <- data.frame(Nutrient = labels[pairs[, 1]], "Compared with" = labels[pairs[, 2]],
    "Spearman rho" = round(rho[pairs], 3), Foods = as.integer(counts[pairs]), check.names = FALSE)
  list(correlations = rho, pair_counts = counts, pairs = table, unavailable = unavailable,
    labels = labels, rows = nrow(raw), selected_count = length(ids))
}

plot_food_correlations <- function(data) {
  n <- length(data$labels)
  old <- graphics::par(mar = c(9, 9, 2, 1), bg = "white")
  on.exit(graphics::par(old))
  short <- abbreviate(data$labels, minlength = 22, strict = TRUE)
  graphics::image(seq_len(n), seq_len(n), data$correlations, zlim = c(-1, 1),
    col = grDevices::colorRampPalette(c("#B55B45", "#F6F7F3", "#17694F"))(101),
    axes = FALSE, xlab = "", ylab = "")
  graphics::axis(1, seq_len(n), short, las = 2, tick = FALSE, cex.axis = .8)
  graphics::axis(2, seq_len(n), short, las = 2, tick = FALSE, cex.axis = .8)
  if (n <= 12L) for (i in seq_len(n)) for (j in seq_len(n)) {
    r <- data$correlations[i, j]
    graphics::text(i, j, if (is.na(r)) "NA" else sprintf("%.2f", r),
      col = if (!is.na(r) && abs(r) > .65) "white" else "#203630", cex = .85)
  }
  graphics::mtext("Spearman rho: -1 (terracotta) / 0 (neutral) / +1 (green)", side = 3, cex = .75)
}
