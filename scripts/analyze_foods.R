manifest_path <- "data-raw/kaggle-manifest.json"
if (!file.exists(manifest_path)) stop("First run scripts/acquire_kaggle.R.")
manifest <- jsonlite::fromJSON(manifest_path)
if (!file.exists(manifest$csv_path)) stop("Raw data missing. Run acquisition again.")
if (unname(tools::md5sum(manifest$csv_path)) != manifest$csv_md5)
  stop("Raw-data checksum mismatch. Re-acquire and review the manifest.")
raw <- utils::read.csv(manifest$csv_path, check.names = FALSE, stringsAsFactors = FALSE)
wanted <- c(iron = "irom", calcium = "calcium", vitamin_c = "vitamin_c", zinc = "zink", copper = "copper")
if (!all(unname(wanted) %in% names(raw))) stop("Expected nutrient columns missing.")
parse_mg <- function(x) {
  x <- trimws(as.character(x)); blank <- is.na(x) | x == ""
  # Never turn arbitrary text, percent values or unknown units into numbers.
  valid <- grepl("^[0-9]+(\\.[0-9]+)?[[:space:]]*(mg|mcg|g)$", x)
  value <- suppressWarnings(as.numeric(sub("[[:space:]]*(mg|mcg|g)$", "", x)))
  factor <- ifelse(grepl("mcg$", x), .001, ifelse(grepl("mg$", x), 1, 1000))
  value <- value * factor; value[blank | !valid | !is.finite(value) | value < 0] <- NA_real_
  value[!blank & x == "0"] <- 0
  value
}
clean <- as.data.frame(lapply(raw[unname(wanted)], parse_mg))
names(clean) <- names(wanted)
coverage <- data.frame(nutrient = names(clean), total_rows = nrow(clean),
  available = vapply(clean, function(x) sum(!is.na(x)), integer(1)),
  missing_or_unparsed = vapply(clean, function(x) sum(is.na(x)), integer(1)),
  zero_values = vapply(clean, function(x) sum(x == 0, na.rm = TRUE), integer(1)))
pair_counts <- crossprod(!is.na(as.matrix(clean)))
correlations <- stats::cor(clean, method = "spearman", use = "pairwise.complete.obs")
dir.create("reports/generated", recursive = TRUE, showWarnings = FALSE)
utils::write.csv(coverage, "reports/generated/food-coverage.csv", row.names = FALSE)
utils::write.csv(correlations, "reports/generated/food-correlations.csv")
utils::write.csv(pair_counts, "reports/generated/food-pair-counts.csv")
summary <- list(source = manifest$source_url, version = manifest$version, rows = nrow(raw),
  columns = ncol(raw), duplicate_names = sum(duplicated(raw$name)),
  serving_sizes = sort(unique(raw$serving_size)),
  nutrient_units = "mg per source serving (100 g; verify serving_sizes)",
  correlation_method = "Spearman, pairwise complete observations", coverage = coverage,
  renamed_columns = as.list(wanted),
  interpretation = "Food composition associations only; no absorption causality or predictive validation.")
jsonlite::write_json(summary, "reports/generated/food-analysis.json", pretty = TRUE,
  auto_unbox = TRUE, dataframe = "rows", na = "null")
grDevices::png("reports/generated/food-correlation.png", width = 1000, height = 900, res = 120)
old <- graphics::par(mar = c(8, 8, 5, 3))
graphics::image(seq_len(ncol(clean)), seq_len(ncol(clean)), correlations,
  zlim = c(-1, 1), col = grDevices::colorRampPalette(c("#D76543", "#FFFFFF", "#129B88"))(101),
  axes = FALSE, xlab = "", ylab = "", main = "Food composition correlations (Spearman)")
graphics::axis(1, at = seq_len(ncol(clean)), labels = names(clean), las = 2)
graphics::axis(2, at = seq_len(ncol(clean)), labels = names(clean), las = 2)
for (i in seq_len(ncol(clean))) for (j in seq_len(ncol(clean)))
  graphics::text(i, j, sprintf("%.2f", correlations[i, j]), cex = 1.1)
graphics::mtext("Kaggle nutrient amounts per 100 g; these are not absorption effects", side = 3, cex = .8)
graphics::par(old); grDevices::dev.off()
print(coverage)
cat("Saved correlation matrix, pair counts, coverage, JSON summary and heatmap.\n")
