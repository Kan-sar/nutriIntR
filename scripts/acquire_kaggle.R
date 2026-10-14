# Public Kaggle API; no credentials are needed for this dataset.
slug <- "trolukovich/nutritional-values-for-common-foods-and-products"
destination <- "data-raw/downloads"
dir.create(destination, recursive = TRUE, showWarnings = FALSE)
metadata_url <- paste0("https://www.kaggle.com/api/v1/datasets/view/", slug)
metadata_path <- file.path(destination, "kaggle-metadata.json")
utils::download.file(metadata_url, metadata_path, mode = "wb", quiet = TRUE)
metadata <- jsonlite::fromJSON(metadata_path)
version <- metadata$currentVersionNumber
if (!is.numeric(version) || length(version) != 1L) stop("Kaggle metadata lacks version.")
archive <- file.path(destination, paste0("nutrition-v", version, ".zip"))
url <- paste0("https://www.kaggle.com/api/v1/datasets/download/", slug, "?datasetVersionNumber=", version)
if (!file.exists(archive)) utils::download.file(url, archive, mode = "wb", quiet = TRUE)
entries <- utils::unzip(archive, list = TRUE)$Name
csv <- entries[basename(entries) == "nutrition.csv"]
if (length(csv) != 1L || grepl("(^/|^[A-Za-z]:|(^|/)\\.\\.(/|$))", csv))
  stop("Expected exactly one safe nutrition.csv archive entry.")
utils::unzip(archive, files = csv, exdir = destination)
csv_path <- file.path(destination, csv)
data <- utils::read.csv(csv_path, check.names = FALSE, stringsAsFactors = FALSE)
manifest <- list(slug = slug, title = metadata$title, version = version,
  source_url = paste0("https://www.kaggle.com/datasets/", slug), download_url = url,
  license = metadata$licenseName, source_updated = metadata$lastUpdated,
  acquired_on = as.character(Sys.Date()), rows = nrow(data), columns = ncol(data),
  archive_md5 = unname(tools::md5sum(archive)), csv_md5 = unname(tools::md5sum(csv_path)),
  csv_path = csv_path,
  role = "food composition and EDA, not measured human absorption outcomes")
jsonlite::write_json(manifest, "data-raw/kaggle-manifest.json", pretty = TRUE, auto_unbox = TRUE)
cat("Acquired", nrow(data), "foods and", ncol(data), "columns from Kaggle version", version, "\n")
