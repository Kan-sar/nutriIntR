# Pin USDA's supporting nutrient definitions from the current Foundation release.
# USDA documents that post-April-2023 archives include all supporting tables.
release <- "2026-04-30"
url <- paste0("https://fdc.nal.usda.gov/fdc-datasets/FoodData_Central_foundation_food_csv_", release, ".zip")
destination <- "data-raw/downloads"
dir.create(destination, recursive = TRUE, showWarnings = FALSE)
options(timeout = max(300, getOption("timeout")))
archive <- file.path(destination, paste0("usda-foundation-", release, ".zip"))
if (!file.exists(archive)) utils::download.file(url, archive, mode = "wb", quiet = TRUE)
entries <- utils::unzip(archive, list = TRUE)$Name
entry <- entries[basename(entries) == "nutrient.csv"]
if (length(entry) != 1L || grepl("(^/|^[A-Za-z]:|(^|/)\\.\\.(/|$))", entry))
  stop("Expected one safe nutrient.csv entry in USDA archive.")
extraction <- tempfile("usda-extract-")
dir.create(extraction)
utils::unzip(archive, files = entry, exdir = extraction)
source_csv <- file.path(extraction, entry)
nutrients <- utils::read.csv(source_csv, stringsAsFactors = FALSE, check.names = FALSE)
required <- c("id", "name", "unit_name", "nutrient_nbr", "rank")
if (!all(required %in% names(nutrients)) || !nrow(nutrients) || anyDuplicated(nutrients$id))
  stop("USDA nutrient dictionary is invalid.")
manifest_path <- "data-raw/usda-manifest.json"
csv_md5 <- unname(tools::md5sum(source_csv))
if (file.exists(manifest_path)) {
  previous <- jsonlite::fromJSON(manifest_path)
  if (identical(previous$release, release) && !identical(previous$csv_md5, csv_md5))
    stop("USDA dictionary changed for the pinned release. Review before replacing the snapshot.")
}
dir.create("inst/extdata", recursive = TRUE, showWarnings = FALSE)
if (!file.copy(source_csv, "inst/extdata/nutrient.csv", overwrite = TRUE)) stop("Cannot save USDA snapshot.")
manifest <- list(source = "USDA FoodData Central", release = release,
  source_url = "https://fdc.nal.usda.gov/download-datasets/", archive_url = url,
  archive_entry = entry, acquired_on = as.character(Sys.Date()),
  license = "USDA FoodData Central public domain / CC0",
  archive_md5 = unname(tools::md5sum(archive)), csv_md5 = csv_md5,
  rows = nrow(nutrients), columns = as.list(names(nutrients)),
  scope = "Complete nutrient.csv supporting dictionary, including energy, factors and other components; not an interaction evidence database.")
jsonlite::write_json(manifest, manifest_path, pretty = TRUE, auto_unbox = TRUE)
cat("Bundled", nrow(nutrients), "USDA nutrient definitions. Units:\n")
print(table(nutrients$unit_name))
