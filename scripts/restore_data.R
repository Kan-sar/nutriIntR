# Restore exactly the committed Kaggle version, without updating provenance.
manifest <- jsonlite::fromJSON("data-raw/kaggle-manifest.json")
destination <- dirname(manifest$csv_path)
dir.create(destination, recursive = TRUE, showWarnings = FALSE)
checksum_matches <- function(path, expected) {
  file.exists(path) && identical(unname(tools::md5sum(path)), expected)
}
if (!checksum_matches(manifest$csv_path, manifest$csv_md5)) {
  archive <- tempfile(fileext = ".zip")
  options(timeout = max(300, getOption("timeout")))
  utils::download.file(manifest$download_url, archive, mode = "wb", quiet = TRUE)
  if (!checksum_matches(archive, manifest$archive_md5))
    stop("Downloaded archive differs from the committed dataset checksum.")
  entries <- utils::unzip(archive, list = TRUE)$Name
  csv <- entries[basename(entries) == "nutrition.csv"]
  if (length(csv) != 1L || grepl("(^/|^[A-Za-z]:|(^|/)\\.\\.(/|$))", csv))
    stop("Expected exactly one safe nutrition.csv archive entry.")
  extracted <- utils::unzip(archive, files = csv, exdir = destination)
  if (!identical(normalizePath(extracted), normalizePath(manifest$csv_path)))
    stop("Extracted dataset path differs from the manifest.")
  unlink(archive)
}
if (!checksum_matches(manifest$csv_path, manifest$csv_md5))
  stop("Food dataset differs from the committed checksum.")
cat("Verified Kaggle version", manifest$version, "with", manifest$rows, "foods.\n")
