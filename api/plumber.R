# Start from the project root: source("scripts/serve_api.R").
project_root <- if (file.exists("scripts/load_project.R")) "." else ".."
for (file in list.files(file.path(project_root, "R"), pattern = "\\.R$", full.names = TRUE)) source(file, local = TRUE)
engine_file <- function(name) file.path(project_root, "inst", "extdata", name)

#* Service status
#* @get /health
#* @serializer unboxedJSON
function() list(status = "ok", engine_version = "0.1.0", mode = "evidence_explorer")

#* Analyze nutrient events with explicit context
#* @post /analyze
#* @parser json
#* @serializer contentType list(type="application/json")
function(req, res) {
  tryCatch({
    payload <- req$body
    if (!is.list(payload) || is.null(payload$events) || is.null(payload$context))
      stop("JSON object requires events and context.")
    interpolate <- if (is.null(payload$interpolate)) FALSE else payload$interpolate
    as.character(result_json(analyze_interactions(payload$events, payload$context, interpolate)))
  }, error = function(e) {
    res$status <- 400L
    as.character(jsonlite::toJSON(list(error = conditionMessage(e)), auto_unbox = TRUE))
  })
}
