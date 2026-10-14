# Start from the project root: source("scripts/serve_api.R").
project_root <- if (file.exists("scripts/load_project.R")) "." else ".."
for (file in list.files(file.path(project_root, "R"), pattern = "\\.R$", full.names = TRUE)) source(file, local = TRUE)
engine_file <- function(name) file.path(project_root, "inst", "extdata", name)

#* Service status
#* @get /health
#* @serializer unboxedJSON
function() list(status = "ok", engine_version = "0.3.0", mode = "evidence_explorer",
  usda_definitions = nrow(nutrient_catalog()), medication_rules = nrow(read_medication_evidence()),
  llm_model = llm_model_name())

#* Local explanation-model status
#* @get /llm/status
#* @serializer unboxedJSON
function() llm_status()

#* Complete USDA nutrient dictionary
#* @get /nutrients
#* @serializer unboxedJSON
function() list(release = "2026-04-30", count = nrow(nutrient_catalog()), nutrients = nutrient_catalog())

#* Selected medication catalog and label-backed nutrient rules
#* @get /medications
#* @serializer unboxedJSON
function() list(count = nrow(medication_catalog()), medications = medication_catalog(),
  rules = read_medication_evidence())

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
    as.character(result_json(analyze_interactions(payload$events, payload$context, interpolate,
      payload$medications)))
  }, error = function(e) {
    res$status <- 400L
    as.character(jsonlite::toJSON(list(error = conditionMessage(e)), auto_unbox = TRUE))
  })
}

#* Generate a grounded explanation with the configured local DeepSeek model
#* @post /explain
#* @parser json
#* @serializer contentType list(type="application/json")
function(req, res) {
  tryCatch({
    payload <- req$body
    if (!is.list(payload) || is.null(payload$events) || is.null(payload$context))
      stop("JSON object requires events and context.")
    interpolate <- if (is.null(payload$interpolate)) FALSE else payload$interpolate
    result <- analyze_interactions(payload$events, payload$context, interpolate, payload$medications)
    as.character(jsonlite::toJSON(generate_llm_explanation(result), auto_unbox = TRUE,
      pretty = TRUE, null = "null"))
  }, error = function(e) {
    res$status <- if (grepl("Ollama|Model is not installed", conditionMessage(e))) 503L else 400L
    as.character(jsonlite::toJSON(list(error = conditionMessage(e)), auto_unbox = TRUE))
  })
}
