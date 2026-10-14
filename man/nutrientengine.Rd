\name{nutrientengine}
\alias{analyze_interactions}
\alias{normalize_events}
\alias{read_evidence}
\alias{nutrient_catalog}
\alias{medication_catalog}
\alias{read_medication_evidence}
\alias{normalize_medications}
\alias{llm_status}
\alias{explainability_payload}
\alias{generate_llm_explanation}
\alias{result_json}
\alias{phytate_reference}
\alias{plot_timeline}
\alias{plot_network}
\alias{plot_phytate_curve}
\alias{plot_medication_network}
\title{Explore Nutrient Interaction Evidence}
\description{Validate chronological nutrient events and match a small curated
human-study catalog. Report supported directions, unknowns, study references,
and JSON without inventing absolute bioavailability.}
\usage{
normalize_events(events)
read_evidence()
nutrient_catalog()
medication_catalog()
read_medication_evidence()
normalize_medications(medications)
phytate_reference(amount_mg, interpolate = FALSE)
analyze_interactions(events,
  context = list(population = "adult", meal_state = "fed"),
  interpolate = FALSE, medications = NULL)
result_json(result, pretty = TRUE)
llm_status()
explainability_payload(result)
generate_llm_explanation(result, timeout_seconds = 120)
plot_timeline(result)
plot_network(result)
plot_phytate_curve(interpolate = FALSE, result = NULL)
plot_medication_network(result)
}
\arguments{
  \item{events}{Data frame or list of event objects with id, nutrient, form,
    amount, unit and meal_id. Supply time_minutes or a local HH:MM
    consumed_at timestamp for every event. Between 1 and 100 events.}
  \item{context}{List with scalar population and meal_state strings. Catalog
    findings currently cover adult fed context.}
  \item{medications}{Optional data frame or list of medication objects with id,
    medication, route and the same chronology mode as the nutrient events.}
  \item{amount_mg}{Finite nonnegative phytate phosphorus mass in mg.}
  \item{interpolate}{Whether to permit exploratory log-dose interpolation between
    phytate study anchors; no extrapolation.}
  \item{result}{Result of analyze_interactions for scenario plots. Retained as an
    optional compatibility argument for plot_phytate_curve; that function now
    draws only the study curve. Intake recency is displayed in a dashboard table.}
  \item{pretty}{Whether to indent JSON output.}
  \item{timeout_seconds}{Maximum wait for the local Ollama generation request.}
}
\value{Normalization functions return sorted data frames; evidence and catalog
functions return data frames; llm_status returns model availability;
explainability_payload returns the deterministic subset sent to the model;
generate_llm_explanation returns generated text plus model and grounding metadata;
phytate_reference a reference list; analyze_interactions a structured result list;
result_json a JSON string. Plot functions draw to the current graphics device.}
\details{The complete USDA nutrient.csv dictionary is bundled with stable IDs
and native units. Mass, energy, activity and specialized equivalent measures remain
distinct. Unknown interactions return unknown findings. Original consumed_at
times are retained; ordering and time gaps use local clock values. Medication
rules are selected DailyMed label findings and do not provide prescribing advice.
The optional local DeepSeek layer explains deterministic findings and does not
create interactions. Phytate anchors describe wheat-roll experiments, not individual predicted
absorption. Nonzero within-meal gaps remain unresolved. Missing evidence does not
mean no biological interaction. Refer to the project methodology and source PMIDs
for limitations.}
\examples{
events <- data.frame(
  id = c("fe", "c"), nutrient = c("iron", "vitamin_c"),
  form = c("non_heme_food", "ascorbic_acid"), amount = c(6, 50),
  unit = "mg", time_minutes = 0, meal_id = "lunch")
result <- analyze_interactions(events)
cat(result_json(result))
phytate_reference(25)
}
