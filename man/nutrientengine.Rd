\name{nutrientengine}
\alias{analyze_interactions}
\alias{normalize_events}
\alias{read_evidence}
\alias{nutrient_catalog}
\alias{result_json}
\alias{phytate_reference}
\alias{plot_timeline}
\alias{plot_network}
\alias{plot_phytate_curve}
\title{Explore Nutrient Interaction Evidence}
\description{Validate chronological nutrient events and match a small curated
human-study catalog. Report supported directions, unknowns, study references,
and JSON without inventing absolute bioavailability.}
\usage{
normalize_events(events)
read_evidence()
nutrient_catalog()
phytate_reference(amount_mg, interpolate = FALSE)
analyze_interactions(events,
  context = list(population = "adult", meal_state = "fed"),
  interpolate = FALSE)
result_json(result, pretty = TRUE)
plot_timeline(result)
plot_network(result)
plot_phytate_curve(interpolate = FALSE)
}
\arguments{
  \item{events}{Data frame or list of event objects with id, nutrient, form,
    amount, unit and meal_id. Supply time_minutes or a local HH:MM
    consumed_at timestamp for every event. Between 1 and 100 events.}
  \item{context}{List with scalar population and meal_state strings. Catalog
    findings currently cover adult fed context.}
  \item{amount_mg}{Finite nonnegative phytate phosphorus mass in mg.}
  \item{interpolate}{Whether to permit exploratory log-dose interpolation between
    phytate study anchors; no extrapolation.}
  \item{result}{Result of analyze_interactions.}
  \item{pretty}{Whether to indent JSON output.}
}
\value{normalize_events returns a sorted data frame; read_evidence and
nutrient_catalog return data frames;
phytate_reference a reference list; analyze_interactions a structured result list;
result_json a JSON string. Plot functions draw to the current graphics device.}
\details{The complete USDA nutrient.csv dictionary is bundled with stable IDs
and native units. Mass, energy, activity and specialized equivalent measures remain
distinct. Unknown interactions return unknown findings. Original consumed_at
times are retained; ordering and time gaps use local clock values. Phytate anchors describe wheat-roll experiments, not individual predicted
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
