# R and JSON interface

From the project root:

```r
source("scripts/load_project.R")
input <- jsonlite::fromJSON("inst/extdata/example.json", simplifyVector = FALSE)
result <- analyze_interactions(input$events, input$context, input$interpolate,
  input$medications)
cat(result_json(result))
```

Every event needs `id`, `nutrient`, `form`, `amount`, `unit` and `meal_id`, plus
either `time_minutes` or `consumed_at`. Mineral amounts mean elemental mineral mass. The phytate reference
requires the explicit form `sodium_phytate_phosphorus` and measures phosphorus
mass. Different meal IDs denote separate meals. Equal timestamps alone do not
establish a shared meal. IDs remain stable when events are sorted.

All 477 definitions in USDA `nutrient.csv` are available through stable FDC IDs
(e.g. `"usda_1003"`) or unique exact names. Existing aliases remain valid.
Mass units convert among g/mg/ug/mcg, energy between kcal/kJ; IU and specialized
equivalent units retain their native measure. `amount_canonical` and
`canonical_unit` describe the normalized value; `amount_mg` is null for non-mass
measures. See [USDA catalog](USDA_CATALOG.md). Unknown nutrients
and forms remain valid inputs and return unsupported evidence findings. The
catalog currently applies to adult fed context; other contexts return unknowns.
Limit: 100 events (up to 9,900 directed pairs). Times are nonnegative offsets in
minutes from scenario start; input order does not change chronological order.

Use the dashboard's **Time consumed** widget to pick hours and minutes, then add
the event. JSON stores `consumed_at`, e.g. `"13:30"`. Times are local clock values;
there are no timezone controls or conversions. Time-only events refer to the
same day. Ordering and gaps derive from these values; `time_minutes` is computed
relative to the earliest event. Original strings and normalized local strings
(`consumed_at_local`) are retained.

For overnight scenarios, JSON also accepts dated local values such as
`"2026-10-14T23:50"` and `"2026-10-15T00:10"`. All events must use the same style
(time-only or dated local times). A time of `00:10` alone is earlier on the same
day than `23:50`; overnight rollover is never inferred from input row order.

Existing offset-only events are anchored to the first selected clock time when
adding a clock event; their gaps are preserved if they fit within the same day.
Existing clock events retain their times when another clock event is added.
Switching to offset entry converts the draft to minutes from its earliest event.
Mixed clock and offset-only events are rejected. Clock values take precedence
over any supplied stale minute offsets. Results update after **Analyze scenario**.

Medication events use `id`, `medication`, `route` and the same `time_minutes` or
`consumed_at` chronology. Nutrient and medication clocks share one origin so the
reported gap is meaningful. The selected medication catalog returns every
medication–nutrient pair as a matched rule or an explicit unsupported pair.
Statuses distinguish potential timing conflicts, separation consistent with the
cited label, non-timing diet guidance, chronic monitoring relationships and gaps.

`inst/extdata/example-clock.json` demonstrates USDA identities, mass and IU units,
local consumption times and oral ciprofloxacin. Schema version 1.2.0 adds
medications and medication findings. Engine/package version is 0.3.0.

`source_minus_target_minutes < 0` means the influencing nutrient occurred before
the target. Every non-self directed pair is returned, including unsupported
pairs, so coverage gaps are machine-readable.

`study_reference.relative_absorption` is a study-control ratio for the phytate
experiment, never an entered person's absorption fraction. Null means unavailable,
not zero. Interpolation is off by default, bounded within 2–250 mg phytate
phosphorus and labeled exploratory. No baseline or combined effect is available.

JSON Schema documents are in `inst/schema/`. Runtime checks additionally enforce
finite values and unique event IDs. New unsupported values are retained as gaps.

## Local HTTP service

```r
source("scripts/serve_api.R")
```

- `GET http://127.0.0.1:8000/health`: status and version.
- `GET http://127.0.0.1:8000/nutrients`: all 477 USDA definitions and native units.
- `GET http://127.0.0.1:8000/medications`: selected medication catalog and label rules.
- `POST http://127.0.0.1:8000/analyze`: JSON object matching the input schema.
- `POST http://127.0.0.1:8000/explain`: deterministic analysis plus a grounded local DeepSeek explanation.
- Success: HTTP 200 JSON result; invalid scenario: HTTP 400 JSON error.

The explanation endpoint uses Ollama at `NUTRIINT_OLLAMA_URL` (default
`http://127.0.0.1:11434`) and model `NUTRIINT_LLM_MODEL` (default
`deepseek-r1:1.5b`). It returns HTTP 503 when the local model is unavailable.
Model text can still be wrong; medication decisions must be verified against the
linked label and a pharmacist or prescriber.

The service binds to localhost. Authentication and public deployment are outside
this first increment. Run the dashboard separately with `shiny::runApp("app")`.
