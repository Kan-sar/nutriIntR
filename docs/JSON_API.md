# R and JSON interface

From the project root:

```r
source("scripts/load_project.R")
input <- jsonlite::fromJSON("inst/extdata/example.json", simplifyVector = FALSE)
result <- analyze_interactions(input$events, input$context, input$interpolate)
cat(result_json(result))
```

Every event needs `id`, `nutrient`, `form`, `amount`, `unit`, `time_minutes`, and
`meal_id`. Mineral amounts mean elemental mineral mass. The phytate reference
requires the explicit form `sodium_phytate_phosphorus` and measures phosphorus
mass. Different meal IDs denote separate meals. Equal timestamps alone do not
establish a shared meal. IDs remain stable when events are sorted.

Units are `g`, `mg`, `mcg` and `ug`. Canonical output is mg. Unknown nutrients
and forms remain valid inputs and return unsupported evidence findings. The
catalog currently applies to adult fed context; other contexts return unknowns.
Limit: 100 events (up to 9,900 directed pairs). Times are nonnegative offsets in
minutes from scenario start; input order does not change chronological order.

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
- `POST http://127.0.0.1:8000/analyze`: JSON object matching the input schema.
- Success: HTTP 200 JSON result; invalid scenario: HTTP 400 JSON error.

The service binds to localhost. Authentication and public deployment are outside
this first increment. Run the dashboard separately with `shiny::runApp("app")`.
