# Nutrient Interaction Engine

An R research project for exploring nutrient amounts, forms, meal context and
chronology through source-backed absorption findings, visualizations and JSON.

Development plan: [DEVELOPMENT_PLAN.md](DEVELOPMENT_PLAN.md).

Version 0.1 is a working research preview: an R engine, Shiny dashboard, JSON
output and localhost API, with a small human-study evidence catalog and a
reproducible Kaggle food-composition analysis.

## Run in RStudio

Open `nutrient-engine.Rproj`, then restore the locked environment:

```r
renv::restore()
shiny::runApp("app")
```

For a first setup without renv activation, use `Rscript --vanilla scripts/restore_environment.R`.
The lockfile records the development environment (R 4.6.1 and package versions).

## Reproduce the data analysis and examples

```r
source("scripts/acquire_kaggle.R")
source("scripts/analyze_foods.R")
source("scripts/demo.R")
```

The public Kaggle dataset has 8,789 food records and 77 columns. Acquisition is
versioned and checksummed in `data-raw/kaggle-manifest.json`; downloads are ignored
by Git. Analysis reports and plots are generated in `reports/generated/`.
The selected source columns `irom` and `zink` are mapped to iron and zinc.

## R and JSON

```r
source("scripts/load_project.R")
input <- jsonlite::fromJSON("inst/extdata/example.json", simplifyVector = FALSE)
result <- analyze_interactions(input$events, input$context, input$interpolate)
cat(result_json(result))
```

Start the local API with `source("scripts/serve_api.R")`. It exposes `GET /health`
and `POST /analyze` at `http://127.0.0.1:8000`. See [JSON/API contract](docs/JSON_API.md)
and schemas in `inst/schema/`.

## What the first version supports

- Unique nutrient events with original and normalized quantities, forms, meal IDs
  and times; deterministic chronological ordering.
- Four curated evidence rules for vitamin C/iron, calcium supplements/iron,
  phytate/iron, and a specific prior-calcium timing experiment.
- Timeline, directed evidence map, phytate study dose curve, source table and
  downloadable JSON. All unsupported directed pairs remain visible in JSON.
- Study-specific phytate observations and optional bounded log-dose interpolation.
- Spearman correlations, coverage and pair counts for five nutrients across food
  records, with explicit limits on interpretation.

This is currently an **evidence explorer**. Absolute bioavailability and combined
effects return `null` because no validated general model has been fitted. A
study-control absorption ratio is not an individual's absorption fraction.
Chemical forms, meal conditions and timing change applicability. Unknown evidence
does not mean no interaction. No formal certainty grade is assigned to abstracts.

See [methodology and citations](docs/METHODOLOGY.md), [development progress](docs/PROGRESS.md),
and [development plan](DEVELOPMENT_PLAN.md). Independent study extraction,
validation, wider nutrient coverage and justified predictive modeling are later
milestones. Food-composition columns do not provide human absorption labels.

## Verify

```text
Rscript tests/run_tests.R
Rscript scripts/demo.R
python tests/validate_schemas.py
R CMD build .
R CMD check --no-manual nutrientengine_0.1.0.tar.gz
```

Schema checks require Python `jsonschema` (`python -m pip install jsonschema`).
GitHub Actions restores R dependencies and runs these checks. Windows sessions
with an inherited Unix `C.UTF-8` locale may need a Windows-supported locale for
`R CMD check`; this does not affect the engine's numeric test cases.

Code is MIT licensed; source data retain their recorded licenses. Do not commit
credentials or identifiable health data.
