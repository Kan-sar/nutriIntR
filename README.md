# nutriIntR

An R  project for exploring nutrient amounts, forms, meal context and
chronology through nutrient correlation findings, visualizations and JSON.

Development plan: [DEVELOPMENT_PLAN.md](DEVELOPMENT_PLAN.md).

The current dashboard now covers **all 477 nutrient definitions in the USDA food dataset** (April 2026).
Search by nutrient name or USDA ID. Use the **Time consumed** picker to record
local consumption times; minute-offset input remains available. See
[catalog coverage and units](docs/USDA_CATALOG.md) and [time/JSON examples](docs/JSON_API.md).

## Run with Docker Compose (recommended)

Repository: [Kan-sar/nutriIntR](https://github.com/Kan-sar/nutriIntR)
(private; authenticate with GitHub before cloning).

Install Docker Desktop with Linux containers (or Docker Engine plus Compose on
Linux), then run from PowerShell, macOS Terminal or a Linux shell:

```sh
git clone https://github.com/Kan-sar/nutriIntR.git
cd nutriIntR
docker compose up --build -d --wait --wait-timeout 120
```

Open the dashboard at **http://localhost:3838**. The API is at
**http://localhost:8000**, with health status at `/health`.
the built services run offline.

The image pins R 4.6.1 by base-image digest, restores package versions from
`renv.lock`, downloads the committed Kaggle dataset version and verifies both
archive and CSV checksums. It runs all R tests, generates reports and validates
the JSON contracts during the build. USDA definitions are already bundled.
Application processes run as a non-root user and ports bind only to localhost.

```sh
docker compose ps                 # both services should be healthy
docker compose logs -f            # show service output
docker compose down              # stop and remove project containers
```

If the ports are occupied, set `APP_PORT` and `API_PORT` before starting:

```powershell
$env:APP_PORT = "3839"
$env:API_PORT = "8001"
docker compose up --build -d --wait --wait-timeout 120
```

On macOS/Linux, use `APP_PORT=3839 API_PORT=8001 docker compose up --build -d --wait`.
See [reproduction and verification](docs/REPRODUCIBILITY.md) for tests, report
export and the boundaries of the version pins.

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
source("scripts/restore_data.R")
source("scripts/analyze_foods.R")
source("scripts/demo.R")
```

`restore_data.R` reproduces the committed snapshot; `acquire_kaggle.R` is an
explicit dataset-update operation and can change the provenance manifest.
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
`GET /nutrients` returns the complete USDA dictionary. Reproduce the bundled
source with `source("scripts/acquire_usda.R")`; its provenance is committed in
`data-raw/usda-manifest.json`.

## What the first version supports

- Unique nutrient events with original and normalized quantities, forms, meal IDs
  and times; deterministic chronological ordering from timestamps or elapsed minutes.
- Every USDA dictionary entry, with stable IDs, searchable selection and native
  units. IU, equivalent measures and energy are kept distinct from mass.
- Four curated evidence rules for vitamin C/iron, calcium supplements/iron,
  phytate/iron, and a specific prior-calcium timing experiment.
- Timeline, directed evidence map, phytate study dose curve, source table and
  downloadable JSON. All unsupported directed pairs remain visible in JSON.
- Interaction map and dose view use grey backgrounds, stable nutrient identity
  colors and intake-recency opacity (25% to 100%) plus brightness (45% to 100%).
  The dose view keeps timeless study observations hollow and lists entered intakes
  in a recency table with consumption times, age and visual scaling values.
- Study-specific phytate observations and optional bounded log-dose interpolation.
- Custom Spearman correlations and pair counts for the nutrients selected in the
  analyzed scenario, using an explicit 71-measure USDA-to-food-data crosswalk.

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
Rscript tests/test_app.R
Rscript tests/test_food.R
Rscript scripts/demo.R
python tests/validate_schemas.py
R CMD build .
R CMD check --no-manual nutrientengine_0.2.0.tar.gz
```

Schema checks require Python `jsonschema` (`python -m pip install jsonschema`).
GitHub Actions restores R dependencies and runs these checks. Windows sessions
with an inherited Unix `C.UTF-8` locale may need a Windows-supported locale for
`R CMD check`; this does not affect the engine's numeric test cases.

Code is MIT licensed; source data retain their recorded licenses. Do not commit
credentials or identifiable health data.
