# Contributing

Open an issue describing the scientific question or software change. Add small,
reviewable commits on a `codex/` topic branch from `main`. Run:

```sh
Rscript tests/run_tests.R
Rscript tests/test_app.R
Rscript tests/test_food.R
docker compose config --quiet
```

Submit pull requests to `main` in `Kan-sar/nutriIntR`. GitHub Actions checks the
R package and the clean Docker Compose build, dashboard and API. Docker startup
and data-restoration instructions are in the README.

For evidence changes, include a primary citation, extraction location, exact
nutrient form, dose basis, population, study conditions and limitations. Keep
observed results, exploratory interpolation and unsupported inference distinct.
Do not use food composition as human absorption labels. Do not commit credentials
or identifiable health data. Document assumptions and add meaningful regression
cases for changed scientific behavior.
