# Reproduce a checkout

Use the commands in the README with Docker running in Linux-container mode.
The Dockerfile and Compose file are checked in with the application; neither
mounts a host R library or depends on untracked downloads. To reproduce a specific
release of the code, check out that commit before building.

## Inputs and pins

- Base: `rocker/r-ver:4.6.1` pinned to multi-platform index digest
  `sha256:4e6e6696ff54a86625c7f80046478e67014a2184e942b85eeff43db7b992d93e`.
- R packages: committed `renv.lock`; committed `renv/activate.R` bootstraps renv
  1.3.1. The container library is isolated at `/opt/renv/library`.
- USDA: `inst/extdata/nutrient.csv`, checked against its provenance manifest by
  the core tests.
- Kaggle: version 1, 8,789 records; download URL, archive checksum and CSV checksum
  are read from `data-raw/kaggle-manifest.json`. `scripts/restore_data.R` does not
  rewrite this manifest or query a floating latest version. A checksum mismatch
  stops the build rather than silently accepting different data.
- Source downloads, secrets, local libraries and generated reports are excluded
  from the Docker build context. Food data and reports are regenerated inside
  the image. Credentials are neither required nor copied.

The first build requires CRAN, the base-image registry, Ubuntu package servers
and Kaggle to be available. Ubuntu security packages and transitive OS packages
are not individually pinned, so this is a version-controlled analytical
environment, not a claim of bit-identical image builds. Archive a built image
with `docker save` if exact binary preservation is required. Once built, neither
service needs network access for analysis.

## Verify

The image build fails unless all of these pass:

```sh
Rscript scripts/restore_data.R
Rscript tests/run_tests.R
Rscript tests/test_app.R
Rscript tests/test_food.R
Rscript scripts/analyze_foods.R
Rscript scripts/demo.R
python3 tests/validate_schemas.py
```

Repeat the tests against an already-built image:

```sh
docker compose run --rm --no-deps app Rscript tests/run_tests.R
docker compose run --rm --no-deps app Rscript tests/test_app.R
docker compose run --rm --no-deps app Rscript tests/test_food.R
docker compose run --rm --no-deps app python3 tests/validate_schemas.py
```

`docker compose up -d --wait` waits for HTTP health checks for both services.
The Docker reproducibility GitHub Actions workflow additionally posts the clock
example to `/analyze` and checks four events and twelve directed pairs. The
separate R workflow runs package build/check as well.

To export the generated reports from the running dashboard container:

```sh
docker compose cp app:/opt/nutrient-engine/reports/generated ./reproduced-reports
```

To inspect the restored package environment:

```sh
docker compose run --rm --no-deps app Rscript -e "renv::status(); sessionInfo()"
```

## Troubleshooting

- Docker daemon unavailable: start Docker Desktop and wait for its Linux engine
  before running Compose. On Windows, WSL 2/virtualization may need enabling by
  the machine administrator; do not change system security settings just to run
  the project.
- Port already allocated: use the `APP_PORT` / `API_PORT` overrides in the README.
- Kaggle/CRAN unavailable: retry the build when connectivity returns. Cached
  layers are reused; checksum failures require source review, not disabling checks.
- View startup errors with `docker compose logs app api`.

Reference implementations: [Rocker versioned R images](https://rocker-project.org/images/versioned/r-ver.html)
and [Docker Compose](https://docs.docker.com/compose/gettingstarted/).
