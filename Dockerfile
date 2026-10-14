# R version and multi-platform base image are pinned, not floating tags.
FROM rocker/r-ver:4.6.1@sha256:4e6e6696ff54a86625c7f80046478e67014a2184e942b85eeff43db7b992d93e

ENV RENV_PATHS_LIBRARY=/opt/renv/library \
    RENV_PATHS_CACHE=/opt/renv/cache \
    RENV_CONFIG_CACHE_SYMLINKS=FALSE \
    RENV_CONFIG_AUTO_SNAPSHOT=FALSE \
    TZ=Asia/Kolkata \
    HOST=0.0.0.0

RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential gfortran libcurl4-openssl-dev libssl-dev libsodium-dev \
    libicu-dev libuv1-dev zlib1g-dev pkg-config \
    ca-certificates curl pandoc python3 python3-jsonschema \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/nutrient-engine
COPY renv.lock .Rprofile ./
COPY renv/activate.R renv/settings.json ./renv/
# The committed activation script bootstraps the exact renv version.
RUN Rscript -e 'options(timeout=600); renv::restore(prompt=FALSE)'

COPY . .
# These steps must succeed on a clean checkout; no host data or library is used.
RUN Rscript scripts/restore_data.R \
    && Rscript tests/run_tests.R \
    && Rscript tests/test_app.R \
    && Rscript tests/test_food.R \
    && Rscript scripts/analyze_foods.R \
    && Rscript scripts/demo.R \
    && python3 tests/validate_schemas.py \
    && useradd --create-home --uid 10001 nutrient \
    && chown -R nutrient:nutrient /opt/nutrient-engine

USER nutrient
EXPOSE 3838 8000
CMD ["Rscript", "scripts/serve_app.R"]
