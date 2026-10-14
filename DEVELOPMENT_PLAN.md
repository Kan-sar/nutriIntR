# Nutrient Interaction Engine — development plan

Saved: 14 October 2026. Status: development started.

## Objective and rubric

Build an end-to-end, reproducible R project that explains how nutrient identity,
chemical form, amounts, meal context, and chronological order relate to absorption
and bioavailability. Deliver interactive visuals and a documented JSON interface.
Cover acquisition, cleaning, EDA, statistics, feature engineering, modeling,
validation, software development through GitHub, documentation, and presentations.
The attached rubric is reference material. Its week labels pace development only;
milestones below are named by outcomes.

## Scope and scientific contract

Start with adult dietary non-heme iron interactions with vitamin C, calcium, and
phytate, then extend to zinc/copper, calcium/vitamin D, fat-soluble vitamins/fat,
and other combinations after evidence extraction. Include nutrient forms, dose
units, meal/fasting status, population, and time differences in the data model.

Separate correlations in food composition from human absorption experiments.
Never treat food nutrient correlations as causal absorption labels. Human, animal,
and in-vitro evidence must remain distinguishable. Do not invent baseline
absorption, effect sizes, uncertainty intervals, physiological timing constants,
or evidence grades. Unsupported combinations and amounts return explicit unknowns.
Absolute bioavailability requires a suitable validated model or measured baseline.
The engine is an educational research tool, not a clinical dosing system.

## Milestones and pacing

The pacing target is eight development weeks, with overlapping documentation and
testing. These are effort estimates, not automatic schedules or milestone names.

| Milestone | Approximate effort | Deliverables and exit criteria |
| --- | --- | --- |
| Problem Definition and Repository Foundation | 1 week | Scope, GitHub repository, README, RStudio project, license, contribution guide, code of conduct, input/output schema, introductory presentation outline |
| Evidence and Dataset Acquisition | 1 week | Kaggle dataset assessment, primary literature survey, versioned source/license manifest, evidence table with nutrient forms, doses, outcomes, timing, population, confounders and limitations |
| Data Preparation and Exploratory Analysis | 1 week | Reproducible acquisition and cleaning, unit normalization, missingness/duplicates/outliers checks, coverage summaries, dose distributions, evidence maps and appropriate correlation plots |
| Statistical Analysis and Feature Engineering | 1 week | Statistical question and analysis plan; dose, form, time-gap, order, meal and study features; appropriate correlations/regression, intervals, heterogeneity, sensitivity analysis and multiple-comparison handling |
| Interaction Engine and Interfaces | 1.5 weeks | Tested R engine, source-backed rule matching, dose and chronology support, JSON export, Shiny dashboard, API; ML only if suitable observed outcomes exist |
| Validation and Performance Evaluation | 1 week | Reference cases, held-out studies where available, unit/integration/schema tests, calibrated uncertainty where justified, runtime benchmarks and CI |
| Documentation, Reports and Presentations | 1 week | Quarto analysis report, methodology, provenance, model card, API reference, user guide, introductory presentation plus two review presentations |
| Final Demonstration and GitHub Submission | 0.5 week | Clean-clone reproduction, end-to-end demo, tagged release, final report and presentation, rubric checklist |

## Evidence acquisition

Use Kaggle for food-composition data exploration, not absorption ground truth.
Prefer primary studies and recognized nutrition references for mechanisms and
effects. Record source URL, access date, version, license, study design, population,
sample size where known, original units, nutrient forms, doses, time gaps,
meal composition, outcome definition, effect and uncertainty, and limitations.
Use `NA`/JSON null for unavailable fields rather than inferred values.

## Data and analysis pipeline

1. Download versioned source data and retain checksums and source metadata.
2. Normalize names and units while preserving originals; detect invalid values,
   duplicates and outliers; document missingness decisions.
3. Produce EDA: coverage, nutrient frequency, dose distributions, missingness,
   study populations, interaction direction and timing-window coverage.
4. Compute food-composition correlations with pairwise sample counts. Report
   serving basis and compositional/confounding limitations.
5. Apply regression, mixed effects or meta-analysis only with suitable observations
   and comparable outcomes. Group validation by study to avoid leakage.
6. Evaluate whether ML has valid labels and enough independent observations;
   retain an explicit feasibility decision if it cannot be justified.

## Engine and interfaces

Input is a sequence of nutrient events: stable ID, nutrient, chemical form,
amount, unit, time in minutes, meal ID, meal state and population. Normalize and
validate, sort events deterministically, retain original identifiers, construct
directed source/target pairs, match evidence context, and return source-backed
findings and uncertainty/gaps. Study-specific dose interpolation must be explicitly
requested and labeled; chronology alone does not justify physiological decay.

Output includes schema/model version, normalized events, ordered pair findings,
dose and timing applicability, direction, quantitative result if supported,
citations, assumptions, warnings and unsupported cases. Aggregate effects only
when a validated combined model exists; never multiply unrelated study effects.

Build a Shiny dashboard with event editor/import, chronological plot, interaction
network, dose-response plot, evidence table, context and uncertainty information,
and JSON download. Add a `plumber` API after the core R contract stabilizes.

## Validation and acceptance

- Test unit equivalence, invalid input, zero and extreme doses, missing fields,
  supported/unsupported forms, meal context, order invariance of sorted input,
  stable IDs, chronology, JSON round trips and absent evidence.
- Verify literature anchor values without claiming these are held-out validation.
- Assess predictive error/calibration only when suitable held-out outcomes exist.
- Check deterministic output, bounded ratios, transparent provenance and no false
  certainty. Validate clean-environment reproduction and CI.
- Benchmark increasing event counts and document pairwise scaling.
- Confirm all displayed estimates distinguish study observations, interpolations,
  assumptions and user-provided baselines.

## Repository and workflow

Use `R/`, `app/`, `api/`, `data-raw/`, `inst/extdata/`, `inst/schema/`, `analysis/`,
`tests/`, `reports/`, `presentations/`, `kaggle/`, `docs/`, and GitHub Actions.
Track dependencies with `renv` and commit `renv.lock`. Ignore credentials, local
RStudio state and redistributable raw downloads; provide acquisition scripts.

Use RStudio for R development, Kaggle for data acquisition/notebook exploration,
and GitHub for incremental commits, issues, CI and submission. Commit meaningful
units: foundation; source/evidence schemas; normalization; rule engine; timing;
JSON; dashboard; acquisition/EDA; validation; reports and presentations.

## Final demonstration

Enter timed nutrient events, inspect changes in evidence applicability when doses
or order change, view the timeline/network and study curves, inspect citations,
download JSON, demonstrate unknown combinations, and reproduce from a clean clone.

## Initial implementation boundary

The first increment establishes the repository and a working evidence browser and
interaction engine with tests, JSON and Shiny visuals. It does not complete the
entire eight-week research and validation program. Later milestones require more
primary-data extraction, independent validation and the statistical/ML feasibility
gate described above.
