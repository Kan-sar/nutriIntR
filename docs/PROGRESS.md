# Development progress

14 October 2026 — first working increment.

## USDA and consumption-time extension (version 0.2)

- Bundled all 477 definitions from USDA's April 2026 `nutrient.csv`, preserving
  source IDs/names/units and a reproducible download manifest.
- Added searchable USDA selection, a filterable catalog tab and `GET /nutrients`.
- Added mass and energy normalization while retaining IU and other specialized
  measures. USDA phytic acid stays distinct from the phytate phosphorus extension.
- Added a native hours/minutes **Time consumed** widget. Inputs and JSON use local
  clock values without timezone conversion. Existing minute offsets remain usable.
- Added local-time chronology and clock labels on the timeline. Time-only values
  refer to the same day; dated local JSON inputs support overnight scenarios.
- Added USDA identity, units, consumption time and catalog provenance to output,
  and extended the JSON schemas to version 1.1.0.
- Expanded regression coverage to 77 checks, including every USDA definition's
  native-unit round trip. Both offset and local-clock JSON examples validate.

Full catalog coverage does not expand the scientific evidence automatically.
New unsupported pairs still return unknown findings; absolute predictions remain
pending the validation work below.

## Nutrient colors and recency display

Interaction map and dose-reference graphics now use grey backgrounds. Each USDA
definition and the phytate study extension has a deterministic, unique RGB color,
consistent across views and input ordering. Same-nutrient events share a color.
Labels and legends remain available because a large catalog inevitably contains
visually similar hues. Opacity is relative to the latest entered event: the
oldest event is 25% opaque, the latest is 100%, with linear scaling between them.
Equal times have equal opacity; a simultaneous scenario is fully opaque.

The dose view retains hollow phytate study anchors without assigning intake ages
to published observations. A separate strip shows every scenario intake with
its nutrient color and recency opacity; events use separate rows to avoid
overplotting changing their apparent transparency. Arrow colors continue to
describe evidence direction. No absorption calculation or JSON contract changed.

The expanded suite has 84 checks, including palette uniqueness, cross-view color
stability, repeat nutrients and recency behavior. Plot exports and both dashboard
tabs were visually inspected.

## Completed

- Saved the rubric-based plan with named milestones and eight-week pacing.
- Created a private GitHub repository and pushed incremental commits.
- Created an RStudio project and an installable R package.
- Implemented amount/unit validation, explicit nutrient forms, chronological
  ordering, unique event IDs, context matching and directed pair findings.
- Extracted four human-study evidence rules with source PMIDs and limitations.
- Implemented study-reference phytate dose points and opt-in interpolation.
- Added JSON input/output schemas, examples and a local HTTP API.
- Built a Shiny scenario editor with timeline, directed map, evidence findings,
  source catalog, dose-reference plot, food correlations and JSON download.
- Acquired Kaggle dataset version 1: 8,789 records, 77 columns, CC0 metadata;
  recorded provenance/checksums and handled source column typos explicitly.
- Generated five-nutrient Spearman correlations, pair counts and parsing coverage.
- Added R regression tests, schema checks, package documentation and CI workflow.
- Captured R/package dependency versions in renv.

## Verification

Core tests cover input validation, unit equivalence, sorting, unsupported
forms/populations/contexts, order-sensitive evidence matching, zero doses,
reported dose anchors, bounded interpolation, JSON serialization and unknowns.
The local API was exercised with a valid scenario (HTTP 200, four events, twelve
directed pairs) and invalid input (HTTP 400). The dashboard was exercised by
adding an event, recalculating, resetting, viewing food correlations and entering
invalid JSON. Generated plots were visually inspected.

Package and schema validation outcomes are recorded in VALIDATION.md after the
final check of this increment. CI is configured but its remote result must be
checked separately from local execution.

RStudio launched, but its project file dialog failed automation with
`element 485 is not available in cached app state for rstudio.exe` on the recovery
attempt. Native UI control was stopped according to the computer-use skill's
recovery rule. Open `nutrient-engine.Rproj` manually; R scripts and the Shiny app
were exercised through R directly. A hosted Kaggle notebook has not been uploaded
or executed; acquisition used Kaggle's public API.

## Next milestone: expand and appraise the evidence

1. Retrieve full texts and independently verify nutrient forms, dose ranges,
   populations, timing protocols, effect definitions and uncertainty.
2. Expand the source schema to study arms and participant-level outcomes where
   licensed; retain observed and inferred fields separately.
3. Add richer context filters and distinguish uncertainty in dose applicability
   from uncertainty in effect magnitude.
4. Evaluate calcium, zinc/copper and other planned interactions with evidence
   conflicts preserved; add new rules only with suitable citations.
5. Define held-out study validation before fitting a quantitative model.

The current food dataset has no absorption labels. Absorption ML, calibration,
absolute bioavailability, multi-nutrient combined effects and personalized
prediction remain pending. They cannot be validated using food correlations.
Quarto reports and presentation deliverables are planned; the report source is
present, but final reports and presentations are not complete.
