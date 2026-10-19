# UX redesign: implementation and validation

## Implemented workflow

Four primary destinations replace ten equal-weight tabs: Your intakes, Findings,
Evidence and Explore. Intake entry switches between nutrients and medications;
timing precedes the add action. Each intake has direct Edit and Remove controls.
Draft timeline preview also supports medication-only drafts. Analysis still needs
a nutrient. Advanced JSON and reset remain available without dominating entry.

Findings combine type and intake filters, a selectable timeline, a keyboard-accessible
relationship list, and a source-linked applicability panel. Network and matrix are
optional disclosures. AI explanation and structured results are secondary details.
Editing clears results and selection; export only appears for a current analysis.

## Visualization decision

Use echarts4r 0.5.0, the available repository release, pinned with its dependencies
in renv.lock. The documentation site displayed 0.5.1 during evaluation, but it was
not available from the configured repository. No development snapshot is required.
The R package supplies browser dependencies locally. No chart CDN is needed.

The app supplies ECharts options through a small R presentation adapter and a
small click callback. Analysis outputs and statistical calculations are unchanged.
Time-axis points represent intakes, with shape distinguishing medication and nutrient.
Network nodes use fixed positions; graph distance and line width do not represent
effect strength. Directed relationships retain arrows; drug-drug checks are undirected
and mirrored in the matrix. Unknown and unresolved statuses have explicit text.
Food heatmaps retain Spearman coefficients and sample counts, with missing values
left blank rather than converted to zero. Reference dose plots remain static.

visNetwork remains a possible network-only fallback if user testing reveals a material
navigation benefit; plotly remains an alternative for a timeline-and-matrix focus.
Neither is added as a dependency by this redesign.

## Interaction and accessibility

Typed identifiers distinguish nutrient and medication collections even when raw IDs
coincide. Repeated nutrients stay separate. Chart events are validated against the
current result and filters before selection. List buttons preserve keyboard focus
when evidence updates. Native filters provide an alternative to canvas interaction.
Motion is disabled in charts and reduced-motion preferences suppress CSS transitions.
Narrow layouts stack the entry and evidence columns; long tables scroll locally.

## Verification

- `Rscript tests/run_tests.R`: existing engine and plotting contracts.
- `Rscript tests/test_app.R`: draft edits, invalid/stale edits, reset and timing.
- `Rscript tests/test_explorer.R`: identifiers, repeated intakes, matrix direction,
  mixed pair types, latest-draft analysis, filters, selections and row actions.
- `Rscript tests/test_food.R`: source-data and correlation behavior.
- Browser checks completed: empty startup; standard nutrient entry with an omitted
  meal; direct modal editing from 6 mg to 8 mg; stale-result clearing and reanalysis;
  mixed scenario rendering; timeline clicks reducing seven relationships to four;
  finding-to-source inspection; matrix-cell selection; and keyboard-focus retention
  while the evidence panel updates. Preview: `screenshots/linked-findings.png`.
- All four R test scripts passed locally, including 110 engine checks. The new linked
  explorer tests run in CI and the Docker build. A fresh Docker build, full mobile
  device pass, and assistive-technology audit have not been run for this change.

## Remaining human validation

The primary audience and acceptable complexity have not been confirmed through user
research. Conduct approximately five representative task sessions, measuring first
analysis completion, correction errors, finding/source retrieval, and interpretation
of unknown coverage. Compare matrix and network at small and crowded sizes before
choosing a default exploration view. Test with assistive technology, touch devices,
and realistic larger scenarios before production adoption. No user-study results or
performance superiority are claimed by the implementation.

## AI explanation verification (2026-10-19)

Live tests with the installed deepseek-r1:1.5b model exposed omitted findings,
unsupported separation wording, and cross-pair substitutions in free-form output.
The explanation now uses structured, per-finding requests. The model chooses from
supplied evidence passages; application validation rejects missing or duplicate IDs,
changed statuses, cross-pair substitutions, and wording outside those passages.
This is explicitly labeled an AI-assisted evidence summary, not unrestricted AI prose.
Statuses, source URLs, scope, unresolved counts, and visualization descriptions are
assembled directly from the deterministic analysis. Scenarios without matches are
labeled evidence-only and do not trigger text generation.

The dashboard now shows progress, restores the Generate button after completion,
and offers Check connection. Live generation and source rendering were verified in
the browser. Test samples completed in a few seconds on this machine; this is not a
latency guarantee. `tests/test_explainability.R` covers missing-model/unavailable
service, empty/malformed/truncated responses, timeout, retry, stale-output clearing,
status refresh, evidence coverage and text integrity without requiring a live model.
