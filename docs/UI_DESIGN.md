# Interface direction

The current four-section redesign and its validation status are documented in
[UX_REDESIGN.md](UX_REDESIGN.md). The notes below describe the earlier interface
and retained chart semantics; its old tab structure and stale-result behavior
have been superseded.

The dashboard uses an original, restrained research-workspace design. No source
code, artwork, logos or stylesheets were copied from the references below.

## References consulted

- [Radiant](https://vnijs.github.io/radiant/), an open-source R/Shiny analytics
  project featured in [Posit's Shiny gallery](https://shiny.posit.co/r/gallery/):
  clear separation of inputs and analytical results, task-based exploration,
  reproducible structured output. Our app retains an explicit analyze step.
- [IBM Carbon dashboard guidance](https://v10.carbondesignsystem.com/data-visualization/dashboards/):
  strong hierarchy, limited summary metrics, consistent data colors, whitespace,
  and details available on demand.
- [Posit application layout guide](https://shiny.posit.co/r/articles/build/layout-guide/):
  separate input and output areas and use tabbed views to organize related work.

## Applied choices

- Compact identity header and a two-column workspace: scenario composer on the
  left and larger analysis surface on the right. Single-column layout below
  760 px; horizontally scrollable tabs and tables avoid page-wide overflow.
- Three derived summaries: analyzed intake count, applicable qualitative matches,
  and all other directed pairs. Counts are not clinical scores or confidence.
- Native local-time picker and native units are retained. JSON editing moves to
  an expandable advanced section, with a visible intake count and analyze action.
- Draft changes explicitly warn that results and JSON export are from the last
  analyzed scenario. Invalid analysis clears results and gives an alert.
- Neutral off-white canvas, forest-green controls, restrained borders, consistent
  spacing, visible keyboard focus and a skip-to-results link. System fonts and
  local CSS require no external font requests or new package dependencies.
- Grey interaction/dose plots and dose recency table remain intact. Nutrient hue,
  opacity and brightness retain their existing meanings. Recency is relative to
  the latest entered intake, not wall-clock time or absorption strength.

## Verification

### Selected-nutrient refinement

The header now shows only Nutrient Lab and the nutrient count. Preview branding,
workspace subtitle, footer disclaimers and repeated interpretation caveats were
removed from the interface at the user's request; scientific limitations remain
in the project documentation and result metadata. Display labels are alphabetical,
use familiar names for common nutrients and retain distinctions such as IU versus
RAE. USDA IDs remain in the selected-nutrient hint and the catalog.

Food correlations now use unique nutrients from the last analyzed scenario, not
the previous fixed five-column image. An explicit 71-measure crosswalk maps USDA
IDs to Kaggle columns, including source misspellings. Only 100 g serving rows are
included; mass units convert to the selected measure's native unit and IU remains
separate. Unsupported measures are listed, not inferred. Pairwise-complete
Spearman coefficients and sample counts are computed reactively; fewer than three
complete foods or constant columns yield NA. Input doses and chronology do not
reweight the underlying food-composition observations. See `tests/test_food.R`.

Run `Rscript tests/run_tests.R` for the engine and plot-style checks and
`Rscript tests/test_app.R` for Shiny server draft/analysis/error/reset/filter
regressions. Browser checks cover timed intake entry, draft warning, analysis,
dose recency, interaction map, catalog search and responsive layout.
