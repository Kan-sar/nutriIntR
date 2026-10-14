# Scientific method and current limits

Version 0.1 is an evidence explorer. Matching a study's nutrient pair does not
establish the outcome in a new person or meal. Qualitative findings show a
reported direction when the current catalog's form, meal and population conditions
are met. Target dose remains visible but is not modeled quantitatively.

## Sources reviewed on 14 October 2026

| PMID | Human evidence | Extraction used |
| --- | --- | --- |
| [3700141](https://pubmed.ncbi.nlm.nih.gov/3700141/) | Hallberg et al., 1986; meal experiments, 299 subjects | Vitamin C enhancement of non-heme food iron; depends on meal |
| [1984334](https://pubmed.ncbi.nlm.nih.gov/1984334/) | Cook et al., 1991; 61 volunteers | Calcium interactions depend on supplement form and meal context |
| [2911999](https://pubmed.ncbi.nlm.nih.gov/2911999/) | Hallberg et al., 1989; paired wheat-roll meals | Added phytate phosphorus 2, 25, 250 mg; relative absorption .82, .36, .18 |
| [8156984](https://pubmed.ncbi.nlm.nih.gov/8156984/) | Gleerup et al., 1993; 21 subjects | 340 mg calcium from milk/cheese before an iron meal by 2 or 4 hours; no duration effect detected |

Extraction is from abstracts. Full-text review, risk-of-bias assessment and
independent extraction remain pending; no formal evidence grade is assigned.
Missing sample sizes and confidence intervals remain unknown.

The phytate unit is **mg phytate phosphorus**, not mg phytic acid or mg sodium
phytate. Compound-mass conversions are deliberately absent. The three dose points
are study references and do not predict absolute absorption for entered meals.
Optional interpolation is linear in log dose between reported points. It is an
exploratory interpolation, not a fitted physiological law or validated model.
No extrapolation, confidence interval, combined multiplier or baseline is invented.

Events retain unique IDs and original quantities and are sorted by minute and ID.
Signed gaps distinguish before/after. Same meal ID permits a meal association;
nonzero within-meal gaps remain timing-unresolved. Separate-meal timing matches
only the recorded direction, form, 340 mg dose and 120/240-minute gaps.
No detected effect is not proof that the effect is zero.

## Correlation and modeling

Kaggle food composition supplies nutrient co-occurrence data per 100 g. Correlations
describe variation among food records, with confounding by food category, processing,
water content and compositional constraints. They are not human absorption outcomes.
No absorption ML model can be trained on these columns without valid measured
labels. The first increment records this feasibility limitation rather than
reporting misleading accuracy. A later independent study-level dataset is needed
for prediction, calibration, uncertainty and study-grouped validation.
