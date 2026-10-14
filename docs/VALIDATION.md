# First-increment validation

## Version 0.2 extension

The extension passes 77 R regression checks and four JSON schema validations
(input/output for minute-offset and local-clock scenarios). Tests cover all 477
USDA definitions in batches, source-file checksum, unit conversion and forbidden
cross-measure conversions, stable IDs, ambiguous names, special units, chronological
sorting, local clock values, dated midnight crossings and invalid calendar/time
values. The dashboard was exercised by selecting USDA protein and adding a timed
event; the final local time-only widget is checked separately in browser QA.

USDA `GET /nutrients` returns 477 records/unique IDs. Local package build/check
passes with Status: OK. Final browser QA selected 09:45 through the native time
widget, added and analyzed the event, and confirmed 09:45 in output with a
105-minute gap from the 08:00 event. There are no timezone fields in the UI or
result. Linux CI repeats the tests for version 0.2. The historical first-increment
results below refer to version 0.1.

Verified on 14 October 2026 using R 4.6.1 on Windows.

| Check | Result |
| --- | --- |
| Core R regression suite | 30 checks passed |
| Example input against JSON Schema 2020-12 | Passed |
| Generated result against JSON Schema 2020-12 | Passed |
| R package build | Passed |
| R CMD check --no-manual | Status: OK; no errors, warnings or notes |
| renv status after local hydration and snapshot | No issues found; project consistent |
| Local API valid example | HTTP 200, evidence_explorer, four events and twelve directed pairs |
| Local API invalid scenario | HTTP 400 |
| Shiny browser exercise | Initial render, add event, recalculate, reset, invalid JSON and correlation tab verified |
| Kaggle acquisition | Version 1; 8,789 records and 77 columns; checksums committed |
| Selected food nutrient parsing | Five columns, all 8,789 values parsed per column |

This verifies software behavior and transcription of the cited study anchors.
It does **not** establish predictive validity, clinical accuracy, causal effects
from food correlations, or independent validation of a quantitative absorption
model. Full-text appraisal, held-out studies, model calibration, uncertainty
estimation and performance benchmarking remain future work.

The initial Windows package check encountered an inherited Unix locale
(`C.UTF-8`), which R on this machine could not set. A Windows-supported locale
(`English_United States.utf8`) was used only for the checking process. No system
locale settings were changed. An unused namespace declaration was corrected before
the clean package check.

The Python schema checker uses jsonschema 4.26.0. Direct pip network access failed;
official PyPI wheels were downloaded over verified HTTPS using PowerShell, their
SHA-256 hashes matched PyPI metadata, and pip installed them locally without a
network index. No certificate verification was bypassed.

The initial Linux CI run passed the core and schema checks but exposed a library
selection bug at package checking: restoring under `--vanilla` populated the user
library while later commands activated an empty project library. Restoration now
targets `renv::paths$library()` explicitly, and test/demo commands activate the
project. CI also pins R 4.6.1 to match the lockfile. See Actions for the subsequent
run; it remains separate from the local outcomes above.
