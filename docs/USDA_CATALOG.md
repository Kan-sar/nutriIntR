# Complete USDA nutrient catalog

The project bundles all **477 records** from `nutrient.csv` in USDA FoodData
Central's **30 April 2026 Foundation Foods CSV archive**. USDA states that
archives from April 2023 onward include supporting tables. The downloaded table
is used in full; no definition is discarded for lacking a rule or an unusual unit.

Sources: [USDA downloads](https://fdc.nal.usda.gov/download-datasets/) and
[field dictionary](https://fdc.nal.usda.gov/portal-data/external/dataDictionary).
The file includes food components and measures such as energy, pH and specific
gravity as well as nutrients. These are distinct measures, not all consumable
nutrient quantities or essential nutrients.

## Identity and units

`nutrient_catalog()` returns every source field (`id`, `name`, `unit_name`,
`nutrient_nbr`, `rank`) plus engine keys, canonical units, unit families and default
forms. Source bytes are preserved in Git; the manifest includes source and archive
URLs, release, archive entry, record count, acquisition date and checksums.
Historical entries explicitly labeled archived by USDA remain included and are
marked `archived_in_source`; their source names are preserved verbatim.

Input `nutrient` accepts an FDC ID such as `"usda_1003"`, `"1003"`, or a unique
exact USDA name. Existing aliases such as `iron`, `vitamin_c`, `calcium`, `zinc`
and `copper` remain valid. USDA `nutrient_nbr` is a different identifier and is
not treated as the FDC ID. Ambiguous names such as `Energy` require the ID.
Aggregate and subtype definitions remain distinct in output.

| Measure family | Behavior |
| --- | --- |
| G, MG, UG | Convert among g/mg/ug/mcg; preserve original input and express canonical amount in the nutrient's USDA unit; `amount_mg` is also supplied |
| KCAL, kJ | Convert with 1 kcal = 4.184 kJ; `amount_mg` is null |
| IU | Preserve IU only; no assumed conversion to mass |
| MCG_RE, MG_ATE, MG_GAE, UMOL_TE | Preserve the native equivalent/activity measure; no conversion to plain ug or mg |
| PH, SP_GR | Preserve the native measure; no conversion to mass |

The `phytate` study extension measures phytate phosphorus and stays separate from
USDA 1042 `Phytic acid`. These dose bases cannot be interchanged. User-selected
chemical forms remain necessary: USDA aggregate iron does not determine whether
the entered iron was heme or non-heme. The UI therefore defaults aggregate iron
to unspecified. Catalog coverage is complete; interaction evidence coverage is
still limited to the curated studies. Unsupported pairs remain explicit unknowns.

## Interfaces and acquisition

- Search the dashboard selector by USDA name or ID. Unit choices follow the selected measure.
- The USDA catalog tab shows the first 50 filtered results and reports the total match count.
- `GET /nutrients` returns the complete catalog as JSON, including all source IDs and units.
- Every analyzed event and target carries its USDA ID/name, canonical amount/unit,
  original local consumption time and normalized chronology.
- Run `Rscript scripts/acquire_usda.R` to reproduce the pinned snapshot. It
  verifies the existing manifest before replacing a dictionary for the same release.

At most 100 events can be submitted in one scenario (9,900 directed pairs), but
any of the 477 definitions can be selected. Exhaustive tests normalize every
dictionary entry and each native unit in batches, in addition to interaction
and clock-time regression cases.
