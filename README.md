# never-in-union

**Childlessness and partnership formation across the world.**

This project investigates the relationship between childlessness, partnership formation (specifically individuals who have never formed a union), and societal-level indices like the Gender Inequality Index (GII). 

## Authors
- Ryohei Mogi
- Ewa Batyra
- Ilya Kashnitsky

## Project Structure

The analysis pipeline consists of three core scripts in the `src/` directory.

- `src/0-prepare-session.R`: Initializes the R environment, sets up fonts, and establishes `ggplot2` theme configurations. 
- `src/1-prepare-data.R`: The primary data pipeline. Consolidates survey microdata from ESS, DHS, and Harmonized Histories (HH). Applies cleanup, caches results to `out/d_all_minage35_edu2_.csv`, and formats the final analytic arrays (`never.rda`, `never_edu2.rda`, `geodata.rda`) merged with the UN GII and Gapminder metadata.
- `src/2-dataviz.R`: Produces publication-ready visualizations matching childlessness against GII by continent, sex, and educational strata. Generates multiple PDFs and PNGs stored in `out/`.

### Directory Layout
* `dat/` — Raw input files (DHS `.dta`, ESS `.sav`, UN Data, etc.). _Note: For reproducible execution, you must point raw extraction lines in `1-prepare-data.R` to your external mounts or drives if the datasets exceed repository tracking._
* `src/` — Primary R scripts.
* `src/old/` — Deprecated data processing chunks and fragmented extraction scripts (archived for reference).
* `out/` — Final aggregated datasets and visualization plots.
* `out/old/` — Deprecated artifacts and legacy data versions.

## System Requirements
The project relies extensively on the modern `tidyverse` (via `|>` pipes and robust formula syntaxes `~.x`), and uses the custom `theme_ik()` setup for aesthetic reproducibility. Spatial boundaries use `sf` plotting linked to `spData`.

# Reproducibility Notes: External Data Sources

*This section provides guidance for readers seeking to access the external datasets used in this paper -- from ESS, DHS, and GGS. Access to these raw data sources requires obtaining free researcher's permission. Six datasets are used in this project to produce the aggregated dataset (see `src/1-prepare-data.R`) that is archived in the `out` sub-directory of this repository. Access procedures, versioning information, and replication caveats are detailed below.*

---

## European Social Survey (ESS)

### ESS Round 3 — `ESS3e03_7.sav`

The filename follows the ESS naming convention: **ESS3** = Round 3 (fielded 2006–07), **e03_7** = edition 3.7. This is the integrated (cross-national) datafile in SPSS format.

**Access:**

1. Register at the ESS Data Portal: <https://www.europeansocialsurvey.org/data-portal>
2. Search for *ESS Round 3*, select the integrated file in **SPSS (.sav)** format, and choose **edition 3.7** specifically to match the file used here.
3. Registration can be completed via institutional login (eduGAIN), Google account, or email — no approval process is required.
4. R users can alternatively retrieve data programmatically via the `essurvey` package: `import_rounds(3)`.

**Reproducibility note:** ESS editions are versioned and cumulative corrections are applied. Using an edition other than 3.7 may yield marginally different case counts or variable codings. Always verify the edition number before replicating.

---

### ESS Round 9 — `ESS9e03_2.sav`

**ESS9** = Round 9 (fielded 2018–19), **e03_2** = edition 3.2. Access follows the identical procedure above via the ESS Data Portal. Select the integrated file in SAV format and specifically request **edition e03_2**.

---

## Generations & Gender Programme — Harmonized Histories

All three Harmonized Histories files are distributed through the **Generations & Gender Programme (GGP)**. The Harmonized Histories project standardises partnership, fertility, and employment biographies from multiple survey programmes (GGS I, GGS II, FFS, and others) into a common retrospective event-history format.

**General access procedure for all HH files:**

1. Register at the GGP Data Catalog: <https://www.ggp-i.org/data-catalog/>
2. Sign the data use agreement (free; for non-commercial scientific use).
3. Request access to the specific Harmonized Histories release required (see individual file notes below).

> **Important:** Because Harmonized Histories draws on third-party surveys — including DHS data — certain sub-samples may require that readers obtain separate data agreements with those source programmes. Contact the GGP data team early in the process.

---

### `HARMONIZED-HISTORIES_ALL_GGSaccess.dta` (`path_hh0`)

This combined file pools histories across GGS-accessible waves. It is available via the GGP Data Catalog after registration and signing the data use agreement.

---

### `HARMONIZED-HISTORIES_I.dta` (`path_hh1`)

This is the **Harmonized Histories I** file, derived from retrospective life-history information collected in Wave 1 of the GGS. It produces a cross-sectional biography dataset. Access is via the GGP Data Catalog. For queries about specific variable construction or access to particular versions, readers may wish to contact the harmonization team directly through the GGP website (<https://www.ggp-i.org/data/>).

---

### `HarmonizedHistoriesII_2023_07_10.dta` (`path_hh2`)

This is the **Harmonized Histories II** file. The datestamp in the filename — **2023-07-10** — is a versioning identifier and is critical for exact replication. Readers must specifically request this release when submitting their data access application via the GGP Data Catalog, as later updates may alter variable content or sample composition.

---

## Demographic and Health Surveys (DHS) — `ucp_red_edu.dta`

The filename `ucp_red_edu` indicates a **researcher-constructed extract** from DHS microdata, likely combining union/cohabitation/partnership (UCP) variables with reduced/recoded covariates and education measures. It does not correspond to a standard off-the-shelf DHS release file.

**Access to source DHS microdata:**

1. Register at The DHS Program: <https://dhsprogram.com/>
2. Select the relevant country surveys and submit a data access request.
3. DHS public-use microdata files are free for non-commercial research after a brief review.
4. Alternatively, harmonized multi-country extracts can be constructed via **IPUMS DHS** (<https://idhsdata.org/>), which offers a variable-selection interface across survey waves and countries.

> **Current access note (as of 2025–2026):** DHS data access was suspended in early 2025 following the elimination of USAID funding, but new user registrations reopened in mid-2025 following bridge funding from the Gates Foundation. Readers should verify current access status at the DHS website.

**Critical replication caveat:** Because `ucp_red_edu.dta` is a custom extract, it cannot be reproduced without knowing (a) which DHS country surveys and waves were included, (b) which variables were selected, and (c) any recoding decisions applied. A data appendix or codebook specifying these decisions is strongly recommended. Readers without access to this information should contact the corresponding author directly.

---

## Quick Reference

| File path variable | Dataset | Edition / Version | Access URL |
|--------------------|---------|-------------------|------------|
| `path_ess3` | ESS Round 3 | e03_7 | <https://www.europeansocialsurvey.org/data-portal> |
| `path_ess9` | ESS Round 9 | e03_2 | <https://www.europeansocialsurvey.org/data-portal> |
| `path_hh0` | GGS Harmonized Histories (all waves) | — | <https://www.ggp-i.org/data-catalog/> |
| `path_hh1` | GGS Harmonized Histories I | — | <https://www.ggp-i.org/data-catalog/> |
| `path_hh2` | GGS Harmonized Histories II | 2023-07-10 | <https://www.ggp-i.org/data-catalog/> |
| `path_dhs` | DHS (custom extract) | researcher-constructed | <https://dhsprogram.com/> or <https://idhsdata.org/> |

---

## References

European Social Survey European Research Infrastructure (ESS ERIC). (2023). *ESS Data Portal*. https://www.europeansocialsurvey.org/data-portal

European Social Survey. (2022, May 18). *The way you access our data is changing*. https://www.europeansocialsurvey.org/news/article/way-you-access-our-data-changing

Generations & Gender Programme. (2025). *Harmonized Histories*. https://www.ggp-i.org/data/harmonized-histories/

Generations & Gender Programme. (2023). *Data Catalog*. https://www.ggp-i.org/data-catalog/

Minnesota Population Center. (2025, July 31). *Demographic and Health Survey (DHS) data again available to new users*. popdata.org. https://blog.popdata.org/dhs-data-again-available-to-new-users/

The DHS Program / MEASURE DHS. (n.d.). *Demographic and Health Surveys microdata*. World Bank Microdata Library. https://microdata.worldbank.org/index.php/catalog/dhs
