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