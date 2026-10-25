# Renewable Energy Transition: A Statistical Analysis for Energy Policy and Sustainable Development

## Purpose

This portfolio project examines the association between renewable shares of electricity capacity and electricity generation across IRENASTAT country/area entries over time. It is designed to demonstrate reproducible R analysis, panel-data modeling, data validation, professional visualization, and policy-oriented reporting for IRENA-related analytical roles.

## Research question

> To what extent is a higher renewable share of electricity capacity associated with a higher renewable share of electricity generation across countries over time?

The study estimates an association, not a causal effect.

## Official data source

- Publisher: International Renewable Energy Agency (IRENA).
- Dataset: IRENASTAT, *Renewable energy share of electricity capacity and generation (%) by Region/country/area, Indicator and Year*.
- Source page: https://www.irena.org/Data/Downloads/IRENASTAT
- Direct table: https://pxweb.irena.org/pxweb/en/IRENASTAT/IRENASTAT__Power%20Capacity%20and%20Generation/RE-SHARE_2026_H2_PX.px
- API endpoint used by the script: https://pxweb.irena.org/api/v1/en/IRENASTAT/Power%20Capacity%20and%20Generation/RE-SHARE_2026_H2_PX.px

The live table metadata verified two indicators, 234 geographic entries, and year labels 2000–2025. Capacity has non-missing values through 2025; generation has non-missing values through 2024 in the retrieved release. Values are percentages of electricity capacity or electricity generation.

## Unit of analysis and exclusions

The source table contains country/area entries and 10 published aggregates: World, Africa, Asia, C America + Carib, Eurasia, Europe, Middle East, N America, Oceania, and S America. These 10 aggregates are excluded from country-level analysis. The remaining 224 country/area entries are retained as published; territories and other country/area entries are not silently reclassified. A complete-case filter is used only for the paired regression, which excludes unavailable values. No missing value is imputed or converted to zero.

## Statistical method

The primary model is a two-way fixed-effects panel regression:

`generation_share_it = beta * capacity_share_it + country_effect_i + year_effect_t + error_it`

The model uses country and year fixed effects and country-clustered standard errors. The coefficient is interpreted as the estimated within-country/year association in percentage points. Because the data are observational and may contain omitted time-varying confounding, the result is not interpreted causally.

## Reproduce the project

1. Install R (tested with R 4.3.3) and the packages `tidyverse`, `httr2`, `jsonlite`, `fixest`, `broom`, `patchwork`, `scales`, `knitr`, and `rmarkdown`.
2. From this folder, run:

```bash
Rscript analyze_irena.R
```

The script queries the official IRENASTAT API, saves the raw response and metadata, validates the data, writes the cleaned CSV, estimates the model, exports results, and creates three 300-dpi PNG figures.

3. Render the HTML report:

```bash
Rscript -e 'rmarkdown::render("report.Rmd", output_file="report.html")'
```

The script is designed to rerun from the live API. Therefore, future releases may change the results; the included raw CSV and metadata preserve the release used for this project.

## Folder structure

- `analyze_irena.R` — complete executable analysis script.
- `report.Rmd` — reproducible professional report source.
- `report.html` — rendered report.
- `data_raw/` — raw API response and metadata used for the run.
- `data_clean/irena_share_clean.csv` — complete-case cleaned analysis data.
- `data_dictionary.md` — variable definitions, units, and transformations.
- `figures/` — three 300-dpi publication-quality PNG charts.
- `results/` — validation, descriptive statistics, model results, diagnostics, rankings, provenance, and session information.

## Quality assurance

The tested run found zero duplicate country-year rows, zero percentage values outside 0–100, and no imputation. The generated figures were visually inspected at 2,550 × 1,650 pixels. Model coefficients and report values are read from the exported analysis results rather than entered manually. The retrieved source and API were accessible at the time of analysis.

## Attribution and licensing

The project attributes the data to IRENA and links to the official IRENASTAT source. Users should consult IRENA’s current terms and conditions and the source table’s notices before redistributing the data or publishing derivative work.
