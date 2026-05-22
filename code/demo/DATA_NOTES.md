# Demo Data Notes

The demo folder uses three types of data files:

- **Sample data** — random subsets of the restricted CDR-derived migration data. Results will differ from the paper.
- **Pre-computed outputs** — full model outputs saved from the complete dataset. Results will match the paper exactly.
- **Public data** — publicly available data included as-is. Results will match the paper exactly.

---

## Main figures

| Script | Output | Data type | Notes |
|---|---|---|---|
| `fig1.R` | `fig1c.pdf` | Pre-computed output | Time series for one example district, derived from full CDR data |
| `fig2.R` | `fig2a.pdf` | Sample data | 10,000-obs subset; district-day in-migration estimates |
| `fig2.R` | `fig2b.pdf` | Sample data | 10,000-obs subset; district-day out-migration estimates |
| `fig2.R` | `fig2c.pdf` | Sample data | Subset; district-year baseline vs. harvest in-migration rates |
| `fig2.R` | `fig2d.pdf` | Sample data | 120-obs subset; district-year regression outcomes |
| `fig3.R` | `fig3a.pdf` | Sample data + public data | 400-obs subset of migration outcomes; full violence data |
| `fig3.R` | `fig3b.pdf` | Sample data + public data | Same inputs as fig3a |
| `fig4.R` | `fig4a.pdf` | Pre-computed output | Full-data bar chart of migrant composition |
| `fig4.R` | `fig4b.pdf` | Sample data | 280-obs subsets stratified by source district type (high/low) |
| `fig4.R` | `fig4c.pdf` | Sample data | 280-obs subsets stratified by violence × Taliban (4 groups) |
| `fig5.R` | `fig5a.pdf` | Sample data + public data | 400-obs subset; full eradication and covariate data |
| `fig5.R` | `fig5b.pdf` | Sample data + public data | Same inputs as fig5a |

---

## Supplementary figures

All supplementary figure scripts load pre-computed `.Rdata` files saved from the full dataset. Results match the paper exactly.

| Script | Output | Data file |
|---|---|---|
| `figS1.R` | `figS1.pdf` | `figS1.Rdata` |
| `figS2.R` | `figS2.pdf` | `figS2.Rdata` |
| `figS4.R` | `figS4.pdf` | `figS4.Rdata` |
| `figS5.R` | `figS5.pdf` | `figS5.Rdata` |
| `figS6.R` | `figS6.pdf` | `figS6.Rdata` |
| `figS7.R` | `figS7.pdf` | `figS7.Rdata` |
| `figS8.R` | `figS8.pdf` | `figS8.Rdata` |
| `figS9.R` | `figS9.pdf` | `figS9.Rdata` |
| `figS10.R` | `figS10.pdf` | `figS10.Rdata` |
| `figS12.R` | `figS12.pdf` | `figS12.Rdata` |
| `figS13.R` | `figS13.pdf` | `figS13.Rdata` |

---

## Supplementary tables

All table scripts use 120-obs subsets of the district-year regression panel. Results will differ from the paper.

| Script | Tables | Data type |
|---|---|---|
| `tableS1.R` | S1 | Sample data (120 obs) |
| `tableS2.R` | S2 | Sample data (120 obs) |
| `tableS3.R` | S3 | Sample data (120 obs) |
| `tableS4-6.R` | S4, S5, S6 | Sample data (120 obs) + public eradication data |

---

## Data files

| File | Type | Description |
|---|---|---|
| `fig1c.RData` | Pre-computed output | In/out migration time series for one district |
| `fig2aDemo.rds` | Sample data | 10,000-obs subset of district-day in-migration estimates |
| `fig2bDemo.rds` | Sample data | 10,000-obs subset of district-day out-migration estimates |
| `fig2cDemo.rds` | Sample data | Subset of district-year baseline/harvest in-migration rates |
| `fig2dDemo.rds` | Sample data | 120-obs subset of district-year regression panel |
| `fig3Demo.rds` | Sample data | 400-obs subset of district-year regression panel with covariates |
| `fig4bcDemo_high.rds` | Sample data | 280-obs subset; migrants from high-cultivation sources |
| `fig4bcDemo_low.rds` | Sample data | 280-obs subset; migrants from low-cultivation sources |
| `fig4cDemo_HVT.rds` | Sample data | 280-obs subset; sources: high cultivation, violent, Taliban |
| `fig4cDemo_H_V_NonT.rds` | Sample data | 280-obs subset; sources: high cultivation, violent, non-Taliban |
| `fig4cDemo_H_NonV_T.rds` | Sample data | 280-obs subset; sources: high cultivation, non-violent, Taliban |
| `fig4cDemo_H_NonV_NonT.rds` | Sample data | 280-obs subset; sources: high cultivation, non-violent, non-Taliban |
| `1-12-24fig4a.rds` | Pre-computed output | Migrant composition (baseline vs. harvest) for fig4a bar chart |
| `6-5-23distYearsIncluded_2020.rds` | Pre-computed output | Vector of district-year IDs with sufficient observations |
| `6-5-23violenceDest_2020.rds` | Pre-computed output | Violence events at destination districts around harvest dates |
| `covariates.rds` | Public data | District-level covariates (satellite, census, infrastructure) |
| `eradication_2014-2016.csv` | Public data | UNODC poppy eradication data, 2014–2016 |
| `tableS1.rds` | Sample data | 120-obs subset for Table S1 robustness checks |
| `tableS2.Rdata` | Sample data | 120-obs subset for Table S2 timing robustness checks |
| `tableS3.rds` | Sample data | 120-obs subset for Table S3 return migration |
| `tableS5.rds` | Sample data | 120-obs subset for Tables S5 and S6 |
| `figS1.Rdata` – `figS13.Rdata` | Pre-computed output | Full-data model results for each supplementary figure |
