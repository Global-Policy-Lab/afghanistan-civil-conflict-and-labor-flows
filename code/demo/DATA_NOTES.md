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

---

## Provenance: how demo files are built from the estimation pipeline

The table below maps each demo data file to the estimation pipeline output(s) it was constructed from. Files marked **[TO FILL IN]** were not traceable from the estimation scripts alone — the construction code is likely in a separate (undocumented) data-preparation script.

Columns:
- **Demo file** — file in `demo/data/`
- **Source config.R variable(s)** — variables in `code/estimation/config.R` whose files were used as inputs
- **Producing script(s)** — estimation script(s) that write those config.R files
- **Derivation** — what was done to the estimation output to produce the demo file

| Demo file | Source config.R variable(s) | Producing script(s) | Derivation |
|---|---|---|---|
| `6-5-23distYearsIncluded_2020.rds` | `DIST_YEARS_RDS` | `1d_makeRegPanel.R` | Direct copy — filename is unchanged |
| `6-5-23violenceDest_2020.rds` | `VIOLENCE_DEST_RDS` | `1d_makeRegPanel.R` | Direct copy — filename is unchanged |
| `1-12-24fig4a.rds` | `FIG4A_DATA_RDS` | `2d_fig4.R` | Direct copy — filename is unchanged |
| `covariates.rds` | `COVARIATES_RDS` | `1a_covariates.R` | Same data as pipeline output; geometry column dropped |
| `eradication_2014-2016.csv` | `ERADICATION_CSV` | — (public UNODC data) | Direct copy of public input; no transformation |
| `fig2aDemo.rds` | `RESULTS_INMIG_RDS` | `1d_makeRegPanel.R` | Random 10,000-row sample (district-day event-study estimates, k=30) |
| `fig2bDemo.rds` | *(out-migration results, not in config.R — see commented-out save at `1d_makeRegPanel.R:155`)* | `1d_makeRegPanel.R` | Random 10,000-row sample of district-day out-migration event-study estimates |
| `fig2cDemo.rds` | `INMIG_OUTCOME_RDS`, `COVARIATES_RDS` | `1d_makeRegPanel.R`, `1a_covariates.R` | Random sample of district-year outcomes joined to covariates; contains `baseline` and `poppyCat` |
| `fig2dDemo.rds` | `INMIG_OUTCOME_RDS`, `COVARIATES_RDS` | `1d_makeRegPanel.R`, `1a_covariates.R` | Random 120-row sample of district-year regression panel |
| `fig3Demo.rds` | `INMIG_OUTCOME_RDS`, `COVARIATES_RDS` | `1d_makeRegPanel.R`, `1a_covariates.R` | Random 400-row sample of district-year regression panel (also used by `fig5.R`) |
| `fig4bcDemo_high.rds` | `SUBGROUP_H*_BASE` files, `COVARIATES_RDS` | `1e_stratified.R`, `1a_covariates.R` | 280-row sample; source districts with high poppy cultivation |
| `fig4bcDemo_low.rds` | `SUBGROUP_L*_BASE` files, `COVARIATES_RDS` | `1e_stratified.R`, `1a_covariates.R` | 280-row sample; source districts with low poppy cultivation |
| `fig4cDemo_HVT.rds` | `SUBGROUP_HVT_BASE`, `COVARIATES_RDS` | `1e_stratified.R`, `1a_covariates.R` | 280-row sample; sources: high cultivation, violent, Taliban |
| `fig4cDemo_H_V_NonT.rds` | `SUBGROUP_HVNONT_BASE`, `COVARIATES_RDS` | `1e_stratified.R`, `1a_covariates.R` | 280-row sample; sources: high cultivation, violent, non-Taliban |
| `fig4cDemo_H_NonV_T.rds` | `SUBGROUP_HNONVT_BASE`, `COVARIATES_RDS` | `1e_stratified.R`, `1a_covariates.R` | 280-row sample; sources: high cultivation, non-violent, Taliban |
| `fig4cDemo_H_NonV_NonT.rds` | `SUBGROUP_HNONVNONT_BASE`, `COVARIATES_RDS` | `1e_stratified.R`, `1a_covariates.R` | 280-row sample; sources: high cultivation, non-violent, non-Taliban |
| `tableS1.rds` | `INMIG_OUTCOME_RDS`, `OUTCOME_K15_RDS`, `OUTCOME_K45_RDS`, `OUTCOME_1_45_RDS`, `OUTCOME_1_45_14D_RDS`, `COVARIATES_RDS` | `1d_makeRegPanel.R`, `4a_robustness_checks.R`, `4b_outcomes_alt_windows.R`, `1a_covariates.R` | 120-row sample; all robustness-check outcomes joined to covariates |
| `tableS2.Rdata` | `OUTCOME_MINUS14_RDS`, `OUTCOME_PLUS14_RDS`, `COVARIATES_RDS` | reviewer-round-1 scripts, `1a_covariates.R` | 120-row sample; timing-shift outcomes (±14 days) joined to covariates |
| `tableS3.rds` | `OUTCOME_STAYS_RDS` or `OUTCOME_GOBACK_RDS`, `COVARIATES_RDS` | `4b_outcomes_alt_windows.R`, `1a_covariates.R` | 120-row sample; return migration / stays outcomes joined to covariates |
| `tableS5.rds` | `INMIG_OUTCOME_RDS`, `VIOLENCE_DEST_RDS`, `SUBGROUP_HVT_BASE` and related, `COVARIATES_RDS` | `1d_makeRegPanel.R`, `1e_stratified.R`, `1a_covariates.R` | 120-row sample; all outcomes joined (used for Tables S5 and S6) |
| `fig1c.RData` | `BEST_DATES_SAT`, `K30_INMIG_RDS`, `K30_IMPACTED_RDS`, `K30_OUTMIG_RDS`, `DISTRICT_IDS`, `POPPY_CSV`, `CONFLICT_CSV` | `2a_fig1.R` | Saves `tmp` (daily in/out-migration time series for district 2407), `relevantRows` (NDVI peak dates), and `conflictDist` (ACLED conflict events for that district) |
| `figS1.Rdata` | `COVARIATES_RDS` (+ external `afghanShapeAllInfo.Rdata`, not in config.R) | ad hoc data prep (not a numbered estimation script) | Loads district GIS shapefile, joins `poppyCat` and `notGovt` from 2018 covariates, retains geometry + those two columns |
| `figS2.Rdata` | `BEST_DATES_SAT`, `INMIG_OUTCOME_RDS`, `COVARIATES_RDS` | `3b_figS2.R` | Saves `plotDTF` (harvest date tile-grid data) and `afghanShape` (district shapefile with geometry) |
| `figS4.Rdata` | `RANDOM_DIR`, `COVARIATES_RDS`, `INMIG_OUTCOME_RDS` | `randomizationInference.R` (generates files in `RANDOM_DIR`); `3c_figS4.R` (assembles) | Saves `collectCoefs` (randomization-inference coefficient distribution) and `originalCoef` (observed estimate) |
| `figS5.Rdata` | `INMIG_OUTCOME_RDS`, `COVARIATES_RDS`, `VIOLENCE_DEST_RDS` | `3d_figS5.R` | District-year panel joined from those three sources; retains `notGovt` and `violence_monthBefore` columns |
| `figS6.Rdata` | `INMIG_OUTCOME_RDS`, `COVARIATES_RDS`, `VIOLENCE_DEST_RDS` | `3e_figS6.R` | Same join as figS5; retains `tmpPoppy`, `maxIn`, `violence_monthBefore`, `notGovt` |
| `figS7.Rdata` | `INMIG_OUTCOME_RDS`, `COVARIATES_RDS`, `VIOLENCE_DEST_RDS` | `3f_figS7.R` | Saves `outDTF` (coefficient estimates and CIs from the trimmed-poppy regression, same structure as Fig 3b) |
| `figS8.Rdata` | `INMIG_OUTCOME_RDS`, `COVARIATES_RDS`, `VIOLENCE_DEST_RDS` | `3g_figS8.R` | Run with pre-2017 and post-2017 subsets; saves `outDTFpre` and `outDTFpost` |
| `figS9.Rdata` | `INMIG_OUTCOME_RDS`, `COVARIATES_RDS`, `VIOLENCE_DEST_RDS` | `3h_figS9.R` | Run with three specification variants; saves `outDTFv1`, `outDTFv2`, `outDTFv3` |
| `figS10.Rdata` | `SUBGROUP_HVT_BASE`, `SUBGROUP_HVNONT_BASE`, `SUBGROUP_HNONVT_BASE`, `SUBGROUP_HNONVNONT_BASE`, `COVARIATES_RDS` | `3i_figS10.R` | Run dropping each harvest year in turn; saves `outDTF_2014` through `outDTF_2020` (2017 excluded) |
| `figS12.Rdata` | `INMIG_OUTCOME_RDS`, `COVARIATES_RDS`, `VIOLENCE_DEST_RDS`, `VIOLENCE_ROAD_CSV` | `5a_figS12.R` | Saves `outDTF_joint` (point-range results using alternative road-network violence measure) |
| `figS13.Rdata` | `BEST_DATES_SAT`, `CDR_K30_DIR`, `OD_RAW_RDS`, `DD_OUTCOMES_VIOLENCE_RDS`, `OD_OUT_RDS`, `COVARIATES_RDS`, `DISTRICT_IDS` | `6a_figS13.R` | Saves `out` (columns: `district_id`, `origin_district`, `diff`), `destinationLabels`, and `sourceLabels` |
