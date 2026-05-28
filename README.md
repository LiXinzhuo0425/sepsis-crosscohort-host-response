# Sepsis cross-cohort host-response transcriptomic transportability audit

This repository contains analysis code, frozen result tables, supplementary files, and reproducibility materials for a cross-cohort transcriptomic transportability and calibration audit in sepsis-control public cohorts.

## Data sources

No raw transcriptomic data are redistributed in this repository. Original data are publicly available from the Gene Expression Omnibus (GEO).

Included bulk GEO datasets:

- GSE137340
- GSE236713
- GSE54514
- GSE57065
- GSE65682
- GSE95233

Single-cell RNA-seq localization used GSE167363. Updated exploratory RNA-seq platform-feasibility records are documented in Additional file 6 and are not prespecified validation datasets.

## Repository contents

- `code/`: R scripts used for data parsing, phenotype harmonization, differential-expression analysis, candidate-gene filtering, nested LODO modeling, reporting-set definition, and reproducibility audits.
- `frozen_results/`: Frozen result tables, audit outputs, model-development outputs, and figure-supporting files used for manuscript generation.
- `additional_files/`: Supplementary tables and reproducibility files corresponding to the manuscript.
- `docs/`: Data dictionary, run order, GEO accession documentation, and repository notes.
- `large_files_for_zenodo_only/`: Files larger than 50 MiB separated for Zenodo-only upload or manual review.

## Software environment

Analyses were conducted in R.

Main software versions from the finalized audit:

- R 4.5.2
- glmnet 5.0
- pROC 1.19.0.1
- PRROC 1.4
- rms 8.1.1
- dplyr 1.1.4
- readr 2.1.6

Operating system:

- macOS, Apple Silicon environment

Detailed package versions are provided in `sessionInfo.txt` and Additional file 5.

## Key methodological settings

- Strict nested leave-one-dataset-out procedure evaluation.
- All candidate screening, scaling, model fitting, tuning, and threshold selection were performed in training-only data within each outer fold.
- Held-out performance estimates come from fold-specific nested models.
- FINAL10 is a post-nested reporting and biological-localization set, not one fixed clinical diagnostic test.
- Outcome-informed local Youden thresholds are descriptive oracle comparators only.

## Additional files

- Additional file 1: Cohort metadata, sample selection, label mapping, gene availability, reference-standard audit, and subject-level dependence risk audit.
- Additional file 2: FINAL10 reporting-set definition and nested LODO recurrence audit.
- Additional file 3: Nested LODO modeling performance, calibration, threshold behavior, uncertainty, binning, leakage audit, cohort contrast difficulty, procedure-level failure diagnostics, calibration failure diagnostics, grouped Brier decomposition, contextual clinical-mimic stress-test evidence, and alternative-threshold executability audit.
- Additional file 4: Differential-expression and candidate-gene filtering audit.
- Additional file 5: Software environment, repository records, run order, README/CITATION text, and reproducibility notes.
- Additional file 6: scRNA-seq, figure supporting data, updated exploratory RNA-seq platform-feasibility records, and gene availability.
- Additional file 7: Reporting checklist, claim boundary, and allowed wording audit.

## License

Code is released under the MIT License. Result tables and documentation are released under CC BY 4.0 where applicable.

## Citation

Archived code and frozen results: https://doi.org/10.5281/zenodo.20120948. Processed merged expression matrix: https://doi.org/10.5281/zenodo.20121035.
