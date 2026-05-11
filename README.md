# Sepsis cross-cohort host-response transcriptomic signature

This repository contains analysis code, frozen result tables, supplementary files, and reproducibility materials for a cross-cohort transcriptomic study of sepsis host-response signatures.

## Data sources

No raw transcriptomic data are redistributed in this repository. Original data are publicly available from the Gene Expression Omnibus (GEO).

Included bulk GEO datasets:

- GSE137340
- GSE236713
- GSE54514
- GSE57065
- GSE65682
- GSE95233

Users should download raw or processed GEO source data directly from GEO according to GEO terms of use.

## Repository contents

- `code/`: R scripts used for data parsing, phenotype harmonization, differential-expression analysis, candidate-gene filtering, nested LODO modeling, final signature definition, and reproducibility audits.
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

Detailed package versions are provided in `sessionInfo.txt`.

## Key methodological settings

- Nested leave-one-dataset-out model development.
- 5-fold inner cross-validation within each outer training set.
- Final compact model trained using separate 10-fold cross-validation.
- glmnet penalized logistic regression with `family = "binomial"`, `type.measure = "auc"`, and `standardize = FALSE`.
- Alpha grid: 1.00, 0.75, 0.50.
- Lambda rule: `lambda.1se`.
- DEG threshold: adjusted P < 0.05 and |log2FC| >= 0.5.
- Candidate-gene filters: direction consistency >= 0.8, mean univariate AUROC >= 0.7, and correlation redundancy threshold |r| >= 0.85.

## Additional files

- Additional file 1: Source references and cohort-specific verification notes for included bulk transcriptomic datasets.
- Additional file 2: Final 10-gene signature definition and nested LODO recurrence audit.
- Additional file 3: Nested LODO modeling-method audit.
- Additional file 4: Differential-expression and candidate-gene selection audit.
- Additional file 5: Nested LODO methods text and reproducibility notes.

## License

Code is released under the MIT License. Result tables and documentation are released under CC BY 4.0 where applicable.

## Citation

A permanent DOI will be added after the first GitHub release is archived in Zenodo.
