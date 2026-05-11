# Results draft v0.2 after nested LODO and scRNA localization

## Cohort composition

The final bulk transcriptomic analysis included 484 samples from 6 independent whole-blood cohorts, comprising 333 sepsis cases and 151 controls. Cohorts differed in platform, clinical context and control definition, including healthy-control contrasts, matched healthy controls and selected contrasts from more complex ICU cohorts.

## Strict nested leave-one-dataset-out model evaluation

To avoid information leakage from feature selection, the diagnostic modeling procedure was re-run under a strict nested leave-one-dataset-out design. In each outer fold, the held-out dataset was excluded from candidate screening, correlation-based redundancy reduction, standardization, regularized model fitting and threshold selection. Across the six held-out datasets, the median AUROC was 0.907 with a range of 0.568 to 1.000. Four of six held-out datasets achieved AUROC >= 0.80, whereas two datasets fell below this threshold.

Gene selection was not completely fixed across folds. 2 genes were selected in all nested folds: CD177, TDRD9. The most frequently selected genes across folds included CD177, TDRD9, LILRA6, RAB31, VNN1, ANKRD22, ARG1, C3AR1, RNASE3, WFDC1.

## Probability-scale instability, calibration drift and threshold transportability

Although per-dataset discrimination was often preserved, pooled nested-LODO predictions showed weaker transportability when predictions from all held-out folds were combined. The pooled AUROC was 0.722 (95% CI 0.670 to 0.774), with AUPRC 0.810 and Brier score 0.333. The mean predicted probability was 0.396 compared with an observed sepsis prevalence of 0.688, corresponding to calibration-in-the-large of -0.292.

Threshold behavior was highly dataset-dependent. The median absolute shift between the fold-specific training threshold and the local Youden threshold was 0.537, with a maximum absolute shift of 0.697. At the fixed training thresholds, 2 held-out datasets had sensitivity below 0.20 and 1 held-out dataset had specificity below 0.20. These findings indicate that discrimination and fixed-threshold transportability diverged under cross-cohort deployment.

Exploratory decision-curve analysis showed heterogeneous threshold-dependent net benefit. In pooled nested-LODO predictions, the model was clinically preferable over both treat-all and treat-none strategies across 27.6% of evaluated threshold probabilities. This analysis was treated as exploratory because the available contrasts were heterogeneous and often involved healthy controls rather than clinical mimics.

## Single-cell localization of the transported host-response signature

To provide biological context for the bulk-derived signature, an independent PBMC single-cell RNA-seq dataset was analyzed. After quality control, the single-cell object contained 59743 cells, 25 Seurat clusters and 6 broad canonical-marker-defined cell-type compartments. All 10 genes in the final compact signature and all 10 recurrent nested-LODO genes were detected in the single-cell object.

Both the final 10-gene module score and the recurrent nested-LODO module score were highest in Monocyte/Myeloid compartments. The mean module scores in this compartment were 0.103 for the final 10-gene signature and 0.066 for the recurrent nested-LODO signature. These results support a predominantly monocyte/myeloid origin of the transported host-response signal.

At the group level, signature scores were descriptively higher in sepsis cells, particularly nonsurvivor samples, than in healthy-control cells. Because the single-cell dataset contained a limited number of independent donors, these group-level score differences were interpreted as biological localization and contextual evidence rather than independent diagnostic validation.
