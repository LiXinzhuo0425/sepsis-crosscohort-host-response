# Final freeze summary

Freeze date: 2026-05-08 18:57:31 CST

## Frozen study identity

Cross-cohort transportability evaluation of a host-response transcriptomic sepsis diagnostic signature with independent single-cell biological localization.

## Frozen primary validation framework

Strict nested leave-one-dataset-out validation. In each outer fold, the held-out dataset was excluded from feature screening, redundancy reduction, standardization, model fitting, tuning and threshold selection.

## Frozen RNA-seq decision

RNA-seq validation status: NO_GO_no_eligible_RNAseq_dataset_detected

Recommended handling: Record attempted RNA-seq screening transparently and proceed to final_freeze without RNA-seq validation.

## Frozen main conclusions

1. Bulk analysis included 484 samples from 6 cohorts.
2. Median nested LODO AUROC was 0.907, with minimum AUROC 0.568.
3. Pooled nested LODO AUROC was 0.722, supporting weaker pooled transport behavior than per-dataset discrimination.
4. Median absolute threshold shift was 0.537, so fixed threshold transportability is not supported.
5. scRNA localization showed Final10 and NestedRecurrent signatures both highest in Monocyte/Myeloid compartments.

## Frozen restrictions

- Do not claim clinical deployment readiness.
- Do not claim same-intended-use external validation.
- Do not claim fixed threshold transportability.
- Do not treat scRNA module score analysis as independent diagnostic validation.
- Do not overinterpret healthy-control contrasts as clinical mimic discrimination.
- Do not state that RNA-seq validation was performed.

## Post-freeze rule

After this freeze, manuscript drafting should use only files in 04_results/final_freeze as the numeric source. Any further rerun must be logged as a new freeze version.
