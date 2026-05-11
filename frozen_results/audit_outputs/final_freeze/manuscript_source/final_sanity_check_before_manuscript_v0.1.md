# Final sanity check before manuscript drafting

## Overall status

- Consistency flags: 0
- High-risk interpretation flags: 5
- Ready for manuscript drafting: YES_WITH_RESTRICTIONS

## Main validated messages

1. Bulk analysis included 484 samples from 6 cohorts, including 333 sepsis cases and 151 controls.
2. Strict nested LODO median AUROC was 0.907, with range 0.568 to 1.000; 4 of 6 datasets had AUROC >= 0.80.
3. Pooled nested LODO AUROC was lower at 0.722, supporting cross-dataset probability-scale/ranking instability.
4. Median absolute threshold shift was 0.537, and fixed-threshold sensitivity was <0.20 in 2 datasets.
5. scRNA analysis included 59743 cells and localized both Final10 and NestedRecurrent signatures to Monocyte/Myeloid compartments.

## Required defensive framing

- Frame the work as cross-cohort transportability evaluation of a host-response diagnostic signature.
- Emphasize that discrimination, calibration and fixed-threshold behavior diverged.
- Treat DCA as exploratory.
- Treat scRNA as biological localization rather than model validation.
- Do not claim clinical deployability or same-intended-use validation.

## Highest-risk points for reviewers

- High: Median per-dataset nested LODO AUROC = 0.907, but pooled nested LODO AUROC = 0.722. Handling: Use per-dataset nested LODO as the main discrimination result; use pooled AUROC as evidence of transport instability.
- High: Held-out dataset(s) with AUROC < 0.70: GSE54514. Handling: Explicitly describe this as a transportability failure scenario; do not claim universal robustness.
- High: Fixed-threshold sensitivity < 0.20 in: GSE236713, GSE65682. Handling: Emphasize threshold transportability failure and need for recalibration.
- High: Fixed-threshold specificity < 0.20 in: GSE54514. Handling: Report as threshold instability; avoid clinical deployment claims.
- High: Median absolute threshold shift = 0.537. Handling: Use as a central finding rather than a minor limitation.
- Moderate: Most evaluated contrasts involve healthy controls or selected healthy-control contrasts. Handling: State that the study evaluates cross-cohort transport behavior under available public contrasts, not definitive intended-use deployment.
