# Interpretation notes v0.2

## Current scientific positioning

The strongest framing is not direct clinical deployment of a locked diagnostic model. The stronger and more defensible framing is cross-cohort transportability of a host-response diagnostic signature under strict nested leave-one-dataset-out evaluation.

## Main positive findings

1. Per-dataset discrimination is frequently preserved under strict nested LODO.
2. The signature is biologically coherent, with enrichment and scRNA localization pointing to monocyte/myeloid and innate immune compartments.
3. The study explicitly exposes calibration and fixed-threshold instability, which is methodologically important for transcriptomic diagnostic signatures.

## Main limitations to state clearly

1. Most validation contrasts are sepsis or septic shock versus healthy controls, limiting direct clinical mimic interpretation.
2. GSE54514 is a clear transportability failure and should be discussed as such.
3. Pooled probability-scale behavior is unstable, so fixed-threshold deployment is not supported.
4. scRNA analysis supports biological localization, not independent diagnostic validation.

## Recommended conclusion style

Use: The signature showed recurrent cross-cohort discrimination and predominantly myeloid single-cell localization, but calibration and fixed-threshold transportability were unstable.

Avoid: The model is ready for clinical diagnosis or shows robust clinical utility.
