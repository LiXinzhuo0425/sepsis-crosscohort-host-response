# Data dictionary

This repository contains analysis code and frozen result tables. Raw GEO expression files are not redistributed.

Common columns:

| Column | Meaning |
|---|---|
| `gene_symbol` | Harmonized gene symbol |
| `logFC` / `log2FC` | Log2 fold change comparing sepsis with comparator samples |
| `adj.P.Val` | Multiple-testing adjusted P value from limma |
| `direction_consistency` | Proportion of datasets or folds with expression direction consistent with global direction |
| `mean_auc` | Mean univariate AUROC across dataset-level screening |
| `nested_lodo_occurrence_count` | Number of nested LODO folds in which a gene was retained |
| `selected_folds` | Outer held-out folds in which the gene was selected in the corresponding training data |
| `predicted_probability` | Model-predicted probability for the sepsis class |
| `brier_score` | Mean squared difference between observed outcome and predicted probability |
