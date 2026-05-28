# DCA DPR Revision Completion Package

Generated: 2026-05-28

## Package layout

- `manuscript/`: submission-ready DPR manuscript DOCX and deep-revision change summary.
- `additional_files_submission_ready/`: cleaned Additional files 1-7 and cleanup report.
- `figures/`: manuscript figures, new Figure 2, and Supplementary Figure S1.
- `reports/`: key generated audit CSVs and completion audit.
- `scripts_used/`: scripts used to generate the deep revision outputs.
- `github_sync_ready/`: README, CITATION, and sync note prepared for repository update.
- `qc/`: DOCX QA status, full LibreOffice render outputs, and visual spot-check images.

## Completed in this package

- Evaluated-object/estimand rewrite.
- FINAL10 separated from the performance estimand.
- Cohort contrast difficulty table and new Figure 2.
- Procedure-level failure diagnostics.
- Calibration failure diagnostics.
- Grouped Brier decomposition.
- Contextual GSE65682 adult ICU clinical-mimic stress-test summary, with explicit boundary that it is not the current six-fold DPR Table 2 estimand.
- Table 2 and Table 3 upgrades with prevalence, uncertainty, mean predicted probability, observed event rate, and stability notes.
- Recoverable subject-first held-out prediction sensitivity.
- RNA-seq platform-feasibility screening audit.
- scRNA gene-by-cell-type matrix and donor-level module-score summary.
- Cleaned supplementary workbooks with residual wording scan.

## Not completed or not claimed

- Same-estimand numerical clinical-mimic extension metrics for GSE236713 SIRS comparator samples.
- Current six-fold DPR-estimand clinical-mimic AUROC/AUPRC for excluded comparator samples.
- Numerical alternative-threshold sensitivity for training-derived sensitivity 0.90 or specificity 0.90.
- Full subject-level nested LODO refit.
- GitHub PR creation.
- Zenodo remote upload.

## Remote upload status

The refreshed package was committed and pushed to GitHub branch `codex/dpr-deep-revision-20260528`. PR creation was not completed because the GitHub connector returned `403 Resource not accessible by integration` and the local `gh` CLI was not authenticated. Zenodo upload was not marked complete because no Zenodo API credential or verified browser deposition flow was available in the session.

Pull request creation URL:
https://github.com/LiXinzhuo0425/sepsis-crosscohort-host-response/pull/new/codex/dpr-deep-revision-20260528

## Primary files for journal submission

- `manuscript/DCA_DPR_submission_ready_manuscript.docx`
- `additional_files_submission_ready/Additional_file_1_Supplementary_Table_S1_cohort_metadata_sample_selection_label_mapping_gene_availability.xlsx`
- `additional_files_submission_ready/Additional_file_2_Supplementary_Table_S2_FINAL10_reporting_set_definition_and_recurrence_audit.xlsx`
- `additional_files_submission_ready/Additional_file_3_Supplementary_Table_S3_nested_LODO_modeling_performance_calibration_threshold_audit.xlsx`
- `additional_files_submission_ready/Additional_file_4_Supplementary_Table_S4_DEG_candidate_gene_filtering_audit.xlsx`
- `additional_files_submission_ready/Additional_file_5_Supplementary_Table_S5_software_repository_run_order_reproducibility.xlsx`
- `additional_files_submission_ready/Additional_file_6_Supplementary_Table_S6_scRNA_and_figure_supporting_data.xlsx`
- `additional_files_submission_ready/Additional_file_7_Supplementary_Table_S7_reporting_checklist_and_claim_boundary.xlsx`
- `figures/Figure_1_study_design_claim_boundary.png`
- `figures/Figure_2_cohort_applicability_transport_audit.png`
- `figures/Figure_3_held_out_discrimination.png`
- `figures/Figure_4_calibration_threshold_transport.png`
- `figures/Figure_5_single_cell_localization.png`
- `figures/Supplementary_Figure_S1_bulk_host_response_context.png`
