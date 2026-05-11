# Suggested run order

The full workflow depends on locally downloaded GEO-derived files and harmonized phenotype tables. Frozen result tables are provided to preserve exact correspondence with the submitted manuscript.

Core workflow:

1. `01b_parse_local_series_matrix_core.R`
2. `02_harmonize_core_phenotype_sepsis_control.R`
3. `04_expression_annotation_merge_DEG.R`
4. `07_candidate_gene_selection_preparation.R`
5. `07b_candidate_gene_refinement_for_modeling.R`
6. `08_LODO_diagnostic_modeling.R`
7. `14_nested_LODO_full_pipeline.R`
8. `10_final_compact_model_training.R`

Manuscript audit and insertion packages:

1. `41_complete_Table1_bulk_GEO_background.R`
2. `42_finalize_Table1_for_BMC_Genomics.R`
3. `43_cleanup_Table1_manuscript_language.R`
4. `44_make_Table1_main_text_compact.R`
5. `45_make_Table1_insertion_package.R`
6. `46c_define_signature_from_forced_sources.R`
7. `46d_finalize_signature_text_package.R`
8. `47d_finalize_nested_LODO_methods_text.R`
9. `48c_force_finalize_DEG_candidate_thresholds.R`
