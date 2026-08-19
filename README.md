# Statistical analysis code

Code used for the statistical analysis in this manuscript. This package covers the **statistical analysis** pipeline only -- it does not
include the LLM-querying/data-collection code, raw model outputs, or the underlying clinical
note text, none of which are included here.

## Requirements

- R (developed against 4.3.3/4.4.0), with packages: `tidyverse`, `emmeans`, `lme4`, `broom`,
  `broom.mixed`, `binom`, `car`, `doParallel`, `foreach`, `knitr`, `rmarkdown`.
- Python 3, with `pandas`, `pyarrow`, `numpy`, `scipy` (only needed for the two `.py` scripts in
  `03_supplementary_analyses/`).
- Pandoc (for rendering `analysis_report_v2.Rmd` to HTML).

All scripts use a placeholder absolute path, `your_path_here`, in place of the original HPC
environment's path (in particular each script's `.libPaths()` call and
`projroot`/`final_data_dir`/`results_dir` variables). Replace `your_path_here` with wherever
this code and the corresponding data files live before running -- e.g. with
`grep -rl your_path_here . | xargs sed -i 's#your_path_here#/actual/path#g'`.

## Data

We cannot share the underlying clinical notes or model responses. `dummy_data/` contains two
synthetic, schema-only example files -- correct column headers and a handful of fabricated rows
(clearly marked `[SYNTHETIC EXAMPLE]`), no real patient or note content -- showing the format
the pipeline expects:

- **`all_data_schema_example.csv`** -- the master table (one row per model x note x diagnosis).
  This is the one real external input; everything else below is generated from it. Only includes
  the columns actually used by scripts in this package.
- **`models_run_schema_example.csv`** -- a small model metadata lookup (one row per model),
  used by a few scripts for display names/plotting.

A handful of `03_supplementary_analyses/` scripts (`collinearity_check.R`,
`glmm_random_effects_check.R`, `dataset_comparability_check.R`, `bootstrap_diagnosis_f1.R`,
`bootstrap_all_f1_tables.R`) read from `glmm_input.csv`, `diagnosis_bootstrap_input.csv`, or
`all_models_bootstrap_input.csv` instead of `DAT.RData`/`all_data.csv` directly. These are not
separately curated files -- they're just the same columns as `all_data.csv`/`DAT.RData`
(`recordId`, `note_id`, `diagnosis`, `model`, `model_family`, `model_size`, `open_source`,
`note_length`, `date_of_release`, `gt`, `GPT-o3-mini_binary`, `duration_since_release`), exported
to their own CSV for convenience. `dataset` (NIH vs. MIMIC) is not a raw
column at all -- every script that uses it derives it on the fly from `recordId`
(`if_else(startsWith(recordId, "NIH"), "NIH", "MIMIC")`), so `recordId` alone is sufficient.
Regenerate these intermediate files with e.g. `write_csv(DAT, "glmm_input.csv")` after step 1
below (select the listed columns first if you want to match the original file exactly).

## Pipeline order

1. **`01_data_preprocessing/`** -- `pre_process.R` reads the master data table (`all_data.csv`;
   see `dummy_data/` above for the expected schema) and produces `DAT.RData`, the input to
   everything downstream.
2. **`02_main_analysis/`** -- the core pipeline. `analysis_report_v2.Rmd` is the main entry
   point (sources `precisionrecall_v2.R` and `bootstrap_f1_helper.R`); it produces the main
   text's tables and figures, including the per-model performance table and the family/size/
   duration GLMs. Render with `render_report.R` (repo root) or
   `rmarkdown::render("analysis_report_v2.Rmd")`.
3. **`03_supplementary_analyses/`** -- additional robustness and sensitivity analyses (see the
   comment at the top of each script for the specific question it addresses): macro-averaging for
   class imbalance, model-size/generation collinearity (VIF), mixed-effects models for the
   repeated-measures structure, dataset (NIH vs. MIMIC) and multimodal-architecture covariate
   checks, quote/evidence-grounding verification, and the empirical delta-method-vs-bootstrap
   comparison for rare comorbidities. `bootstrap_diagnosis_f1.R` and `bootstrap_all_f1_tables.R`
   are the standalone scripts used to derive and verify the bootstrap approach that is now
   integrated directly into `analysis_report_v2.Rmd`; they are kept here for transparency but
   are not what generated the final published numbers.
4. **`04_figure_generation/`** -- regenerates the manuscript's figures (Times New Roman font
   patch applied to SVG output; PDF via `cairo_pdf`) from the tables/CSVs produced in step 3.



## Statistical methods summary

- Precision/recall point estimates: raw proportions per model. 95% CIs: exact Clopper-Pearson.
- F1 point estimates: computed from those proportions (harmonic mean). 95% CIs: nonparametric
  bootstrap, resampling notes (not individual note-diagnosis pairs, to preserve within-note
  correlation across diagnoses), 2,000 resamples, 2.5th/97.5th percentiles.
- Family/size/duration trend GLMs: binomial GLM with logit link, covariates model family,
  log10(model size) (open-weights only), note length, duration since release. Same
  note-level bootstrap for F1 CIs; recall/precision GLM coefficients use standard Wald SEs.
- By-comorbidity analysis: as above, plus comorbidity (diagnosis) and dataset as covariates;
  per-family-per-comorbidity estimates are marginal means (`emmeans`) from these GLMs.
