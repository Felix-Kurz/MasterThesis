# Meta Project Information

This file is a handoff document for Codex chats working on this master thesis / research project.
Read this first before editing scripts or interpreting results.

## Project Context

Workspace:

```text
C:\Users\felix\Documents\EBA\Forschungsprojekt
```

This project uses social security / BG wage data where wages are censored or topcoded above a legal threshold. The theoretical benchmark is a Tobit-style wage imputation model adapted from a Stata/Card-style social security wage correction approach.

The current contribution is to use Lohnzettel wage data, which contains uncensored wage information for a historical period, to train XGBoost models that can predict/impute true wages when only censored BG/social-security wages are available.

Important setup constraint: the full data and heavy computations run on an HPC/Altix cluster for privacy reasons. The local machine mostly contains scripts, copied result CSVs, logs, and selected outputs. Do not assume full `.RData` input files are available locally. Local `Rscript` may also be unavailable.

## Pipeline Overview

The workflow has two major parts.

Historical dataset creation scripts:

```text
01_build_pre_model_dataset_Version2.R
01_build_pre_model_dataset_Version3.R
```

Historical modeling scripts:

```text
tobit_and_xgboost_v5_250k.R
tobit_and_xgboost_v5_temporal_250k.R
```

The modeling scripts use `pre_model_full_history` plus `lohnzettel` to build a common BG-LZ comparison sample, fit Tobit and XGBoost variants, and compare predictions to LZ daily wages.

The current authoritative seed-123 workflow is:

```text
Dataset builder:
  01_build_pre_model_dataset_Version4_v2.R

Modeling:
  tobit_and_xgboost_v5_temporal_250k_data_seed123_v2.R

Exploratory LZ analysis:
  analyze_lz_dwage_ywage_v5_for_seed123_v2.R

Finite-Tobit table repair for the completed in-memory run:
  rebuild_finite_tobit_tables_v5.R
```

The older Version 2/3 and earlier seed-123 results remain useful historical
references, but future thesis tables should use the current finite V4v2 result
folder described below.

## Intended Full-Sample Workflow (Confirmed 2026-09-23)

The full-sample workflow is separate from the authoritative 250k V4v2 results
described below. When working on the full sample, the intended modeling script
and its exploratory-analysis companion are:

```text
Modeling:
  tobit_and_xgboost_v5_temporal_full_sample.R

Exploratory LZ analysis:
  analyze_lz_dwage_ywage_v5_for_seed123_v2_full_sample.R
```

The selected full-sample model is **not**
`tobit_and_xgboost_v5_full_memory_safe.R`. That file came from an exploratory
idea in another Codex chat and is not the desired/default path. Future Codex
instances must not substitute it for `tobit_and_xgboost_v5_temporal_full_sample.R`
unless the user explicitly asks to revisit the memory-safe alternative.

Both intended full-sample scripts use:

```text
Pre-model input:
  pre_model_output_full_v4/
    pre_model_full_history_full_v4.RData

Lohnzettel input:
  lohnzettel_dta.RData

Analysis/model window:
  1994-2012

Model output root:
  model_output_v5_temporal_full_sample_v4
```

In `tobit_and_xgboost_v5_temporal_full_sample.R`, the intended setting is:

```r
use_debug_sample <- FALSE
```

Therefore the run uses all eligible persons and rows in the 1994-2012 window,
not a 250k or debug subsample.

Despite retaining `seed123_v2` in its historical filename,
`analyze_lz_dwage_ywage_v5_for_seed123_v2_full_sample.R` performs no random or
fixed-size sampling. It rebuilds the full matched BG-LZ comparison sample and
writes aggregate-only diagnostics to:

```text
model_output_v5_temporal_full_sample_v4/
  lz_exploratory_analysis/
    lz_wage_sample_flow.csv
    lz_wage_summary_overall.csv
    lz_wage_summary_by_censor.csv
    lz_wage_summary_by_year.csv
    lz_wage_relationship_summary.csv
```

The diagnostic was rewritten with `data.table`, sequential input loading,
by-reference transformations, keyed matching, and memory checks because the
full pre-model file is large. Those implementation safeguards do not make it a
companion to the rejected `full_memory_safe` model branch; its inputs, sample
definition, and output location are aligned with the temporal full-sample
script. The diagnostic currently retains `memory_budget_gib <- 430` and a
conservative BG-LZ join preflight; these are protective checks inside the
diagnostic, not a decision to use the memory-safe modeling script. The private
`.RData` inputs and `Rscript` were unavailable locally, so the rewritten
diagnostic received static validation but has not been executed locally on the
full data.

## Current Authoritative Seed-123 V4v2 Workflow (August/September 2026)

### Window-Eligible Person Sampling

`01_build_pre_model_dataset_Version4_v2.R` was created from the more advanced
dataset-builder branch and changes the sampling frame. Instead of drawing
persons from the complete historical data and only later restricting years, it
first identifies persons with at least one HV observation during the actual LZ
and model period:

```text
Sampling window: 1994-2012
Requested persons: 250,000
Eligible persons in HV sampling frame: 7,643,751
Seed: 123
```

After the persons are drawn, all available years for those persons are retained
so that employment histories, career measures, and lagged variables are not
truncated. The relevant paths are:

```text
Script:
  01_build_pre_model_dataset_Version4_v2.R

BG preparation directory:
  bg_prep_250k_seed123_hv_1994_2012_v4v2

Pre-model output directory on MACH:
  pre_model_output_250k_seed123_hv_1994_2012_v4v2

Pre-model file:
  pre_model_full_history_250k_seed123_hv_1994_2012_v4v2.RData

Dataset-generation transcript copied locally:
  log_of_250k_pre_mode_dataset_v4.R
```

Completed dataset-builder output:

```text
Full-history pre-model rows:             3,366,502
Full-history persons after filters:        207,275
Year range retained:                     1978-2020
Rows in the 1994-2012 analysis window:   1,699,930
Persons in the 1994-2012 window:           174,514
Share of sampled persons retained:           69.81%
Final duplicate penr/benr/year keys:               0
```

This design was intended to increase the effective LZ-window sample. It did not
literally double the final matched observations, but it increased them by about
57% relative to the older seed-123 run while keeping the censoring share almost
unchanged.

### Current V2 Modeling Script And Scope

The current model script is:

```text
tobit_and_xgboost_v5_temporal_250k_data_seed123_v2.R
```

It reads the V4v2 pre-model file and writes to:

```text
model_output_v5_temporal_250k_data_seed123_hv_1994_2012_v4v2
```

The retained model scope is intentionally:

```text
BG baseline
Tobit V1
XGBoost V1
XGBoost V4
```

Tobit V4 was removed from this script and from the thesis comparison because it
was an exploratory specification that performed poorly and was not needed for
the final comparison. Removing it does not alter the other fitted models or
their evaluation samples.

The script now has explicit checks for required input objects and columns,
duplicate comparison keys, and non-finite input wages. It also writes
`sample_flow_v5.csv`.

Completed model sample flow:

```text
Stage                                           Observations   Persons
Loaded pre-model full history                     3,366,502    207,275
Pre-model analysis window 1994-2012               1,699,930    174,514
Valid LZ rows for selected persons 1994-2012      2,636,640    173,127
Final matched BG-LZ comparison sample             1,631,405    171,670
```

The third stage is not a nested subset of the second row count. It collects all
valid LZ records for the selected persons before the final BG-LZ match, so its
observation count can be larger.

Comparison with the older predominant seed-123 thesis run:

```text
Metric                         Older run      V4v2 run      Change
Final matched observations     1,036,156      1,631,405     +57.45%
Censored observations             74,963        118,792     +58.47%
Censored share                     7.235%          7.282%    +0.047 percentage points
2012 test observations             61,922         97,453     +57.38%
2012 censored observations          4,550          7,106     +56.18%
```

Because the sampling frame changed, the old and new results are not paired
estimates on the same persons. Differences reflect both the larger sample and
its composition. The V4v2 frame is nevertheless better aligned with the
1994-2012 thesis estimand.

### Authoritative Finite-Tobit Tables

The completed model run exposed a small numerical edge case in the Tobit
imputation. The truncated-normal draw uses an expression of the form:

```r
qnorm(k + runif(length(k)) * (1 - k))
```

For a few extreme censored cases, the probability evaluates or rounds to
exactly one, so `qnorm(1)` returns positive infinity. `build_eval_table()`
correctly carries that value into the row-level error. The reporting problem was
in `summarise_eval()`: `na.rm = TRUE` removes `NA` but does not remove `Inf`.

The one-off repair script:

```text
rebuild_finite_tobit_tables_v5.R
```

was run in the still-open capsule R session, so no models needed to be refit. It
filters only non-finite Tobit evaluation rows, rebuilds the two affected RMSE
tables, copies the complete root CSV table set, records the excluded counts,
and verifies that every saved CSV contains zero infinite values.

Use this as the authoritative table folder:

```text
model_output_v5_temporal_250k_data_seed123_hv_1994_2012_v4v2/
  finite_tobit_tables/
```

Do not use the root-level raw Tobit rows from
`comprehensive_rmse_comparison_v5.csv` or `temporal_split_rmse_v5.csv` for the
thesis, because their Tobit aggregates contain `Inf`. Other model rows and
unaffected CSVs are identical between the original and finite folders.

Recorded non-finite counts:

```text
Evaluation setting                 Total rows   Positive Inf   Share
In-sample common comparison         1,631,405          5       0.00031%
Temporal train 1994-2011            1,533,952          3       0.00020%
Temporal test 2012                     97,453          3       0.00308%
```

All non-finite values were positive infinities among censored observations.
The in-sample, temporal-train, and temporal-test counts refer to separately
fitted evaluation settings and should not be added as unique observations.

For future full reruns, prevent the edge case upstream by clamping the
truncated-normal probability strictly below one before calling `qnorm()`. The
finite-row repair is appropriate for the completed run because the exclusions
are negligible, transparent, and consistently applied.

### Current V4v2 Results

In-sample censored log-wage errors:

```text
Method         RMSE     MAE
XGBoost V4    0.3362   0.1860
XGBoost V1    0.4178   0.2472
Tobit V1      0.4980   0.3004
BG baseline   0.5732   0.3948
```

Temporal 2012 censored log-wage errors:

```text
Method         RMSE     MAE     Mean error
XGBoost V4    0.4538   0.2176    -0.0809
Tobit V1      0.5101   0.2853    -0.0641
XGBoost V1    0.5330   0.3255    -0.2005
BG baseline   0.5890   0.3914    -0.3231
```

Thus, after correcting the finite Tobit summary, Tobit V1 is second on the
censored 2012 log metrics: it beats XGBoost V1 and the BG baseline, but XGBoost
V4 remains clearly best. XGBoost V4 lowers censored-2012 log RMSE by about 23.0%
relative to BG, almost the same as the 23.5% reduction in the older seed-123
run.

Temporal 2012 censored wage-level errors in 2000-price daily-wage units:

```text
Method         Wage RMSE   Wage MAE   Median AE
XGBoost V4       412.57      51.18       20.61
Tobit V1         412.65      63.52       31.21
XGBoost V1       415.21      64.92       35.51
BG baseline      416.99      77.43       41.65
```

The XGBoost V4 and Tobit V1 wage-level RMSE values are nearly tied because this
metric is dominated by a very small extreme upper tail. XGBoost V4 remains much
better by log RMSE, wage MAE, and median absolute error.

Censored-only person-level test experiment:

```text
Method         Log RMSE   Log MAE   Wage RMSE   Wage MAE   Median AE
XGBoost V4       0.4052    0.2298      297.43      48.74      24.90
XGBoost V1       0.4153    0.2390      305.68      50.24      26.61
BG baseline      0.5551    0.3917      320.64      71.55      41.65
```

This experiment splits by person (`penr`), not row, so the same person cannot
appear in both training and test data. XGBoost V4's log-RMSE advantage over BG
in the censored-only test increased from about 22.7% in the older run to 27.0%
in V4v2.

The core thesis conclusion is therefore robust: XGBoost V4 remains the best
model for censored wages on the primary log scale and by typical wage-level
errors, and its temporal/censored-only generalisation is at least as strong in
the enlarged sample.

### Why Wage-Level RMSE Increased

Compared with the older seed-123 sample, the central censored-wage distribution
hardly moved, while the maximum became much more extreme:

```text
Censored true daily wage              Older run     V4v2 run
Median                                   162.60        163.56
99th percentile                          545.96        563.88
Maximum                               25,895.88     84,267.82
Standard deviation                       282.16        414.76
```

This explains why wage-level RMSE rose by roughly 22-24% for all methods even
while temporal log RMSE improved and median absolute errors remained stable.
The correct interpretation is a heavier extreme upper tail, not a reversal of
the model ranking or a general loss of predictive quality. Always report wage
MAE and median absolute error alongside wage RMSE.

### Runtime Of The V4v2 Run

Selected runtimes from `model_runtime_v5.csv`:

```text
Stage                         Model         Minutes
In-sample benchmark           Tobit V1        0.51
In-sample benchmark           XGBoost V1    227.99
In-sample benchmark           XGBoost V4    336.09
Temporal validation           Tobit V1        0.88
Temporal validation           XGBoost V1    208.26
Temporal validation           XGBoost V4    325.37
Censored-only train/test      XGBoost V1      2.26
Censored-only train/test      XGBoost V4     17.74
```

The enlarged sample increased XGBoost runtime more than proportionally. These
times describe this implementation and MACH environment; they are not portable
hardware benchmarks.

### Exploratory Diagnostics And Plot Decisions

The current exploratory script is:

```text
analyze_lz_dwage_ywage_v5_for_seed123_v2.R
```

It reads the V4v2 sample and writes its aggregate diagnostics into a dedicated
subfolder so the model-output root remains uncluttered:

```text
model_output_v5_temporal_250k_data_seed123_hv_1994_2012_v4v2/
  lz_exploratory_analysis/
    lz_wage_sample_flow.csv
    lz_wage_summary_overall.csv
    lz_wage_summary_by_censor.csv
    lz_wage_summary_by_year.csv
    lz_wage_relationship_summary.csv
```

The V2 modeling script also creates:

```text
plots/exploratory_wage_density_bg_lz_full.png
```

This is a model-free exploratory distribution plot of observed BG and true LZ
log daily wages, using the same colour scheme and density settings as the model
comparison figures.

For the temporal 2012 censored prediction-density figure, the BG baseline is
intentionally excluded because the figure's purpose is to compare the true LZ
distribution with the fitted Tobit and XGBoost predictions. BG remains included
in the numerical error tables and in the other relevant distributions.

### Finite V2 Overleaf Bundle

One Overleaf-ready LaTeX fragment was created for each of the 11 CSVs in the
finite-Tobit folder. The complete bundle is:

```text
overleaf_data/finite_v2/
  README.md
  data/      # copied finite source CSVs
  tables/    # 11 matching .tex fragments
```

The table fragments match the current thesis conventions (`booktabs`,
`threeparttable`, panel headings, restrained bolding of within-panel minima,
and explanatory notes). Their labels are prefixed with `v2-finite` so they do
not collide with existing labels in `master_thesis.tex`.

The current `master_thesis.tex` was deliberately not changed when this bundle
was created. Use the `\input{...}` commands and recommended placement listed in:

```text
overleaf_data/finite_v2/README.md
```

For the censored-only RMSE and wage-level CSVs, the source contains identical
`All` and `Censored` rows because every observation in that experiment is
censored. The LaTeX fragments display each result once and explain the
duplication in the table note.

## Important Dataset Finding

Version 2 of the dataset creation was likely broken.

The key issue: it saved yearly BG prep files to `bg_prep_test/...`, but later loaded from stale `bg_prep/bg_prep_YYYY.RData`. This caused nonsensical scaling and inconsistent final datasets.

Version 3 fixes this by using sample-specific paths, e.g.

```text
bg_prep_test/bg_prep_250k_YYYY.RData
```

Version 3 also deduplicates `dg_full_merge` by `benr, contryear` before joining. This is important because `dg_full_merge` has massive duplicate keys; old joins could create many-to-many row inflation or strange collapses.

Corrected Version 3 scaling looked sane:

```text
10k:      94,409 pre_model rows
100k:   931,687
200k: 1,854,076
250k: 2,317,104
```

Old Version 2 scaling was not sane, for example:

```text
10k:    678,984 pre_model rows
200k: 3,645,204
250k:   180,066
```

Conclusion: Version 3 is the more credible dataset builder. Earlier ultra-good XGBoost results should be treated with suspicion because they likely came from stale BG prep files, many-to-many joins, or distorted sample construction.

## Evaluation Design

The professors wanted the main comparison to use an in-sample approach for comparability with the classic Tobit/Card benchmark.

The agreed compromise is to report both:

```text
Main benchmark:
  In-sample common BG-LZ comparison sample.

Validation:
  Temporal split.
  Train = 1994-2011.
  Test  = 2012.
```

Interpretation:

The in-sample comparison is not an out-of-sample forecasting exercise. It is used to place Tobit and XGBoost on the same benchmark footing as the classical Tobit approach.

Predictive validity is assessed separately using the temporal 2012 holdout.

## Thesis / Presentation Structure

Recent meeting notes suggest this structure:

```text
Intro:
  - Motivation
  - Literature
  - Contribution

Data:
  - BG data first
  - LZ data second

Methods:
  - Tobit
  - XGBoost
  - Evaluation criteria
  - SHAP

Results:
  - Standard Tobit/Card model, V1 specification
  - New results, V4 specification with Tobit
  - XGBoost V1 and V4
  - Main comparison table without train/test split
  - SHAP
  - Temporal train/test validation
```

## Presentation Workflow / Overleaf Context

The presentation is mainly created and maintained in Overleaf. Local `.tex`
files are useful working copies, but do not assume the local folder contains all
Overleaf assets or that local compilation is the main workflow.

Relevant local presentation files:

```text
first_pres_v2.tex
  Local copy/export of the first presentation. Useful as the original structural
  and stylistic reference.

final_pres_v1.tex
  Current working copy of the final master thesis presentation.
```

Both presentations use a Beamer `metropolis` style. Image paths such as:

```text
iniPres/Plot_1.png
iniPres/Plot_2.png
iniPres/Plot_3.png
iniPres/Plot_4.png
```

may exist only in the online Overleaf project folder. Missing local presentation
assets are not evidence that the Overleaf presentation is broken.

Current state of `final_pres_v1.tex` as of 2026-06-17:

```text
Title
Motivation
Why censoring matters
Four top-coding illustration slides
Research question
Contribution
Administrative data
Lohnzettel data
Empirical design
Evaluation strategy
Tobit methodology
XGBoost setup and objective
Evaluation criteria
SHAP methodology
Results overview
In-sample predicted wage distributions
In-sample error densities
In-sample log RMSE
In-sample daily-wage-level errors
RMSE interpretation
Temporal split predicted wage distributions
Temporal split error densities
Temporal split log RMSE
Temporal split daily-wage-level errors
SHAP feature contributions
XGBoost feature importance
Conclusion
Fin
References
Backup: In-sample log RMSE (V2)
```

Professor / presentation notes now reflected in the file:

```text
- The remaining literature slide was removed from the main flow by commenting it
  out in LaTeX.
- The large "In-Sample: Log RMSE (V2)" table was moved to the end as a backup
  slide for questions.
- The four top-coding illustration slides should stay in the deck; they can be
  presented quickly.
- The final presentation should avoid proposal-style repetition of what the
  thesis "wants to do" and instead speak in completed-thesis language.
- Make clear earlier why Lohnzettel data are central: they provide uncensored
  wages, the supervised-learning target, and the evaluation benchmark.
```

Current main presentation logic:

```text
Intro:
  - motivation
  - why censoring matters
  - research question
  - contribution

Data:
  - BG / Austrian social-security administrative data first
  - Lohnzettel second, with stronger emphasis on why LZ is essential

Methods:
  - Tobit baseline
  - XGBoost
  - evaluation criteria
  - SHAP

Results:
  - in-sample benchmark
  - temporal split validation
  - SHAP / feature importance
  - RMSE interpretation around the in-sample results
```

Current `final_pres_v1.tex` uses actual image includes for some final-presentation
figures, for example:

```text
iniPres/predicted_wage_density_full_all.png
iniPres/error_density_censored.png
iniPres/shap_summary_xgb_v4_in_sample_v5_sample.png
iniPres/shap_importance_bar_xgb_v4_in_sample_v5_sample.png
```

The temporal split plot slides still contain placeholder text / working notes in
German. Treat these as TODO markers, not final slide text.

When helping with the presentation, prefer drafting slide text, structure, and
tables/figures that can be copied into the Overleaf project. Do not modify
`final_pres_v1.tex` unless explicitly asked.

## Historical Main 250k In-Sample Results (Pre-V4v2)

Historical reference only. For the final thesis, use the finite V4v2 tables and
results documented in the current authoritative section above.

Main result CSV:

```text
model_output_v5_250k/comprehensive_rmse_comparison_v5.csv
```

Key 250k in-sample results:

```text
All observations:
  XGBoost V4   RMSE 0.2126
  BG Baseline  RMSE 0.2870
  XGBoost V1   RMSE 0.3018
  Tobit V4     RMSE 0.3662

Censored observations:
  XGBoost V4   RMSE 0.3082
  XGBoost V1   RMSE 0.4029
  BG Baseline  RMSE 0.5851
  Tobit V4     RMSE 1.0289

Uncensored observations:
  XGBoost V4   RMSE 0.2033
  Tobit V4     RMSE 0.2481
  BG Baseline  RMSE 0.2491
  XGBoost V1   RMSE 0.2924
```

Interpretation: BG is centered reasonably overall but misses the upper tail because of censoring/topcoding. XGBoost V4 helps most on censored observations.

## Lohnzettel Wage Variables

In the Lohnzettel data:

```text
lz_ywage = annual Lohnzettel wage
lz_dwage = daily Lohnzettel wage
```

In the modeling scripts:

```r
lz_ywage_log  = log(pmax(lz_ywage * cpi_2000, 1))
lz_dwage_log  = log(pmax(lz_dwage * cpi_2000, 1))
lz_golden_log = log(pmax(lz_golden_p67_2_6_10 * cpi_2000 + 1, 1))
```

`lz_dwage_log` is the main true-wage target for the models and RMSE tables.

`lz_ywage_log` is annual Lohnzettel wage on the log scale.

`lz_golden_log` is a special LZ-derived variable carried in some comparison data, but it is not the main target.

Useful sanity check from full Lohnzettel data:

```text
mean(lz_ywage) = 18,066.89
mean(lz_dwage) = 61.81124
```

This implies `lz_dwage` is not simply `lz_ywage / 365`; it is closer to annual wage divided by covered/paid/employed days.

## Log RMSE Interpretation

The v5 RMSE and MAE are computed on log daily wages:

```r
error = predicted_log_wage - true_log_wage
RMSE = sqrt(mean(error^2))
MAE = mean(abs(error))
```

They are absolute errors in log-wage units, not direct euro errors.

Because:

```r
log(predicted_wage) - log(true_wage)
= log(predicted_wage / true_wage)
```

log errors are multiplicative. Exponentiating a log error gives a wage ratio:

```r
exp(log_error) = predicted_wage / true_wage
```

A rough percentage-style interpretation is:

```r
exp(RMSE) - 1
```

Examples:

```text
RMSE 0.2870 -> exp(0.2870) - 1 = about 33%
RMSE 0.2126 -> exp(0.2126) - 1 = about 24%
RMSE 0.5851 -> exp(0.5851) - 1 = about 80%
```

To approximate euro error, multiply by a reference wage:

```r
reference_wage * (exp(RMSE) - 1)
```

To approximate euro improvement between two models:

```r
reference_wage * (exp(rmse_old) - exp(rmse_new))
```

To report relative RMSE reduction on the log scale:

```r
1 - rmse_new / rmse_old
```

Do not interpret this as euros:

```r
exp(rmse_model_a) - exp(rmse_model_b)
```

That expression is a difference in multiplicative error factors, not a euro amount unless multiplied by a wage level.

## Historical: Sunday 2026-06-14 Wage-Level Results

Historical reference only. These results predate the V4v2 sampling frame and
the finite-Tobit table repair.

New result folder:

```text
model_output_v5_temporal_plus_wage_lvl_250k
```

Important CSVs in this folder:

```text
temporal_split_rmse_v5.csv
  Log-wage RMSE and MAE for temporal train/test validation.

temporal_split_wage_level_error_v5.csv
  Daily-wage-level RMSE, MAE, and median absolute error after converting
  log predictions back with exp().

comprehensive_rmse_comparison_v5.csv
  In-sample common-sample log-wage RMSE comparison.

comprehensive_wage_level_error_v5.csv
  In-sample common-sample daily-wage-level error comparison.

model_runtime_v5.csv
  Runtime for each in-sample and temporal model.

corrected_error_summary_with_bg_baseline_v5.csv
  Corrected in-sample log-error summary where non-finite predictions are filtered.
```

The wage-level files are not log RMSE. They are computed from the model target by:

```r
true_wage = exp(lz_dwage_log)
pred_wage = exp(pred_log)
Wage_RMSE = sqrt(mean((pred_wage - true_wage)^2))
```

In this project, `lz_dwage_log` was created from `lz_dwage`, so the wage-level metrics are daily-wage errors in the `lz_dwage` scale.

Important warning for presentation:

```text
RMSE in temporal_split_rmse_v5.csv
  = log daily-wage RMSE.

Wage_RMSE in temporal_split_wage_level_error_v5.csv
  = daily-wage-level RMSE after exp().
```

For the 2012 temporal test set, censored observations:

```text
Wage-level errors:
  XGBoost V4   Wage_RMSE 344.75   Wage_MAE 50.46   Median AE 21.39
  Tobit V1     Wage_RMSE 347.45   Wage_MAE 64.28   Median AE 31.59
  XGBoost V1   Wage_RMSE 351.05   Wage_MAE 65.77   Median AE 36.72
  BG Baseline  Wage_RMSE 352.73   Wage_MAE 78.05   Median AE 42.98
  Tobit V4     Wage_RMSE 428.12   Wage_MAE 267.14  Median AE 273.00
```

Wage-level RMSE reduction of XGBoost V4 on 2012 censored observations:

```text
vs BG Baseline:  2.26%
vs Tobit V1:     0.78%
vs Tobit V4:    19.47%
```

The corresponding log-RMSE results for the same 2012 censored observations are:

```text
Log-RMSE:
  XGBoost V4   0.4746
  Tobit V1     0.5474
  XGBoost V1   0.5696
  BG Baseline  0.6182
  Tobit V4     1.0700
```

Log-RMSE reduction of XGBoost V4 on 2012 censored observations:

```text
vs BG Baseline: 23.23%
vs Tobit V1:    13.30%
vs Tobit V4:    55.64%
```

Interpretation:

```text
XGBoost V4 substantially improves log-wage prediction for censored wages.
After transforming predictions back to daily-wage levels, RMSE improvements
are much smaller because wage-level RMSE is dominated by extreme high-wage
observations. Therefore, report wage-level MAE and median absolute error as
complementary practical measures.
```

Recommended presentation wording:

```text
On the log wage scale, XGBoost V4 clearly improves prediction accuracy for
censored wages. In daily-wage levels, the RMSE gain is smaller because squared
euro errors are highly sensitive to a small number of very large wage values.
However, XGBoost V4 still meaningfully reduces the average absolute daily-wage
error: for 2012 censored observations, MAE falls from 78.05 euros/day for the
BG baseline and 64.28 euros/day for Tobit V1 to 50.46 euros/day for XGBoost V4.
```

For the 2012 temporal test set, all observations:

```text
Wage-level errors:
  XGBoost V4   Wage_RMSE 149.12   Wage_MAE 8.14
  Tobit V1     Wage_RMSE 149.98   Wage_MAE 16.91
  BG Baseline  Wage_RMSE 150.12   Wage_MAE 9.68
  XGBoost V1   Wage_RMSE 150.49   Wage_MAE 15.52
  Tobit V4     Wage_RMSE 167.11   Wage_MAE 23.96
```

For the 2012 temporal test set, uncensored observations:

```text
Wage-level errors:
  BG Baseline  Wage_RMSE 120.38   Wage_MAE 4.28   Median AE 0.34
  XGBoost V4   Wage_RMSE 120.87   Wage_MAE 4.80   Median AE 1.24
  XGBoost V1   Wage_RMSE 121.27   Wage_MAE 11.55
  Tobit V1     Wage_RMSE 121.40   Wage_MAE 13.17
  Tobit V4     Wage_RMSE 123.87   Wage_MAE 4.26   Median AE 0.34
```

Interpret uncensored results carefully. The BG baseline is typically very close
to Lohnzettel for uncensored observations, especially by MAE and median absolute
error. Its RMSE can still look high because a small number of extreme
`lz_dwage` values dominate squared error.

## Historical: Monday 2026-06-15 Seed123 BG Baseline And P90 Diagnostics

This section documents the older seed-123 run used for comparison with V4v2.
It is not the authoritative source for the final thesis tables.

New seed123 result folder:

```text
model_output_v5_temporal_250k_data_seed123
```

The seed123 Lohnzettel wage diagnostic script is:

```text
analyze_lz_dwage_ywage_v5_for_seed123.R
```

It now uses:

```text
pre_model_output_test/pre_model_full_history_250k_seed123.RData
```

and writes its aggregate diagnostics directly into:

```text
model_output_v5_temporal_250k_data_seed123
```

Important diagnostic CSVs:

```text
lz_wage_summary_overall.csv
lz_wage_summary_by_censor.csv
lz_wage_summary_by_year.csv
lz_wage_relationship_summary.csv
```

The diagnostics now include `p90`. This is useful for explaining the BG
baseline because the legal topcoding/censoring threshold sits around the upper
tail of the wage distribution.

Seed123 comparison sample size and censoring share:

```text
All observations:       1,036,156
Censored observations:     74,963
Uncensored observations:  961,193
Censored share:             7.23%
```

Therefore, the relevant threshold region is not exactly the 90th percentile;
it is closer to the top 7-8% of observations. Still, the p90 is a useful
simple anchor for presentation.

Overall p90 daily wages in 2000-price daily-wage units:

```text
LZ true daily wage p90:   116.98
BG observed wage p90:     112.63
```

This shows that around the overall p90, BG and LZ are still close. The BG
baseline only breaks down once observations are censored/topcoded.

By censoring status:

```text
Uncensored observations:
  LZ true daily wage p90:   96.62
  BG observed wage p90:     96.98
  Difference:              -0.36

Censored observations:
  LZ true daily wage median: 162.60
  BG observed wage median:  121.71
  Difference:               40.88

  LZ true daily wage p90:   266.79
  BG observed wage p90:     129.64
  Difference:              137.16
```

This supports the interpretation that the BG baseline is very accurate for
uncensored wages, but for censored wages it is stuck near the threshold while
true Lohnzettel wages continue above it.

Seed123 BG baseline wage-level errors:

```text
All observations:
  Wage_RMSE       152.34
  Wage_MAE          9.78
  Median AE         0.58

Censored observations:
  Wage_RMSE       289.07
  Wage_MAE         71.12
  Median AE        41.19
  Mean error      -66.74
  Mean true wage  190.20

Uncensored observations:
  Wage_RMSE       136.02
  Wage_MAE          5.00
  Median AE         0.45
```

The censored BG baseline median absolute error is almost exactly the median
gap between true LZ wages and BG wages:

```text
162.60 - 121.71 = 40.89 euros/day
BG censored median AE = 41.19 euros/day
```

The censored BG baseline MAE is also close to the difference between the
overall p90/threshold region and the average true wage among censored workers:

```text
Mean true censored wage - overall LZ p90
= 190.20 - 116.98
= 73.22 euros/day

BG censored Wage_MAE = 71.12 euros/day
```

Interpretation:

```text
The BG censored MAE is not literally a p90 difference. It is the average
observation-level absolute error. But because censored BG wages are clustered
near the threshold and errors are mostly one-sided underprediction, the MAE is
approximately the distance from the threshold/p90 region to the average true
censored LZ wage.
```

Do not compare the BG censored MAE to the censored p90 gap:

```text
Censored LZ p90 - overall LZ p90
= 266.79 - 116.98
= 149.81 euros/day
```

That is much larger than the MAE because it describes the upper part of the
already-censored group, not the average absolute error across censored
observations.

## Important Files Added Or Changed

The following files were added or edited during recent Codex work:

```text
01_build_pre_model_dataset_Version4_v2.R
analyze_lz_dwage_ywage_v5.R
analyze_lz_dwage_ywage_v5_for_seed123.R
analyze_lz_dwage_ywage_v5_for_seed123_v2.R
analyze_lz_dwage_ywage_v5_for_seed123_v2_full_sample.R
log_rmse_interpretation_note.md
rebuild_finite_tobit_tables_v5.R
tobit_and_xgboost_v5_250k.R
tobit_and_xgboost_v5_temporal_250k.R
tobit_and_xgboost_v5_temporal_250k_data_seed123_v2.R
tobit_and_xgboost_v5_temporal_full_sample.R
overleaf_data/finite_v2/README.md
META_PROJECT_INFORMATION.md
```

`01_build_pre_model_dataset_Version4_v2.R`:
Current seed-123 builder. It samples persons from those observed in HV during
1994-2012, then retains their full histories for feature construction.

`analyze_lz_dwage_ywage_v5.R`:
Creates diagnostics for raw/log `lz_ywage` and `lz_dwage`, implied days, correlations, and summary statistics. It should only write aggregate CSVs, not row-level private data.

`analyze_lz_dwage_ywage_v5_for_seed123.R`:
Seed123 version of the Lohnzettel wage diagnostics. It reads `pre_model_full_history_250k_seed123.RData`, writes aggregate diagnostics into `model_output_v5_temporal_250k_data_seed123`, and includes `p90` in the summary tables.

`analyze_lz_dwage_ywage_v5_for_seed123_v2.R`:
Current exploratory-analysis script for V4v2. It writes the five aggregate CSVs
to the dedicated `lz_exploratory_analysis` subfolder.

`analyze_lz_dwage_ywage_v5_for_seed123_v2_full_sample.R`:
Full-sample exploratory companion to
`tobit_and_xgboost_v5_temporal_full_sample.R`. It uses the full v4 pre-model
file, applies no person subsampling, and writes the same five aggregate CSVs to
`model_output_v5_temporal_full_sample_v4/lz_exploratory_analysis`.

`log_rmse_interpretation_note.md`:
Short note explaining log RMSE, multiplicative interpretation, euro approximation, and model improvement formulas.

`tobit_and_xgboost_v5_250k.R`:
Main 250k in-sample benchmark script, plus temporal validation additions in Section 19.

`tobit_and_xgboost_v5_temporal_250k.R`:
Temporal version of v5. It removes unused `censReg` and `VGAM` imports because the HPC/Altix system cannot install `censReg`.

`tobit_and_xgboost_v5_temporal_250k_data_seed123_v2.R`:
Current V4v2 modeling script. It fits the BG baseline, Tobit V1, XGBoost V1,
and XGBoost V4, writes sample-flow diagnostics, adds the model-free BG/LZ
density plot, and excludes BG from the specific 2012 censored prediction-density
figure.

`tobit_and_xgboost_v5_temporal_full_sample.R`:
Intended full-sample modeling script. With `use_debug_sample <- FALSE`, it uses
the complete eligible population in the 1994-2012 model window and writes to
`model_output_v5_temporal_full_sample_v4`. Do not replace it with the
`full_memory_safe` variant unless the user explicitly changes this decision.

`rebuild_finite_tobit_tables_v5.R`:
One-off in-memory repair script for the completed V4v2 run. It excludes the
few non-finite Tobit evaluation rows and writes the authoritative finite table
folder without refitting the models.

`overleaf_data/finite_v2/README.md`:
Index and placement guide for the 11 Overleaf-ready LaTeX table fragments and
their copied finite source CSVs.

## Tobit Implementation

The Tobit-style model does not require `censReg` in the current scripts.

It is estimated using `survival::survreg()`:

```r
survival::survreg(
  survival::Surv(interval_lower, interval_upper, type = "interval2") ~ ...,
  data = data,
  dist = "gaussian"
)
```

So `survival` is required, but `censReg` is not required for the current code path.

## Legacy Temporal Script Outputs

This section describes the older `tobit_and_xgboost_v5_temporal_250k.R`
workflow, which still included Tobit V4. The current V4v2 workflow does not fit
or report Tobit V4; use the authoritative section above for current outputs.

`tobit_and_xgboost_v5_temporal_250k.R` includes in-sample and temporal validation for:

```text
BG Baseline
Tobit V1
Tobit V4
XGBoost V1
XGBoost V4
```

It writes:

```text
model_output_v5_temporal_250k/temporal_split_rmse_v5.csv
model_output_v5_temporal_250k/temporal_split_models_v5.RData
model_output_v5_temporal_250k/model_runtime_v5.csv
```

The runtime CSV times each model run for:

```text
In-sample common comparison:
  - Tobit V1
  - Tobit V4
  - XGBoost V1
  - XGBoost V4

Temporal validation:
  - Tobit V1
  - Tobit V4
  - XGBoost V1
  - XGBoost V4
```

## Practical Guidance For Future Codex Chats

Prefer reading scripts and copied result files locally. Do not expect full private data to exist on the local computer.

For full-sample modeling, use `tobit_and_xgboost_v5_temporal_full_sample.R`
and its companion diagnostic
`analyze_lz_dwage_ywage_v5_for_seed123_v2_full_sample.R`. Do not redirect
full-sample work to `tobit_and_xgboost_v5_full_memory_safe.R` unless the user
explicitly asks to reconsider that alternative.

Be careful when interpreting old outputs. Results from the broken Version 2 / old 10k pipeline may be too optimistic or distorted.

For code that must run on the HPC, avoid adding hard-to-install packages. In particular, avoid `censReg` unless absolutely necessary.

When adding diagnostics, write aggregate CSVs and log output rather than row-level private data.

When interpreting model quality, separate:

```text
In-sample benchmark comparability
```

from:

```text
Temporal out-of-sample validation
```

These answer different questions and should not be collapsed into one claim.

## MACH / HPC Runtime Context

The heavy computations run on the MACH high-performance cluster, with prompts seen as:

```text
mach k2303/k232474>
```

The interactive shell on MACH is `tcsh`, not bash. Bash-style constructs such as `for ... do ... done`, `${var#prefix}`, and complex one-line `if ... then ... endif` blocks can fail or drop the terminal into prompts such as `foreach?` or `if?`. Prefer simple `tcsh` commands, or enter `foreach ... end` loops line by line.

The cluster/capsule scheduling layer appears to use PBS Professional / PBSPro. RStudio work is launched through `qsub`, so `pstree` does not reliably show a direct parent-child path from a visible `Capsule-*` launcher to the final `rsession`. The reliable mapping from an R session PID to its PBS job is:

```tcsh
cat /proc/<PID>/cpuset
```

This returns paths like:

```text
/PBSPro/274948.mach
```

The PBS job id can then be matched to the capsule job table, for example:

```text
274923.mach -> Capsule-XL
274947.mach -> Capsule-XX
274948.mach -> Capsule-XX
274949.mach -> Capsule-XX
```

Confirmed example PID mappings from the Gemini HPC transcript:

```text
PID 457120   -> 274923.mach -> Capsule-XL
PID 1773679  -> 274947.mach -> Capsule-XX
PID 1774668  -> 274948.mach -> Capsule-XX
PID 1780437  -> 274949.mach -> Capsule-XX
```

Each `rsession` runs in its own capsule. The largest standard capsule used in the project was 512 GB RAM. A special 6 TB capsule was later created for the full dataset-generation run; see the dedicated runbook below. If a capsule exceeds its RAM allocation and spills into swap, performance can be badly harmed for other capsules/users on MACH. Therefore, monitor memory at the individual `rsession` / capsule level, not only through global `top` memory totals.

Useful monitoring rule:

```text
one rsession PID = one PBS job = one capsule
standard project capsule = max 512 GB RAM
special full-data capsule = 6,144 GB RAM
```

The important memory metric is `RES` for each `rsession`. Typical heavy sessions seen in the transcript used approximately:

```text
PID 1774668: RES about 310G
PID 1773679: RES about 279G
PID 1780437: RES about 249G
PID 457120:  RES about 35G
```

Practical danger thresholds for the standard 512 GB capsules:

```text
< 400G    comfortable
400-470G  warning
470-512G  danger
> 512G    likely swap / severe performance risk
```

To list active `rsession` processes and their PBS job ids in `tcsh`, the command pattern that worked was:

```tcsh
echo "PID\t%CPU\t%MEM\tKAPSEL / PBS-JOB"; echo "----+----+----+----+----+----+----+----+----+"; foreach pid (`pgrep -u $USER rsession`)
set cpuset = `cat /proc/$pid/cpuset`
set cpu = `ps -p $pid -o %cpu=`
set mem = `ps -p $pid -o %mem=`
set job = `echo $cpuset | sed 's|/PBSPro/||'`
echo "$pid\t$cpu%\t$mem%\t$job"
end
```

For memory monitoring, add `VIRT` and `RES` using `ps -p $pid -o vsz=` and `ps -p $pid -o rss=`. Avoid interpreting `VIRT - RES` as exact swap usage; it is not a reliable per-process swap measure.

### Special 6 TB Full-Data Capsule (July/August 2026)

A custom interactive capsule was created on the Altix UV1000 specifically for the full dataset-generation run. This was a one-off/special allocation, not a normal capsule size.

Recorded allocation:

```text
PBS job id:        276489.mach
Job name:          BigDATA_R_6TB
GNU Screen:        299673.BigDATA
Requested CPUs:    64
Requested memory:  6144 GB
Wall time:         240 hours (10 days)
Memory topology:   97 vnodes
Visible cores:     776 across those vnodes
Initial memory:    6207.250 GB total, 6110.056 GB free, 97.196 GB used
```

The capsule was submitted manually inside a GNU Screen session with:

```tcsh
qsub -I -N BigDATA_R_6TB -l select=ncpus=64:mem=6144gb
```

The 776 cores printed by `meminfo-free-RAM` are physically visible across the selected memory nodes. The PBS request was for 64 CPUs, so code should treat 64 as the allocated CPU count. The main reason for this special capsule is its 6 TB shared-memory allocation.

The job lifetime can be checked without entering the capsule:

```tcsh
my-walltimes
```

Do not submit a second `qsub` job merely because no capsule terminal/tab is visible. The manually created capsule does not necessarily create a separately labelled desktop terminal. Do not use `qdel` while the dataset run or RStudio session is needed.

### Reattaching the 6 TB Capsule and Starting RStudio

The successful graphical setup used the existing VNC server:

```text
VNC address:  mach.edvz.uni-linz.ac.at:5911
X display:    :11.0
```

No second VNC port is required. Open a new terminal within this VNC desktop and confirm the display before attaching:

```tcsh
printenv DISPLAY
```

Expected output:

```text
:11.0
```

The MACH installation has an old GNU Screen version (`4.00.02`) that does not support `screen -Q windows`. The saved session may show as `Attached` even when its terminal is not visible. Useful attachment commands are:

```tcsh
screen -list
screen -x 299673.BigDATA
```

`screen -x` joins an already-attached session. On this old Screen version, multi-display attachment can redraw badly or appear frozen. In that case, open another ordinary terminal and take exclusive control with:

```tcsh
screen -D -r 299673.BigDATA
```

An observed `screen -d -r 299673.BigDATA` attempt only detached the remote display and did not reattach locally. If using lowercase `-d`, run detachment and reattachment as two commands:

```tcsh
screen -d 299673.BigDATA
screen -r 299673.BigDATA
```

Screen names are case-sensitive: use `BigDATA`, not `BigData`.

After attachment, the definitive check is the current shell's cpuset:

```tcsh
cat /proc/$$/cpuset
```

Correct capsule shell:

```text
/PBSPro/276489.mach
```

Wrong/base VNC shell:

```text
/boot/exploratur
```

Do not launch the heavy run from `/boot/exploratur`. `printenv STY` can be empty even while the visible terminal is connected through Screen because the interactive PBS shell does not necessarily inherit `STY`; the `/proc/$$/cpuset` result is the authoritative check.

The administrator's manual `qsub` command did not include `-v DISPLAY`. Consequently, launching RStudio initially failed with:

```text
QXcbConnection: Could not connect to display
```

From the confirmed `/PBSPro/276489.mach` shell, set the VNC display and memory limit before loading the runtime:

```tcsh
setenv DISPLAY :11.0
set max_memory = 6000g
echo $max_memory
load.ExecEnv::Xenial_Ubuntu16_04_x86_64
```

The spaces around `=` are required for the `tcsh` form used here. `set max_memory 6000g` fails with `Variable name must begin with a letter.` The 6000 GB limit leaves some headroom below the 6144 GB PBS allocation.

At the `[Xenial @ mach]:->` prompt, start RStudio with:

```tcsh
printenv DISPLAY
rstudio
```

`printenv DISPLAY` should still return `:11.0`. This successfully launched RStudio 1.1.463 using R 3.6.3 on the 6 TB capsule.

If X11 authorization still fails despite the correct `DISPLAY`, run the following once from an ordinary terminal on the same VNC desktop, then retry `rstudio` inside the capsule:

```tcsh
xhost +SI:localuser:k232474
```

Keep the outer PBS capsule shell alive while RStudio or the dataset-generation process is running. Exiting that shell can terminate the interactive allocation.

### Non-Default Historical Alternative: Memory-Safe Full-Sample Script

The completed full pre-model file contains 112,238,803 rows and 8,220,895 persons. The earlier 250k V5 model script must not simply be pointed at that file: it retains several complete data copies, two dense XGBoost matrices, five-fold CV objects, and model result data simultaneously.

The following script was created as an exploratory standard-XXL/512-GB
alternative:

```text
tobit_and_xgboost_v5_full_memory_safe.R
```

It is **not the selected full-sample workflow**. As confirmed on 2026-09-23,
use `tobit_and_xgboost_v5_temporal_full_sample.R` instead. Keep the information
below only as historical documentation of the alternative; do not recommend or
run it by default.

The non-default script's main safeguards are:

- a 430 GiB application-level RSS budget inside the nominal 512 GB capsule;
- preflight peak estimates before feature construction, the BG-LZ join, Tobit, and every XGBoost fit;
- by-reference `data.table` feature construction;
- `tree_method = "hist"`, 16 XGBoost threads, and one dense matrix/model at a time;
- XGBoost CV on at most 1,000,000 rows, followed by the final fit on the complete applicable training sample;
- compact model objects without complete data or matrices embedded in them;
- full-sample numerical results but plots based on at most 250,000 rows;
- restart checkpoints under `model_output_v5_full_memory_safe/checkpoints`.

Only if the user explicitly decides to revisit this non-default script, the
associated Xenial/XXL setup used a vector-heap backstop as well as the capsule
memory setting:

```tcsh
cat /proc/$$/cpuset
set max_memory = 470g
setenv R_MAX_VSIZE 429496729600
load.ExecEnv::Xenial_Ubuntu16_04_x86_64
rstudio
```

`R_MAX_VSIZE=429496729600` is 400 GiB. It limits R's vector heap, not native allocations made by XGBoost, so it is an extra backstop rather than a total-process limit.

For a separate whole-process guard, identify the RStudio R process from another terminal:

```tcsh
pgrep -u $USER rsession
cat /proc/PID/cpuset
```

Confirm that the PID belongs to the current `/PBSPro/JOBID` capsule, then start:

```tcsh
tcsh watch_capsule_memory.csh PID 460
```

The watcher checks RSS every two seconds and sends `SIGSTOP` at 460 GiB. To terminate a stopped process cleanly, queue termination and then continue it with `kill -TERM PID; kill -CONT PID`; to resume the calculation, use only `kill -CONT PID`. The PBS/capsule allocation remains the final hard boundary; the 430 GiB script budget and 460 GiB watcher provide operational headroom below it. These limits belong to the non-default memory-safe experiment and must not be treated as the configuration for the selected temporal full-sample workflow.
