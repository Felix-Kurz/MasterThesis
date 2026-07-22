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

Dataset creation:

```text
01_build_pre_model_dataset_Version2.R
01_build_pre_model_dataset_Version3.R
```

Modeling/computation:

```text
tobit_and_xgboost_v5_250k.R
tobit_and_xgboost_v5_temporal_250k.R
```

The modeling scripts use `pre_model_full_history` plus `lohnzettel` to build a common BG-LZ comparison sample, fit Tobit and XGBoost variants, and compare predictions to LZ daily wages.

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

## Main 250k In-Sample Results

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

## Sunday 2026-06-14 Wage-Level Results

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

## Monday 2026-06-15 Seed123 BG Baseline And P90 Diagnostics

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
analyze_lz_dwage_ywage_v5.R
analyze_lz_dwage_ywage_v5_for_seed123.R
log_rmse_interpretation_note.md
tobit_and_xgboost_v5_250k.R
tobit_and_xgboost_v5_temporal_250k.R
META_PROJECT_INFORMATION.md
```

`analyze_lz_dwage_ywage_v5.R`:
Creates diagnostics for raw/log `lz_ywage` and `lz_dwage`, implied days, correlations, and summary statistics. It should only write aggregate CSVs, not row-level private data.

`analyze_lz_dwage_ywage_v5_for_seed123.R`:
Seed123 version of the Lohnzettel wage diagnostics. It reads `pre_model_full_history_250k_seed123.RData`, writes aggregate diagnostics into `model_output_v5_temporal_250k_data_seed123`, and includes `p90` in the summary tables.

`log_rmse_interpretation_note.md`:
Short note explaining log RMSE, multiplicative interpretation, euro approximation, and model improvement formulas.

`tobit_and_xgboost_v5_250k.R`:
Main 250k in-sample benchmark script, plus temporal validation additions in Section 19.

`tobit_and_xgboost_v5_temporal_250k.R`:
Temporal version of v5. It removes unused `censReg` and `VGAM` imports because the HPC/Altix system cannot install `censReg`.

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

## Temporal Script Outputs

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

Each `rsession` runs in its own capsule. The maximum capsule size available is 512 GB RAM. If a capsule exceeds its RAM allocation and spills into swap, performance can be badly harmed for other capsules/users on MACH. Therefore, monitor memory at the individual `rsession` / capsule level, not only through global `top` memory totals.

Useful monitoring rule:

```text
one rsession PID = one PBS job = one capsule = max 512 GB RAM
```

The important memory metric is `RES` for each `rsession`. Typical heavy sessions seen in the transcript used approximately:

```text
PID 1774668: RES about 310G
PID 1773679: RES about 279G
PID 1780437: RES about 249G
PID 457120:  RES about 35G
```

Practical danger thresholds:

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
