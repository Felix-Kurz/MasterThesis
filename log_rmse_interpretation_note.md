# Log RMSE Interpretation Refresher

In the v5 model output, RMSE and MAE are computed on **log daily wages**:

```r
error = predicted_log_wage - true_log_wage
RMSE = sqrt(mean(error^2))
MAE = mean(abs(error))
```

So the error is absolute in **log-wage units**, not in euros.

## Why Log Errors Are Multiplicative

Because:

```r
log(predicted_wage) - log(true_wage)
= log(predicted_wage / true_wage)
```

Exponentiating a log error gives a wage ratio:

```r
exp(log_error) = predicted_wage / true_wage
```

Example:

```text
true wage      = 60 euros/day
predicted wage = 80 euros/day

log(80) - log(60) = log(80 / 60) = log(1.333) = 0.287
```

So a log error of `0.287` means the prediction is about `1.333` times the true wage, or about `33%` too high.

If prediction is too low:

```text
true wage      = 60 euros/day
predicted wage = 45 euros/day

log(45) - log(60) = log(0.75) = -0.288
```

The absolute log error is again about `0.288`, but this means the prediction is about `25%` too low.

## Interpreting RMSE

A rough percentage interpretation is:

```r
exp(RMSE) - 1
```

For example:

```text
RMSE = 0.287
exp(0.287) - 1 = 0.332
```

This means a typical multiplicative error of about `33%`.

## Converting Log RMSE to Euros

Log RMSE is not in euros by itself. To get an approximate euro error, choose a reference wage:

```r
euro_error_approx = reference_wage * (exp(RMSE) - 1)
```

Example with a 60 euro/day reference wage:

```r
60 * (exp(0.287) - 1)
= 60 * 0.332
= 19.9 euros/day
```

With a 120 euro/day reference wage:

```r
120 * (exp(0.287) - 1)
= 39.8 euros/day
```

Same log RMSE, larger euro error because the wage level is larger.

## Comparing Models

Do not interpret this as euros:

```r
exp(rmse_model_a) - exp(rmse_model_b)
```

That is a difference in multiplicative error factors, not a euro amount.

To express improvement in euros, multiply by a reference wage:

```r
euro_improvement_approx =
  reference_wage * (exp(rmse_old) - exp(rmse_new))
```

Example:

```text
BG baseline censored RMSE = 0.5851
XGBoost V4 censored RMSE  = 0.3082
reference wage            = 120 euros/day
```

```r
120 * (exp(0.5851) - exp(0.3082))
= 120 * (1.795 - 1.361)
= 52.1 euros/day
```

## Relative RMSE Reduction

For reporting model improvement on the log scale:

```r
1 - rmse_new / rmse_old
```

Example:

```r
1 - 0.3082 / 0.5851
= 0.473
```

This means XGBoost V4 reduces censored-observation log RMSE by about `47%` relative to the BG baseline.

## Main Takeaway

The v5 RMSE values are valid, but they are **log-scale errors**. They should be interpreted as multiplicative percentage-style errors, not as direct euro errors.
