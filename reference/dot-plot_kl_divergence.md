# Plot KL Divergence vs. Sample Size

Plot KL Divergence vs. Sample Size

## Usage

``` r
.plot_kl_divergence(summary_results, fitted_curve, step_size)
```

## Arguments

- summary_results:

  Data frame with `sample_size`, `mean_kl`, `sd_kl`.

- fitted_curve:

  Data frame with `sample_size`, `fitted_kl`, or `NULL`.

- step_size:

  Integer, used to size error bar width.

## Value

A `ggplot` object.
