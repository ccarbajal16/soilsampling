# Plot Cumulative Density Function Used to Pick the Optimal Sample Size

Plot Cumulative Density Function Used to Pick the Optimal Sample Size

## Usage

``` r
.plot_kl_cdf(
  sample_sizes,
  cdf_values,
  probability_threshold,
  optimal_sample_size,
  step_size
)
```

## Arguments

- sample_sizes:

  Numeric vector of tested sample sizes.

- cdf_values:

  Numeric vector, CDF of (1 - KL divergence).

- probability_threshold:

  Numeric, CDF threshold.

- optimal_sample_size:

  Integer, the selected sample size.

- step_size:

  Integer, used to position the annotation.

## Value

A `ggplot` object.
