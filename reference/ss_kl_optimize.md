# Optimize cLHS Sample Size Using KL Divergence

Determines the sample size at which a conditioned Latin hypercube sample
(cLHS) best represents the population distribution, following the
KL-divergence-based approach of Malone et al. (2019). Runs cLHS at a
range of sample sizes, fits an exponential decay curve to the mean KL
divergence, and reports the smallest sample size that reaches a target
proportion of the maximum achievable improvement.

## Usage

``` r
ss_kl_optimize(
  population_data,
  min_samples = 10,
  max_samples = 500,
  step_size = 10,
  n_replicates = 10,
  n_bins = 25,
  probability_threshold = 0.95
)
```

## Arguments

- population_data:

  Data frame of population ancillary data (one row per population unit,
  one column per covariate).

- min_samples:

  Integer, minimum sample size to test. Default `10`.

- max_samples:

  Integer, maximum sample size to test. Default `500`.

- step_size:

  Integer, increment between tested sample sizes. Default `10`.

- n_replicates:

  Integer, number of cLHS replicates per sample size. Default `10`.

- n_bins:

  Integer, number of histogram bins used by
  [`ss_kl_divergence()`](https://ccarbajal16.github.io/soilsampling/reference/ss_kl_divergence.md).
  Default `25`.

- probability_threshold:

  Numeric in (0, 1\], CDF threshold used to pick the optimal sample
  size. Default `0.95`.

## Value

A list with:

- raw_results:

  Data frame, one row per replicate.

- summary_results:

  Data frame, mean/sd KL divergence per sample size.

- fitted_model:

  The fitted `nls` exponential decay model, or `NULL`.

- fitted_curve:

  Data frame of fitted KL divergence per sample size.

- optimal_sample_size:

  Integer, the recommended sample size.

- plot_kl:

  A `ggplot` of KL divergence vs. sample size.

- plot_cdf:

  A `ggplot` of the CDF used to pick the optimal size.

## Details

The relationship between sample size \\n\\ and KL divergence is modeled
as \\KL(n) = b_1 e^{-kn} + b_0\\. The optimal sample size is the
smallest \\n\\ for which the cumulative proportion of improvement,
\\(max(KL) - KL(n)) / (max(KL) - min(KL))\\, reaches
`probability_threshold`.

## See also

[`ss_kl_divergence()`](https://ccarbajal16.github.io/soilsampling/reference/ss_kl_divergence.md)

## Examples

``` r
if (FALSE) { # \dontrun{
pop <- data.frame(a = rnorm(2000), b = runif(2000))
res <- ss_kl_optimize(pop, min_samples = 10, max_samples = 60,
  step_size = 10, n_replicates = 3)
res$optimal_sample_size
} # }
```
