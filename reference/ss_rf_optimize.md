# Optimize a Sample Design With Random Forest Simulated Annealing

Starts from a cLHS baseline and iteratively swaps sample points using
simulated annealing to minimize cross-validated Random Forest MSE
([`ss_rf_mse()`](https://ccarbajal16.github.io/soilsampling/reference/ss_rf_mse.md)),
following Wadoux et al. (2019).

## Usage

``` r
ss_rf_optimize(
  covariates,
  n_samples,
  n_iterations = 500,
  seed = NULL,
  temperature = 1000,
  cooling_rate = 0.95,
  target_var = NULL
)
```

## Arguments

- covariates:

  A `SpatRaster` stack of environmental covariates.

- n_samples:

  Integer, number of samples in the design.

- n_iterations:

  Integer, number of simulated annealing iterations. Default `500`.

- seed:

  Optional integer seed. The cLHS baseline uses `seed`; the annealing
  loop uses `seed + 1`, so the two stages stay reproducible but
  independent.

- temperature:

  Numeric, initial annealing temperature. Default `1000`.

- cooling_rate:

  Numeric in (0, 1), multiplicative cooling factor applied each
  iteration. Default `0.95`.

- target_var:

  Character, covariate to use as the RF response. Passed to
  [`ss_rf_mse()`](https://ccarbajal16.github.io/soilsampling/reference/ss_rf_mse.md).

## Value

A list with:

- optimized_samples:

  Data frame, the best design found.

- initial_samples:

  Data frame, the cLHS baseline design.

- initial_mse:

  Numeric, baseline MSE.

- final_mse:

  Numeric, MSE of `optimized_samples`.

- improvement:

  Numeric, percent MSE reduction vs. baseline.

## See also

[`ss_clhs_sample()`](https://ccarbajal16.github.io/soilsampling/reference/ss_clhs_sample.md),
[`ss_rf_mse()`](https://ccarbajal16.github.io/soilsampling/reference/ss_rf_mse.md),
[`ss_rf_size()`](https://ccarbajal16.github.io/soilsampling/reference/ss_rf_size.md)

## Examples

``` r
if (FALSE) { # \dontrun{
r <- terra::rast("data/predictors.tif")
res <- ss_rf_optimize(r, n_samples = 100, n_iterations = 500, seed = 123)
res$improvement
} # }
```
