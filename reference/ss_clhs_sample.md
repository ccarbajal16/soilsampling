# Conditioned Latin Hypercube Sampling From a Raster Stack

Selects a representative baseline sample from a covariate raster stack
using conditioned Latin hypercube sampling (cLHS). Typically used as the
starting design for
[`ss_rf_optimize()`](https://ccarbajal16.github.io/soilsampling/reference/ss_rf_optimize.md).

## Usage

``` r
ss_clhs_sample(covariates, n_samples, seed = NULL)
```

## Arguments

- covariates:

  A `SpatRaster` stack of environmental covariates.

- n_samples:

  Integer, number of samples to select.

- seed:

  Optional integer seed for reproducibility.

## Value

A data frame with `x`, `y` and covariate columns for the selected sample
locations.

## See also

[`ss_rf_optimize()`](https://ccarbajal16.github.io/soilsampling/reference/ss_rf_optimize.md),
[`ss_rf_size()`](https://ccarbajal16.github.io/soilsampling/reference/ss_rf_size.md)

## Examples

``` r
if (FALSE) { # \dontrun{
r <- terra::rast("data/predictors.tif")
samples <- ss_clhs_sample(r, n_samples = 100, seed = 123)
} # }
```
