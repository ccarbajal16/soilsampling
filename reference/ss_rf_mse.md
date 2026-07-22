# Cross-Validated Random Forest MSE for a Sample Design

Trains a Random Forest with k-fold cross-validation on a candidate
sample design and returns the mean squared error for a chosen response
covariate. Used as the objective function for
[`ss_rf_optimize()`](https://ccarbajal16.github.io/soilsampling/reference/ss_rf_optimize.md).

## Usage

``` r
ss_rf_mse(sample_points, full_data = NULL, n_folds = 5, target_var = NULL)
```

## Arguments

- sample_points:

  Data frame with `x`, `y` as the first two columns followed by
  covariate columns (as returned by
  [`ss_clhs_sample()`](https://ccarbajal16.github.io/soilsampling/reference/ss_clhs_sample.md)).

- full_data:

  Optional data frame of the full covariate population; if supplied,
  `sample_points` is restricted to the shared columns.

- n_folds:

  Integer, number of cross-validation folds. Default `5`.

- target_var:

  Character, name of the covariate to predict. If `NULL` (default), the
  third covariate column is used, or the first if fewer than three are
  available.

## Value

Numeric, the mean cross-validated MSE, or `NA_real_` if the design has
too few rows/columns or every fold fails.

## See also

[`ss_rf_optimize()`](https://ccarbajal16.github.io/soilsampling/reference/ss_rf_optimize.md)

## Examples

``` r
if (FALSE) { # \dontrun{
samples <- ss_clhs_sample(r, n_samples = 100, seed = 1)
ss_rf_mse(samples)
} # }
```
