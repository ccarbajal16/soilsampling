# End-to-End cLHS + Random Forest Sampling Optimization

Runs the full two-stage workflow: a cLHS baseline design
([`ss_clhs_sample()`](https://ccarbajal16.github.io/soilsampling/reference/ss_clhs_sample.md))
refined by Random Forest simulated annealing
([`ss_rf_optimize()`](https://ccarbajal16.github.io/soilsampling/reference/ss_rf_optimize.md)),
with side-by-side comparison plots and an optional export of CSV/PNG
outputs.

## Usage

``` r
ss_rf_size(
  covariates,
  n_samples = 100,
  n_iterations = 500,
  output_dir = NULL,
  seed = 123,
  target_var = NULL,
  covariate_name = NULL
)
```

## Arguments

- covariates:

  A character path to a `.tif` file or directory (see
  [`ss_load_rasters()`](https://ccarbajal16.github.io/soilsampling/reference/ss_load_rasters.md)),
  or an already-loaded `SpatRaster` stack.

- n_samples:

  Integer, number of samples in the design. Default `100`.

- n_iterations:

  Integer, simulated annealing iterations. Default `500`.

- output_dir:

  Character, directory to write CSV/PNG outputs to. If `NULL` (default),
  nothing is written to disk.

- seed:

  Optional integer seed, passed to
  [`ss_rf_optimize()`](https://ccarbajal16.github.io/soilsampling/reference/ss_rf_optimize.md).
  Default `123`.

- target_var:

  Character, covariate to use as the RF response. Passed to
  [`ss_rf_mse()`](https://ccarbajal16.github.io/soilsampling/reference/ss_rf_mse.md).

- covariate_name:

  Character, background layer for the comparison plots. Passed to the
  internal plotting helper.

## Value

A list with:

- clhs_samples:

  Data frame, the cLHS baseline design.

- rf_optimized_samples:

  Data frame, the RF-optimized design.

- comparison_table:

  Data frame comparing MSE and improvement.

- plots:

  List with `clhs_plot` and `rf_plot`.

- improvement:

  Numeric, percent MSE reduction.

- file_paths:

  Only when `output_dir` is supplied: named list of files written.

## See also

[`ss_clhs_sample()`](https://ccarbajal16.github.io/soilsampling/reference/ss_clhs_sample.md),
[`ss_rf_optimize()`](https://ccarbajal16.github.io/soilsampling/reference/ss_rf_optimize.md),
[`ss_rf_mse()`](https://ccarbajal16.github.io/soilsampling/reference/ss_rf_mse.md)

## Examples

``` r
if (FALSE) { # \dontrun{
res <- ss_rf_size("data/predictors.tif", n_samples = 100, n_iterations = 500)
res$improvement
} # }
```
