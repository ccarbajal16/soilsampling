# End-to-End cLHS Sample Size Optimization

Runs
[`ss_kl_optimize()`](https://ccarbajal16.github.io/soilsampling/reference/ss_kl_optimize.md)
starting from predictor rasters (a file path, a directory of `.tif`
files, or an already-loaded `SpatRaster`) or directly from a population
data frame. Optionally writes results and plots to disk.

## Usage

``` r
ss_kl_size(
  x,
  output_dir = NULL,
  max_population = 1e+05,
  min_samples = 10,
  max_samples = 500,
  step_size = 10,
  n_replicates = 10,
  n_bins = 25,
  probability_threshold = 0.95
)
```

## Arguments

- x:

  One of: a character path to a `.tif` file or a directory of `.tif`
  files, a `SpatRaster` stack, or a data frame of population ancillary
  data (used as-is, skipping raster extraction).

- output_dir:

  Character, directory to write CSV/PNG outputs to. If `NULL` (default),
  nothing is written to disk.

- max_population:

  Integer, if the population has more rows than this, a random subsample
  of this size is used to keep runtime reasonable. Default `100000`.

- min_samples, max_samples, step_size, n_replicates, n_bins,
  probability_threshold:

  Passed to
  [`ss_kl_optimize()`](https://ccarbajal16.github.io/soilsampling/reference/ss_kl_optimize.md).

## Value

The list returned by
[`ss_kl_optimize()`](https://ccarbajal16.github.io/soilsampling/reference/ss_kl_optimize.md),
plus a `file_paths` element (only when `output_dir` is supplied) listing
the files written.

## See also

[`ss_kl_optimize()`](https://ccarbajal16.github.io/soilsampling/reference/ss_kl_optimize.md),
[`ss_load_rasters()`](https://ccarbajal16.github.io/soilsampling/reference/ss_load_rasters.md)

## Examples

``` r
if (FALSE) { # \dontrun{
res <- ss_kl_size("data/predictors.tif", output_dir = "outputs")
res$optimal_sample_size
} # }
```
