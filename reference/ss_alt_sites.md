# Find Alternative Sampling Sites for Inaccessible Locations

End-to-end workflow: generates a candidate pool
([`ss_alt_candidates()`](https://ccarbajal16.github.io/soilsampling/reference/ss_alt_candidates.md)),
optionally excludes candidates near the target sites
([`ss_alt_filter_buffer()`](https://ccarbajal16.github.io/soilsampling/reference/ss_alt_filter_buffer.md)),
then computes environmental similarity
([`ss_alt_similarity()`](https://ccarbajal16.github.io/soilsampling/reference/ss_alt_similarity.md))
and selects the top alternatives
([`ss_alt_rank()`](https://ccarbajal16.github.io/soilsampling/reference/ss_alt_rank.md))
for each target (e.g. inaccessible) site.

## Usage

``` r
ss_alt_sites(
  covariates,
  target_sites,
  method = c("mahalanobis", "euclidean", "gower"),
  n_alternatives = 5,
  n_candidates = NULL,
  min_distance_buffer = NULL,
  categorical_vars = NULL,
  weights = NULL,
  normalize = TRUE,
  missing_method = "pairwise",
  candidate_method = c("random", "systematic"),
  seed = NULL,
  output_dir = NULL
)
```

## Arguments

- covariates:

  A character path to a `.tif` file or directory (see
  [`ss_load_rasters()`](https://ccarbajal16.github.io/soilsampling/reference/ss_load_rasters.md)),
  or an already-loaded `SpatRaster` stack.

- target_sites:

  Data frame of sites needing alternatives, with `x` and `y` coordinate
  columns. A `site_id` column is generated if missing. Environmental
  values are extracted automatically for any covariate layer not already
  present as a column.

- method:

  Similarity metric: `"mahalanobis"` (default), `"euclidean"`, or
  `"gower"`. Passed to
  [`ss_alt_similarity()`](https://ccarbajal16.github.io/soilsampling/reference/ss_alt_similarity.md).

- n_alternatives:

  Integer, number of alternatives per target site. Default `5`.

- n_candidates:

  Integer, size of the candidate pool. If `NULL` (default), set to
  `max(n_alternatives * 50, 1000)`.

- min_distance_buffer:

  Numeric, minimum distance (raster coordinate units) candidates must
  keep from every target site. If `NULL` (default), no buffer is
  applied.

- categorical_vars:

  Character vector of categorical covariate names. Passed to
  [`ss_alt_similarity()`](https://ccarbajal16.github.io/soilsampling/reference/ss_alt_similarity.md).

- weights:

  Optional named numeric vector of variable weights. Passed to
  [`ss_alt_similarity()`](https://ccarbajal16.github.io/soilsampling/reference/ss_alt_similarity.md).

- normalize:

  Logical, passed to
  [`ss_alt_similarity()`](https://ccarbajal16.github.io/soilsampling/reference/ss_alt_similarity.md).
  Default `TRUE`.

- missing_method:

  Passed to
  [`ss_alt_similarity()`](https://ccarbajal16.github.io/soilsampling/reference/ss_alt_similarity.md).
  Default `"pairwise"`.

- candidate_method:

  `"random"` (default) or `"systematic"`. Passed to
  [`ss_alt_candidates()`](https://ccarbajal16.github.io/soilsampling/reference/ss_alt_candidates.md).

- seed:

  Optional integer seed for reproducibility.

- output_dir:

  Character, directory to write CSV outputs to. If `NULL` (default),
  nothing is written to disk.

## Value

A list with:

- target_sites:

  Data frame, targets with environmental values.

- candidate_sites:

  Data frame, the (buffer-filtered) candidate pool.

- alternatives:

  Data frame, all selected alternatives for all target sites, with a
  `target_site_id` column.

- selected_by_site:

  Named list of per-target alternative data frames, keyed by `site_id`.

- method_info:

  List recording the parameters used.

- file_paths:

  Only when `output_dir` is supplied: named list of files written.

## See also

[`ss_alt_candidates()`](https://ccarbajal16.github.io/soilsampling/reference/ss_alt_candidates.md),
[`ss_alt_filter_buffer()`](https://ccarbajal16.github.io/soilsampling/reference/ss_alt_filter_buffer.md),
[`ss_alt_similarity()`](https://ccarbajal16.github.io/soilsampling/reference/ss_alt_similarity.md),
[`ss_alt_rank()`](https://ccarbajal16.github.io/soilsampling/reference/ss_alt_rank.md)

## Examples

``` r
if (FALSE) { # \dontrun{
inaccessible <- read.csv("data/inaccessible_sites.csv")
res <- ss_alt_sites("data/predictors.tif", inaccessible,
  n_alternatives = 3, min_distance_buffer = 300, seed = 123
)
res$alternatives
} # }
```
