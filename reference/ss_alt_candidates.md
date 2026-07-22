# Generate Candidate Sites for Alternative Site Selection

Generates a pool of candidate sites (random or on a systematic grid)
within a raster's extent, restricted to valid (non-`NA`) cells, and
extracts their environmental values.

## Usage

``` r
ss_alt_candidates(
  raster_data,
  n_candidates,
  method = c("random", "systematic"),
  spacing = NULL,
  seed = NULL
)
```

## Arguments

- raster_data:

  A `SpatRaster` stack of environmental covariates.

- n_candidates:

  Integer, number of candidate sites to generate.

- method:

  `"random"` (default) or `"systematic"` (regular grid).

- spacing:

  Numeric, grid spacing for `method = "systematic"` (in raster
  coordinate units). If `NULL` (default), calculated from the raster
  extent to approximate `n_candidates`.

- seed:

  Optional integer seed for reproducibility.

## Value

A data frame with `site_id`, `x`, `y`, `type` (`"candidate"`), and one
column per raster layer.

## See also

[`ss_alt_similarity()`](https://ccarbajal16.github.io/soilsampling/reference/ss_alt_similarity.md),
[`ss_alt_filter_buffer()`](https://ccarbajal16.github.io/soilsampling/reference/ss_alt_filter_buffer.md),
[`ss_alt_sites()`](https://ccarbajal16.github.io/soilsampling/reference/ss_alt_sites.md)

## Examples

``` r
if (FALSE) { # \dontrun{
r <- terra::rast("data/predictors.tif")
candidates <- ss_alt_candidates(r, n_candidates = 1000, seed = 123)
} # }
```
