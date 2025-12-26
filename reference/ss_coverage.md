# Spatial Coverage Sampling

Generates sampling points at the centroids of compact geographical
strata. This is purposive sampling designed for optimal spatial
coverage.

## Usage

``` r
ss_coverage(
  x,
  n_strata = NULL,
  prior_points = NULL,
  n_try = 1L,
  n_cells = 2500L,
  verbose = FALSE
)
```

## Arguments

- x:

  An `sf` object representing the study area, OR an `ss_strata` object
  from
  [`ss_stratify()`](https://ccarbajal16.github.io/soilsampling/reference/ss_stratify.md).

- n_strata:

  Integer, number of strata (and samples) to create. Only used if `x` is
  an sf object.

- prior_points:

  Optional `sf` object with prior sampling locations.

- n_try:

  Integer, number of random initializations (default 1).

- n_cells:

  Integer, approximate number of grid cells (default 2500).

- verbose:

  Logical, whether to print progress (default FALSE).

## Value

An object of class `ss_samples` containing:

- samples:

  An `sf` object with sampling points at stratum centroids.

- method:

  Character, "coverage".

- n_samples:

  Integer, number of samples.

- strata:

  The stratification object.

- crs:

  The coordinate reference system.

## Details

Spatial coverage sampling places sampling points at the centroids of
compact geographical strata. This is optimal for model-based inference
(kriging) as it provides even spatial coverage.

If a centroid falls outside the study area boundary, it is moved to the
nearest cell center within the corresponding stratum.

## See also

[`ss_stratify()`](https://ccarbajal16.github.io/soilsampling/reference/ss_stratify.md),
[`ss_random()`](https://ccarbajal16.github.io/soilsampling/reference/ss_random.md),
[`ss_stratified()`](https://ccarbajal16.github.io/soilsampling/reference/ss_stratified.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(sf)
poly <- st_polygon(list(rbind(c(0, 0), c(100, 0), c(100, 50), c(0, 50), c(0, 0))))
study_area <- st_sf(geometry = st_sfc(poly))

# Direct usage
samples <- ss_coverage(study_area, n_strata = 25, n_try = 5)

# Or using pre-computed strata
strata <- ss_stratify(study_area, n_strata = 25, n_try = 5)
samples <- ss_coverage(strata)
} # }
```
