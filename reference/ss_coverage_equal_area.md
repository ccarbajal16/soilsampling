# Spatial Coverage Sampling with Equal-Area Strata

Generates sampling points using spatial coverage with equal-area strata.
Useful when strata need to have similar sizes.

## Usage

``` r
ss_coverage_equal_area(
  x,
  n_strata,
  n_try = 1L,
  n_cells = 2500L,
  verbose = FALSE
)
```

## Arguments

- x:

  An `sf` object representing the study area.

- n_strata:

  Integer, number of strata to create.

- n_try:

  Integer, number of random initializations (default 1).

- n_cells:

  Integer, approximate number of grid cells (default 2500).

- verbose:

  Logical, whether to print progress (default FALSE).

## Value

An object of class `ss_samples`.

## Details

This function creates strata of approximately equal area using the
swopping algorithm, then returns centroid samples.

## See also

[`ss_coverage()`](https://ccarbajal16.github.io/soilsampling/reference/ss_coverage.md),
[`ss_composite()`](https://ccarbajal16.github.io/soilsampling/reference/ss_composite.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(sf)
poly <- st_polygon(list(rbind(c(0, 0), c(100, 0), c(100, 50), c(0, 50), c(0, 0))))
study_area <- st_sf(geometry = st_sfc(poly))

samples <- ss_coverage_equal_area(study_area, n_strata = 25)
} # }
```
