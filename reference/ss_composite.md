# Composite Sampling

Generates sampling points for composite sampling from equal-area strata.
Multiple samples per stratum are grouped into composites.

## Usage

``` r
ss_composite(
  x,
  n_strata = NULL,
  n_composites,
  n_try = 1L,
  n_cells = 2500L,
  seed = NULL,
  verbose = FALSE
)
```

## Arguments

- x:

  An `sf` object representing the study area, OR an `ss_strata` object
  with `equal_area = TRUE`.

- n_strata:

  Integer, number of strata. Only used if `x` is an sf object.

- n_composites:

  Integer, number of composite samples to create.

- n_try:

  Integer, number of random initializations (default 1).

- n_cells:

  Integer, approximate number of grid cells (default 2500).

- seed:

  Optional integer seed for reproducibility.

- verbose:

  Logical, whether to print progress (default FALSE).

## Value

An object of class `ss_samples` with a `composite` column indicating
composite group membership.

## Details

Composite sampling is used when individual samples are combined before
analysis. This function creates equal-area strata and generates multiple
random samples per stratum, grouped into composite samples.

Each composite includes one sample from each stratum.

## See also

[`ss_coverage_equal_area()`](https://ccarbajal16.github.io/soilsampling/reference/ss_coverage_equal_area.md),
[`ss_stratified()`](https://ccarbajal16.github.io/soilsampling/reference/ss_stratified.md)

## Examples

``` r
library(sf)
#> Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE
poly <- st_polygon(list(rbind(c(0, 0), c(100, 0), c(100, 50), c(0, 50), c(0, 0))))
study_area <- st_sf(geometry = st_sfc(poly))

# Create 3 composite samples from 20 strata
samples <- ss_composite(study_area, n_strata = 20, n_composites = 3)
```
