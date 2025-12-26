# Simple Random Sampling

Generates sampling points using simple random sampling within a study
area.

## Usage

``` r
ss_random(x, n, seed = NULL)
```

## Arguments

- x:

  An `sf` object representing the study area (polygon geometry).

- n:

  Integer, the number of sampling points to generate.

- seed:

  Optional integer seed for reproducibility.

## Value

An object of class `ss_samples` containing:

- samples:

  An `sf` object with the sampling points.

- method:

  Character, "simple_random".

- n_samples:

  Integer, number of samples.

- crs:

  The coordinate reference system.

## Details

Simple random sampling selects locations uniformly at random within the
study area. Each location has an equal probability of being selected.

## See also

[`ss_stratified()`](https://ccarbajal16.github.io/soilsampling/reference/ss_stratified.md),
[`ss_coverage()`](https://ccarbajal16.github.io/soilsampling/reference/ss_coverage.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(sf)
poly <- st_polygon(list(rbind(c(0, 0), c(100, 0), c(100, 50), c(0, 50), c(0, 0))))
study_area <- st_sf(geometry = st_sfc(poly))

samples <- ss_random(study_area, n = 20)
plot(samples)
} # }
```
