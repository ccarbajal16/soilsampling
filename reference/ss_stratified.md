# Stratified Random Sampling

Generates sampling points using stratified random sampling, where random
samples are taken within each stratum.

## Usage

``` r
ss_stratified(strata, n_per_stratum = 1L, seed = NULL)
```

## Arguments

- strata:

  An object of class `ss_strata` created by
  [`ss_stratify()`](https://ccarbajal16.github.io/soilsampling/reference/ss_stratify.md).

- n_per_stratum:

  Integer, number of samples per stratum (default 1).

- seed:

  Optional integer seed for reproducibility.

## Value

An object of class `ss_samples` containing:

- samples:

  An `sf` object with sampling points and stratum assignments.

- method:

  Character, "stratified_random".

- n_samples:

  Integer, total number of samples.

- strata:

  The input strata object.

- crs:

  The coordinate reference system.

## Details

Stratified random sampling first divides the study area into strata,
then takes random samples within each stratum. This ensures spatial
coverage while maintaining the randomness required for design-based
inference.

## See also

[`ss_stratify()`](https://ccarbajal16.github.io/soilsampling/reference/ss_stratify.md),
[`ss_random()`](https://ccarbajal16.github.io/soilsampling/reference/ss_random.md),
[`ss_coverage()`](https://ccarbajal16.github.io/soilsampling/reference/ss_coverage.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(sf)
poly <- st_polygon(list(rbind(c(0, 0), c(100, 0), c(100, 50), c(0, 50), c(0, 0))))
study_area <- st_sf(geometry = st_sfc(poly))

strata <- ss_stratify(study_area, n_strata = 20)
samples <- ss_stratified(strata, n_per_stratum = 1)
plot(samples)
} # }
```
