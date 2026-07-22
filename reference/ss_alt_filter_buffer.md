# Exclude Candidate Sites Within a Distance Buffer of Target Sites

Removes candidate sites that fall within `min_distance` of any target
(e.g. inaccessible) site, ensuring spatial separation between original
and alternative sampling locations.

## Usage

``` r
ss_alt_filter_buffer(candidate_sites, target_sites, min_distance)
```

## Arguments

- candidate_sites:

  Data frame with `x`, `y` candidate coordinates.

- target_sites:

  Data frame with `x`, `y` target coordinates.

- min_distance:

  Numeric, minimum allowed distance (in raster coordinate units) between
  a candidate and every target site.

## Value

The subset of `candidate_sites` at least `min_distance` from every row
of `target_sites`.

## See also

[`ss_alt_candidates()`](https://ccarbajal16.github.io/soilsampling/reference/ss_alt_candidates.md),
[`ss_alt_sites()`](https://ccarbajal16.github.io/soilsampling/reference/ss_alt_sites.md)

## Examples

``` r
candidates <- data.frame(x = c(0, 100, 500), y = c(0, 100, 500))
targets <- data.frame(x = 0, y = 0)
ss_alt_filter_buffer(candidates, targets, min_distance = 200)
#>     x   y
#> 3 500 500
```
