# Standardize Site Coordinate Data

Normalizes a data frame of site coordinates (typically loaded from CSV)
to the `site_id`, `x`, `y`, `type` column layout used throughout the
`ss_alt_*` functions.

## Usage

``` r
ss_alt_standardize_sites(
  sites_data,
  x_col = "x",
  y_col = "y",
  id_col = NULL,
  site_type = "inaccessible"
)
```

## Arguments

- sites_data:

  Data frame with at least x/y coordinate columns.

- x_col:

  Character, name of the x coordinate column. Default `"x"`.

- y_col:

  Character, name of the y coordinate column. Default `"y"`.

- id_col:

  Optional character, name of an existing site ID column. If `NULL` or
  not found, IDs are generated as `"<site_type>_<n>"`.

- site_type:

  Character, value for the `type` column and the ID prefix when `id_col`
  is not supplied. Default `"inaccessible"`.

## Value

A data frame with `site_id`, `x`, `y`, `type` as the first four columns,
followed by any remaining columns from `sites_data`.

## See also

[`ss_alt_sites()`](https://ccarbajal16.github.io/soilsampling/reference/ss_alt_sites.md)

## Examples

``` r
raw <- data.frame(lon = c(629500, 630200), lat = c(9879500, 9880100))
ss_alt_standardize_sites(raw, x_col = "lon", y_col = "lat")
#>          site_id      x       y         type
#> 1 inaccessible_1 629500 9879500 inaccessible
#> 2 inaccessible_2 630200 9880100 inaccessible
```
