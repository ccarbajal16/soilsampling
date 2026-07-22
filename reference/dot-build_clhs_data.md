# Prepare Covariate Data for cLHS

Drops coordinate columns, coerces to numeric, and keeps only columns
with non-negligible variance (cLHS cannot use constant covariates).

## Usage

``` r
.build_clhs_data(cov_df)
```

## Arguments

- cov_df:

  Data frame as returned by
  [`.raster_to_covariate_df()`](https://ccarbajal16.github.io/soilsampling/reference/dot-raster_to_covariate_df.md),
  with `x`, `y` as the first two columns.

## Value

A data frame of numeric covariates suitable for
[`clhs::clhs()`](https://rdrr.io/pkg/clhs/man/clhs.html).
