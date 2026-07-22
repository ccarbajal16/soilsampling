# Convert a Raster Stack to a Covariate Data Frame

Convert a Raster Stack to a Covariate Data Frame

## Usage

``` r
.raster_to_covariate_df(covariates)
```

## Arguments

- covariates:

  A `SpatRaster` stack of environmental covariates.

## Value

A data frame with `x`, `y` coordinate columns followed by one column per
covariate layer, with incomplete rows removed.
