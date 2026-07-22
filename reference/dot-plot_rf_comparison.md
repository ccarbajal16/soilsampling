# Side-by-Side Plots of cLHS vs. RF-Optimized Designs

Side-by-Side Plots of cLHS vs. RF-Optimized Designs

## Usage

``` r
.plot_rf_comparison(
  covariates,
  clhs_samples,
  rf_samples,
  covariate_name = NULL
)
```

## Arguments

- covariates:

  A `SpatRaster` stack of environmental covariates.

- clhs_samples:

  Data frame of cLHS baseline sample locations.

- rf_samples:

  Data frame of RF-optimized sample locations.

- covariate_name:

  Character, name of the layer to show as background. Defaults to
  `"dem"` if present, otherwise the first layer name.

## Value

A list with `clhs_plot` and `rf_plot`, both `ggplot` objects.
