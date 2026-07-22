# Comparison Table for a cLHS vs. RF-Optimized Design

Comparison Table for a cLHS vs. RF-Optimized Design

## Usage

``` r
.rf_comparison_table(rf_results)
```

## Arguments

- rf_results:

  List returned by
  [`ss_rf_optimize()`](https://ccarbajal16.github.io/soilsampling/reference/ss_rf_optimize.md).

## Value

A data frame with one row per method (`"cLHS"`, `"RF Optimized"`), and
`MSE`, `Samples`, `Improvement` columns.
