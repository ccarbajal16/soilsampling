# Select cLHS Sample Row Indices

Select cLHS Sample Row Indices

## Usage

``` r
.select_clhs_indices(cov_df, n_samples, seed = NULL)
```

## Arguments

- cov_df:

  Data frame as returned by
  [`.raster_to_covariate_df()`](https://ccarbajal16.github.io/soilsampling/reference/dot-raster_to_covariate_df.md).

- n_samples:

  Integer, number of samples to select.

- seed:

  Optional integer seed for reproducibility.

## Value

Integer vector of row indices into `cov_df`.
