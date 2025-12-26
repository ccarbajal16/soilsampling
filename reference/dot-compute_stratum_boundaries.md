# Compute Stratum Boundaries (Internal)

Simple edge detection algorithm to find boundaries between strata.

## Usage

``` r
.compute_stratum_boundaries(plot_df, cell_size)
```

## Arguments

- plot_df:

  Data frame with x, y, stratum_id columns

- cell_size:

  Numeric vector of length 2 with cell size

## Value

A list with horizontal and vertical boundary segments
