# K-means Swop Algorithm (Pure R)

Implements the k-means swop algorithm for equal-area strata.

## Usage

``` r
.kmeans_swop(cell_coords, n_strata, max_iter = 1000L, is_latlon = FALSE)
```

## Arguments

- cell_coords:

  Matrix of cell center coordinates (n x 2)

- n_strata:

  Number of strata (clusters) to create

- max_iter:

  Maximum number of iterations

- is_latlon:

  Logical, whether coordinates are lat/lon

## Value

A list with clustering results
