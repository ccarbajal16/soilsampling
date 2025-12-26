# K-means Transfer Algorithm (Pure R)

Implements the k-means transfer algorithm for spatial coverage sampling.

## Usage

``` r
.kmeans_transfer(
  cell_coords,
  n_strata,
  prior_coords = NULL,
  max_iter = 1000L,
  is_latlon = FALSE
)
```

## Arguments

- cell_coords:

  Matrix of cell center coordinates (n x 2)

- n_strata:

  Number of strata (clusters) to create

- prior_coords:

  Optional matrix of prior point coordinates

- max_iter:

  Maximum number of iterations

- is_latlon:

  Logical, whether coordinates are lat/lon

## Value

A list with clustering results
