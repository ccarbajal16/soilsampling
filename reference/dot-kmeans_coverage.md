# Run K-means with Multiple Tries

Runs the k-means algorithm multiple times and returns the best result.

## Usage

``` r
.kmeans_coverage(
  cell_coords,
  n_strata,
  prior_coords = NULL,
  max_iter = 1000L,
  n_try = 1L,
  equal_area = FALSE,
  is_latlon = FALSE,
  verbose = FALSE
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

  Maximum number of iterations per try

- n_try:

  Number of random initializations to try

- equal_area:

  Logical, whether to use swop algorithm

- is_latlon:

  Logical, whether coordinates are lat/lon

- verbose:

  Logical, whether to print progress

## Value

A list with the best clustering result
