# Apply Distance Constraint During Iteration

Zeros out the selection weights for candidate points that are too close
to already selected points.

## Usage

``` r
.apply_distance_constraint(weights, coords, selected, min_dist)
```

## Arguments

- weights:

  Numeric vector of selection weights

- coords:

  Matrix (m x 2) of coordinates

- selected:

  Integer vector of currently selected indices

- min_dist:

  Minimum allowed distance

## Value

Modified weights vector
