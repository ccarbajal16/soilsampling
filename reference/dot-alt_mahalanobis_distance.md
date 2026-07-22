# Mahalanobis Distance From a Target to Each Candidate

Mahalanobis Distance From a Target to Each Candidate

## Usage

``` r
.alt_mahalanobis_distance(target_values, candidate_values, covariance_matrix)
```

## Arguments

- target_values:

  Named numeric vector of target environmental values.

- candidate_values:

  Matrix or data frame of candidate environmental values, one row per
  candidate, columns matching `target_values`.

- covariance_matrix:

  Covariance matrix used for the distance.

## Value

Numeric vector of Mahalanobis distances, one per candidate row.
