# Euclidean Distance From a Target to Each Candidate

Euclidean Distance From a Target to Each Candidate

## Usage

``` r
.alt_euclidean_distance(target_values, candidate_values, normalized = FALSE)
```

## Arguments

- target_values:

  Named numeric vector of target environmental values.

- candidate_values:

  Matrix or data frame of candidate environmental values, one row per
  candidate, columns matching `target_values`.

- normalized:

  Logical, whether `target_values`/`candidate_values` are already
  normalized. If `FALSE`, range-normalizes internally.

## Value

Numeric vector of Euclidean distances, one per candidate row.
