# Gower Distance From a Target to Each Candidate

Gower Distance From a Target to Each Candidate

## Usage

``` r
.alt_gower_distance(target_values, candidate_values, categorical_vars = NULL)
```

## Arguments

- target_values:

  Named vector or single-row data frame of target environmental values.

- candidate_values:

  Matrix or data frame of candidate environmental values, one row per
  candidate.

- categorical_vars:

  Character vector of categorical column names, compared by exact match
  instead of scaled absolute difference.

## Value

Numeric vector of Gower distances (0-1), one per candidate row.
