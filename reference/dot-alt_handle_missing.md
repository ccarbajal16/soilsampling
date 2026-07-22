# Handle Missing Values Before Distance Calculation

Handle Missing Values Before Distance Calculation

## Usage

``` r
.alt_handle_missing(data, method = "pairwise", categorical_vars = NULL)
```

## Arguments

- data:

  Data frame with potential missing values.

- method:

  One of `"pairwise"` (no-op, handled downstream), `"listwise"` (drop
  incomplete rows), `"mean"`, or `"median"`.

- categorical_vars:

  Character vector of categorical column names, left untouched by
  numeric imputation.

## Value

A data frame with missing values handled per `method`.
