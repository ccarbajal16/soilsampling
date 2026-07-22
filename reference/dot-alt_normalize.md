# Normalize Environmental Data

Normalize Environmental Data

## Usage

``` r
.alt_normalize(data, method = "zscore", categorical_vars = NULL)
```

## Arguments

- data:

  Data frame of environmental variables.

- method:

  One of `"zscore"`, `"minmax"`, `"robust"`.

- categorical_vars:

  Character vector of columns to leave untouched.

## Value

A data frame with numeric columns normalized.
