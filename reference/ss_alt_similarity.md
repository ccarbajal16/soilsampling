# Environmental Similarity Between a Target Site and Candidate Sites

Computes a 0-1 similarity score between a target (e.g. an inaccessible
sampling site) and a set of candidate sites, based on multivariate
distance in environmental covariate space.

## Usage

``` r
ss_alt_similarity(
  target_values,
  candidate_values,
  categorical_vars = NULL,
  weights = NULL,
  method = c("mahalanobis", "euclidean", "gower"),
  normalize = TRUE,
  missing_method = "pairwise"
)
```

## Arguments

- target_values:

  Environmental values at the target site: a named numeric vector, or a
  single-row data frame with the same columns as `candidate_values`.

- candidate_values:

  Environmental values at candidate sites: a matrix or data frame, one
  row per candidate.

- categorical_vars:

  Character vector of categorical column names. Only used by
  `method = "gower"`, and excluded from the other methods.

- weights:

  Optional named numeric vector of variable importance weights; unnamed
  weights must match `length(target_values)`.

- method:

  Distance metric: `"mahalanobis"` (default, accounts for covariance
  among variables), `"euclidean"`, or `"gower"` (handles mixed
  continuous/categorical data).

- normalize:

  Logical, whether to z-score normalize numeric variables before
  distance calculation. Default `TRUE`.

- missing_method:

  How to handle missing values: `"pairwise"` (default, handled
  per-comparison), `"listwise"`, `"mean"`, or `"median"`.

## Value

Numeric vector of similarity scores in `[0, 1]`, one per candidate row
(`1` = perfect match).

## See also

[`ss_alt_candidates()`](https://ccarbajal16.github.io/soilsampling/reference/ss_alt_candidates.md),
[`ss_alt_rank()`](https://ccarbajal16.github.io/soilsampling/reference/ss_alt_rank.md),
[`ss_alt_sites()`](https://ccarbajal16.github.io/soilsampling/reference/ss_alt_sites.md)

## Examples

``` r
target <- c(dem = 4000, slope = 0.3, ndvi = 0.4)
candidates <- data.frame(
  dem = c(3990, 4200, 3500),
  slope = c(0.31, 0.5, 0.1),
  ndvi = c(0.41, 0.2, 0.6)
)
ss_alt_similarity(target, candidates)
#> Warning: Singular covariance matrix detected, added regularization
#> [1] 0.3679722 0.3678800 0.3678794
```
