# Rank and Select the Top Similar Alternative Sites

Orders candidate sites by similarity score (highest first) and returns
the top `n_select`, annotated with rank and target site information.

## Usage

``` r
ss_alt_rank(
  similarity_scores,
  candidate_sites,
  n_select = 5,
  target_site_id = NULL
)
```

## Arguments

- similarity_scores:

  Numeric vector of similarity scores, as returned by
  [`ss_alt_similarity()`](https://ccarbajal16.github.io/soilsampling/reference/ss_alt_similarity.md),
  one per row of `candidate_sites`.

- candidate_sites:

  Data frame of candidate sites, as returned by
  [`ss_alt_candidates()`](https://ccarbajal16.github.io/soilsampling/reference/ss_alt_candidates.md)
  or
  [`ss_alt_filter_buffer()`](https://ccarbajal16.github.io/soilsampling/reference/ss_alt_filter_buffer.md).

- n_select:

  Integer, number of top alternatives to select. If fewer valid
  (non-`NA`, finite) scores are available, all of them are returned with
  a warning.

- target_site_id:

  Optional character, ID of the target site these alternatives are for;
  stored in a `target_site_id` column. If `NULL` (default), that column
  is filled with `NA`.

## Value

A data frame of the top `n_select` candidate sites, with added
`target_site_id`, `similarity_score`, `similarity_rank`, and
`selection_method` columns. Empty data frame if no valid scores.

## See also

[`ss_alt_similarity()`](https://ccarbajal16.github.io/soilsampling/reference/ss_alt_similarity.md),
[`ss_alt_sites()`](https://ccarbajal16.github.io/soilsampling/reference/ss_alt_sites.md)

## Examples

``` r
candidates <- data.frame(site_id = paste0("c", 1:5), x = 1:5, y = 1:5)
scores <- c(0.9, 0.4, 0.95, 0.2, 0.7)
ss_alt_rank(scores, candidates, n_select = 2, target_site_id = "site_001")
#>   site_id x y target_site_id similarity_score similarity_rank selection_method
#> 3      c3 3 3       site_001             0.95               1       similarity
#> 1      c1 1 1       site_001             0.90               2       similarity
```
