# Alternative Site Selection

## Introduction

Planned sampling locations sometimes turn out to be inaccessible in the
field — private land, dangerous terrain, or logistical constraints.
Rather than dropping those locations entirely,
[`ss_alt_sites()`](https://ccarbajal16.github.io/soilsampling/reference/ss_alt_sites.md)
finds **environmentally similar replacement sites** elsewhere in the
study area, so the overall design keeps covering the same range of
conditions.

The approach:

1.  Generate a pool of **candidate sites** across the study area
    ([`ss_alt_candidates()`](https://ccarbajal16.github.io/soilsampling/reference/ss_alt_candidates.md))
2.  Optionally **exclude candidates too close** to the inaccessible
    sites, so replacements are spatially distinct
    ([`ss_alt_filter_buffer()`](https://ccarbajal16.github.io/soilsampling/reference/ss_alt_filter_buffer.md))
3.  Compute **environmental similarity** between each inaccessible site
    and every candidate
    ([`ss_alt_similarity()`](https://ccarbajal16.github.io/soilsampling/reference/ss_alt_similarity.md))
4.  **Rank and select** the most similar candidates as alternatives
    ([`ss_alt_rank()`](https://ccarbajal16.github.io/soilsampling/reference/ss_alt_rank.md))

## Theoretical Background

### Similarity Metrics

[`ss_alt_similarity()`](https://ccarbajal16.github.io/soilsampling/reference/ss_alt_similarity.md)
supports three distance metrics between a target site and a candidate,
converted to a `[0, 1]` similarity score (`1` = identical):

- **Mahalanobis** (default): accounts for correlation between covariates
  via the population covariance matrix. Most robust when covariates are
  correlated, but needs enough candidates to estimate a stable
  covariance matrix.
- **Euclidean**: simple geometric distance after range-normalization.
  Faster, ignores covariate correlation.
- **Gower**: handles a mix of continuous and categorical covariates via
  per-variable normalized differences.

### Distance Buffering

[`ss_alt_filter_buffer()`](https://ccarbajal16.github.io/soilsampling/reference/ss_alt_filter_buffer.md)
removes any candidate within `min_distance` of *any* inaccessible site,
before similarity is computed. This avoids picking “alternatives” that
are really just adjacent to the original problem location (e.g. the same
inaccessible area).

## Basic Usage

``` r

library(soilsampling)
library(terra)

set.seed(11)

nr <- 40
nc <- 40

r <- rast(nrows = nr, ncols = nc, xmin = 0, xmax = 1000, ymin = 0, ymax = 1000, nlyrs = 3)
names(r) <- c("dem", "slope", "ndvi")

xy <- xyFromCell(r, seq_len(ncell(r)))
values(r)[, "dem"] <- 100 + 20 * sin(xy[, 1] / 150) + rnorm(ncell(r), 0, 3)
#> Warning: [readValues] raster has no values
values(r)[, "slope"] <- abs(cos(xy[, 2] / 120) * 8 + rnorm(ncell(r), 0, 1))
values(r)[, "ndvi"] <- pmin(pmax(0.5 + 0.3 * cos(xy[, 1] / 200 + xy[, 2] / 250) +
  rnorm(ncell(r), 0, 0.05), -1), 1)

# A few "inaccessible" sites needing alternatives
inaccessible <- data.frame(
  x = c(200, 500, 800),
  y = c(200, 500, 800)
)
inaccessible
#>     x   y
#> 1 200 200
#> 2 500 500
#> 3 800 800
```

### Running the End-to-End Workflow

``` r

result <- ss_alt_sites(
  r,
  inaccessible,
  method = "mahalanobis",
  n_alternatives = 3,
  min_distance_buffer = 100,
  seed = 123
)

result$alternatives[, c("site_id", "target_site_id", "similarity_score", "similarity_rank")]
#>         site_id target_site_id similarity_score similarity_rank
#> 1  candidate_46       target_1        0.9392170               1
#> 2 candidate_395       target_1        0.9289847               2
#> 3 candidate_858       target_1        0.9289847               3
#> 4  candidate_11       target_2        0.9575523               1
#> 5 candidate_953       target_2        0.9575523               2
#> 6 candidate_956       target_2        0.9575523               3
#> 7 candidate_119       target_3        0.9479020               1
#> 8 candidate_185       target_3        0.9479020               2
#> 9 candidate_477       target_3        0.9257395               3
```

Each row of `result$alternatives` is one candidate selected for one
target site, ranked by similarity (`similarity_rank = 1` is the best
match).

## Function Walkthrough

### Generating Candidates

``` r

candidates <- ss_alt_candidates(r, n_candidates = 300, method = "random", seed = 1)
head(candidates)
#>       site_id        x        y      type       dem     slope      ndvi
#> 1 candidate_1 265.5087 814.2518 candidate 116.55597 6.7796984 0.4365338
#> 2 candidate_2 372.1239 928.7772 candidate 114.80089 1.0778848 0.6631923
#> 3 candidate_3 572.8534 147.4810 candidate  87.55092 3.9554009 0.1469143
#> 4 candidate_4 908.2078 749.8217 candidate  91.51938 8.1660975 0.6208423
#> 5 candidate_5 201.6819 975.6573 candidate 119.62574 0.7472459 0.5826633
#> 6 candidate_6 898.3897 974.7925 candidate  93.30237 0.4474288 0.3751245
```

`method = "systematic"` generates a regular grid instead of random
points — useful when you want even coverage of the candidate pool
itself.

### Excluding Candidates Near the Inaccessible Sites

``` r

filtered <- ss_alt_filter_buffer(candidates, inaccessible, min_distance = 100)
cat("candidates before:", nrow(candidates), " after buffer:", nrow(filtered), "\n")
#> candidates before: 300  after buffer: 271
```

### Computing Similarity for One Target Site

``` r

env_vars <- c("dem", "slope", "ndvi")
target_values <- unlist(terra::extract(r, inaccessible[1, c("x", "y")], ID = FALSE))

scores <- ss_alt_similarity(target_values, filtered[env_vars], method = "mahalanobis")
summary(scores)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>  0.3048  0.4426  0.5072  0.5346  0.6010  0.9345
```

### Ranking Alternatives

``` r

top3 <- ss_alt_rank(scores, filtered, n_select = 3, target_site_id = "site_1")
top3[, c("site_id", "target_site_id", "similarity_score", "similarity_rank")]
#>           site_id target_site_id similarity_score similarity_rank
#> 90   candidate_90         site_1        0.9344683               1
#> 232 candidate_232         site_1        0.9344683               2
#> 193 candidate_193         site_1        0.9311112               3
```

### Standardizing Raw Site Data

Field data often arrives as a plain CSV with arbitrary column names.
[`ss_alt_standardize_sites()`](https://ccarbajal16.github.io/soilsampling/reference/ss_alt_standardize_sites.md)
normalizes it to the `site_id`/`x`/`y`/`type` layout the other
`ss_alt_*` functions expect:

``` r

raw_sites <- data.frame(lon = c(200, 500), lat = c(200, 500), Point_ID = c("A1", "A2"))
ss_alt_standardize_sites(raw_sites, x_col = "lon", y_col = "lat", id_col = "Point_ID")
#>   site_id   x   y         type
#> 1      A1 200 200 inaccessible
#> 2      A2 500 500 inaccessible
```

## Choosing Parameters

- **`method`**: use `"mahalanobis"` when covariates are correlated and
  the candidate pool is reasonably large; fall back to `"euclidean"` for
  speed on very large problems, or `"gower"` when covariates mix
  continuous and categorical variables.
- **`n_candidates`**: larger pools give better matches but cost more to
  generate and compare.
  [`ss_alt_sites()`](https://ccarbajal16.github.io/soilsampling/reference/ss_alt_sites.md)
  defaults to `max(n_alternatives * 50, 1000)`.
- **`min_distance_buffer`**: set to a distance that meaningfully
  separates alternatives from the original inaccessible areas (e.g. the
  size of the inaccessible zone itself). Leave `NULL` if spatial
  proximity to the original site isn’t a concern.
- **Small candidate pools**: with very few candidates relative to the
  number of covariates, the Mahalanobis covariance matrix can become
  poorly conditioned (all similarity scores end up nearly identical).
  Increase `n_candidates` if you see this.

## Practical Workflow

``` r

library(soilsampling)
library(terra)

predictors <- rast("data/predictors.tif")
inaccessible <- read.csv("data/inaccessible_sites.csv")  # needs x, y columns

result <- ss_alt_sites(
  predictors,
  inaccessible,
  method = "mahalanobis",
  n_alternatives = 3,
  min_distance_buffer = 300,
  seed = 123,
  output_dir = "outputs"
)

result$alternatives

# outputs/ now contains:
#   similarity_analysis_sites.csv, similarity_analysis_inaccessible_sites.csv
```

## When to Use Alternative Site Selection

✅ **Use it when:**

- Some planned sites turn out to be inaccessible after the design was
  finalized
- You want replacements that preserve the environmental coverage of the
  original design
- You need documented, reproducible justification for the substitution
  (similarity scores and ranks)

❌ **Don’t rely on it when:**

- The inaccessible sites are a small fraction of the design and simple
  nearest-neighbor substitution is sufficient
- No environmental covariates are available to compare candidates
  against

## References

- Malone, B.P., Minasny, B., and Brungard, C. (2019). Some methods to
  improve the utility of conditioned Latin hypercube sampling. *PeerJ*
  7, e6451. DOI:
  [10.7717/peerj.6451](https://doi.org/10.7717/peerj.6451)
