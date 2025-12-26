# Create Spatial Strata for Soil Sampling

Partitions a study area into compact geographical strata using k-means
clustering. This is the foundation for spatial coverage sampling.

## Usage

``` r
ss_stratify(
  x,
  n_strata,
  prior_points = NULL,
  max_iter = 1000L,
  n_try = 1L,
  n_cells = 2500L,
  cell_size = NULL,
  equal_area = FALSE,
  verbose = FALSE
)
```

## Arguments

- x:

  An `sf` object representing the study area. Can be a polygon (POLYGON
  or MULTIPOLYGON) or point geometry (POINT or MULTIPOINT).

- n_strata:

  Integer, the number of strata to create.

- prior_points:

  Optional `sf` object with point geometry containing locations that
  should be used as fixed cluster centers.

- max_iter:

  Integer, maximum number of iterations for the k-means algorithm
  (default 1000).

- n_try:

  Integer, number of random initial configurations to try (default 1).
  Higher values reduce risk of suboptimal local minima.

- n_cells:

  Integer, approximate number of grid cells to use when discretizing a
  polygon (default 2500).

- cell_size:

  Numeric vector of length 1 or 2, cell size for discretization. If
  provided, `n_cells` is ignored.

- equal_area:

  Logical, if `TRUE`, use the swopping algorithm to create strata of
  equal area (default `FALSE`). Cannot be used with prior points.

- verbose:

  Logical, whether to print progress messages (default `FALSE`).

## Value

An object of class `ss_strata` containing:

- cells:

  An `sf` object with the grid cells and their stratum assignments.

- centroids:

  An `sf` object with the stratum centroids.

- cell_size:

  Numeric vector with cell dimensions.

- n_strata:

  Integer, number of strata.

- mssd:

  Numeric, mean squared shortest distance (objective function).

- converged:

  Logical, whether the algorithm converged.

- prior_points:

  The prior points if provided, otherwise NULL.

- equal_area:

  Logical, whether equal-area stratification was used.

- crs:

  The coordinate reference system.

## Details

The function partitions the study area into compact spatial strata by
minimizing the mean squared shortest distance (MSSD) between grid cells
and their assigned cluster centroids.

Two algorithms are available:

- **Transfer algorithm** (default): Standard k-means, creates compact
  strata of potentially unequal sizes.

- **Swop algorithm** (`equal_area = TRUE`): Creates compact strata of
  equal size by swapping cells between clusters.

## See also

[`ss_coverage()`](https://ccarbajal16.github.io/soilsampling/reference/ss_coverage.md),
[`ss_plot()`](https://ccarbajal16.github.io/soilsampling/reference/ss_plot.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(sf)

# Create a study area
poly <- st_polygon(list(rbind(c(0, 0), c(100, 0), c(100, 50), c(0, 50), c(0, 0))))
study_area <- st_sf(geometry = st_sfc(poly))

# Create 20 strata
strata <- ss_stratify(study_area, n_strata = 20, n_try = 5)
plot(strata)
} # }
```
