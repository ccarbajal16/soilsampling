# Maxvol Optimal Design Sampling

Generates sampling points using the maxvol algorithm, which selects
locations by maximizing the determinant (volume) of a feature submatrix.
This is a D-optimal design approach that selects points with the most
diverse feature characteristics.

## Usage

``` r
ss_maxvol(
  x,
  n,
  features = NULL,
  coords = NULL,
  min_dist = NULL,
  normalize = TRUE,
  add_coords = TRUE,
  tol = 1.1,
  max_iters = 100,
  verbose = FALSE,
  seed = NULL
)
```

## Arguments

- x:

  An `sf` object representing the study area with feature attributes, OR
  a numeric matrix of features (rows = locations, columns = features).

- n:

  Integer, the number of sampling points to select.

- features:

  Character vector of feature names to use from `x` if it's an sf
  object. If NULL, all numeric attributes are used.

- coords:

  Matrix (n_locations x 2) of coordinates. Required if `x` is a matrix.
  If `x` is sf, coordinates are extracted automatically.

- min_dist:

  Numeric, minimum distance between sampling points. If NULL, no
  distance constraint is applied.

- normalize:

  Logical, whether to normalize features before applying maxvol (default
  TRUE). Recommended when features have different scales.

- add_coords:

  Logical, whether to add coordinates as features (default TRUE). This
  helps balance feature space and geographic space.

- tol:

  Numeric, convergence tolerance for maxvol algorithm (default 1.1).

- max_iters:

  Integer, maximum iterations for maxvol (default 100).

- verbose:

  Logical, whether to print progress messages (default FALSE).

- seed:

  Optional integer seed for reproducibility.

## Value

An object of class `ss_samples` containing:

- samples:

  An `sf` object with the sampling points.

- method:

  Character, "maxvol".

- n_samples:

  Integer, number of samples.

- features_used:

  Character vector of feature names used.

- converged:

  Logical, whether maxvol converged.

- iterations:

  Integer, number of maxvol iterations.

- crs:

  The coordinate reference system.

## Details

The maxvol algorithm selects sampling locations by finding a submatrix
of the feature matrix with approximately maximum determinant (volume).
Geometrically, this maximizes the volume of the parallelepiped spanned
by the selected feature vectors, ensuring that sampled locations have
maximal diversity in feature space.

The algorithm is based on D-optimal experimental design and is
particularly effective when:

- Features explain the main soil-forming factors

- You want deterministic (non-random) point selection

- You need optimal coverage with few samples

**Feature Selection:** Terrain features typically used include:

- Elevation

- Slope

- Aspect

- Topographic Wetness Index (TWI)

- Closed depressions

- Flow accumulation

**Distance Constraint:** The `min_dist` parameter prevents spatial
clustering. Points closer than this distance will not be selected
together. The value should be chosen based on:

- Study area size

- Terrain ruggedness (more rugged = smaller min_dist)

- Typical size of soil mapping units

## References

Petrovskaia, A., Ryzhakov, G., & Oseledets, I. (2021). Optimal soil
sampling design based on the maxvol algorithm. Geoderma, 383, 114733.
[doi:10.1016/j.geoderma.2020.114733](https://doi.org/10.1016/j.geoderma.2020.114733)

Goreinov, S. A., Oseledets, I. V., Savostyanov, D. V., Tyrtyshnikov, E.
E., & Zamarashkin, N. L. (2010). How to find a good submatrix. In Matrix
Methods: Theory, Algorithms And Applications (pp. 247-256). World
Scientific.

## See also

[`ss_coverage()`](https://ccarbajal16.github.io/soilsampling/reference/ss_coverage.md),
[`ss_stratified()`](https://ccarbajal16.github.io/soilsampling/reference/ss_stratified.md),
[`ss_random()`](https://ccarbajal16.github.io/soilsampling/reference/ss_random.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(sf)

# Create a study area with feature attributes
poly <- st_polygon(list(rbind(c(0, 0), c(100, 0), c(100, 50), c(0, 50), c(0, 0))))
study_area <- st_sf(geometry = st_sfc(poly))

# In practice, you would load terrain features from a DEM
# For this example, we'll use stratification cells with computed features
strata <- ss_stratify(study_area, n_strata = 100, n_try = 3)

# Add terrain-like features to cells
cells_sf <- strata$cells
coords <- st_coordinates(cells_sf)
cells_sf$elevation <- coords[,2] + rnorm(nrow(coords), 0, 5)
cells_sf$slope <- abs(rnorm(nrow(coords), 5, 2))

# Select 20 sampling points using maxvol
samples <- ss_maxvol(
  cells_sf,
  n = 20,
  features = c("elevation", "slope"),
  min_dist = 5,
  normalize = TRUE,
  add_coords = TRUE
)

# Plot results
ss_plot_samples(samples)
} # }
```
