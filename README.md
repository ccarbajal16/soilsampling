# soilsampling

Soil Sampling Design Methods Including Spatial Coverage, Maxvol Optimal Design, Simple Random, and Stratified Sampling

## Overview

The **soilsampling** package provides methods for designing soil sampling schemes. It implements:

- **Simple Random Sampling**: Uniform random selection of sampling locations
- **Stratified Random Sampling**: Random sampling within compact geographical strata
- **Spatial Coverage Sampling**: Purposive sampling at stratum centroids for optimal coverage
- **Maxvol Optimal Design Sampling**: Feature-based sampling using D-optimal experimental design
- **Composite Sampling**: Sampling from equal-area strata for combined samples
- **Validation Metrics**: Assess stratification quality through coverage efficiency and distance statistics


## Installation

### Install from GitHub (Recommended)

```r
# Using pak (fastest)
pak::pak("ccarbajal16/soilsampling")

# Or using devtools
devtools::install_github("ccarbajal16/soilsampling")
```

### Install from Local Source

```r
# Install from local directory
install.packages(".", repos = NULL, type = "source")

# Or using devtools
devtools::install_local("path/to/soilsampling")
```

### Dependencies

- R (>= 4.1.0)
- sf (>= 1.0-12)
- ggplot2 (>= 3.3.0)

## Quick Start

```r
library(soilsampling)
library(sf)

# Create a study area
poly <- st_polygon(list(rbind(
  c(0, 0), c(100, 0), c(100, 50), c(0, 50), c(0, 0)
)))
study_area <- st_sf(geometry = st_sfc(poly))

# Set seed for reproducibility
set.seed(42)

# Spatial coverage sampling
samples <- ss_coverage(study_area, n_strata = 25, n_try = 5)
ss_plot(samples)

# Get coordinates
coords <- ss_to_data_frame(samples)
head(coords)
```

## Sampling Methods

### Simple Random Sampling

Select locations uniformly at random:

```r
samples <- ss_random(study_area, n = 30)
ss_plot_samples(samples)
```

### Stratified Random Sampling

Create strata first, then sample randomly within each:
```r
# Create 25 compact strata
strata <- ss_stratify(study_area, n_strata = 25, n_try = 5)

# Take 1 random sample per stratum
samples <- ss_stratified(strata, n_per_stratum = 1)
ss_plot(strata, samples = samples)
```

### Spatial Coverage Sampling

Place samples at stratum centroids for optimal spatial coverage:

```r
# Direct approach
samples <- ss_coverage(study_area, n_strata = 25, n_try = 5)

# Or use pre-computed strata
strata <- ss_stratify(study_area, n_strata = 25, n_try = 5)
samples <- ss_coverage(strata)

ss_plot(samples)
```

### Spatial Coverage with Prior Points

Incorporate existing sampling locations:

```r
# Existing sample locations
prior_pts <- st_as_sf(
  data.frame(x = c(25, 75), y = c(25, 25)),
  coords = c("x", "y")
)

# Create stratification around prior points
strata <- ss_stratify(
  study_area, 
  n_strata = 20, 
  prior_points = prior_pts,
  n_try = 5
)

samples <- ss_coverage(strata)
ss_plot(samples)  # Prior points shown with different symbol
```

### Equal-Area Stratification

Create strata of equal size:

```r
samples <- ss_coverage_equal_area(study_area, n_strata = 25, n_try = 5)
```

### Composite Sampling

For combining samples from multiple locations:

```r
# Create 3 composite samples from 20 equal-area strata
samples <- ss_composite(study_area, n_strata = 20, n_composites = 3)
ss_plot(samples)  # Different symbols for each composite
```

### Maxvol Optimal Design Sampling

Feature-based sampling using D-optimal experimental design to maximize diversity:

```r
# Create a grid with terrain features
strata <- ss_stratify(study_area, n_strata = 100, n_try = 5)
cells_sf <- strata$cells

# Add terrain features (in practice, extract from DEM)
coords <- st_coordinates(cells_sf)
cells_sf$elevation <- coords[,2] + rnorm(nrow(coords), 0, 5)
cells_sf$slope <- abs(rnorm(nrow(coords), 5, 2))
cells_sf$twi <- rnorm(nrow(coords), 8, 1.5)

# Select 20 optimal samples based on features
samples <- ss_maxvol(
  cells_sf,
  n = 20,
  features = c("elevation", "slope", "twi"),
  min_dist = 5,        # Minimum distance between samples
  normalize = TRUE,    # Normalize features to equal scales
  add_coords = TRUE    # Include coordinates as features
)

ss_plot_samples(samples)
```

The maxvol algorithm selects locations that maximize the determinant (volume) of the feature submatrix, ensuring maximum diversity in feature space. This is particularly effective when:
- Features explain soil-forming factors
- You need deterministic (non-random) point selection
- You want optimal coverage with few samples

## Assessing Stratification Quality

The package provides validation metrics to assess the quality of your stratification:

### Coverage Efficiency

Evaluate how uniformly cells are distributed across strata:

```r
# Create stratification
strata <- ss_stratify(study_area, n_strata = 25, n_try = 5)

# Assess coverage efficiency
efficiency <- ss_coverage_efficiency(strata)
print(efficiency)
```

**Output metrics:**
- **CV (Coefficient of Variation)**: Lower values indicate more uniform distribution
- **Min/Max Ratio**: Ratio of smallest to largest stratum (1.0 = perfect equality)
- **Gini Coefficient**: Standard inequality measure (0 = perfect equality)
- **Efficiency Score**: Qualitative assessment (Excellent/Good/Fair/Poor)

**Comparing stratification methods:**

```r
# Regular stratification
strata_regular <- ss_stratify(study_area, n_strata = 20, n_try = 5)

# Equal-area stratification
strata_equal <- ss_stratify(study_area, n_strata = 20,
                             equal_area = TRUE, n_try = 5)

# Compare efficiency
eff_regular <- ss_coverage_efficiency(strata_regular)
eff_equal <- ss_coverage_efficiency(strata_equal)

cat("Regular CV:", eff_regular$cv, "\n")
cat("Equal-area CV:", eff_equal$cv, "\n")
# Equal-area should have much lower CV (< 0.05 vs 0.15-0.30)
```

### Distance Statistics

Compute distance from cells or samples to stratum centroids:

```r
# For stratification
dist_summary <- ss_distance_summary(strata)
print(dist_summary)

# For samples (works with coverage or stratified samples)
samples <- ss_coverage(strata)
dist_samples <- ss_distance_summary(samples, actual_distance = TRUE)
print(dist_samples)
```

**Output (per stratum):**
- Mean, SD, min, max, median distances to centroid
- Helps identify dispersed or compact strata
- Default: squared distances (consistent with MSSD metric)

**Use cases:**
- Compare different stratification runs (`n_try` values)
- Verify equal-area maintains compactness
- Identify strata with unusual geometry

## Working with Shapefiles

```r
# Read study area
study_area <- st_read("path/to/study_area.shp")

# Create sampling design
set.seed(42)
samples <- ss_coverage(study_area, n_strata = 50, n_try = 10)

# Export as CSV
coords <- ss_to_data_frame(samples)
write.csv(coords, "sampling_points.csv", row.names = FALSE)

# Export as GeoPackage
samples_sf <- ss_to_sf(samples)
st_write(samples_sf, "sampling_points.gpkg")
```

## Function Reference

### Stratification

| Function | Description |
|----------|-------------|
| `ss_stratify()` | Create compact geographical strata using k-means |

### Sampling

| Function | Description |
|----------|-------------|
| `ss_random()` | Simple random sampling |
| `ss_stratified()` | Stratified random sampling |
| `ss_coverage()` | Spatial coverage sampling (centroids) |
| `ss_coverage_equal_area()` | Coverage sampling with equal-area strata |
| `ss_maxvol()` | Maxvol optimal design sampling (feature-based) |
| `ss_composite()` | Composite sampling |

### Visualization

| Function | Description |
|----------|-------------|
| `ss_plot()` | Plot stratification and/or samples |
| `ss_plot_samples()` | Plot sampling points only |

### Validation

| Function | Description |
|----------|-------------|
| `ss_coverage_efficiency()` | Assess uniformity of cell distribution across strata |
| `ss_distance_summary()` | Compute distance statistics from cells/samples to centroids |

### Utilities

| Function | Description |
|----------|-------------|
| `ss_to_sf()` | Convert to sf object |
| `ss_to_data_frame()` | Convert to data frame with coordinates |
| `ss_summary()` | Get summary statistics |
| `ss_get_samples()` | Extract samples sf object |
| `ss_n_strata()` | Get number of strata |
| `ss_n_samples()` | Get number of samples |
| `ss_area()` | Get stratum areas |
| `ss_relative_area()` | Get relative stratum areas |

## When to Use Each Method

| Method | Best For | Inference Type |
|--------|----------|----------------|
| `ss_coverage()` | Interpolation (kriging) | Model-based |
| `ss_maxvol()` | Feature diversity, D-optimal design | Model-based |
| `ss_stratified()` | Estimating means/totals | Design-based |
| `ss_random()` | Simple design-based analysis | Design-based |
| `ss_composite()` | Reducing laboratory costs | Design-based |

## Algorithm Details

The package uses pure R implementations of k-means algorithms:

1. **Transfer Algorithm** (default): Standard k-means that iteratively transfers cells to nearer cluster centers. Creates compact but potentially unequal-sized strata.

2. **Swop Algorithm** (`equal_area = TRUE`): Modified k-means that swaps cells between clusters to maintain equal sizes while optimizing compactness.

Both algorithms minimize the Mean Squared Shortest Distance (MSSD) between cells and their assigned cluster centroids.

## Tips for Best Results

1. **Use multiple tries**: Set `n_try = 10` or higher to avoid local optima
2. **Set a seed**: Use `set.seed()` for reproducible results
3. **Adjust resolution**: Use `n_cells` parameter to control grid density
4. **Check convergence**: The output includes a `converged` flag
5. **Validate your stratification**: Use `ss_coverage_efficiency()` to ensure uniform strata
6. **Compare alternatives**: Test different `n_strata` values and assess efficiency

## References

- de Gruijter, J.J., Brus, D.J., Bierkens, M.F.P., and Knotters, M. (2006).
  *Sampling for Natural Resource Monitoring*. Springer, Berlin.

- Walvoort, D.J.J., Brus, D.J., and de Gruijter, J.J. (2010). An R package
  for spatial coverage sampling and random sampling from compact geographical
  strata by k-means. *Computers & Geosciences* 36, 1261-1267.
  DOI: [10.1016/j.cageo.2010.04.005](https://doi.org/10.1016/j.cageo.2010.04.005)

- Petrovskaia, N., Korveh, K., and Maas, E. (2021). Optimal soil sampling
  design based on the maxvol algorithm. *Geoderma* 381, 114733.
  DOI: [10.1016/j.geoderma.2020.114733](https://doi.org/10.1016/j.geoderma.2020.114733)

## License

GPL (>= 3)