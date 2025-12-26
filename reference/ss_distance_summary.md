# Compute Distance Summary Statistics by Stratum

Calculates distance statistics from cells or samples to their assigned
stratum centroids. Provides mean, standard deviation, minimum, maximum,
and median distances for each stratum.

## Usage

``` r
ss_distance_summary(x, actual_distance = FALSE)
```

## Arguments

- x:

  An object of class `ss_strata` or `ss_samples`

- actual_distance:

  Logical. If `TRUE`, return actual distances (default: `FALSE` returns
  squared distances for consistency with MSSD metric)

## Value

A data frame with one row per stratum containing:

- stratum_id:

  Stratum identifier

- n_cells or n_samples:

  Number of cells (for ss_strata) or samples (for ss_samples)

- mean_dist:

  Mean distance to centroid

- sd_dist:

  Standard deviation of distances

- min_dist:

  Minimum distance to centroid

- max_dist:

  Maximum distance to centroid

- median_dist:

  Median distance to centroid

## Details

For ss_strata objects, the function computes distances from all grid
cells to their assigned stratum centroids. For ss_samples objects, it
computes distances from sample points to centroids.

The function automatically detects coordinate system type (planar vs
lat/lon) and uses appropriate distance calculations:

- Planar coordinates: Euclidean distance

- Geographic coordinates (lat/lon): Haversine distance (great circle)

By default, squared distances are returned for computational efficiency
and consistency with the MSSD (Mean Squared Shortest Distance) metric
used in stratification. Set `actual_distance = TRUE` to get actual
distances.

## Examples

``` r
if (FALSE) { # \dontrun{
library(sf)

# Create study area and stratification
poly <- st_polygon(list(rbind(
  c(0, 0), c(100, 0), c(100, 50), c(0, 50), c(0, 0)
)))
study_area <- st_sf(geometry = st_sfc(poly))
set.seed(42)
strata <- ss_stratify(study_area, n_strata = 20, n_try = 5)

# Distance summary for strata
dist_summary <- ss_distance_summary(strata)
print(dist_summary)

# Distance summary for samples
samples <- ss_coverage(strata)
dist_samples <- ss_distance_summary(samples, actual_distance = TRUE)
print(dist_samples)
} # }
```
