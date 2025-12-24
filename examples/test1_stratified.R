library(soilsampling)
library(sf)

# Study area from geopackage
# Load study area
study_area <- st_read("data/basin.gpkg")

# Set seed for reproducibility
set.seed(123)

## Option 1: One-step coverage sampling
# Create 50 coverage samples in one step
samples <- ss_coverage(study_area, n_strata = 50, n_try = 5)

# Plot study area and samples
ss_plot(samples)

# Export samples to csv
coords <- ss_to_data_frame(samples)
write.csv(coords, "outputs/sampling_points.csv", row.names = FALSE)

## Option 2: Two-step coverage sampling
# Step 1: Create stratification
strata <- ss_stratify(study_area, 
  n_strata = 80, 
  n_try = 2, 
  n_cells = 10000, 
  equal_area = FALSE
)

print(strata)

# Step 2: Extract centroids as sampling points
samples <- ss_coverage(strata)

# Plot both strata and samples
ss_plot(strata, samples = samples)

## Using prior points

# Load study area
utm_area <- st_read("data/basin_mantaro.gpkg")

# Upload prior points
prior_points <- st_read("data/prior_points.gpkg")

# Step 1: Create stratification
strata <- ss_stratify(
  utm_area, 
  n_strata = 100,
  prior_points = prior_points, 
  n_try = 5, 
  n_cells = 8000, 
  equal_area = FALSE
)

# Step 2: Extract centroids as sampling points
samples_new <- ss_coverage(strata)

# Plot both strata and samples
ss_plot(strata, samples = samples_new)

# Export samples to csv
coords <- ss_to_data_frame(samples_new)
write.csv(coords, "outputs/sampling_points_new.csv", row.names = FALSE)

## Using equal area option

# Step 1: Create stratification
strata_eq <- ss_stratify(
  utm_area, 
  n_strata = 100,
  n_try = 5, 
  n_cells = 10000, 
  equal_area = TRUE
)

# Step 2: Extract centroids as sampling points
samples_eq <- ss_coverage(strata_eq)

# Plot both strata and samples
ss_plot(strata_eq, samples = samples_eq)

# Export samples to csv
coords <- ss_to_data_frame(samples_eq)
write.csv(coords, "outputs/sampling_points_eq.csv", row.names = FALSE)

## Summary distance statistics

# Distance summary for strata
dist_summary_regular <- ss_distance_summary(strata)
print(dist_summary_regular)

## Assesing stratification efficiency

eff_regular <- ss_coverage_efficiency(strata)
eff_equal <- ss_coverage_efficiency(strata_eq)

# Print results
eff_regular
eff_equal

# Compare results
if (eff_regular$cv < eff_equal$cv) {
  print("Regular stratification is more efficient")
} else {
  print("Equal area stratification is more efficient")
}

