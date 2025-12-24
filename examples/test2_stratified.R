library(soilsampling)
library(sf)

# Load study area
study_area <- st_read("data/basin.gpkg")
utm_area <- st_read("data/basin_mantaro.gpkg")

## Stratified Random Sampling (Multiple per stratum)

strata_multiple <- ss_stratify(study_area, 
  n_strata = 80, 
  n_try = 2, 
  n_cells = 8000, 
  equal_area = FALSE
)

stratified_samples <- ss_stratified(
  strata_multiple, 
  n_per_stratum = 3,
  seed = 468
)

# Plot both strata and samples
ss_plot(strata_multiple, samples = stratified_samples)

## Stratified Random Sampling with equal area strata

strata_eq <- ss_stratify(
  utm_area, 
  n_strata = 100,
  n_try = 5, 
  n_cells = 8000, 
  equal_area = TRUE
)

stratified_samples_eq <- ss_stratified(
  strata_eq, 
  n_per_stratum = 3,
  seed = 579
)

# Plot both strata and samples
ss_plot(strata_eq, samples = stratified_samples_eq)

# Export samples to csv
coords <- ss_to_data_frame(stratified_samples_eq)
write.csv(coords, "outputs/sampling_points_stratified_eq.csv", row.names = FALSE)

## Assessing stratification efficiency
efficiency_stratified <- ss_coverage_efficiency(strata_multiple)
efficiency_stratified_eq <- ss_coverage_efficiency(strata_eq)

# Compare results
if (efficiency_stratified$cv < efficiency_stratified_eq$cv) {
  print("Regular stratification is more efficient")
} else {
  print("Equal area stratification is more efficient")
}

