## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## ----message=FALSE------------------------------------------------------------
library(soilsampling)
library(sf)

# Create a study area
poly <- st_polygon(list(rbind(
  c(0, 0), c(100, 0), c(100, 50), c(0, 50), c(0, 0)
)))
study_area <- st_sf(geometry = st_sfc(poly))

# Set seed for reproducibility
set.seed(123)

## -----------------------------------------------------------------------------
# Create 25 coverage samples in one step
samples <- ss_coverage(study_area, n_strata = 25, n_try = 5)

print(samples)

## -----------------------------------------------------------------------------
# Step 1: Create stratification
strata <- ss_stratify(study_area, n_strata = 25, n_try = 5)

print(strata)

# Step 2: Extract centroids as sampling points
samples <- ss_coverage(strata)

# Plot both strata and samples
ss_plot(strata, samples = samples)

## -----------------------------------------------------------------------------
# Standard spatial coverage
samples_transfer <- ss_coverage(study_area, n_strata = 20, n_try = 5)

# Check stratum sizes
strata_transfer <- ss_stratify(study_area, n_strata = 20, n_try = 5)
areas <- ss_relative_area(strata_transfer)
summary(areas)

## -----------------------------------------------------------------------------
# Equal-area coverage sampling
samples_equal <- ss_coverage_equal_area(study_area, n_strata = 20, n_try = 5)

# Check stratum sizes
strata_equal <- ss_stratify(
  study_area,
  n_strata = 20,
  n_try = 5,
  equal_area = TRUE
)
areas_equal <- ss_relative_area(strata_equal)
summary(areas_equal)

## -----------------------------------------------------------------------------
# Suppose we have 2 existing sampling locations
prior_pts <- st_as_sf(
  data.frame(x = c(25, 75), y = c(25, 25)),
  coords = c("x", "y")
)

# Create stratification with prior points
strata_prior <- ss_stratify(
  study_area,
  n_strata = 20,
  prior_points = prior_pts,
  n_try = 5
)

# Get coverage samples
samples_prior <- ss_coverage(strata_prior)

# Plot with prior points highlighted
ss_plot(samples_prior)

## -----------------------------------------------------------------------------
# Compare different numbers of strata
samples_10 <- ss_coverage(study_area, n_strata = 10, n_try = 5)
samples_50 <- ss_coverage(study_area, n_strata = 50, n_try = 5)

par(mfrow = c(1, 2))
plot(st_geometry(study_area), main = "10 strata")
plot(st_geometry(samples_10$samples), add = TRUE, pch = 19, col = "red")

plot(st_geometry(study_area), main = "50 strata")
plot(st_geometry(samples_50$samples), add = TRUE, pch = 19, col = "red")
par(mfrow = c(1, 1))

## -----------------------------------------------------------------------------
# Compare convergence with different n_try values
set.seed(456)
result_1 <- ss_stratify(study_area, n_strata = 25, n_try = 1)
result_5 <- ss_stratify(study_area, n_strata = 25, n_try = 5)
result_20 <- ss_stratify(study_area, n_strata = 25, n_try = 20)

cat("n_try = 1:  MSSD =", format(result_1$mssd, digits = 6), "\n")
cat("n_try = 5:  MSSD =", format(result_5$mssd, digits = 6), "\n")
cat("n_try = 20: MSSD =", format(result_20$mssd, digits = 6), "\n")

## -----------------------------------------------------------------------------
# Coarse grid (fast)
strata_coarse <- ss_stratify(
  study_area,
  n_strata = 25,
  n_cells = 500,
  n_try = 3
)

# Fine grid (slower but more precise)
strata_fine <- ss_stratify(
  study_area,
  n_strata = 25,
  n_cells = 5000,
  n_try = 3
)

cat("Coarse grid:", nrow(strata_coarse$cells), "cells\n")
cat("Fine grid:  ", nrow(strata_fine$cells), "cells\n")

## -----------------------------------------------------------------------------
# Create strata
strata <- ss_stratify(study_area, n_strata = 25, n_try = 5)

# Take 1 random sample per stratum
samples_strat <- ss_stratified(strata, n_per_stratum = 1)

# Or take multiple samples per stratum
samples_strat_3 <- ss_stratified(strata, n_per_stratum = 3)

ss_plot(strata, samples = samples_strat_3)

## -----------------------------------------------------------------------------
# Create 3 composite samples from 20 equal-area strata
samples_comp <- ss_composite(study_area, n_strata = 20, n_composites = 3)

# Each composite includes one sample from each stratum
ss_plot(samples_comp)

## ----eval=FALSE---------------------------------------------------------------
# # Study area with geographic CRS
# study_area_geo <- st_transform(study_area, crs = 4326)
# 
# # The algorithm uses haversine distance automatically
# samples_geo <- ss_coverage(study_area_geo, n_strata = 25, n_try = 5)
# 
# # Note: equal_area is not supported for lat/lon
# # This will produce an error:
# # samples_error <- ss_coverage_equal_area(study_area_geo, n_strata = 25)

## ----eval=FALSE---------------------------------------------------------------
# # 1. Load study area
# study_area <- st_read("my_field.shp")
# 
# # 2. Design sampling scheme
# set.seed(42)  # For reproducibility
# samples <- ss_coverage(study_area, n_strata = 50, n_try = 10)
# 
# # 3. Examine results
# print(samples)
# summary(samples)
# 
# # 4. Export for field work
# coords <- ss_to_data_frame(samples)
# write.csv(coords, "field_sampling_locations.csv", row.names = FALSE)
# 
# # 5. Export as shapefile for GPS
# st_write(ss_to_sf(samples), "sampling_points.gpkg")

## -----------------------------------------------------------------------------
set.seed(789)

# Spatial coverage
samp_coverage <- ss_coverage(study_area, n_strata = 25, n_try = 5)

# Stratified random
strata <- ss_stratify(study_area, n_strata = 25, n_try = 5)
samp_stratified <- ss_stratified(strata, n_per_stratum = 1)

# Simple random
samp_random <- ss_random(study_area, n = 25)

# Compare spatial distribution
par(mfrow = c(1, 3))

plot(st_geometry(study_area), main = "Coverage")
plot(st_geometry(samp_coverage$samples), add = TRUE, pch = 19, col = "red")

plot(st_geometry(study_area), main = "Stratified Random")
plot(st_geometry(samp_stratified$samples), add = TRUE, pch = 19, col = "blue")

plot(st_geometry(study_area), main = "Simple Random")
plot(st_geometry(samp_random$samples), add = TRUE, pch = 19, col = "green")

par(mfrow = c(1, 1))

