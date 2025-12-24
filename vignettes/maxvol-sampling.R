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

# Create a study area with terrain features
poly <- st_polygon(list(rbind(
  c(0, 0), c(100, 0), c(100, 50), c(0, 50), c(0, 0)
)))
study_area <- st_sf(geometry = st_sfc(poly))

# In practice, you would compute features from a DEM
# For this example, we'll create a grid with synthetic features
set.seed(42)

# Create fine grid to represent potential sampling locations
grid <- st_make_grid(study_area, cellsize = c(2.5, 2.5), what = "centers")
grid_sf <- st_sf(geometry = grid)
grid_sf <- grid_sf[st_intersects(grid_sf, study_area, sparse = FALSE)[,1], ]

# Add synthetic terrain features
coords <- st_coordinates(grid_sf)
grid_sf$elevation <- coords[,2] + 10 * sin(coords[,1] / 20) + rnorm(nrow(coords), 0, 2)
grid_sf$slope <- abs(cos(coords[,1] / 15) * 5 + rnorm(nrow(coords), 0, 1))
grid_sf$twi <- 10 - grid_sf$slope + rnorm(nrow(coords), 0, 0.5)
grid_sf$aspect <- (atan2(coords[,2] - 25, coords[,1] - 50) + pi) / (2 * pi) * 360

## -----------------------------------------------------------------------------
# Select 20 sampling points using maxvol
samples_maxvol <- ss_maxvol(
  grid_sf,
  n = 20,
  features = c("elevation", "slope", "twi", "aspect"),
  normalize = TRUE,
  add_coords = TRUE
)

print(samples_maxvol)

# Plot results
ss_plot_samples(samples_maxvol)

## -----------------------------------------------------------------------------
# Select points with minimum 10-unit spacing
samples_constrained <- ss_maxvol(
  grid_sf,
  n = 20,
  features = c("elevation", "slope", "twi"),
  min_dist = 10,
  normalize = TRUE,
  add_coords = TRUE
)

ss_plot_samples(samples_constrained)

## -----------------------------------------------------------------------------
# Terrain features commonly used in pedometrics
features_topo <- c(
  "elevation",      # Height above reference
  "slope",          # Rate of elevation change
  "aspect",         # Direction of slope
  "twi"             # Topographic Wetness Index
)

# Other useful features (if computed from DEM):
# - Curvature (plan, profile)
# - Flow accumulation
# - Closed depressions
# - Topographic Position Index (TPI)
# - Terrain Ruggedness Index (TRI)

## -----------------------------------------------------------------------------
# Without coordinates: purely feature-based
samples_no_coords <- ss_maxvol(
  grid_sf,
  n = 20,
  features = c("elevation", "slope"),
  add_coords = FALSE,
  normalize = TRUE
)

# With coordinates: balances features and geography
samples_with_coords <- ss_maxvol(
  grid_sf,
  n = 20,
  features = c("elevation", "slope"),
  add_coords = TRUE,  # Default
  normalize = TRUE
)

par(mfrow = c(1, 2))
plot(st_geometry(study_area), main = "Without Coordinates")
plot(st_geometry(samples_no_coords$samples), add = TRUE, pch = 19, col = "red")

plot(st_geometry(study_area), main = "With Coordinates")
plot(st_geometry(samples_with_coords$samples), add = TRUE, pch = 19, col = "blue")
par(mfrow = c(1, 1))

## -----------------------------------------------------------------------------
# Check feature scales
summary(grid_sf[, c("elevation", "slope", "twi")])

# Without normalization: features with larger variance dominate
samples_raw <- ss_maxvol(
  grid_sf,
  n = 15,
  features = c("elevation", "slope", "twi"),
  normalize = FALSE,
  add_coords = FALSE
)

# With normalization: equal weight to all features
samples_norm <- ss_maxvol(
  grid_sf,
  n = 15,
  features = c("elevation", "slope", "twi"),
  normalize = TRUE,  # Recommended
  add_coords = FALSE
)

## -----------------------------------------------------------------------------
# No distance constraint
samples_no_dist <- ss_maxvol(
  grid_sf,
  n = 20,
  features = c("elevation", "slope", "twi"),
  min_dist = NULL
)

# Small distance constraint
samples_small_dist <- ss_maxvol(
  grid_sf,
  n = 20,
  features = c("elevation", "slope", "twi"),
  min_dist = 5
)

# Large distance constraint
samples_large_dist <- ss_maxvol(
  grid_sf,
  n = 20,
  features = c("elevation", "slope", "twi"),
  min_dist = 15
)

par(mfrow = c(1, 3))
plot(st_geometry(study_area), main = "No constraint")
plot(st_geometry(samples_no_dist$samples), add = TRUE, pch = 19, col = "red", cex = 0.8)

plot(st_geometry(study_area), main = "min_dist = 5")
plot(st_geometry(samples_small_dist$samples), add = TRUE, pch = 19, col = "blue", cex = 0.8)

plot(st_geometry(study_area), main = "min_dist = 15")
plot(st_geometry(samples_large_dist$samples), add = TRUE, pch = 19, col = "green", cex = 0.8)
par(mfrow = c(1, 1))

## -----------------------------------------------------------------------------
# Rule of thumb: min_dist = (study area extent) / (2 * sqrt(n_samples))
bbox <- st_bbox(study_area)
extent <- sqrt((bbox$xmax - bbox$xmin) * (bbox$ymax - bbox$ymin))
n_samples <- 20
suggested_min_dist <- extent / (2 * sqrt(n_samples))

cat("Suggested min_dist:", round(suggested_min_dist, 1), "\n")

## -----------------------------------------------------------------------------
set.seed(123)

# Maxvol sampling
samp_maxvol <- ss_maxvol(
  grid_sf,
  n = 25,
  features = c("elevation", "slope", "twi"),
  min_dist = 8,
  normalize = TRUE
)

# Spatial coverage (k-means based)
samp_coverage <- ss_coverage(study_area, n_strata = 25, n_try = 5)

# Simple random
samp_random <- ss_random(study_area, n = 25)

par(mfrow = c(1, 3))

plot(st_geometry(study_area), main = "Maxvol (Feature-based)")
plot(st_geometry(samp_maxvol$samples), add = TRUE, pch = 19, col = "red")

plot(st_geometry(study_area), main = "Coverage (Geography-based)")
plot(st_geometry(samp_coverage$samples), add = TRUE, pch = 19, col = "blue")

plot(st_geometry(study_area), main = "Random")
plot(st_geometry(samp_random$samples), add = TRUE, pch = 19, col = "green")

par(mfrow = c(1, 1))

## ----eval=FALSE---------------------------------------------------------------
# library(terra)  # For raster processing
# 
# # Load DEM
# dem <- rast("path/to/dem.tif")
# 
# # Compute terrain features
# slope <- terrain(dem, "slope")
# aspect <- terrain(dem, "aspect")
# tpi <- terrain(dem, "TPI")
# tri <- terrain(dem, "TRI")
# 
# # Compute TWI (requires flow direction)
# # ... (depends on your workflow)
# 
# # Stack features
# features_stack <- c(dem, slope, aspect, tpi, tri)
# 
# # Convert to points
# feature_points <- as.points(features_stack)
# feature_sf <- st_as_sf(feature_points)

## ----eval=FALSE---------------------------------------------------------------
# # Select sampling points
# samples <- ss_maxvol(
#   feature_sf,
#   n = 50,
#   features = c("elevation", "slope", "aspect", "tpi", "tri"),
#   min_dist = 100,  # meters, adjust for your study
#   normalize = TRUE,
#   add_coords = TRUE,
#   verbose = TRUE
# )
# 
# # Check convergence
# if (!samples$converged) {
#   warning("Maxvol did not converge. Consider increasing max_iters.")
# }

## ----eval=FALSE---------------------------------------------------------------
# # Get coordinates
# coords <- ss_to_data_frame(samples)
# 
# # Export for field work
# write.csv(coords, "maxvol_sampling_points.csv", row.names = FALSE)
# 
# # Export as GeoPackage
# st_write(ss_to_sf(samples), "maxvol_points.gpkg")

## -----------------------------------------------------------------------------
# Create custom feature matrix
coords_mat <- st_coordinates(grid_sf)
n_loc <- nrow(coords_mat)

custom_features <- matrix(
  c(
    grid_sf$elevation,
    grid_sf$slope,
    grid_sf$twi
  ),
  ncol = 3
)
colnames(custom_features) <- c("elev", "slope", "twi")

# Use with maxvol
samples_custom <- ss_maxvol(
  custom_features,
  n = 20,
  coords = coords_mat,
  normalize = TRUE
)

## -----------------------------------------------------------------------------
# Use maxvol to densify an existing sample
# Suppose we have prior samples
prior_samples <- ss_random(study_area, n = 5)

# Use stratification to avoid prior samples
# Then use maxvol for additional points
# (This would require custom code to exclude prior locations)

## -----------------------------------------------------------------------------
# For large datasets, reduce candidate locations
# Option 1: Coarser grid
grid_coarse <- st_make_grid(study_area, cellsize = c(5, 5), what = "centers")

# Option 2: Pre-filter using simpler criteria
# (e.g., remove unsuitable areas)

# Option 3: Sample from candidate pool
candidate_sample <- grid_sf[sample(nrow(grid_sf), 500), ]

