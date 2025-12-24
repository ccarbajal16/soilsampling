## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## ----eval=FALSE---------------------------------------------------------------
# # Install from local source
# install.packages("soilsampling", repos = NULL, type = "source")
# 
# # Or using devtools
# devtools::install_local("path/to/soilsampling")

## -----------------------------------------------------------------------------
library(soilsampling)
library(sf)

# Set seed for reproducibility
set.seed(42)

## -----------------------------------------------------------------------------
# Create a polygon
poly <- st_polygon(list(rbind(
  c(0, 0), c(100, 0), c(100, 50), c(0, 50), c(0, 0)
)))

# Create sf object
study_area <- st_sf(geometry = st_sfc(poly))

## -----------------------------------------------------------------------------
# Create 25 compact strata
strata <- ss_stratify(study_area, n_strata = 25, n_try = 5)

# View structure
print(strata)

## -----------------------------------------------------------------------------
# Create 20 equal-area strata
strata_equal <- ss_stratify(
  study_area,
  n_strata = 20,
  equal_area = TRUE,
  n_try = 5
)

## -----------------------------------------------------------------------------
# Existing sample locations
prior_pts <- st_as_sf(
  data.frame(x = c(25, 75), y = c(25, 25)),
  coords = c("x", "y")
)

# Create stratification around prior points
strata_prior <- ss_stratify(
  study_area,
  n_strata = 20,
  prior_points = prior_pts,
  n_try = 5
)

## -----------------------------------------------------------------------------
# Fine-tune stratification
strata_custom <- ss_stratify(
  study_area,
  n_strata = 30,
  n_cells = 2500,      # Higher resolution grid
  n_try = 10,          # More attempts to find optimal
  equal_area = FALSE,
  verbose = TRUE
)

## -----------------------------------------------------------------------------
# Select 30 random points
samples_random <- ss_random(study_area, n = 30)
print(samples_random)

## -----------------------------------------------------------------------------
# Reproducible random sampling
samples_random_rep <- ss_random(study_area, n = 25, seed = 123)

## -----------------------------------------------------------------------------
# Create strata first
strata <- ss_stratify(study_area, n_strata = 25, n_try = 5)

# Sample 1 point per stratum
samples_strat <- ss_stratified(strata, n_per_stratum = 1)
print(samples_strat)

## -----------------------------------------------------------------------------
# Sample 2 points per stratum
samples_strat2 <- ss_stratified(strata, n_per_stratum = 2, seed = 456)

## -----------------------------------------------------------------------------
# Different random realization
samples_strat3 <- ss_stratified(strata, n_per_stratum = 1, seed = 789)

## -----------------------------------------------------------------------------
# Use pre-computed strata
strata <- ss_stratify(study_area, n_strata = 25, n_try = 5)
samples_cov <- ss_coverage(strata)
print(samples_cov)

## -----------------------------------------------------------------------------
# All-in-one: stratify and sample
samples_cov_direct <- ss_coverage(
  study_area,
  n_strata = 20,
  n_try = 5
)

## -----------------------------------------------------------------------------
# Prior points marked differently
prior_pts <- st_as_sf(
  data.frame(x = c(50), y = c(25)),
  coords = c("x", "y")
)

strata_prior <- ss_stratify(
  study_area,
  n_strata = 24,
  prior_points = prior_pts,
  n_try = 5
)

samples_prior <- ss_coverage(strata_prior)

## -----------------------------------------------------------------------------
# Create 25 equal-area strata and sample
samples_eq <- ss_coverage_equal_area(
  study_area,
  n_strata = 25,
  n_try = 5
)
print(samples_eq)

## -----------------------------------------------------------------------------
# Fine-tuned equal-area sampling
samples_eq_custom <- ss_coverage_equal_area(
  study_area,
  n_strata = 30,
  n_cells = 3000,
  n_try = 10
)

## -----------------------------------------------------------------------------
# Create grid with features
strata <- ss_stratify(study_area, n_strata = 100, n_try = 3)
cells_sf <- strata$cells

# Add terrain-like features
coords <- st_coordinates(cells_sf)
cells_sf$elevation <- coords[,2] + rnorm(nrow(coords), 0, 5)
cells_sf$slope <- abs(rnorm(nrow(coords), 5, 2))
cells_sf$twi <- rnorm(nrow(coords), 8, 1.5)

# Select optimal samples based on features
samples_maxvol <- ss_maxvol(
  cells_sf,
  n = 20,
  features = c("elevation", "slope", "twi"),
  normalize = TRUE,
  add_coords = TRUE
)
print(samples_maxvol)

## -----------------------------------------------------------------------------
# Custom feature matrix
n_loc <- 100
feature_mat <- matrix(
  c(
    rnorm(n_loc, 100, 10),   # elevation
    abs(rnorm(n_loc, 5, 2)),  # slope
    rnorm(n_loc, 8, 1.5)      # TWI
  ),
  ncol = 3
)
colnames(feature_mat) <- c("elevation", "slope", "twi")

coords_mat <- cbind(
  x = runif(n_loc, 0, 100),
  y = runif(n_loc, 0, 50)
)

samples_maxvol_mat <- ss_maxvol(
  feature_mat,
  n = 15,
  coords = coords_mat,
  normalize = TRUE,
  seed = 789
)

## -----------------------------------------------------------------------------
# Prevent spatial clustering
samples_maxvol_dist <- ss_maxvol(
  cells_sf,
  n = 15,
  features = c("elevation", "slope", "twi"),
  min_dist = 8,        # Minimum 8 units between samples
  normalize = TRUE,
  add_coords = TRUE
)

## -----------------------------------------------------------------------------
# Fine-tune maxvol algorithm
samples_maxvol_adv <- ss_maxvol(
  cells_sf,
  n = 25,
  features = c("elevation", "slope"),
  min_dist = 5,
  normalize = TRUE,
  add_coords = FALSE,   # Don't include coordinates
  tol = 1.2,            # Convergence tolerance
  max_iters = 200,      # Maximum iterations
  verbose = TRUE,
  seed = 999
)

## -----------------------------------------------------------------------------
# Create 3 composite samples from 20 strata
samples_comp <- ss_composite(
  study_area,
  n_strata = 20,
  n_composites = 3
)
print(samples_comp)

