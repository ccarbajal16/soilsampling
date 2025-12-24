# Test maxvol sampling

test_that("ss_maxvol works with sf object with features", {
  skip_if_not_installed("sf")

  library(sf)

  # Create a simple study area
  poly <- st_polygon(list(rbind(
    c(0, 0), c(100, 0), c(100, 50), c(0, 50), c(0, 0)
  )))
  study_area <- st_sf(geometry = st_sfc(poly))

  # Create grid with features
  grid <- st_make_grid(study_area, cellsize = c(5, 5), what = "centers")
  grid_sf <- st_sf(geometry = grid)
  grid_sf <- grid_sf[st_intersects(grid_sf, study_area, sparse = FALSE)[, 1], ]

  # Add features
  coords <- st_coordinates(grid_sf)
  grid_sf$elevation <- coords[, 2] + rnorm(nrow(coords), 0, 2)
  grid_sf$slope <- abs(rnorm(nrow(coords), 5, 2))

  set.seed(123)
  samples <- ss_maxvol(
    grid_sf,
    n = 15,
    features = c("elevation", "slope"),
    normalize = TRUE,
    add_coords = TRUE
  )

  # Check structure
  expect_s3_class(samples, "ss_samples")
  expect_equal(samples$method, "maxvol")
  expect_equal(samples$n_samples, 15)
  expect_equal(nrow(samples$samples), 15)

  # Check that samples is an sf object
  expect_s3_class(samples$samples, "sf")

  # Check that sample_id exists
  expect_true("sample_id" %in% names(samples$samples))
})


test_that("ss_maxvol works with feature matrix", {
  # Create custom feature matrix
  n_loc <- 100
  feature_mat <- matrix(
    c(
      rnorm(n_loc, 100, 10),  # elevation
      abs(rnorm(n_loc, 5, 2)), # slope
      rnorm(n_loc, 8, 1.5)     # TWI
    ),
    ncol = 3
  )
  colnames(feature_mat) <- c("elevation", "slope", "twi")

  coords_mat <- cbind(
    x = runif(n_loc, 0, 100),
    y = runif(n_loc, 0, 50)
  )

  set.seed(456)
  samples <- ss_maxvol(
    feature_mat,
    n = 20,
    coords = coords_mat,
    normalize = TRUE
  )

  # Check structure
  expect_s3_class(samples, "ss_samples")
  expect_equal(samples$method, "maxvol")
  expect_equal(samples$n_samples, 20)
  expect_equal(nrow(samples$samples), 20)
})


test_that("ss_maxvol respects minimum distance constraint", {
  library(sf)

  # Create random point grid with features (not regular grid)
  set.seed(888)
  n_grid <- 100
  coords_mat <- cbind(
    x = runif(n_grid, 0, 100),
    y = runif(n_grid, 0, 50)
  )

  # Create diverse features not directly correlated with coordinates
  feature_mat <- matrix(
    c(
      rnorm(n_grid, 50, 20),           # Independent feature 1
      abs(rnorm(n_grid, 10, 5)),       # Independent feature 2
      rnorm(n_grid, 100, 30),          # Independent feature 3
      runif(n_grid, 0, 15)             # Independent feature 4
    ),
    ncol = 4
  )

  set.seed(789)
  samples <- ss_maxvol(
    feature_mat,
    n = 15,
    coords = coords_mat,
    min_dist = 10,
    normalize = TRUE,
    add_coords = FALSE  # Don't add coords to avoid correlation issues
  )

  # Check that all pairwise distances are >= min_dist
  sample_coords <- st_coordinates(samples$samples)
  n_samples <- nrow(sample_coords)

  min_observed_dist <- Inf
  for (i in 1:(n_samples - 1)) {
    for (j in (i + 1):n_samples) {
      dist_ij <- sqrt(
        (sample_coords[i, 1] - sample_coords[j, 1])^2 +
          (sample_coords[i, 2] - sample_coords[j, 2])^2
      )
      min_observed_dist <- min(min_observed_dist, dist_ij)
    }
  }

  expect_gte(min_observed_dist, 10 - 1e-6)  # Small tolerance for numerical errors
})


test_that("ss_maxvol normalization works correctly", {
  # Create features with very different scales
  feature_mat <- matrix(
    c(
      rnorm(100, 1000, 100),  # Large scale
      rnorm(100, 0.5, 0.1)    # Small scale
    ),
    ncol = 2
  )

  coords_mat <- cbind(
    x = runif(100, 0, 100),
    y = runif(100, 0, 50)
  )

  set.seed(111)

  # Without normalization, large scale feature dominates
  samples_raw <- ss_maxvol(
    feature_mat,
    n = 10,
    coords = coords_mat,
    normalize = FALSE,
    add_coords = FALSE
  )

  # With normalization, both features contribute equally
  samples_norm <- ss_maxvol(
    feature_mat,
    n = 10,
    coords = coords_mat,
    normalize = TRUE,
    add_coords = FALSE
  )

  # Both should produce valid results
  expect_equal(nrow(samples_raw$samples), 10)
  expect_equal(nrow(samples_norm$samples), 10)

  # Both should have valid sample_ids
  expect_true(all(samples_raw$samples$sample_id >= 1))
  expect_true(all(samples_raw$samples$sample_id <= 100))
  expect_true(all(samples_norm$samples$sample_id >= 1))
  expect_true(all(samples_norm$samples$sample_id <= 100))

  # No duplicate selections
  expect_equal(length(unique(samples_raw$samples$sample_id)), 10)
  expect_equal(length(unique(samples_norm$samples$sample_id)), 10)
})


test_that("ss_maxvol handles edge cases", {
  # Create minimal feature matrix with more diversity
  set.seed(999)
  feature_mat <- matrix(
    c(runif(10, 0, 100), runif(10, 0, 50), runif(10, 5, 15)),
    ncol = 3
  )

  coords_mat <- cbind(
    x = 1:10,
    y = 1:10
  )

  # n equals number of features (minimum allowed)
  samples <- ss_maxvol(
    feature_mat,
    n = 3,
    coords = coords_mat,
    normalize = TRUE,
    add_coords = FALSE  # Don't add coords to test n == n_features edge case
  )

  expect_equal(nrow(samples$samples), 3)

  # n equals number of locations (maximum allowed)
  samples_all <- ss_maxvol(
    feature_mat,
    n = 10,
    coords = coords_mat,
    normalize = TRUE,
    add_coords = FALSE  # Keep consistent with above
  )

  expect_equal(nrow(samples_all$samples), 10)
})


test_that("ss_maxvol produces deterministic results with same seed", {
  set.seed(555)
  feature_mat <- matrix(rnorm(300), ncol = 3)
  coords_mat <- cbind(x = runif(100, 0, 100), y = runif(100, 0, 50))

  samples1 <- ss_maxvol(
    feature_mat,
    n = 15,
    coords = coords_mat,
    seed = 42
  )

  samples2 <- ss_maxvol(
    feature_mat,
    n = 15,
    coords = coords_mat,
    seed = 42
  )

  # Same seed should produce identical results
  expect_identical(
    st_coordinates(samples1$samples),
    st_coordinates(samples2$samples)
  )
})


test_that("ss_maxvol validates inputs correctly", {
  feature_mat <- matrix(rnorm(100), ncol = 2)
  coords_mat <- cbind(x = 1:50, y = 1:50)

  # n < number of features should error
  expect_error(
    ss_maxvol(feature_mat, n = 1, coords = coords_mat),
    "number of features"
  )

  # n > number of locations should error
  expect_error(
    ss_maxvol(feature_mat, n = 100, coords = coords_mat),
    "cannot exceed number of locations"
  )

  # Missing coords when x is matrix should error
  expect_error(
    ss_maxvol(feature_mat, n = 10),
    "must be provided"
  )
})


test_that("ss_maxvol convergence reporting works", {
  library(sf)

  poly <- st_polygon(list(rbind(
    c(0, 0), c(10, 0), c(10, 10), c(0, 10), c(0, 0)
  )))
  study_area <- st_sf(geometry = st_sfc(poly))

  grid <- st_make_grid(study_area, cellsize = c(1, 1), what = "centers")
  grid_sf <- st_sf(geometry = grid)
  grid_sf <- grid_sf[st_intersects(grid_sf, study_area, sparse = FALSE)[, 1], ]

  coords <- st_coordinates(grid_sf)
  set.seed(777)
  grid_sf$feat1 <- coords[, 1] + rnorm(nrow(coords), 0, 1)
  grid_sf$feat2 <- coords[, 2] + rnorm(nrow(coords), 0, 1)
  grid_sf$feat3 <- abs(rnorm(nrow(coords), 5, 2))

  samples <- ss_maxvol(
    grid_sf,
    n = 5,
    features = c("feat1", "feat2", "feat3"),
    max_iters = 100
  )

  # Check that convergence info is returned
  expect_type(samples$converged, "logical")
  expect_type(samples$iterations, "integer")
  expect_gte(samples$iterations, 0)
  expect_lte(samples$iterations, 100)
})
