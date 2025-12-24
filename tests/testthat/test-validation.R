test_that("ss_coverage_efficiency computes correctly", {
  # Create simple test polygon
  poly <- sf::st_polygon(list(rbind(
    c(0, 0), c(100, 0), c(100, 50), c(0, 50), c(0, 0)
  )))
  study_area <- sf::st_sf(geometry = sf::st_sfc(poly))

  set.seed(42)
  strata <- ss_stratify(study_area, n_strata = 10, n_try = 2)

  efficiency <- ss_coverage_efficiency(strata)

  # Test object structure
  expect_s3_class(efficiency, "ss_coverage_efficiency")
  expect_type(efficiency$cell_counts, "integer")
  expect_type(efficiency$cv, "double")
  expect_type(efficiency$gini, "double")
  expect_type(efficiency$efficiency_score, "character")

  # Test metric ranges
  expect_true(efficiency$cv >= 0)
  expect_true(efficiency$gini >= 0 && efficiency$gini <= 1)
  expect_true(efficiency$min_max_ratio > 0 && efficiency$min_max_ratio <= 1)
  expect_true(efficiency$min_cells <= efficiency$max_cells)
  expect_equal(efficiency$n_strata, 10)
  expect_equal(sum(efficiency$cell_counts), efficiency$total_cells)

  # Test efficiency score is valid
  expect_true(efficiency$efficiency_score %in% c("Excellent", "Good", "Fair", "Poor"))
})


test_that("ss_coverage_efficiency distinguishes equal-area from regular strata", {
  poly <- sf::st_polygon(list(rbind(
    c(0, 0), c(100, 0), c(100, 50), c(0, 50), c(0, 0)
  )))
  study_area <- sf::st_sf(geometry = sf::st_sfc(poly))

  set.seed(42)
  strata_regular <- ss_stratify(study_area, n_strata = 15, n_try = 3)
  strata_equal <- ss_stratify(study_area, n_strata = 15, equal_area = TRUE, n_try = 3)

  eff_regular <- ss_coverage_efficiency(strata_regular)
  eff_equal <- ss_coverage_efficiency(strata_equal)

  # Equal-area should have much lower CV
  expect_true(eff_equal$cv < eff_regular$cv)

  # Equal-area should have higher min/max ratio
  expect_true(eff_equal$min_max_ratio > eff_regular$min_max_ratio)

  # Equal-area should have lower Gini coefficient
  expect_true(eff_equal$gini < eff_regular$gini)

  # Check is_equal_area flag
  expect_false(eff_regular$is_equal_area)
  expect_true(eff_equal$is_equal_area)
})


test_that("ss_coverage_efficiency print method works", {
  poly <- sf::st_polygon(list(rbind(
    c(0, 0), c(100, 0), c(100, 50), c(0, 50), c(0, 0)
  )))
  study_area <- sf::st_sf(geometry = sf::st_sfc(poly))

  set.seed(42)
  strata <- ss_stratify(study_area, n_strata = 10, n_try = 2)
  efficiency <- ss_coverage_efficiency(strata)

  # Should print without error
  expect_output(print(efficiency), "Stratification Coverage Efficiency")
  expect_output(print(efficiency), "Number of strata")
  expect_output(print(efficiency), "Uniformity Metrics")
  expect_output(print(efficiency), "Efficiency Score")
})


test_that("ss_coverage_efficiency handles edge cases", {
  # Single stratum
  poly <- sf::st_polygon(list(rbind(
    c(0, 0), c(50, 0), c(50, 25), c(0, 25), c(0, 0)
  )))
  study_area <- sf::st_sf(geometry = sf::st_sfc(poly))

  set.seed(42)
  strata_single <- ss_stratify(study_area, n_strata = 1, n_try = 1)
  efficiency <- ss_coverage_efficiency(strata_single)

  # Should compute even with single stratum
  expect_s3_class(efficiency, "ss_coverage_efficiency")
  expect_equal(efficiency$n_strata, 1)
  expect_equal(efficiency$min_cells, efficiency$max_cells)
  expect_equal(efficiency$min_max_ratio, 1)
  expect_true(is.na(efficiency$cv) || efficiency$cv == 0)  # SD is 0 for single stratum
})


test_that("ss_coverage_efficiency validates input", {
  # Test with non-ss_strata object
  expect_error(
    ss_coverage_efficiency(list(a = 1)),
    "'x' must be an ss_strata object"
  )
})


test_that("ss_distance_summary works with ss_strata objects", {
  poly <- sf::st_polygon(list(rbind(
    c(0, 0), c(100, 0), c(100, 50), c(0, 50), c(0, 0)
  )))
  study_area <- sf::st_sf(geometry = sf::st_sfc(poly))

  set.seed(42)
  strata <- ss_stratify(study_area, n_strata = 10, n_try = 2)

  dist_summary <- ss_distance_summary(strata)

  # Test structure
  expect_s3_class(dist_summary, "data.frame")
  expect_equal(nrow(dist_summary), 10)  # One row per stratum
  expect_true("stratum_id" %in% names(dist_summary))
  expect_true("n_cells" %in% names(dist_summary))
  expect_true("mean_dist" %in% names(dist_summary))
  expect_true("sd_dist" %in% names(dist_summary))
  expect_true("min_dist" %in% names(dist_summary))
  expect_true("max_dist" %in% names(dist_summary))
  expect_true("median_dist" %in% names(dist_summary))

  # Test values
  expect_true(all(dist_summary$n_cells > 0))
  expect_true(all(dist_summary$mean_dist >= 0))
  expect_true(all(dist_summary$min_dist >= 0))
  expect_true(all(dist_summary$max_dist >= dist_summary$min_dist))
  expect_equal(sum(dist_summary$n_cells), nrow(strata$cells))
})


test_that("ss_distance_summary works with ss_samples objects", {
  poly <- sf::st_polygon(list(rbind(
    c(0, 0), c(100, 0), c(100, 50), c(0, 50), c(0, 0)
  )))
  study_area <- sf::st_sf(geometry = sf::st_sfc(poly))

  set.seed(42)
  strata <- ss_stratify(study_area, n_strata = 10, n_try = 2)

  # Test with coverage samples
  samples_coverage <- ss_coverage(strata)
  dist_summary <- ss_distance_summary(samples_coverage)

  expect_s3_class(dist_summary, "data.frame")
  expect_true("n_samples" %in% names(dist_summary))
  expect_equal(nrow(dist_summary), 10)

  # Test with stratified samples
  samples_strat <- ss_stratified(strata, n_per_stratum = 2)
  dist_summary2 <- ss_distance_summary(samples_strat)

  expect_s3_class(dist_summary2, "data.frame")
  expect_true("n_samples" %in% names(dist_summary2))
})


test_that("ss_distance_summary actual_distance parameter works", {
  poly <- sf::st_polygon(list(rbind(
    c(0, 0), c(100, 0), c(100, 50), c(0, 50), c(0, 0)
  )))
  study_area <- sf::st_sf(geometry = sf::st_sfc(poly))

  set.seed(42)
  strata <- ss_stratify(study_area, n_strata = 10, n_try = 2)

  # Squared distances (default)
  dist_squared <- ss_distance_summary(strata, actual_distance = FALSE)

  # Actual distances
  dist_actual <- ss_distance_summary(strata, actual_distance = TRUE)

  # Actual distances should be sqrt of squared distances
  expect_true(all(dist_actual$mean_dist < dist_squared$mean_dist |
                  abs(dist_actual$mean_dist - dist_squared$mean_dist) < 1e-10))
})


test_that("ss_distance_summary handles lat/lon coordinates", {
  # Create study area with lat/lon coordinates
  poly_latlon <- sf::st_polygon(list(rbind(
    c(-80, 35), c(-79, 35), c(-79, 36), c(-80, 36), c(-80, 35)
  )))
  study_area_latlon <- sf::st_sf(geometry = sf::st_sfc(poly_latlon, crs = 4326))

  set.seed(42)
  strata_latlon <- ss_stratify(study_area_latlon, n_strata = 5, n_try = 2)

  # Should use Haversine distance
  dist_summary <- ss_distance_summary(strata_latlon)

  expect_s3_class(dist_summary, "data.frame")
  expect_equal(nrow(dist_summary), 5)
  expect_true(all(dist_summary$mean_dist >= 0))
})


test_that("ss_distance_summary validates input", {
  # Test with non-ss_strata/ss_samples object
  expect_error(
    ss_distance_summary(list(a = 1)),
    "'x' must be an ss_strata or ss_samples object"
  )

  # Test with ss_samples without strata component
  poly <- sf::st_polygon(list(rbind(
    c(0, 0), c(100, 0), c(100, 50), c(0, 50), c(0, 0)
  )))
  study_area <- sf::st_sf(geometry = sf::st_sfc(poly))

  # Create samples without strata
  samples_random <- ss_random(study_area, n = 20)

  expect_error(
    ss_distance_summary(samples_random),
    "ss_samples object must have a 'strata' component"
  )
})


test_that("ss_distance_summary handles strata with prior points", {
  poly <- sf::st_polygon(list(rbind(
    c(0, 0), c(100, 0), c(100, 50), c(0, 50), c(0, 0)
  )))
  study_area <- sf::st_sf(geometry = sf::st_sfc(poly))

  # Create prior points
  prior_coords <- rbind(c(25, 25), c(75, 25))
  prior_points <- sf::st_as_sf(
    data.frame(x = prior_coords[, 1], y = prior_coords[, 2]),
    coords = c("x", "y")
  )

  set.seed(42)
  strata <- ss_stratify(study_area, n_strata = 10,
                         prior_points = prior_points, n_try = 2)

  dist_summary <- ss_distance_summary(strata)

  # Should work with prior points
  expect_s3_class(dist_summary, "data.frame")
  expect_equal(nrow(dist_summary), 10)
})


test_that("ss_distance_summary computes statistics correctly", {
  poly <- sf::st_polygon(list(rbind(
    c(0, 0), c(100, 0), c(100, 50), c(0, 50), c(0, 0)
  )))
  study_area <- sf::st_sf(geometry = sf::st_sfc(poly))

  set.seed(42)
  strata <- ss_stratify(study_area, n_strata = 5, n_try = 2)

  dist_summary <- ss_distance_summary(strata, actual_distance = TRUE)

  # Check that statistics are ordered correctly
  for (i in seq_len(nrow(dist_summary))) {
    expect_true(dist_summary$min_dist[i] <= dist_summary$median_dist[i])
    expect_true(dist_summary$median_dist[i] <= dist_summary$max_dist[i])
    expect_true(dist_summary$min_dist[i] <= dist_summary$mean_dist[i])
    expect_true(dist_summary$mean_dist[i] <= dist_summary$max_dist[i])

    # SD should be non-negative
    if (!is.na(dist_summary$sd_dist[i])) {
      expect_true(dist_summary$sd_dist[i] >= 0)
    }
  }
})
