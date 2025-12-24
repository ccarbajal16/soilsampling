# Tests for soilsampling package

test_that("ss_stratify creates valid strata", {
  skip_if_not_installed("sf")

  # Create test polygon
  poly <- sf::st_polygon(list(rbind(
    c(0, 0), c(100, 0), c(100, 50), c(0, 50), c(0, 0)
  )))
  study_area <- sf::st_sf(geometry = sf::st_sfc(poly))

  # Test basic stratification
  set.seed(42)
  strata <- ss_stratify(study_area, n_strata = 10, n_try = 2)

  expect_s3_class(strata, "ss_strata")
  expect_equal(strata$n_strata, 10)
  expect_true(nrow(strata$cells) > 0)
  expect_equal(nrow(strata$centroids), 10)
  expect_true(all(strata$cells$stratum_id >= 1))
  expect_true(all(strata$cells$stratum_id <= 10))
  expect_true(strata$mssd > 0)
  expect_true(is.logical(strata$converged))
})


test_that("ss_stratify works with equal_area option", {
  skip_if_not_installed("sf")

  poly <- sf::st_polygon(list(rbind(
    c(0, 0), c(100, 0), c(100, 100), c(0, 100), c(0, 0)
  )))
  study_area <- sf::st_sf(geometry = sf::st_sfc(poly))

  set.seed(42)
  strata <- ss_stratify(study_area,
    n_strata = 4, n_try = 2,
    equal_area = TRUE, n_cells = 400
  )

  expect_s3_class(strata, "ss_strata")
  expect_true(strata$equal_area)

  # Check approximately equal sizes

  counts <- table(strata$cells$stratum_id)
  expected_count <- nrow(strata$cells) / 4
  expect_true(all(abs(counts - expected_count) / expected_count < 0.15))
})


test_that("ss_stratify works with prior points", {
  skip_if_not_installed("sf")

  poly <- sf::st_polygon(list(rbind(
    c(0, 0), c(100, 0), c(100, 50), c(0, 50), c(0, 0)
  )))
  study_area <- sf::st_sf(geometry = sf::st_sfc(poly))

  prior_coords <- data.frame(x = c(25, 75), y = c(25, 25))
  prior_pts <- sf::st_as_sf(prior_coords, coords = c("x", "y"))

  set.seed(42)
  strata <- ss_stratify(study_area,
    n_strata = 5,
    prior_points = prior_pts,
    n_try = 2
  )

  expect_s3_class(strata, "ss_strata")
  expect_false(is.null(strata$prior_points))
  expect_equal(nrow(strata$prior_points), 2)
})


test_that("ss_random generates valid samples", {
  skip_if_not_installed("sf")

  poly <- sf::st_polygon(list(rbind(
    c(0, 0), c(100, 0), c(100, 50), c(0, 50), c(0, 0)
  )))
  study_area <- sf::st_sf(geometry = sf::st_sfc(poly))

  samples <- ss_random(study_area, n = 20, seed = 42)

  expect_s3_class(samples, "ss_samples")
  expect_equal(samples$method, "simple_random")
  expect_equal(samples$n_samples, 20)
  expect_s3_class(samples$samples, "sf")
})


test_that("ss_stratified generates valid samples", {
  skip_if_not_installed("sf")

  poly <- sf::st_polygon(list(rbind(
    c(0, 0), c(100, 0), c(100, 50), c(0, 50), c(0, 0)
  )))
  study_area <- sf::st_sf(geometry = sf::st_sfc(poly))

  set.seed(42)
  strata <- ss_stratify(study_area, n_strata = 10, n_try = 2)
  samples <- ss_stratified(strata, n_per_stratum = 2, seed = 42)

  expect_s3_class(samples, "ss_samples")
  expect_equal(samples$method, "stratified_random")
  expect_equal(samples$n_samples, 20) # 10 strata * 2 per stratum
  expect_true("stratum_id" %in% names(samples$samples))
})


test_that("ss_coverage generates valid samples", {
  skip_if_not_installed("sf")

  poly <- sf::st_polygon(list(rbind(
    c(0, 0), c(100, 0), c(100, 50), c(0, 50), c(0, 0)
  )))
  study_area <- sf::st_sf(geometry = sf::st_sfc(poly))

  set.seed(42)
  samples <- ss_coverage(study_area, n_strata = 15, n_try = 2)

  expect_s3_class(samples, "ss_samples")
  expect_equal(samples$method, "coverage")
  expect_equal(samples$n_samples, 15)
  expect_true("stratum_id" %in% names(samples$samples))
})


test_that("ss_coverage works with pre-computed strata", {
  skip_if_not_installed("sf")

  poly <- sf::st_polygon(list(rbind(
    c(0, 0), c(100, 0), c(100, 50), c(0, 50), c(0, 0)
  )))
  study_area <- sf::st_sf(geometry = sf::st_sfc(poly))

  set.seed(42)
  strata <- ss_stratify(study_area, n_strata = 10, n_try = 2)
  samples <- ss_coverage(strata)

  expect_s3_class(samples, "ss_samples")
  expect_equal(samples$n_samples, 10)
})


test_that("ss_coverage_equal_area generates valid samples", {
  skip_if_not_installed("sf")

  poly <- sf::st_polygon(list(rbind(
    c(0, 0), c(100, 0), c(100, 100), c(0, 100), c(0, 0)
  )))
  study_area <- sf::st_sf(geometry = sf::st_sfc(poly))

  set.seed(42)
  samples <- ss_coverage_equal_area(study_area,
    n_strata = 9, n_try = 2,
    n_cells = 400
  )

  expect_s3_class(samples, "ss_samples")
  expect_equal(samples$method, "coverage_equal_area")
  expect_true(samples$strata$equal_area)
})


test_that("ss_composite generates valid samples", {
  skip_if_not_installed("sf")

  poly <- sf::st_polygon(list(rbind(
    c(0, 0), c(100, 0), c(100, 100), c(0, 100), c(0, 0)
  )))
  study_area <- sf::st_sf(geometry = sf::st_sfc(poly))

  set.seed(42)
  samples <- ss_composite(study_area,
    n_strata = 9,
    n_composites = 3,
    n_try = 2, n_cells = 400
  )

  expect_s3_class(samples, "ss_samples")
  expect_equal(samples$method, "composite")
  expect_equal(samples$n_composites, 3)
  expect_true("composite" %in% names(samples$samples))
  expect_equal(nrow(samples$samples), 27) # 9 strata * 3 composites
})


test_that("ss_to_sf extracts correct objects", {
  skip_if_not_installed("sf")

  poly <- sf::st_polygon(list(rbind(
    c(0, 0), c(100, 0), c(100, 50), c(0, 50), c(0, 0)
  )))
  study_area <- sf::st_sf(geometry = sf::st_sfc(poly))

  set.seed(42)
  samples <- ss_coverage(study_area, n_strata = 10, n_try = 1)

  samples_sf <- ss_to_sf(samples, what = "samples")
  expect_s3_class(samples_sf, "sf")
  expect_equal(nrow(samples_sf), 10)

  cells_sf <- ss_to_sf(samples, what = "cells")
  expect_s3_class(cells_sf, "sf")

  centroids_sf <- ss_to_sf(samples, what = "centroids")
  expect_s3_class(centroids_sf, "sf")
  expect_equal(nrow(centroids_sf), 10)
})


test_that("ss_to_data_frame returns correct structure", {
  skip_if_not_installed("sf")

  poly <- sf::st_polygon(list(rbind(
    c(0, 0), c(100, 0), c(100, 50), c(0, 50), c(0, 0)
  )))
  study_area <- sf::st_sf(geometry = sf::st_sfc(poly))

  set.seed(42)
  samples <- ss_coverage(study_area, n_strata = 10, n_try = 1)

  df <- ss_to_data_frame(samples)

  expect_s3_class(df, "data.frame")
  expect_true("X" %in% names(df))
  expect_true("Y" %in% names(df))
  expect_true("sample_id" %in% names(df))
  expect_equal(nrow(df), 10)
})


test_that("ss_n_strata returns correct count", {
  skip_if_not_installed("sf")

  poly <- sf::st_polygon(list(rbind(
    c(0, 0), c(100, 0), c(100, 50), c(0, 50), c(0, 0)
  )))
  study_area <- sf::st_sf(geometry = sf::st_sfc(poly))

  set.seed(42)
  strata <- ss_stratify(study_area, n_strata = 15, n_try = 1)

  expect_equal(ss_n_strata(strata), 15)
})


test_that("ss_n_samples returns correct count", {
  skip_if_not_installed("sf")

  poly <- sf::st_polygon(list(rbind(
    c(0, 0), c(100, 0), c(100, 50), c(0, 50), c(0, 0)
  )))
  study_area <- sf::st_sf(geometry = sf::st_sfc(poly))

  samples <- ss_random(study_area, n = 25, seed = 42)
  expect_equal(ss_n_samples(samples), 25)
})


test_that("ss_relative_area sums to 1", {
  skip_if_not_installed("sf")

  poly <- sf::st_polygon(list(rbind(
    c(0, 0), c(100, 0), c(100, 50), c(0, 50), c(0, 0)
  )))
  study_area <- sf::st_sf(geometry = sf::st_sfc(poly))

  set.seed(42)
  strata <- ss_stratify(study_area, n_strata = 10, n_try = 1)

  rel_area <- ss_relative_area(strata)
  expect_equal(sum(rel_area), 1, tolerance = 1e-10)
  expect_length(rel_area, 10)
})


test_that("ss_plot returns ggplot object", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")

  poly <- sf::st_polygon(list(rbind(
    c(0, 0), c(100, 0), c(100, 50), c(0, 50), c(0, 0)
  )))
  study_area <- sf::st_sf(geometry = sf::st_sfc(poly))

  set.seed(42)
  strata <- ss_stratify(study_area, n_strata = 5, n_try = 1)
  samples <- ss_coverage(strata)

  p1 <- ss_plot(strata)
  expect_s3_class(p1, "ggplot")

  p2 <- ss_plot(strata, samples = samples)
  expect_s3_class(p2, "ggplot")

  p3 <- ss_plot(samples)
  expect_s3_class(p3, "ggplot")
})


test_that("ss_plot_samples returns ggplot object", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")

  poly <- sf::st_polygon(list(rbind(
    c(0, 0), c(100, 0), c(100, 50), c(0, 50), c(0, 0)
  )))
  study_area <- sf::st_sf(geometry = sf::st_sfc(poly))

  samples <- ss_random(study_area, n = 20, seed = 42)

  p <- ss_plot_samples(samples)
  expect_s3_class(p, "ggplot")
})


test_that("ss_summary returns data frame", {
  skip_if_not_installed("sf")

  poly <- sf::st_polygon(list(rbind(
    c(0, 0), c(100, 0), c(100, 50), c(0, 50), c(0, 0)
  )))
  study_area <- sf::st_sf(geometry = sf::st_sfc(poly))

  set.seed(42)
  samples <- ss_coverage(study_area, n_strata = 10, n_try = 1)

  summary_df <- ss_summary(samples)
  expect_s3_class(summary_df, "data.frame")
  expect_true("method" %in% names(summary_df))
  expect_true("n_samples" %in% names(summary_df))
})


test_that("print and summary methods work", {
  skip_if_not_installed("sf")

  poly <- sf::st_polygon(list(rbind(
    c(0, 0), c(100, 0), c(100, 50), c(0, 50), c(0, 0)
  )))
  study_area <- sf::st_sf(geometry = sf::st_sfc(poly))

  set.seed(42)
  strata <- ss_stratify(study_area, n_strata = 5, n_try = 1)
  samples <- ss_coverage(strata)

  # Should not throw errors

  expect_output(print(strata))
  expect_output(print(samples))
  expect_output(summary(strata))
  expect_output(summary(samples))
})


test_that("input validation works correctly", {
  skip_if_not_installed("sf")

  poly <- sf::st_polygon(list(rbind(
    c(0, 0), c(100, 0), c(100, 50), c(0, 50), c(0, 0)
  )))
  study_area <- sf::st_sf(geometry = sf::st_sfc(poly))

  # Invalid n_strata

  expect_error(ss_stratify(study_area, n_strata = 0))
  expect_error(ss_stratify(study_area, n_strata = -5))

  # Invalid input object

  expect_error(ss_stratify("not an sf object", n_strata = 10))

  # equal_area with prior points
  prior_pts <- sf::st_as_sf(
    data.frame(x = 50, y = 25),
    coords = c("x", "y")
  )
  expect_error(ss_stratify(study_area,
    n_strata = 10,
    prior_points = prior_pts,
    equal_area = TRUE
  ))
})


test_that("k-means algorithms converge", {
  # Test internal functions
  set.seed(42)
  coords <- as.matrix(expand.grid(x = 1:10, y = 1:10))

  # Transfer algorithm
  result_transfer <- soilsampling:::.kmeans_transfer(coords, n_strata = 4, max_iter = 100)
  expect_true(is.list(result_transfer))
  expect_true("cluster_id" %in% names(result_transfer))
  expect_true("centroids" %in% names(result_transfer))
  expect_true(all(result_transfer$cluster_id >= 0))
  expect_true(all(result_transfer$cluster_id < 4))

  # Swop algorithm
  result_swop <- soilsampling:::.kmeans_swop(coords, n_strata = 4, max_iter = 100)
  expect_true(is.list(result_swop))

  # Swop should produce equal-sized clusters
  counts <- table(result_swop$cluster_id)
  expect_equal(length(unique(counts)), 1)
  expect_equal(as.numeric(counts[1]), 25) # 100 / 4
})
