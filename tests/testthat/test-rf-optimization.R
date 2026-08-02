# Builds a small covariate stack with genuine spatial structure, so the
# Random Forest objective has a learnable signal rather than pure noise.
make_test_covariates <- function(nrow = 20, ncol = 20, seed = 42, constant_layer = FALSE) {
  set.seed(seed)

  r <- terra::rast(
    nrows = nrow, ncols = ncol,
    xmin = 0, xmax = 100, ymin = 0, ymax = 100,
    nlyrs = 4
  )

  n_cells <- terra::ncell(r)
  coords <- terra::xyFromCell(r, seq_len(n_cells))

  dem <- 500 + coords[, 2] * 2 + stats::rnorm(n_cells, 0, 5)
  slope <- abs(stats::rnorm(n_cells, 8, 3))
  ndvi <- 0.2 + (dem - min(dem)) / (max(dem) - min(dem)) * 0.5 + stats::rnorm(n_cells, 0, 0.02)
  twi <- if (constant_layer) rep(1, n_cells) else stats::rnorm(n_cells, 8, 1.5)

  terra::values(r) <- cbind(dem, slope, ndvi, twi)
  names(r) <- c("dem", "slope", "ndvi", "twi")
  r
}


test_that(".raster_to_covariate_df returns coordinates followed by layers", {
  r <- make_test_covariates(nrow = 10, ncol = 10)

  cov_df <- .raster_to_covariate_df(r)

  expect_s3_class(cov_df, "data.frame")
  expect_equal(names(cov_df)[1:2], c("x", "y"))
  expect_equal(names(cov_df)[-(1:2)], c("dem", "slope", "ndvi", "twi"))
  expect_equal(nrow(cov_df), 100)
  expect_false(anyNA(cov_df))
})


test_that(".raster_to_covariate_df drops cells with NA values", {
  r <- make_test_covariates(nrow = 10, ncol = 10)
  vals <- terra::values(r)
  vals[1:10, 1] <- NA
  terra::values(r) <- vals

  cov_df <- .raster_to_covariate_df(r)

  expect_equal(nrow(cov_df), 90)
  expect_false(anyNA(cov_df))
})


test_that(".build_clhs_data drops coordinates and zero-variance covariates", {
  r <- make_test_covariates(nrow = 10, ncol = 10, constant_layer = TRUE)
  cov_df <- .raster_to_covariate_df(r)

  clhs_data <- .build_clhs_data(cov_df)

  expect_false(any(c("x", "y") %in% names(clhs_data)))
  # 'twi' is constant in this fixture and cannot be used by cLHS.
  expect_false("twi" %in% names(clhs_data))
  expect_setequal(names(clhs_data), c("dem", "slope", "ndvi"))
})


test_that(".select_clhs_indices errors when no covariate has variance", {
  cov_df <- data.frame(x = 1:10, y = 1:10, flat = rep(2, 10))

  expect_error(
    .select_clhs_indices(cov_df, n_samples = 3),
    "No valid covariates available for cLHS"
  )
})


test_that("ss_clhs_sample returns the requested number of sample locations", {
  skip_on_cran()

  r <- make_test_covariates(nrow = 12, ncol = 12)

  samples <- ss_clhs_sample(r, n_samples = 15, seed = 123)

  expect_s3_class(samples, "data.frame")
  expect_equal(nrow(samples), 15)
  expect_equal(names(samples)[1:2], c("x", "y"))
  expect_true(all(c("dem", "slope", "ndvi", "twi") %in% names(samples)))
})


test_that("ss_clhs_sample is reproducible for a fixed seed", {
  skip_on_cran()

  r <- make_test_covariates(nrow = 12, ncol = 12)

  first <- ss_clhs_sample(r, n_samples = 10, seed = 7)
  second <- ss_clhs_sample(r, n_samples = 10, seed = 7)

  expect_equal(first, second)
})


test_that("ss_rf_mse returns NA for designs that are too small to cross-validate", {
  one_row <- data.frame(x = 1, y = 1, dem = 500, slope = 3, ndvi = 0.4)
  expect_true(is.na(ss_rf_mse(one_row)))

  single_covariate <- data.frame(x = 1:10, y = 1:10, dem = stats::rnorm(10))
  expect_true(is.na(ss_rf_mse(single_covariate)))
})


test_that("ss_rf_mse returns a non-negative error for a usable design", {
  skip_on_cran()

  set.seed(3)
  design <- data.frame(
    x = stats::runif(40, 0, 100),
    y = stats::runif(40, 0, 100),
    dem = stats::rnorm(40, 600, 50),
    slope = stats::rnorm(40, 8, 2),
    ndvi = stats::rnorm(40, 0.4, 0.1),
    twi = stats::rnorm(40, 8, 1.5)
  )

  mse <- ss_rf_mse(design, n_folds = 3)

  expect_type(mse, "double")
  expect_length(mse, 1)
  expect_false(is.na(mse))
  expect_gte(mse, 0)
})


test_that("ss_rf_mse honours an explicit target_var", {
  skip_on_cran()

  set.seed(21)
  n <- 40
  design <- data.frame(
    x = stats::runif(n, 0, 100),
    y = stats::runif(n, 0, 100),
    dem = stats::rnorm(n, 600, 50),
    slope = stats::rnorm(n, 8, 2),
    # 'ndvi' is the third covariate, so it is the default response.
    ndvi = stats::rnorm(n, 0.4, 0.1)
  )

  mse_default <- ss_rf_mse(design, n_folds = 3)
  mse_target <- ss_rf_mse(design, n_folds = 3, target_var = "slope")

  expect_false(is.na(mse_default))
  expect_false(is.na(mse_target))
  # Different responses have different scales, so the errors must differ.
  expect_false(isTRUE(all.equal(mse_default, mse_target)))
})


test_that("ss_rf_mse restricts the design to columns shared with full_data", {
  skip_on_cran()

  set.seed(8)
  n <- 30
  design <- data.frame(
    x = stats::runif(n), y = stats::runif(n),
    dem = stats::rnorm(n), slope = stats::rnorm(n),
    ndvi = stats::rnorm(n), extra = stats::rnorm(n)
  )
  full <- data.frame(dem = stats::rnorm(n), slope = stats::rnorm(n), ndvi = stats::rnorm(n))

  mse <- ss_rf_mse(design, full_data = full, n_folds = 3)

  expect_false(is.na(mse))
  expect_gte(mse, 0)
})


test_that("ss_rf_optimize returns the documented structure", {
  skip_on_cran()

  r <- make_test_covariates(nrow = 12, ncol = 12)

  result <- ss_rf_optimize(r, n_samples = 12, n_iterations = 3, seed = 123)

  expect_named(
    result,
    c("optimized_samples", "initial_samples", "initial_mse", "final_mse", "improvement")
  )
  expect_s3_class(result$optimized_samples, "data.frame")
  expect_s3_class(result$initial_samples, "data.frame")
  expect_equal(nrow(result$optimized_samples), 12)
  expect_equal(nrow(result$initial_samples), 12)
})


test_that("ss_rf_optimize never returns a design worse than its cLHS baseline", {
  skip_on_cran()

  r <- make_test_covariates(nrow = 12, ncol = 12)

  result <- ss_rf_optimize(r, n_samples = 12, n_iterations = 5, seed = 42)

  skip_if(is.na(result$initial_mse) || is.na(result$final_mse), "RF objective returned NA")

  # best_mse only ever moves downward, so the reported design cannot be worse
  # than the baseline and the improvement cannot be negative.
  expect_lte(result$final_mse, result$initial_mse)
  expect_gte(result$improvement, 0)
})


test_that(".rf_comparison_table summarises both designs", {
  rf_results <- list(
    initial_samples = data.frame(x = 1:10, y = 1:10),
    optimized_samples = data.frame(x = 1:10, y = 1:10),
    initial_mse = 4.5,
    final_mse = 3.0,
    improvement = 33.333
  )

  tbl <- .rf_comparison_table(rf_results)

  expect_s3_class(tbl, "data.frame")
  expect_equal(tbl$Method, c("cLHS", "RF Optimized"))
  expect_equal(tbl$MSE, c(4.5, 3.0))
  expect_equal(tbl$Samples, c(10L, 10L))
  expect_equal(tbl$Improvement, c(0, 33.33))
})


test_that("ss_rf_size writes outputs only when output_dir is supplied", {
  skip_on_cran()

  r <- make_test_covariates(nrow = 10, ncol = 10)

  without_dir <- ss_rf_size(r, n_samples = 8, n_iterations = 2, seed = 5)
  expect_null(without_dir$file_paths)
  expect_named(
    without_dir,
    c("clhs_samples", "rf_optimized_samples", "comparison_table", "plots", "improvement")
  )
  expect_s3_class(without_dir$plots$clhs_plot, "ggplot")
  expect_s3_class(without_dir$plots$rf_plot, "ggplot")

  out_dir <- file.path(tempdir(), "rf-outputs")
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  with_dir <- ss_rf_size(r, n_samples = 8, n_iterations = 2, seed = 5, output_dir = out_dir)

  expect_true(file.exists(with_dir$file_paths$clhs_locations))
  expect_true(file.exists(with_dir$file_paths$rf_locations))
  expect_true(file.exists(with_dir$file_paths$comparison_table))
})
