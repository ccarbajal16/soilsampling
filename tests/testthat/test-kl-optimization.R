test_that("ss_kl_divergence is zero when the sample equals the population", {
  pop <- data.frame(a = seq(0, 10, length.out = 200), b = seq(-5, 5, length.out = 200))

  expect_equal(ss_kl_divergence(pop, pop), 0)
})


test_that("ss_kl_divergence is larger for a biased sample than a representative one", {
  set.seed(42)
  pop <- data.frame(a = runif(500, 0, 100))

  representative <- pop[seq(1, 500, by = 5), , drop = FALSE]
  biased <- pop[pop$a < 20, , drop = FALSE]

  kl_representative <- ss_kl_divergence(pop, representative)
  kl_biased <- ss_kl_divergence(pop, biased)

  expect_gt(kl_biased, kl_representative)
})


test_that("ss_kl_divergence ignores non-numeric columns", {
  set.seed(1)
  numeric_only <- data.frame(a = runif(200))
  with_character <- numeric_only
  with_character$label <- rep(c("north", "south"), length.out = 200)

  sample_idx <- seq(1, 200, by = 4)

  expect_equal(
    ss_kl_divergence(with_character, with_character[sample_idx, ]),
    ss_kl_divergence(numeric_only, numeric_only[sample_idx, , drop = FALSE])
  )
})


test_that("ss_kl_divergence averages across variables", {
  set.seed(7)
  # A variable sampled representatively and one sampled with strong bias:
  # the mean divergence must fall between the two per-variable values.
  pop <- data.frame(a = runif(400, 0, 50), b = runif(400, 0, 50))
  idx <- which(pop$b < 15)

  kl_both <- ss_kl_divergence(pop, pop[idx, ])
  kl_a <- ss_kl_divergence(pop["a"], pop[idx, "a", drop = FALSE])
  kl_b <- ss_kl_divergence(pop["b"], pop[idx, "b", drop = FALSE])

  expect_equal(kl_both, mean(c(kl_a, kl_b)))
  expect_gt(kl_b, kl_a)
})


test_that("ss_load_rasters rejects paths that are neither .tif nor a directory", {
  expect_error(
    ss_load_rasters(file.path(tempdir(), "does-not-exist.tif")),
    "neither an existing .tif file nor a directory"
  )
})


test_that(".load_raster_dir errors on a missing directory and on an empty one", {
  expect_error(
    .load_raster_dir(file.path(tempdir(), "no-such-dir")),
    "Directory does not exist"
  )

  empty_dir <- file.path(tempdir(), "empty-raster-dir")
  dir.create(empty_dir, showWarnings = FALSE)
  on.exit(unlink(empty_dir, recursive = TRUE), add = TRUE)

  expect_error(.load_raster_dir(empty_dir), "No raster files found")
})


test_that("ss_kl_size errors when no complete cases remain", {
  pop <- data.frame(a = c(1, NA, 3), b = c(NA, 2, NA))

  expect_error(ss_kl_size(pop), "No complete cases found")
})


test_that("ss_kl_size subsamples populations larger than max_population", {
  skip_on_cran()

  set.seed(11)
  pop <- data.frame(a = runif(300), b = runif(300))

  result <- ss_kl_size(
    pop,
    max_population = 50,
    min_samples = 5,
    max_samples = 15,
    step_size = 5,
    n_replicates = 1
  )

  # Every tested sample size must have been drawn from the 50-row subsample,
  # so no replicate may report a sample size above max_population.
  expect_true(all(result$raw_results$sample_size <= 50))
  expect_true(nrow(result$raw_results) > 0)
})


test_that("ss_kl_optimize returns the documented structure", {
  skip_on_cran()

  set.seed(99)
  pop <- data.frame(a = runif(200, 0, 10), b = runif(200, 0, 10))

  result <- ss_kl_optimize(
    pop,
    min_samples = 10,
    max_samples = 50,
    step_size = 10,
    n_replicates = 2
  )

  expect_named(
    result,
    c(
      "raw_results", "summary_results", "fitted_model", "fitted_curve",
      "optimal_sample_size", "plot_kl", "plot_cdf"
    )
  )

  expect_s3_class(result$raw_results, "data.frame")
  expect_named(result$raw_results, c("sample_size", "replicate", "kl_divergence"))
  expect_equal(nrow(result$raw_results), 5 * 2)

  expect_s3_class(result$summary_results, "data.frame")
  expect_equal(nrow(result$summary_results), 5)
  expect_true(all(c("mean_kl", "sd_kl") %in% names(result$summary_results)))

  expect_s3_class(result$plot_kl, "ggplot")
})


test_that("ss_kl_optimize picks an optimal size within the tested range", {
  skip_on_cran()

  set.seed(123)
  pop <- data.frame(a = runif(200, 0, 10), b = runif(200, 0, 10))

  result <- ss_kl_optimize(
    pop,
    min_samples = 10,
    max_samples = 50,
    step_size = 10,
    n_replicates = 2
  )

  skip_if(is.na(result$optimal_sample_size), "exponential decay fit did not converge")

  expect_true(result$optimal_sample_size %in% seq(10, 50, by = 10))
})


test_that("ss_kl_optimize returns an empty result when every cLHS call fails", {
  # A single-row population cannot support cLHS at any of the requested sizes,
  # so every replicate fails and the empty-result branch is taken.
  pop <- data.frame(a = 1)

  result <- suppressWarnings(
    ss_kl_optimize(pop, min_samples = 5, max_samples = 15, step_size = 5, n_replicates = 1)
  )

  expect_equal(nrow(result$raw_results), 0)
  expect_true(is.na(result$optimal_sample_size))
  expect_null(result$fitted_model)
  expect_null(result$plot_kl)
})


test_that("ss_kl_size writes outputs only when output_dir is supplied", {
  skip_on_cran()

  set.seed(5)
  pop <- data.frame(a = runif(150, 0, 10), b = runif(150, 0, 10))

  args <- list(pop, min_samples = 5, max_samples = 15, step_size = 5, n_replicates = 1)

  without_dir <- do.call(ss_kl_size, args)
  expect_null(without_dir$file_paths)

  out_dir <- file.path(tempdir(), "kl-outputs")
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  with_dir <- do.call(ss_kl_size, c(args, list(output_dir = out_dir)))

  expect_type(with_dir$file_paths, "list")
  expect_true(file.exists(with_dir$file_paths$raw))
  expect_true(file.exists(with_dir$file_paths$summary))
})


test_that("ss_kl_save_plots warns and saves nothing when there are no plots", {
  out_dir <- file.path(tempdir(), "kl-plots-empty")
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  results <- list(plot_kl = NULL, plot_cdf = NULL)

  expect_warning(
    saved <- ss_kl_save_plots(results, output_dir = out_dir),
    "No plots were saved"
  )
  expect_length(saved, 0)
})


test_that("ss_kl_save_plots writes both plots with the requested prefix", {
  out_dir <- file.path(tempdir(), "kl-plots-prefix")
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  results <- list(
    plot_kl = ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) + ggplot2::geom_point(),
    plot_cdf = ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) + ggplot2::geom_point()
  )

  saved <- ss_kl_save_plots(results, output_dir = out_dir, prefix = "clhs")

  expect_length(saved, 2)
  expect_true(all(file.exists(saved)))
  expect_true(all(grepl("clhs_", basename(saved))))
})
