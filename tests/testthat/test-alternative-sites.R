make_alt_covariates <- function(nrow = 20, ncol = 20, seed = 42) {
  set.seed(seed)

  r <- terra::rast(
    nrows = nrow, ncols = ncol,
    xmin = 0, xmax = 1000, ymin = 0, ymax = 1000,
    nlyrs = 3
  )

  n_cells <- terra::ncell(r)
  coords <- terra::xyFromCell(r, seq_len(n_cells))

  dem <- 3500 + coords[, 2] * 0.5 + stats::rnorm(n_cells, 0, 10)
  slope <- abs(stats::rnorm(n_cells, 0.3, 0.1))
  ndvi <- 0.3 + coords[, 1] / 5000 + stats::rnorm(n_cells, 0, 0.01)

  terra::values(r) <- cbind(dem, slope, ndvi)
  names(r) <- c("dem", "slope", "ndvi")
  r
}


test_that("ss_alt_similarity scores an exact match highest", {
  target <- c(dem = 4000, slope = 0.3, ndvi = 0.4)
  candidates <- data.frame(
    dem = c(4000, 4200, 3500),
    slope = c(0.3, 0.5, 0.1),
    ndvi = c(0.4, 0.2, 0.6)
  )

  scores <- ss_alt_similarity(target, candidates, method = "euclidean")

  expect_length(scores, 3)
  expect_equal(which.max(scores), 1L)
})


test_that("ss_alt_similarity returns scores bounded in [0, 1] for every method", {
  set.seed(4)
  target <- c(dem = 4000, slope = 0.3, ndvi = 0.4)
  candidates <- data.frame(
    dem = stats::rnorm(20, 4000, 200),
    slope = stats::rnorm(20, 0.3, 0.1),
    ndvi = stats::rnorm(20, 0.4, 0.05)
  )

  for (m in c("mahalanobis", "euclidean", "gower")) {
    scores <- ss_alt_similarity(target, candidates, method = m)

    expect_length(scores, 20)
    expect_true(all(scores >= 0 & scores <= 1), info = paste("method:", m))
    expect_false(anyNA(scores), info = paste("method:", m))
  }
})


test_that("ss_alt_similarity rejects invalid inputs", {
  target <- c(dem = 4000, slope = 0.3)

  expect_error(
    ss_alt_similarity(target, c(1, 2, 3)),
    "must be a matrix or data.frame"
  )

  expect_error(
    ss_alt_similarity(data.frame(dem = c(1, 2), slope = c(3, 4)), data.frame(dem = 1, slope = 2)),
    "must have exactly one row"
  )

  expect_error(
    ss_alt_similarity(target, data.frame(dem = "a", slope = "b"), method = "euclidean"),
    "No numeric variables found"
  )
})


test_that("ss_alt_similarity warns when weights are missing for some variables", {
  target <- c(dem = 4000, slope = 0.3, ndvi = 0.4)
  candidates <- data.frame(
    dem = c(3990, 4200), slope = c(0.31, 0.5), ndvi = c(0.41, 0.2)
  )

  expect_warning(
    ss_alt_similarity(target, candidates, weights = c(dem = 2), method = "euclidean"),
    "Missing weights for variables"
  )
})


test_that("ss_alt_similarity errors on unnamed weights of the wrong length", {
  target <- c(dem = 4000, slope = 0.3, ndvi = 0.4)
  candidates <- data.frame(
    dem = c(3990, 4200), slope = c(0.31, 0.5), ndvi = c(0.41, 0.2)
  )

  expect_error(
    ss_alt_similarity(target, candidates, weights = c(1, 2), method = "euclidean"),
    "Unnamed weights must have the same length"
  )
})


test_that("ss_alt_filter_buffer removes only candidates inside the buffer", {
  candidates <- data.frame(x = c(0, 100, 300, 500), y = c(0, 0, 0, 0))
  targets <- data.frame(x = 0, y = 0)

  kept <- ss_alt_filter_buffer(candidates, targets, min_distance = 200)

  # Distances to the single target are 0, 100, 300, 500; only the last two
  # clear a 200-unit buffer.
  expect_equal(nrow(kept), 2)
  expect_equal(kept$x, c(300, 500))
})


test_that("ss_alt_filter_buffer measures distance to the nearest target", {
  candidates <- data.frame(x = c(500), y = c(500))
  # The candidate is far from the first target but close to the second.
  targets <- data.frame(x = c(0, 450), y = c(0, 500))

  expect_equal(nrow(ss_alt_filter_buffer(candidates, targets, min_distance = 100)), 0)
  expect_equal(nrow(ss_alt_filter_buffer(candidates, targets, min_distance = 40)), 1)
})


test_that("ss_alt_filter_buffer validates its arguments", {
  candidates <- data.frame(x = 1, y = 1)
  targets <- data.frame(x = 0, y = 0)

  expect_error(
    ss_alt_filter_buffer(data.frame(lon = 1, lat = 1), targets, 10),
    "'candidate_sites' must contain 'x' and 'y'"
  )
  expect_error(
    ss_alt_filter_buffer(candidates, data.frame(lon = 0, lat = 0), 10),
    "'target_sites' must contain 'x' and 'y'"
  )
  expect_error(
    ss_alt_filter_buffer(candidates, targets, -5),
    "'min_distance' must be a single positive number"
  )
  expect_error(
    ss_alt_filter_buffer(candidates, targets, c(10, 20)),
    "'min_distance' must be a single positive number"
  )
})


test_that("ss_alt_rank orders alternatives by descending similarity", {
  candidates <- data.frame(site_id = paste0("c", 1:5), x = 1:5, y = 1:5)
  scores <- c(0.9, 0.4, 0.95, 0.2, 0.7)

  ranked <- ss_alt_rank(scores, candidates, n_select = 3, target_site_id = "site_001")

  expect_equal(nrow(ranked), 3)
  expect_equal(ranked$site_id, c("c3", "c1", "c5"))
  expect_equal(ranked$similarity_score, c(0.95, 0.9, 0.7))
  expect_equal(ranked$similarity_rank, 1:3)
  expect_true(all(ranked$target_site_id == "site_001"))
  expect_true(all(ranked$selection_method == "similarity"))
})


test_that("ss_alt_rank drops NA and infinite scores", {
  candidates <- data.frame(site_id = paste0("c", 1:5), x = 1:5, y = 1:5)
  scores <- c(0.9, NA, Inf, 0.2, 0.7)

  ranked <- ss_alt_rank(scores, candidates, n_select = 3)

  expect_equal(nrow(ranked), 3)
  expect_equal(ranked$site_id, c("c1", "c5", "c4"))
})


test_that("ss_alt_rank warns when fewer valid scores than requested are available", {
  candidates <- data.frame(site_id = paste0("c", 1:4), x = 1:4, y = 1:4)
  scores <- c(0.9, NA, NA, 0.5)

  expect_warning(
    ranked <- ss_alt_rank(scores, candidates, n_select = 4),
    "Only 2 valid similarity scores available"
  )
  expect_equal(nrow(ranked), 2)
})


test_that("ss_alt_rank warns and returns an empty frame when no score is valid", {
  candidates <- data.frame(site_id = paste0("c", 1:3), x = 1:3, y = 1:3)

  expect_warning(
    ranked <- ss_alt_rank(c(NA, NA, NA), candidates, n_select = 2),
    "No valid similarity scores found"
  )
  expect_equal(nrow(ranked), 0)
})


test_that("ss_alt_rank validates its arguments", {
  candidates <- data.frame(site_id = "c1", x = 1, y = 1)

  expect_error(ss_alt_rank(numeric(0), candidates), "non-empty numeric vector")
  expect_error(ss_alt_rank(0.5, candidates[0, ]), "non-empty data.frame")
  expect_error(ss_alt_rank(c(0.5, 0.6), candidates), "must match the number of rows")
  expect_error(ss_alt_rank(0.5, candidates, n_select = 0), "single positive integer")
})


test_that("ss_alt_standardize_sites renames coordinates and generates IDs", {
  raw <- data.frame(lon = c(629500, 630200), lat = c(9879500, 9880100))

  out <- ss_alt_standardize_sites(raw, x_col = "lon", y_col = "lat")

  expect_equal(names(out)[1:4], c("site_id", "x", "y", "type"))
  expect_equal(out$x, c(629500, 630200))
  expect_equal(out$y, c(9879500, 9880100))
  expect_equal(out$site_id, c("inaccessible_1", "inaccessible_2"))
  expect_true(all(out$type == "inaccessible"))
  expect_false(any(c("lon", "lat") %in% names(out)))
})


test_that("ss_alt_standardize_sites preserves an existing ID column and extra columns", {
  raw <- data.frame(
    code = c("A1", "A2"),
    x = c(10, 20), y = c(30, 40),
    note = c("wet", "dry")
  )

  out <- ss_alt_standardize_sites(raw, id_col = "code", site_type = "target")

  expect_equal(out$site_id, c("A1", "A2"))
  expect_true(all(out$type == "target"))
  expect_true("note" %in% names(out))
  expect_false("code" %in% names(out))
})


test_that("ss_alt_standardize_sites validates its arguments", {
  expect_error(ss_alt_standardize_sites(data.frame()), "non-empty data.frame")
  expect_error(
    ss_alt_standardize_sites(data.frame(a = 1, y = 2)),
    "Column 'x' not found"
  )
  expect_error(
    ss_alt_standardize_sites(data.frame(x = 1, b = 2)),
    "Column 'y' not found"
  )
})


test_that("ss_alt_candidates generates a labelled candidate pool", {
  r <- make_alt_covariates(nrow = 15, ncol = 15)

  candidates <- ss_alt_candidates(r, n_candidates = 30, seed = 123)

  expect_s3_class(candidates, "data.frame")
  expect_equal(nrow(candidates), 30)
  expect_equal(names(candidates)[1:4], c("site_id", "x", "y", "type"))
  expect_true(all(candidates$type == "candidate"))
  expect_true(all(c("dem", "slope", "ndvi") %in% names(candidates)))
  expect_false(anyNA(candidates$dem))
})


test_that("ss_alt_candidates keeps candidates inside the raster extent", {
  r <- make_alt_covariates(nrow = 15, ncol = 15)

  candidates <- ss_alt_candidates(r, n_candidates = 25, seed = 7)

  expect_true(all(candidates$x >= 0 & candidates$x <= 1000))
  expect_true(all(candidates$y >= 0 & candidates$y <= 1000))
})


test_that("ss_alt_candidates supports systematic sampling", {
  r <- make_alt_covariates(nrow = 15, ncol = 15)

  candidates <- ss_alt_candidates(r, n_candidates = 25, method = "systematic", seed = 1)

  expect_lte(nrow(candidates), 25)
  expect_gt(nrow(candidates), 0)
  expect_true(all(candidates$type == "candidate"))
})


test_that("ss_alt_candidates rejects non-raster input", {
  expect_error(
    ss_alt_candidates(data.frame(x = 1, y = 1), n_candidates = 5),
    "must be a SpatRaster object"
  )
})


test_that("ss_alt_sites validates target_sites before doing any work", {
  r <- make_alt_covariates(nrow = 10, ncol = 10)

  expect_error(ss_alt_sites(r, data.frame()), "non-empty data.frame")
  expect_error(
    ss_alt_sites(r, data.frame(lon = 500, lat = 500)),
    "must contain 'x' and 'y' coordinate columns"
  )
})


test_that("ss_alt_sites returns ranked alternatives for every target site", {
  skip_on_cran()

  r <- make_alt_covariates(nrow = 20, ncol = 20)
  targets <- data.frame(x = c(300, 700), y = c(300, 700))

  result <- ss_alt_sites(
    r, targets,
    n_alternatives = 3,
    n_candidates = 100,
    seed = 123
  )

  expect_true(all(c("target_sites", "candidate_sites", "alternatives", "selected_by_site") %in% names(result)))
  expect_equal(nrow(result$target_sites), 2)
  expect_equal(nrow(result$alternatives), 2 * 3)
  expect_length(result$selected_by_site, 2)
  expect_setequal(unique(result$alternatives$target_site_id), result$target_sites$site_id)
  expect_true(all(result$alternatives$similarity_score >= 0 & result$alternatives$similarity_score <= 1))
})


test_that("ss_alt_sites honours the minimum distance buffer", {
  skip_on_cran()

  r <- make_alt_covariates(nrow = 20, ncol = 20)
  targets <- data.frame(x = 500, y = 500)
  buffer <- 300

  result <- ss_alt_sites(
    r, targets,
    n_alternatives = 3,
    n_candidates = 200,
    min_distance_buffer = buffer,
    seed = 42
  )

  distances <- sqrt((result$alternatives$x - 500)^2 + (result$alternatives$y - 500)^2)
  expect_true(all(distances >= buffer))
})


test_that("ss_alt_sites writes outputs only when output_dir is supplied", {
  skip_on_cran()

  r <- make_alt_covariates(nrow = 15, ncol = 15)
  targets <- data.frame(x = 500, y = 500)

  without_dir <- ss_alt_sites(r, targets, n_alternatives = 2, n_candidates = 60, seed = 1)
  expect_null(without_dir$file_paths)

  out_dir <- file.path(tempdir(), "alt-outputs")
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  with_dir <- ss_alt_sites(
    r, targets,
    n_alternatives = 2, n_candidates = 60, seed = 1,
    output_dir = out_dir
  )

  expect_type(with_dir$file_paths, "list")
  expect_true(all(file.exists(unlist(with_dir$file_paths))))
})
