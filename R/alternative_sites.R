#' Alternative Sampling Site Selection
#'
#' Functions for finding environmentally similar alternative sites when
#' original sampling locations become inaccessible, based on multivariate
#' distance in covariate space.
#'
#' @name alternative_sites
#' @keywords internal
NULL


#' Handle Missing Values Before Distance Calculation
#'
#' @param data Data frame with potential missing values.
#' @param method One of `"pairwise"` (no-op, handled downstream),
#'   `"listwise"` (drop incomplete rows), `"mean"`, or `"median"`.
#' @param categorical_vars Character vector of categorical column names,
#'   left untouched by numeric imputation.
#'
#' @return A data frame with missing values handled per `method`.
#'
#' @keywords internal
.alt_handle_missing <- function(data, method = "pairwise", categorical_vars = NULL) {
  if (is.null(categorical_vars)) {
    categorical_vars <- character(0)
  }

  if (method == "pairwise") {
    return(data)
  }

  if (method == "listwise") {
    return(data[stats::complete.cases(data), , drop = FALSE])
  }

  imputed_data <- data
  numeric_vars <- setdiff(names(data), categorical_vars)
  numeric_vars <- numeric_vars[vapply(data[numeric_vars], is.numeric, logical(1))]

  for (var in numeric_vars) {
    var_data <- data[[var]]
    missing_idx <- is.na(var_data)
    if (!any(missing_idx)) {
      next
    }

    impute_value <- if (method == "mean") {
      mean(var_data, na.rm = TRUE)
    } else {
      stats::median(var_data, na.rm = TRUE)
    }

    if (!is.na(impute_value)) {
      imputed_data[[var]][missing_idx] <- impute_value
    }
  }

  imputed_data
}


#' Normalize Environmental Data
#'
#' @param data Data frame of environmental variables.
#' @param method One of `"zscore"`, `"minmax"`, `"robust"`.
#' @param categorical_vars Character vector of columns to leave untouched.
#'
#' @return A data frame with numeric columns normalized.
#'
#' @keywords internal
.alt_normalize <- function(data, method = "zscore", categorical_vars = NULL) {
  if (is.null(categorical_vars)) {
    categorical_vars <- character(0)
  }

  numeric_vars <- setdiff(names(data), categorical_vars)
  numeric_vars <- numeric_vars[vapply(data[numeric_vars], is.numeric, logical(1))]

  normalized_data <- data

  for (var in numeric_vars) {
    var_data <- data[[var]]
    if (all(is.na(var_data))) {
      next
    }

    if (method == "zscore") {
      var_sd <- stats::sd(var_data, na.rm = TRUE)
      normalized_data[[var]] <- if (var_sd == 0) {
        rep(0, length(var_data))
      } else {
        (var_data - mean(var_data, na.rm = TRUE)) / var_sd
      }
    } else if (method == "minmax") {
      var_min <- min(var_data, na.rm = TRUE)
      var_max <- max(var_data, na.rm = TRUE)
      normalized_data[[var]] <- if (var_min == var_max) {
        rep(0.5, length(var_data))
      } else {
        (var_data - var_min) / (var_max - var_min)
      }
    } else if (method == "robust") {
      var_mad <- stats::mad(var_data, na.rm = TRUE)
      normalized_data[[var]] <- if (var_mad == 0) {
        rep(0, length(var_data))
      } else {
        (var_data - stats::median(var_data, na.rm = TRUE)) / var_mad
      }
    }
  }

  normalized_data
}


#' Mahalanobis Distance From a Target to Each Candidate
#'
#' @param target_values Named numeric vector of target environmental values.
#' @param candidate_values Matrix or data frame of candidate environmental
#'   values, one row per candidate, columns matching `target_values`.
#' @param covariance_matrix Covariance matrix used for the distance.
#'
#' @return Numeric vector of Mahalanobis distances, one per candidate row.
#'
#' @keywords internal
.alt_mahalanobis_distance <- function(target_values, candidate_values, covariance_matrix) {
  if (is.data.frame(candidate_values)) {
    candidate_values <- as.matrix(candidate_values)
  }

  inv_cov <- tryCatch(
    solve(covariance_matrix),
    error = function(e) {
      warning("Singular covariance matrix detected, added regularization", call. = FALSE)
      solve(covariance_matrix + diag(1e-6, nrow(covariance_matrix)))
    }
  )

  distances <- numeric(nrow(candidate_values))
  for (i in seq_len(nrow(candidate_values))) {
    diff <- candidate_values[i, ] - target_values
    distances[i] <- sqrt(as.numeric(t(diff) %*% inv_cov %*% diff))
  }

  distances
}


#' Euclidean Distance From a Target to Each Candidate
#'
#' @param target_values Named numeric vector of target environmental values.
#' @param candidate_values Matrix or data frame of candidate environmental
#'   values, one row per candidate, columns matching `target_values`.
#' @param normalized Logical, whether `target_values`/`candidate_values`
#'   are already normalized. If `FALSE`, range-normalizes internally.
#'
#' @return Numeric vector of Euclidean distances, one per candidate row.
#'
#' @keywords internal
.alt_euclidean_distance <- function(target_values, candidate_values, normalized = FALSE) {
  if (is.data.frame(candidate_values)) {
    candidate_values <- as.matrix(candidate_values)
  }

  if (!normalized) {
    all_values <- rbind(target_values, candidate_values)
    ranges <- apply(all_values, 2, function(x) diff(range(x, na.rm = TRUE)))
    ranges[ranges == 0] <- 1

    target_values <- target_values / ranges
    candidate_values <- t(t(candidate_values) / ranges)
  }

  distances <- numeric(nrow(candidate_values))
  for (i in seq_len(nrow(candidate_values))) {
    diff <- candidate_values[i, ] - target_values
    distances[i] <- sqrt(sum(diff^2, na.rm = TRUE))
  }

  distances
}


#' Gower Distance From a Target to Each Candidate
#'
#' @param target_values Named vector or single-row data frame of target
#'   environmental values.
#' @param candidate_values Matrix or data frame of candidate environmental
#'   values, one row per candidate.
#' @param categorical_vars Character vector of categorical column names,
#'   compared by exact match instead of scaled absolute difference.
#'
#' @return Numeric vector of Gower distances (0-1), one per candidate row.
#'
#' @keywords internal
.alt_gower_distance <- function(target_values, candidate_values, categorical_vars = NULL) {
  if (is.data.frame(target_values)) {
    target_values <- as.vector(target_values[1, ])
    names(target_values) <- names(candidate_values)
  }

  if (is.matrix(candidate_values)) {
    candidate_values <- as.data.frame(candidate_values)
  }

  if (is.null(categorical_vars)) {
    categorical_vars <- character(0)
  }
  continuous_vars <- setdiff(names(candidate_values), categorical_vars)

  distances <- numeric(nrow(candidate_values))

  for (i in seq_len(nrow(candidate_values))) {
    candidate_row <- candidate_values[i, ]
    total_distance <- 0
    valid_comparisons <- 0

    for (var in continuous_vars) {
      target_val <- target_values[var]
      candidate_val <- candidate_row[[var]]
      if (is.na(target_val) || is.na(candidate_val)) {
        next
      }

      var_range <- diff(range(c(target_val, candidate_values[[var]]), na.rm = TRUE))
      distance_component <- if (var_range == 0) 0 else abs(target_val - candidate_val) / var_range

      total_distance <- total_distance + distance_component
      valid_comparisons <- valid_comparisons + 1
    }

    for (var in categorical_vars) {
      target_val <- target_values[var]
      candidate_val <- candidate_row[[var]]
      if (is.na(target_val) || is.na(candidate_val)) {
        next
      }

      distance_component <- ifelse(target_val == candidate_val, 0, 1)
      total_distance <- total_distance + distance_component
      valid_comparisons <- valid_comparisons + 1
    }

    distances[i] <- if (valid_comparisons > 0) total_distance / valid_comparisons else NA
  }

  distances
}


#' Environmental Similarity Between a Target Site and Candidate Sites
#'
#' Computes a 0-1 similarity score between a target (e.g. an inaccessible
#' sampling site) and a set of candidate sites, based on multivariate
#' distance in environmental covariate space.
#'
#' @param target_values Environmental values at the target site: a named
#'   numeric vector, or a single-row data frame with the same columns as
#'   `candidate_values`.
#' @param candidate_values Environmental values at candidate sites: a
#'   matrix or data frame, one row per candidate.
#' @param categorical_vars Character vector of categorical column names.
#'   Only used by `method = "gower"`, and excluded from the other methods.
#' @param weights Optional named numeric vector of variable importance
#'   weights; unnamed weights must match `length(target_values)`.
#' @param method Distance metric: `"mahalanobis"` (default, accounts for
#'   covariance among variables), `"euclidean"`, or `"gower"` (handles
#'   mixed continuous/categorical data).
#' @param normalize Logical, whether to z-score normalize numeric
#'   variables before distance calculation. Default `TRUE`.
#' @param missing_method How to handle missing values: `"pairwise"`
#'   (default, handled per-comparison), `"listwise"`, `"mean"`, or
#'   `"median"`.
#'
#' @return Numeric vector of similarity scores in `[0, 1]`, one per
#'   candidate row (`1` = perfect match).
#'
#' @examples
#' target <- c(dem = 4000, slope = 0.3, ndvi = 0.4)
#' candidates <- data.frame(
#'   dem = c(3990, 4200, 3500),
#'   slope = c(0.31, 0.5, 0.1),
#'   ndvi = c(0.41, 0.2, 0.6)
#' )
#' ss_alt_similarity(target, candidates)
#'
#' @seealso [ss_alt_candidates()], [ss_alt_rank()], [ss_alt_sites()]
#' @export
ss_alt_similarity <- function(target_values,
                               candidate_values,
                               categorical_vars = NULL,
                               weights = NULL,
                               method = c("mahalanobis", "euclidean", "gower"),
                               normalize = TRUE,
                               missing_method = "pairwise") {
  method <- match.arg(method)

  if (!is.data.frame(candidate_values) && !is.matrix(candidate_values)) {
    stop("'candidate_values' must be a matrix or data.frame", call. = FALSE)
  }

  if (is.data.frame(target_values)) {
    if (nrow(target_values) != 1) {
      stop("'target_values' data.frame must have exactly one row", call. = FALSE)
    }
    target_values <- unlist(target_values[1, , drop = FALSE], use.names = FALSE)
    names(target_values) <- colnames(candidate_values)
  } else if (is.null(names(target_values))) {
    names(target_values) <- colnames(candidate_values)
  }

  if (is.matrix(candidate_values)) {
    candidate_values <- as.data.frame(candidate_values)
  }

  if (missing_method != "pairwise") {
    combined_data <- rbind(data.frame(t(target_values)), candidate_values)
    processed_data <- .alt_handle_missing(combined_data, missing_method, categorical_vars)
    target_values <- as.vector(processed_data[1, ])
    names(target_values) <- names(processed_data)
    candidate_values <- processed_data[-1, , drop = FALSE]
  }

  if (!is.null(weights)) {
    if (!is.numeric(weights)) {
      stop("'weights' must be numeric", call. = FALSE)
    }
    if (is.null(names(weights))) {
      if (length(weights) != length(target_values)) {
        stop("Unnamed weights must have the same length as the number of variables", call. = FALSE)
      }
      names(weights) <- names(target_values)
    }

    missing_weights <- setdiff(names(target_values), names(weights))
    if (length(missing_weights) > 0) {
      warning(
        "Missing weights for variables: ", paste(missing_weights, collapse = ", "),
        ". Using weight = 1.0",
        call. = FALSE
      )
      for (var in missing_weights) weights[var] <- 1.0
    }

    for (var in names(target_values)) {
      if (var %in% names(weights)) {
        weight_factor <- sqrt(weights[var])
        target_values[var] <- target_values[var] * weight_factor
        candidate_values[[var]] <- candidate_values[[var]] * weight_factor
      }
    }
  }

  if (is.null(categorical_vars)) {
    categorical_vars <- character(0)
  }

  if (method == "gower") {
    distances <- .alt_gower_distance(target_values, candidate_values, categorical_vars)
  } else {
    numeric_vars <- setdiff(names(candidate_values), categorical_vars)
    numeric_vars <- numeric_vars[vapply(candidate_values[numeric_vars], is.numeric, logical(1))]
    if (length(numeric_vars) == 0) {
      stop("No numeric variables found for '", method, "' distance", call. = FALSE)
    }

    target_numeric <- target_values[numeric_vars]
    candidate_numeric <- candidate_values[numeric_vars]

    if (method == "mahalanobis") {
      if (normalize) {
        combined_numeric <- rbind(data.frame(t(target_numeric)), candidate_numeric)
        normalized <- .alt_normalize(combined_numeric, method = "zscore")
        target_numeric <- unlist(normalized[1, , drop = FALSE], use.names = FALSE)
        names(target_numeric) <- names(normalized)
        candidate_numeric <- normalized[-1, , drop = FALSE]
      }

      cov_matrix <- stats::cov(candidate_numeric, use = "pairwise.complete.obs")
      distances <- .alt_mahalanobis_distance(target_numeric, candidate_numeric, cov_matrix)
    } else {
      distances <- .alt_euclidean_distance(target_numeric, candidate_numeric, normalized = !normalize)
    }
  }

  distances[is.infinite(distances)] <- max(distances[is.finite(distances)], na.rm = TRUE) * 2
  distances[is.na(distances)] <- max(distances[is.finite(distances)], na.rm = TRUE) * 2

  if (method == "gower") {
    similarity_scores <- 1 - distances
  } else {
    max_distance <- stats::quantile(distances, 0.95, na.rm = TRUE)
    if (max_distance == 0) max_distance <- 1
    similarity_scores <- exp(-distances / max_distance)
  }

  pmax(0, pmin(1, similarity_scores))
}


#' Generate Candidate Sites for Alternative Site Selection
#'
#' Generates a pool of candidate sites (random or on a systematic grid)
#' within a raster's extent, restricted to valid (non-`NA`) cells, and
#' extracts their environmental values.
#'
#' @param raster_data A `SpatRaster` stack of environmental covariates.
#' @param n_candidates Integer, number of candidate sites to generate.
#' @param method `"random"` (default) or `"systematic"` (regular grid).
#' @param spacing Numeric, grid spacing for `method = "systematic"` (in
#'   raster coordinate units). If `NULL` (default), calculated from the
#'   raster extent to approximate `n_candidates`.
#' @param seed Optional integer seed for reproducibility.
#'
#' @return A data frame with `site_id`, `x`, `y`, `type` (`"candidate"`),
#'   and one column per raster layer.
#'
#' @examples
#' \dontrun{
#' r <- terra::rast("data/predictors.tif")
#' candidates <- ss_alt_candidates(r, n_candidates = 1000, seed = 123)
#' }
#'
#' @seealso [ss_alt_similarity()], [ss_alt_filter_buffer()], [ss_alt_sites()]
#' @export
ss_alt_candidates <- function(raster_data, n_candidates,
                               method = c("random", "systematic"),
                               spacing = NULL, seed = NULL) {
  method <- match.arg(method)

  if (!inherits(raster_data, "SpatRaster")) {
    stop("'raster_data' must be a SpatRaster object", call. = FALSE)
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  ext <- terra::ext(raster_data)
  first_layer <- if (terra::nlyr(raster_data) > 1) raster_data[[1]] else raster_data

  if (method == "random") {
    max_attempts <- n_candidates * 10
    generated_sites <- data.frame(x = numeric(0), y = numeric(0))
    attempts <- 0

    while (nrow(generated_sites) < n_candidates && attempts < max_attempts) {
      batch_size <- min(n_candidates * 2, max_attempts - attempts)
      batch_coords <- data.frame(
        x = stats::runif(batch_size, min = ext[1], max = ext[2]),
        y = stats::runif(batch_size, min = ext[3], max = ext[4])
      )

      valid_batch <- .alt_filter_valid_coords(first_layer, batch_coords)
      if (nrow(valid_batch) > 0) {
        valid_batch$x <- round(valid_batch$x, 8)
        valid_batch$y <- round(valid_batch$y, 8)
        combined <- rbind(generated_sites, valid_batch)
        combined <- combined[!duplicated(combined), ]
        if (nrow(combined) > n_candidates) {
          combined <- combined[seq_len(n_candidates), ]
        }
        generated_sites <- combined
      }

      attempts <- attempts + batch_size
    }
  } else {
    if (is.null(spacing)) {
      area <- (ext[2] - ext[1]) * (ext[4] - ext[3])
      spacing <- sqrt(area / n_candidates)
    }

    grid_coords <- expand.grid(
      x = seq(ext[1] + spacing / 2, ext[2] - spacing / 2, by = spacing),
      y = seq(ext[3] + spacing / 2, ext[4] - spacing / 2, by = spacing)
    )

    generated_sites <- .alt_filter_valid_coords(first_layer, grid_coords)

    if (nrow(generated_sites) > n_candidates) {
      generated_sites <- generated_sites[sample(nrow(generated_sites), n_candidates), ]
    }
  }

  if (nrow(generated_sites) == 0) {
    stop("Could not generate any valid candidate sites", call. = FALSE)
  }

  generated_sites$site_id <- paste0("candidate_", seq_len(nrow(generated_sites)))

  extracted_values <- as.data.frame(terra::extract(raster_data, generated_sites[, c("x", "y")], ID = FALSE))

  candidate_sites <- cbind(generated_sites[, c("site_id", "x", "y")], extracted_values)
  candidate_sites$type <- "candidate"

  coord_cols <- c("site_id", "x", "y", "type")
  env_cols <- setdiff(names(candidate_sites), coord_cols)
  candidate_sites[, c(coord_cols, env_cols)]
}


#' Filter Candidate Coordinates to Valid (Non-NA) Raster Cells
#'
#' @param layer A single-layer `SpatRaster`.
#' @param coords Data frame with `x`, `y` columns.
#'
#' @return The subset of `coords` falling on non-`NA` cells of `layer`.
#'
#' @keywords internal
.alt_filter_valid_coords <- function(layer, coords) {
  test_values <- terra::extract(layer, coords[, c("x", "y")], ID = FALSE)
  valid_mask <- !is.na(test_values[[1]])
  coords[valid_mask, , drop = FALSE]
}


#' Exclude Candidate Sites Within a Distance Buffer of Target Sites
#'
#' Removes candidate sites that fall within `min_distance` of any target
#' (e.g. inaccessible) site, ensuring spatial separation between original
#' and alternative sampling locations.
#'
#' @param candidate_sites Data frame with `x`, `y` candidate coordinates.
#' @param target_sites Data frame with `x`, `y` target coordinates.
#' @param min_distance Numeric, minimum allowed distance (in raster
#'   coordinate units) between a candidate and every target site.
#'
#' @return The subset of `candidate_sites` at least `min_distance` from
#'   every row of `target_sites`.
#'
#' @examples
#' candidates <- data.frame(x = c(0, 100, 500), y = c(0, 100, 500))
#' targets <- data.frame(x = 0, y = 0)
#' ss_alt_filter_buffer(candidates, targets, min_distance = 200)
#'
#' @seealso [ss_alt_candidates()], [ss_alt_sites()]
#' @export
ss_alt_filter_buffer <- function(candidate_sites, target_sites, min_distance) {
  if (!all(c("x", "y") %in% names(candidate_sites))) {
    stop("'candidate_sites' must contain 'x' and 'y' columns", call. = FALSE)
  }
  if (!all(c("x", "y") %in% names(target_sites))) {
    stop("'target_sites' must contain 'x' and 'y' columns", call. = FALSE)
  }
  if (!is.numeric(min_distance) || length(min_distance) != 1 || min_distance <= 0) {
    stop("'min_distance' must be a single positive number", call. = FALSE)
  }

  dx <- outer(candidate_sites$x, target_sites$x, "-")
  dy <- outer(candidate_sites$y, target_sites$y, "-")
  dist_matrix <- sqrt(dx^2 + dy^2)
  min_dist_to_target <- apply(dist_matrix, 1, min)

  candidate_sites[min_dist_to_target >= min_distance, , drop = FALSE]
}


#' Rank and Select the Top Similar Alternative Sites
#'
#' Orders candidate sites by similarity score (highest first) and returns
#' the top `n_select`, annotated with rank and target site information.
#'
#' @param similarity_scores Numeric vector of similarity scores, as
#'   returned by [ss_alt_similarity()], one per row of `candidate_sites`.
#' @param candidate_sites Data frame of candidate sites, as returned by
#'   [ss_alt_candidates()] or [ss_alt_filter_buffer()].
#' @param n_select Integer, number of top alternatives to select. If
#'   fewer valid (non-`NA`, finite) scores are available, all of them are
#'   returned with a warning.
#' @param target_site_id Optional character, ID of the target site these
#'   alternatives are for; stored in a `target_site_id` column. If `NULL`
#'   (default), that column is filled with `NA`.
#'
#' @return A data frame of the top `n_select` candidate sites, with added
#'   `target_site_id`, `similarity_score`, `similarity_rank`, and
#'   `selection_method` columns. Empty data frame if no valid scores.
#'
#' @examples
#' candidates <- data.frame(site_id = paste0("c", 1:5), x = 1:5, y = 1:5)
#' scores <- c(0.9, 0.4, 0.95, 0.2, 0.7)
#' ss_alt_rank(scores, candidates, n_select = 2, target_site_id = "site_001")
#'
#' @seealso [ss_alt_similarity()], [ss_alt_sites()]
#' @export
ss_alt_rank <- function(similarity_scores, candidate_sites, n_select = 5, target_site_id = NULL) {
  if (!is.numeric(similarity_scores) || length(similarity_scores) == 0) {
    stop("'similarity_scores' must be a non-empty numeric vector", call. = FALSE)
  }
  if (!is.data.frame(candidate_sites) || nrow(candidate_sites) == 0) {
    stop("'candidate_sites' must be a non-empty data.frame", call. = FALSE)
  }
  if (length(similarity_scores) != nrow(candidate_sites)) {
    stop("Length of 'similarity_scores' must match the number of rows in 'candidate_sites'", call. = FALSE)
  }
  if (!is.numeric(n_select) || length(n_select) != 1 || n_select <= 0) {
    stop("'n_select' must be a single positive integer", call. = FALSE)
  }

  valid_scores <- !is.na(similarity_scores) & !is.infinite(similarity_scores)
  if (sum(valid_scores) == 0) {
    warning("No valid similarity scores found", call. = FALSE)
    return(candidate_sites[0, , drop = FALSE])
  }

  if (sum(valid_scores) < n_select) {
    warning("Only ", sum(valid_scores), " valid similarity scores available, selecting all", call. = FALSE)
    n_select <- sum(valid_scores)
  }

  candidates_with_scores <- candidate_sites
  candidates_with_scores$similarity_score <- similarity_scores
  # Assigning NULL to a data frame column drops it rather than creating one,
  # so an absent target_site_id must become NA to keep the documented layout.
  candidates_with_scores$target_site_id <- if (is.null(target_site_id)) {
    NA_character_
  } else {
    target_site_id
  }

  candidates_valid <- candidates_with_scores[valid_scores, ]
  candidates_ranked <- candidates_valid[order(candidates_valid$similarity_score, decreasing = TRUE), ]
  selected_alternatives <- candidates_ranked[seq_len(n_select), ]

  selected_alternatives$similarity_rank <- seq_len(n_select)
  selected_alternatives$selection_method <- "similarity"

  coord_cols <- intersect(c("site_id", "x", "y", "type"), names(selected_alternatives))
  score_cols <- c("target_site_id", "similarity_score", "similarity_rank", "selection_method")
  env_cols <- setdiff(names(selected_alternatives), c(coord_cols, score_cols))

  selected_alternatives[, c(coord_cols, score_cols, env_cols)]
}


#' Standardize Site Coordinate Data
#'
#' Normalizes a data frame of site coordinates (typically loaded from
#' CSV) to the `site_id`, `x`, `y`, `type` column layout used throughout
#' the `ss_alt_*` functions.
#'
#' @param sites_data Data frame with at least x/y coordinate columns.
#' @param x_col Character, name of the x coordinate column. Default `"x"`.
#' @param y_col Character, name of the y coordinate column. Default `"y"`.
#' @param id_col Optional character, name of an existing site ID column.
#'   If `NULL` or not found, IDs are generated as `"<site_type>_<n>"`.
#' @param site_type Character, value for the `type` column and the ID
#'   prefix when `id_col` is not supplied. Default `"inaccessible"`.
#'
#' @return A data frame with `site_id`, `x`, `y`, `type` as the first
#'   four columns, followed by any remaining columns from `sites_data`.
#'
#' @examples
#' raw <- data.frame(lon = c(629500, 630200), lat = c(9879500, 9880100))
#' ss_alt_standardize_sites(raw, x_col = "lon", y_col = "lat")
#'
#' @seealso [ss_alt_sites()]
#' @export
ss_alt_standardize_sites <- function(sites_data, x_col = "x", y_col = "y",
                                      id_col = NULL, site_type = "inaccessible") {
  if (!is.data.frame(sites_data) || nrow(sites_data) == 0) {
    stop("'sites_data' must be a non-empty data.frame", call. = FALSE)
  }
  if (!x_col %in% names(sites_data)) {
    stop("Column '", x_col, "' not found in 'sites_data'", call. = FALSE)
  }
  if (!y_col %in% names(sites_data)) {
    stop("Column '", y_col, "' not found in 'sites_data'", call. = FALSE)
  }

  sites_data$x <- sites_data[[x_col]]
  sites_data$y <- sites_data[[y_col]]
  if (x_col != "x") sites_data[[x_col]] <- NULL
  if (y_col != "y") sites_data[[y_col]] <- NULL

  if (is.null(id_col) || !id_col %in% names(sites_data)) {
    sites_data$site_id <- paste0(site_type, "_", seq_len(nrow(sites_data)))
  } else {
    sites_data$site_id <- sites_data[[id_col]]
    if (id_col != "site_id") sites_data[[id_col]] <- NULL
  }

  sites_data$type <- site_type

  coord_cols <- c("site_id", "x", "y", "type")
  other_cols <- setdiff(names(sites_data), coord_cols)
  sites_data[, c(coord_cols, other_cols), drop = FALSE]
}


#' Find Alternative Sampling Sites for Inaccessible Locations
#'
#' End-to-end workflow: generates a candidate pool ([ss_alt_candidates()]),
#' optionally excludes candidates near the target sites
#' ([ss_alt_filter_buffer()]), then computes environmental similarity
#' ([ss_alt_similarity()]) and selects the top alternatives
#' ([ss_alt_rank()]) for each target (e.g. inaccessible) site.
#'
#' @param covariates A character path to a `.tif` file or directory (see
#'   [ss_load_rasters()]), or an already-loaded `SpatRaster` stack.
#' @param target_sites Data frame of sites needing alternatives, with `x`
#'   and `y` coordinate columns. A `site_id` column is generated if
#'   missing. Environmental values are extracted automatically for any
#'   covariate layer not already present as a column.
#' @param method Similarity metric: `"mahalanobis"` (default),
#'   `"euclidean"`, or `"gower"`. Passed to [ss_alt_similarity()].
#' @param n_alternatives Integer, number of alternatives per target site.
#'   Default `5`.
#' @param n_candidates Integer, size of the candidate pool. If `NULL`
#'   (default), set to `max(n_alternatives * 50, 1000)`.
#' @param min_distance_buffer Numeric, minimum distance (raster
#'   coordinate units) candidates must keep from every target site. If
#'   `NULL` (default), no buffer is applied.
#' @param categorical_vars Character vector of categorical covariate
#'   names. Passed to [ss_alt_similarity()].
#' @param weights Optional named numeric vector of variable weights.
#'   Passed to [ss_alt_similarity()].
#' @param normalize Logical, passed to [ss_alt_similarity()]. Default `TRUE`.
#' @param missing_method Passed to [ss_alt_similarity()]. Default `"pairwise"`.
#' @param candidate_method `"random"` (default) or `"systematic"`.
#'   Passed to [ss_alt_candidates()].
#' @param seed Optional integer seed for reproducibility.
#' @param output_dir Character, directory to write CSV outputs to. If
#'   `NULL` (default), nothing is written to disk.
#'
#' @return A list with:
#'   \describe{
#'     \item{target_sites}{Data frame, targets with environmental values.}
#'     \item{candidate_sites}{Data frame, the (buffer-filtered) candidate pool.}
#'     \item{alternatives}{Data frame, all selected alternatives for all
#'       target sites, with a `target_site_id` column.}
#'     \item{selected_by_site}{Named list of per-target alternative data
#'       frames, keyed by `site_id`.}
#'     \item{method_info}{List recording the parameters used.}
#'     \item{file_paths}{Only when `output_dir` is supplied: named list
#'       of files written.}
#'   }
#'
#' @examples
#' \dontrun{
#' inaccessible <- read.csv("data/inaccessible_sites.csv")
#' res <- ss_alt_sites("data/predictors.tif", inaccessible,
#'   n_alternatives = 3, min_distance_buffer = 300, seed = 123
#' )
#' res$alternatives
#' }
#'
#' @seealso [ss_alt_candidates()], [ss_alt_filter_buffer()],
#'   [ss_alt_similarity()], [ss_alt_rank()]
#' @export
ss_alt_sites <- function(covariates,
                          target_sites,
                          method = c("mahalanobis", "euclidean", "gower"),
                          n_alternatives = 5,
                          n_candidates = NULL,
                          min_distance_buffer = NULL,
                          categorical_vars = NULL,
                          weights = NULL,
                          normalize = TRUE,
                          missing_method = "pairwise",
                          candidate_method = c("random", "systematic"),
                          seed = NULL,
                          output_dir = NULL) {
  method <- match.arg(method)
  candidate_method <- match.arg(candidate_method)

  if (!is.data.frame(target_sites) || nrow(target_sites) == 0) {
    stop("'target_sites' must be a non-empty data.frame", call. = FALSE)
  }
  if (!all(c("x", "y") %in% names(target_sites))) {
    stop("'target_sites' must contain 'x' and 'y' coordinate columns", call. = FALSE)
  }
  if (!"site_id" %in% names(target_sites)) {
    target_sites$site_id <- paste0("target_", seq_len(nrow(target_sites)))
  }

  rasters <- if (inherits(covariates, "SpatRaster")) covariates else ss_load_rasters(covariates)

  if (is.null(n_candidates)) {
    n_candidates <- max(n_alternatives * 50, 1000)
  }
  if (!is.null(seed)) {
    set.seed(seed)
  }

  candidate_sites <- ss_alt_candidates(rasters, n_candidates, method = candidate_method, seed = seed)

  if (!is.null(min_distance_buffer)) {
    n_before <- nrow(candidate_sites)
    candidate_sites <- ss_alt_filter_buffer(candidate_sites, target_sites, min_distance_buffer)
    if (nrow(candidate_sites) == 0) {
      stop(
        "All candidates were excluded by 'min_distance_buffer'. ",
        "Try reducing it or increasing 'n_candidates'.",
        call. = FALSE
      )
    }
    if (nrow(candidate_sites) < 10) {
      warning(
        "Only ", nrow(candidate_sites), " candidates remain after distance filtering (",
        n_before, " before). Consider reducing 'min_distance_buffer' or increasing 'n_candidates'.",
        call. = FALSE
      )
    }
  }

  env_vars <- setdiff(names(candidate_sites), c("site_id", "x", "y", "type"))
  if (length(env_vars) == 0) {
    stop("No environmental variables found in 'candidate_sites'", call. = FALSE)
  }

  missing_env_vars <- setdiff(env_vars, names(target_sites))
  if (length(missing_env_vars) > 0) {
    target_env <- as.data.frame(terra::extract(rasters, target_sites[, c("x", "y")], ID = FALSE))
    target_sites <- cbind(target_sites, target_env)
  }

  if (!is.null(categorical_vars)) {
    categorical_vars <- intersect(categorical_vars, env_vars)
  }

  selected_by_site <- list()
  for (i in seq_len(nrow(target_sites))) {
    site_id <- target_sites$site_id[i]
    target_values <- unlist(target_sites[i, env_vars])

    scores <- ss_alt_similarity(
      target_values, candidate_sites[env_vars],
      categorical_vars = categorical_vars, weights = weights,
      method = method, normalize = normalize, missing_method = missing_method
    )

    selected_by_site[[site_id]] <- ss_alt_rank(
      scores, candidate_sites,
      n_select = n_alternatives, target_site_id = site_id
    )
  }

  alternatives <- do.call(rbind, c(selected_by_site, list(make.row.names = FALSE)))

  out <- list(
    target_sites = target_sites,
    candidate_sites = candidate_sites,
    alternatives = alternatives,
    selected_by_site = selected_by_site,
    method_info = list(
      method = method,
      n_alternatives = n_alternatives,
      n_candidates = nrow(candidate_sites),
      min_distance_buffer = min_distance_buffer,
      candidate_method = candidate_method
    )
  )

  if (!is.null(output_dir)) {
    out$file_paths <- .write_alt_outputs(target_sites, alternatives, output_dir)
  }

  out
}


#' Write Alternative Site Selection Outputs to Disk
#'
#' @param target_sites Data frame of target sites with environmental values.
#' @param alternatives Data frame of selected alternatives for all sites.
#' @param output_dir Character, directory to write outputs to.
#'
#' @return A named list of file paths written.
#'
#' @keywords internal
.write_alt_outputs <- function(target_sites, alternatives, output_dir) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  sites_path <- file.path(output_dir, "similarity_analysis_sites.csv")
  targets_path <- file.path(output_dir, "similarity_analysis_inaccessible_sites.csv")

  utils::write.csv(alternatives, sites_path, row.names = FALSE)
  utils::write.csv(target_sites, targets_path, row.names = FALSE)

  list(alternatives = sites_path, target_sites = targets_path)
}
