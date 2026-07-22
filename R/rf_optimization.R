#' cLHS + Random Forest Sample Optimization
#'
#' Functions supporting a two-stage sampling design workflow: a
#' conditioned Latin hypercube (cLHS) baseline followed by Random Forest
#' simulated-annealing refinement, following Wadoux et al. (2019).
#'
#' @name rf_optimization
#' @keywords internal
NULL


#' Convert a Raster Stack to a Covariate Data Frame
#'
#' @param covariates A `SpatRaster` stack of environmental covariates.
#'
#' @return A data frame with `x`, `y` coordinate columns followed by one
#'   column per covariate layer, with incomplete rows removed.
#'
#' @keywords internal
.raster_to_covariate_df <- function(covariates) {
  cov_df <- as.data.frame(covariates, xy = TRUE, na.rm = TRUE)
  na.omit(cov_df)
}


#' Prepare Covariate Data for cLHS
#'
#' Drops coordinate columns, coerces to numeric, and keeps only columns
#' with non-negligible variance (cLHS cannot use constant covariates).
#'
#' @param cov_df Data frame as returned by [.raster_to_covariate_df()],
#'   with `x`, `y` as the first two columns.
#'
#' @return A data frame of numeric covariates suitable for [clhs::clhs()].
#'
#' @keywords internal
.build_clhs_data <- function(cov_df) {
  clhs_data <- cov_df[, -c(1, 2), drop = FALSE]
  names(clhs_data) <- make.names(names(clhs_data))
  clhs_data <- as.data.frame(lapply(clhs_data, function(x) as.numeric(as.character(x))))

  valid_cols <- vapply(clhs_data, function(x) {
    if (all(is.na(x))) {
      return(FALSE)
    }
    var_val <- stats::var(x, na.rm = TRUE)
    !is.na(var_val) && var_val > 1e-10
  }, logical(1))

  clhs_data[, valid_cols, drop = FALSE]
}


#' Select cLHS Sample Row Indices
#'
#' @param cov_df Data frame as returned by [.raster_to_covariate_df()].
#' @param n_samples Integer, number of samples to select.
#' @param seed Optional integer seed for reproducibility.
#'
#' @return Integer vector of row indices into `cov_df`.
#'
#' @keywords internal
.select_clhs_indices <- function(cov_df, n_samples, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  clhs_data <- .build_clhs_data(cov_df)
  if (ncol(clhs_data) == 0) {
    stop("No valid covariates available for cLHS", call. = FALSE)
  }

  clhs::clhs(clhs_data, size = n_samples, progress = FALSE)
}


#' Conditioned Latin Hypercube Sampling From a Raster Stack
#'
#' Selects a representative baseline sample from a covariate raster stack
#' using conditioned Latin hypercube sampling (cLHS). Typically used as the
#' starting design for [ss_rf_optimize()].
#'
#' @param covariates A `SpatRaster` stack of environmental covariates.
#' @param n_samples Integer, number of samples to select.
#' @param seed Optional integer seed for reproducibility.
#'
#' @return A data frame with `x`, `y` and covariate columns for the
#'   selected sample locations.
#'
#' @examples
#' \dontrun{
#' r <- terra::rast("data/predictors.tif")
#' samples <- ss_clhs_sample(r, n_samples = 100, seed = 123)
#' }
#'
#' @seealso [ss_rf_optimize()], [ss_rf_size()]
#' @export
ss_clhs_sample <- function(covariates, n_samples, seed = NULL) {
  cov_df <- .raster_to_covariate_df(covariates)
  indices <- .select_clhs_indices(cov_df, n_samples, seed)
  cov_df[indices, , drop = FALSE]
}


#' Cross-Validated Random Forest MSE for a Sample Design
#'
#' Trains a Random Forest with k-fold cross-validation on a candidate
#' sample design and returns the mean squared error for a chosen response
#' covariate. Used as the objective function for [ss_rf_optimize()].
#'
#' @param sample_points Data frame with `x`, `y` as the first two columns
#'   followed by covariate columns (as returned by [ss_clhs_sample()]).
#' @param full_data Optional data frame of the full covariate population;
#'   if supplied, `sample_points` is restricted to the shared columns.
#' @param n_folds Integer, number of cross-validation folds. Default `5`.
#' @param target_var Character, name of the covariate to predict. If
#'   `NULL` (default), the third covariate column is used, or the first
#'   if fewer than three are available.
#'
#' @return Numeric, the mean cross-validated MSE, or `NA_real_` if the
#'   design has too few rows/columns or every fold fails.
#'
#' @examples
#' \dontrun{
#' samples <- ss_clhs_sample(r, n_samples = 100, seed = 1)
#' ss_rf_mse(samples)
#' }
#'
#' @seealso [ss_rf_optimize()]
#' @export
ss_rf_mse <- function(sample_points, full_data = NULL, n_folds = 5, target_var = NULL) {
  sample_data <- sample_points[, -c(1, 2), drop = FALSE]

  if (!is.null(full_data)) {
    shared_cols <- intersect(names(sample_data), names(full_data))
    if (length(shared_cols) > 0) {
      sample_data <- sample_data[, shared_cols, drop = FALSE]
    }
  }

  sample_data <- sample_data[, vapply(sample_data, function(col) !all(is.na(col)), logical(1)), drop = FALSE]

  if (nrow(sample_data) < 2 || ncol(sample_data) < 2) {
    return(NA_real_)
  }

  if (!is.null(target_var) && target_var %in% names(sample_data)) {
    response <- target_var
  } else if (ncol(sample_data) >= 3) {
    response <- names(sample_data)[3]
  } else {
    response <- names(sample_data)[1]
  }

  predictors <- setdiff(names(sample_data), response)
  if (length(predictors) == 0) {
    return(NA_real_)
  }

  n_samples <- nrow(sample_data)
  n_folds <- max(1, min(n_folds, n_samples))
  fold_ids <- sample(rep(seq_len(n_folds), length.out = n_samples))

  mse_values <- numeric(n_folds)
  for (fold in seq_len(n_folds)) {
    test_indices <- which(fold_ids == fold)
    train_indices <- setdiff(seq_len(n_samples), test_indices)

    if (length(test_indices) == 0 || length(train_indices) == 0) {
      next
    }

    train_data <- sample_data[train_indices, , drop = FALSE]
    test_data <- sample_data[test_indices, , drop = FALSE]

    if (anyNA(train_data) || anyNA(test_data)) {
      next
    }

    rf_model <- randomForest::randomForest(
      stats::as.formula(paste(response, "~", paste(predictors, collapse = "+"))),
      data = train_data, ntree = 100
    )

    predictions <- stats::predict(rf_model, test_data)
    mse_values[fold] <- mean((test_data[[response]] - predictions)^2, na.rm = TRUE)
  }

  mse_values <- mse_values[is.finite(mse_values)]
  if (length(mse_values) == 0) {
    return(NA_real_)
  }
  mean(mse_values)
}


#' Optimize a Sample Design With Random Forest Simulated Annealing
#'
#' Starts from a cLHS baseline and iteratively swaps sample points using
#' simulated annealing to minimize cross-validated Random Forest MSE
#' ([ss_rf_mse()]), following Wadoux et al. (2019).
#'
#' @param covariates A `SpatRaster` stack of environmental covariates.
#' @param n_samples Integer, number of samples in the design.
#' @param n_iterations Integer, number of simulated annealing iterations.
#'   Default `500`.
#' @param seed Optional integer seed. The cLHS baseline uses `seed`; the
#'   annealing loop uses `seed + 1`, so the two stages stay reproducible
#'   but independent.
#' @param temperature Numeric, initial annealing temperature. Default `1000`.
#' @param cooling_rate Numeric in (0, 1), multiplicative cooling factor
#'   applied each iteration. Default `0.95`.
#' @param target_var Character, covariate to use as the RF response.
#'   Passed to [ss_rf_mse()].
#'
#' @return A list with:
#'   \describe{
#'     \item{optimized_samples}{Data frame, the best design found.}
#'     \item{initial_samples}{Data frame, the cLHS baseline design.}
#'     \item{initial_mse}{Numeric, baseline MSE.}
#'     \item{final_mse}{Numeric, MSE of `optimized_samples`.}
#'     \item{improvement}{Numeric, percent MSE reduction vs. baseline.}
#'   }
#'
#' @examples
#' \dontrun{
#' r <- terra::rast("data/predictors.tif")
#' res <- ss_rf_optimize(r, n_samples = 100, n_iterations = 500, seed = 123)
#' res$improvement
#' }
#'
#' @seealso [ss_clhs_sample()], [ss_rf_mse()], [ss_rf_size()]
#' @export
ss_rf_optimize <- function(covariates, n_samples, n_iterations = 500,
                            seed = NULL, temperature = 1000,
                            cooling_rate = 0.95, target_var = NULL) {
  if (!is.null(seed)) {
    set.seed(seed + 1)
  }

  cov_df <- .raster_to_covariate_df(covariates)
  initial_indices <- .select_clhs_indices(cov_df, n_samples, seed)
  current_sample <- cov_df[initial_indices, , drop = FALSE]
  best_sample <- current_sample

  current_mse <- ss_rf_mse(current_sample, cov_df, target_var = target_var)
  best_mse <- current_mse
  initial_mse <- current_mse

  for (i in seq_len(n_iterations)) {
    candidate_sample <- current_sample
    replace_idx <- sample(nrow(candidate_sample), 1)

    occupied_indices <- match(
      paste(candidate_sample$x, candidate_sample$y),
      paste(cov_df$x, cov_df$y)
    )
    occupied_indices <- occupied_indices[!is.na(occupied_indices)]
    remaining_points <- setdiff(seq_len(nrow(cov_df)), occupied_indices)

    if (length(remaining_points) == 0) {
      break
    }

    new_point_idx <- sample(remaining_points, 1)
    candidate_sample[replace_idx, ] <- cov_df[new_point_idx, ]

    candidate_mse <- ss_rf_mse(candidate_sample, cov_df, target_var = target_var)

    delta <- candidate_mse - current_mse
    if (delta < 0 || stats::runif(1) < exp(-delta / temperature)) {
      current_sample <- candidate_sample
      current_mse <- candidate_mse

      if (!is.na(current_mse) && (is.na(best_mse) || current_mse < best_mse)) {
        best_sample <- current_sample
        best_mse <- current_mse
      }
    }

    temperature <- temperature * cooling_rate
  }

  improvement <- if (is.na(initial_mse) || is.na(best_mse)) {
    NA_real_
  } else {
    ((initial_mse - best_mse) / initial_mse) * 100
  }

  list(
    optimized_samples = best_sample,
    initial_samples = cov_df[initial_indices, , drop = FALSE],
    initial_mse = initial_mse,
    final_mse = best_mse,
    improvement = improvement
  )
}
