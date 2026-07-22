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
