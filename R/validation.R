#' Validation Metrics for Soil Sampling
#'
#' Functions for assessing the quality of stratification and sampling designs.
#'
#' @importFrom stats median sd
#' @name validation
NULL


#' Assess Coverage Efficiency of Stratification
#'
#' Evaluates how uniformly cells are distributed across strata using multiple
#' metrics including coefficient of variation (CV), min/max ratio, and Gini
#' coefficient. Lower values indicate more uniform distribution.
#'
#' @param x An object of class `ss_strata`
#' @param digits Number of decimal places for rounding (default: 3)
#'
#' @return An object of class `ss_coverage_efficiency` (list) containing:
#' \describe{
#'   \item{cell_counts}{Integer vector of cells per stratum}
#'   \item{n_strata}{Number of strata}
#'   \item{total_cells}{Total number of cells}
#'   \item{mean_cells}{Mean cells per stratum}
#'   \item{sd_cells}{Standard deviation of cells per stratum}
#'   \item{cv}{Coefficient of variation (sd/mean)}
#'   \item{min_cells}{Minimum cells in any stratum}
#'   \item{max_cells}{Maximum cells in any stratum}
#'   \item{min_max_ratio}{Ratio of min to max (1.0 = perfect equality)}
#'   \item{range_cells}{Range (max - min)}
#'   \item{gini}{Gini coefficient (0 = perfect equality, 1 = inequality)}
#'   \item{efficiency_score}{Qualitative assessment: "Excellent", "Good", "Fair", or "Poor"}
#'   \item{is_equal_area}{Whether equal-area algorithm was used}
#' }
#'
#' @details
#' The function computes several metrics to assess uniformity:
#'
#' **Coefficient of Variation (CV)**: Ratio of standard deviation to mean.
#' Lower values indicate more uniform distribution. For equal-area stratification,
#' expect CV < 0.05.
#'
#' **Min/Max Ratio**: Ratio of smallest to largest stratum. Value of 1.0 indicates
#' perfect equality. Values > 0.9 are excellent.
#'
#' **Gini Coefficient**: Standard inequality measure from economics. 0 = perfect
#' equality, 1 = maximal inequality. Lower values indicate better uniformity.
#'
#' **Efficiency Score**:
#' \itemize{
#'   \item Excellent: CV < 0.1 and min/max > 0.9
#'   \item Good: CV < 0.2 and min/max > 0.75
#'   \item Fair: CV < 0.35 and min/max > 0.5
#'   \item Poor: Otherwise
#' }
#'
#' @examples
#' \dontrun{
#' library(sf)
#'
#' # Create study area
#' poly <- st_polygon(list(rbind(
#'   c(0, 0), c(100, 0), c(100, 50), c(0, 50), c(0, 0)
#' )))
#' study_area <- st_sf(geometry = st_sfc(poly))
#'
#' # Create stratification
#' set.seed(42)
#' strata <- ss_stratify(study_area, n_strata = 20, n_try = 5)
#'
#' # Assess coverage efficiency
#' efficiency <- ss_coverage_efficiency(strata)
#' print(efficiency)
#'
#' # Compare regular vs equal-area
#' strata_equal <- ss_stratify(study_area, n_strata = 20,
#'                              equal_area = TRUE, n_try = 5)
#' eff_regular <- ss_coverage_efficiency(strata)
#' eff_equal <- ss_coverage_efficiency(strata_equal)
#'
#' cat("Regular CV:", eff_regular$cv, "\n")
#' cat("Equal-area CV:", eff_equal$cv, "\n")
#' }
#'
#' @export
ss_coverage_efficiency <- function(x, digits = 3) {
  # Input validation
  if (!inherits(x, "ss_strata")) {
    stop("'x' must be an ss_strata object", call. = FALSE)
  }

  # Count cells per stratum
  cell_counts <- table(x$cells$stratum_id)
  cell_counts <- as.integer(cell_counts)
  names(cell_counts) <- names(table(x$cells$stratum_id))

  # Basic statistics
  n_strata <- x$n_strata
  total_cells <- sum(cell_counts)
  mean_cells <- mean(cell_counts)
  sd_cells <- sd(cell_counts)
  cv <- sd_cells / mean_cells

  # Range statistics
  min_cells <- min(cell_counts)
  max_cells <- max(cell_counts)
  min_max_ratio <- min_cells / max_cells
  range_cells <- max_cells - min_cells

  # Gini coefficient
  # Formula: G = (2 * sum(i * x_i)) / (n * sum(x_i)) - (n + 1) / n
  sorted_counts <- sort(cell_counts)
  n <- length(sorted_counts)
  gini <- (2 * sum(seq_len(n) * sorted_counts)) / (n * sum(sorted_counts)) - (n + 1) / n

  # Efficiency score (interpretive categorization)
  # Handle edge case where CV is NA (single stratum or zero variance)
  efficiency_score <- if (is.na(cv) || is.na(min_max_ratio)) {
    "Excellent"  # Single stratum or perfect uniformity
  } else if (cv < 0.1 && min_max_ratio > 0.9) {
    "Excellent"
  } else if (cv < 0.2 && min_max_ratio > 0.75) {
    "Good"
  } else if (cv < 0.35 && min_max_ratio > 0.5) {
    "Fair"
  } else {
    "Poor"
  }

  # Create result object
  result <- list(
    cell_counts = cell_counts,
    n_strata = n_strata,
    total_cells = total_cells,
    mean_cells = round(mean_cells, digits),
    sd_cells = round(sd_cells, digits),
    cv = round(cv, digits),
    min_cells = min_cells,
    max_cells = max_cells,
    min_max_ratio = round(min_max_ratio, digits),
    range_cells = range_cells,
    gini = round(gini, digits),
    efficiency_score = efficiency_score,
    is_equal_area = x$equal_area
  )

  class(result) <- "ss_coverage_efficiency"
  result
}


#' Print Method for ss_coverage_efficiency Objects
#'
#' @param x An object of class `ss_coverage_efficiency`
#' @param ... Additional arguments (ignored)
#' @export
print.ss_coverage_efficiency <- function(x, ...) {
  cat("Stratification Coverage Efficiency\n")
  cat("===================================\n\n")

  cat("Number of strata:", x$n_strata, "\n")
  cat("Total cells:", x$total_cells, "\n")
  cat("Mean cells per stratum:", x$mean_cells, "\n")
  cat("Range:", x$min_cells, "-", x$max_cells, "\n\n")

  cat("Uniformity Metrics:\n")
  cat("  CV (coefficient of variation):", x$cv, "\n")
  cat("  Min/Max ratio:", x$min_max_ratio, "\n")
  cat("  Gini coefficient:", x$gini, "\n\n")

  cat("Efficiency Score:", x$efficiency_score, "\n")

  if (x$is_equal_area) {
    cat("\n(Equal-area stratification was used)\n")
  }

  invisible(x)
}


#' Compute Distance Summary Statistics by Stratum
#'
#' Calculates distance statistics from cells or samples to their assigned stratum
#' centroids. Provides mean, standard deviation, minimum, maximum, and median
#' distances for each stratum.
#'
#' @param x An object of class `ss_strata` or `ss_samples`
#' @param actual_distance Logical. If `TRUE`, return actual distances (default: `FALSE`
#'   returns squared distances for consistency with MSSD metric)
#'
#' @return A data frame with one row per stratum containing:
#' \describe{
#'   \item{stratum_id}{Stratum identifier}
#'   \item{n_cells or n_samples}{Number of cells (for ss_strata) or samples (for ss_samples)}
#'   \item{mean_dist}{Mean distance to centroid}
#'   \item{sd_dist}{Standard deviation of distances}
#'   \item{min_dist}{Minimum distance to centroid}
#'   \item{max_dist}{Maximum distance to centroid}
#'   \item{median_dist}{Median distance to centroid}
#' }
#'
#' @details
#' For ss_strata objects, the function computes distances from all grid cells
#' to their assigned stratum centroids. For ss_samples objects, it computes
#' distances from sample points to centroids.
#'
#' The function automatically detects coordinate system type (planar vs lat/lon)
#' and uses appropriate distance calculations:
#' \itemize{
#'   \item Planar coordinates: Euclidean distance
#'   \item Geographic coordinates (lat/lon): Haversine distance (great circle)
#' }
#'
#' By default, squared distances are returned for computational efficiency and
#' consistency with the MSSD (Mean Squared Shortest Distance) metric used in
#' stratification. Set `actual_distance = TRUE` to get actual distances.
#'
#' @examples
#' \dontrun{
#' library(sf)
#'
#' # Create study area and stratification
#' poly <- st_polygon(list(rbind(
#'   c(0, 0), c(100, 0), c(100, 50), c(0, 50), c(0, 0)
#' )))
#' study_area <- st_sf(geometry = st_sfc(poly))
#' set.seed(42)
#' strata <- ss_stratify(study_area, n_strata = 20, n_try = 5)
#'
#' # Distance summary for strata
#' dist_summary <- ss_distance_summary(strata)
#' print(dist_summary)
#'
#' # Distance summary for samples
#' samples <- ss_coverage(strata)
#' dist_samples <- ss_distance_summary(samples, actual_distance = TRUE)
#' print(dist_samples)
#' }
#'
#' @export
ss_distance_summary <- function(x, actual_distance = FALSE) {
  # Determine object type and extract data
  if (inherits(x, "ss_strata")) {
    coords <- sf::st_coordinates(x$cells)
    stratum_ids <- x$cells$stratum_id
    centroid_coords <- sf::st_coordinates(x$centroids)
    crs <- x$crs
    n_strata <- x$n_strata
    object_type <- "cells"
  } else if (inherits(x, "ss_samples")) {
    if (is.null(x$strata)) {
      stop("ss_samples object must have a 'strata' component for distance summary",
           call. = FALSE)
    }
    coords <- sf::st_coordinates(x$samples)
    stratum_ids <- x$samples$stratum_id
    centroid_coords <- sf::st_coordinates(x$strata$centroids)
    crs <- x$strata$crs
    n_strata <- x$strata$n_strata
    object_type <- "samples"
  } else {
    stop("'x' must be an ss_strata or ss_samples object", call. = FALSE)
  }

  # Detect coordinate system type
  is_longlat <- !is.na(crs) && isTRUE(sf::st_is_longlat(
    if (inherits(x, "ss_strata")) x$cells else x$samples
  ))

  # Compute distances for each stratum
  results <- vector("list", n_strata)

  for (k in seq_len(n_strata)) {
    # Get points in this stratum
    stratum_mask <- stratum_ids == k
    stratum_coords <- coords[stratum_mask, , drop = FALSE]
    n_points <- sum(stratum_mask)

    if (n_points == 0) {
      # Empty stratum (shouldn't happen but handle gracefully)
      results[[k]] <- data.frame(
        stratum_id = k,
        n = 0,
        mean_dist = NA_real_,
        sd_dist = NA_real_,
        min_dist = NA_real_,
        max_dist = NA_real_,
        median_dist = NA_real_
      )
      next
    }

    # Get centroid for this stratum
    centroid <- centroid_coords[k, , drop = FALSE]

    # Compute distances
    if (is_longlat) {
      # Use haversine distance
      dists <- .haversine_distance_sq(
        stratum_coords[, 1], stratum_coords[, 2],
        centroid[1], centroid[2]
      )
    } else {
      # Use Euclidean distance
      dists <- (stratum_coords[, 1] - centroid[1])^2 +
               (stratum_coords[, 2] - centroid[2])^2
    }

    # Take sqrt if actual_distance requested
    if (actual_distance) {
      dists <- sqrt(dists)
    }

    # Compute summary statistics
    results[[k]] <- data.frame(
      stratum_id = k,
      n = n_points,
      mean_dist = mean(dists),
      sd_dist = if (n_points > 1) sd(dists) else NA_real_,
      min_dist = min(dists),
      max_dist = max(dists),
      median_dist = median(dists)
    )
  }

  # Combine results
  result_df <- do.call(rbind, results)

  # Add appropriate column name for count
  if (object_type == "cells") {
    names(result_df)[2] <- "n_cells"
  } else {
    names(result_df)[2] <- "n_samples"
  }

  result_df
}
