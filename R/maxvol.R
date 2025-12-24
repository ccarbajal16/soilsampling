#' Maxvol Optimal Design Sampling
#'
#' Generates sampling points using the maxvol algorithm, which selects
#' locations by maximizing the determinant (volume) of a feature submatrix.
#' This is a D-optimal design approach that selects points with the most
#' diverse feature characteristics.
#'
#' @param x An `sf` object representing the study area with feature attributes,
#'   OR a numeric matrix of features (rows = locations, columns = features).
#' @param n Integer, the number of sampling points to select.
#' @param features Character vector of feature names to use from `x` if it's
#'   an sf object. If NULL, all numeric attributes are used.
#' @param coords Matrix (n_locations x 2) of coordinates. Required if `x` is
#'   a matrix. If `x` is sf, coordinates are extracted automatically.
#' @param min_dist Numeric, minimum distance between sampling points. If NULL,
#'   no distance constraint is applied.
#' @param normalize Logical, whether to normalize features before applying
#'   maxvol (default TRUE). Recommended when features have different scales.
#' @param add_coords Logical, whether to add coordinates as features (default
#'   TRUE). This helps balance feature space and geographic space.
#' @param tol Numeric, convergence tolerance for maxvol algorithm (default 1.1).
#' @param max_iters Integer, maximum iterations for maxvol (default 100).
#' @param verbose Logical, whether to print progress messages (default FALSE).
#' @param seed Optional integer seed for reproducibility.
#'
#' @return An object of class `ss_samples` containing:
#'   \describe{
#'     \item{samples}{An `sf` object with the sampling points.}
#'     \item{method}{Character, "maxvol".}
#'     \item{n_samples}{Integer, number of samples.}
#'     \item{features_used}{Character vector of feature names used.}
#'     \item{converged}{Logical, whether maxvol converged.}
#'     \item{iterations}{Integer, number of maxvol iterations.}
#'     \item{crs}{The coordinate reference system.}
#'   }
#'
#' @details
#' The maxvol algorithm selects sampling locations by finding a submatrix
#' of the feature matrix with approximately maximum determinant (volume).
#' Geometrically, this maximizes the volume of the parallelepiped spanned
#' by the selected feature vectors, ensuring that sampled locations have
#' maximal diversity in feature space.
#'
#' The algorithm is based on D-optimal experimental design and is particularly
#' effective when:
#' \itemize{
#'   \item Features explain the main soil-forming factors
#'   \item You want deterministic (non-random) point selection
#'   \item You need optimal coverage with few samples
#' }
#'
#' **Feature Selection:**
#' Terrain features typically used include:
#' \itemize{
#'   \item Elevation
#'   \item Slope
#'   \item Aspect
#'   \item Topographic Wetness Index (TWI)
#'   \item Closed depressions
#'   \item Flow accumulation
#' }
#'
#' **Distance Constraint:**
#' The `min_dist` parameter prevents spatial clustering. Points closer than
#' this distance will not be selected together. The value should be chosen
#' based on:
#' \itemize{
#'   \item Study area size
#'   \item Terrain ruggedness (more rugged = smaller min_dist)
#'   \item Typical size of soil mapping units
#' }
#'
#' @examples
#' \dontrun{
#' library(sf)
#'
#' # Create a study area with feature attributes
#' poly <- st_polygon(list(rbind(c(0, 0), c(100, 0), c(100, 50), c(0, 50), c(0, 0))))
#' study_area <- st_sf(geometry = st_sfc(poly))
#'
#' # In practice, you would load terrain features from a DEM
#' # For this example, we'll use stratification cells with computed features
#' strata <- ss_stratify(study_area, n_strata = 100, n_try = 3)
#'
#' # Add terrain-like features to cells
#' cells_sf <- strata$cells
#' coords <- st_coordinates(cells_sf)
#' cells_sf$elevation <- coords[,2] + rnorm(nrow(coords), 0, 5)
#' cells_sf$slope <- abs(rnorm(nrow(coords), 5, 2))
#'
#' # Select 20 sampling points using maxvol
#' samples <- ss_maxvol(
#'   cells_sf,
#'   n = 20,
#'   features = c("elevation", "slope"),
#'   min_dist = 5,
#'   normalize = TRUE,
#'   add_coords = TRUE
#' )
#'
#' # Plot results
#' ss_plot_samples(samples)
#' }
#'
#' @references
#' Petrovskaia, A., Ryzhakov, G., & Oseledets, I. (2021). Optimal soil
#' sampling design based on the maxvol algorithm. Geoderma, 383, 114733.
#' \doi{10.1016/j.geoderma.2020.114733}
#'
#' Goreinov, S. A., Oseledets, I. V., Savostyanov, D. V., Tyrtyshnikov, E. E.,
#' & Zamarashkin, N. L. (2010). How to find a good submatrix. In Matrix
#' Methods: Theory, Algorithms And Applications (pp. 247-256). World Scientific.
#'
#' @seealso [ss_coverage()], [ss_stratified()], [ss_random()]
#' @export
ss_maxvol <- function(x, n, features = NULL, coords = NULL,
                      min_dist = NULL, normalize = TRUE,
                      add_coords = TRUE, tol = 1.1, max_iters = 100,
                      verbose = FALSE, seed = NULL) {

  # Set seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Extract features and coordinates
  if (inherits(x, c("sf", "sfc"))) {
    crs <- st_crs(x)

    # Get coordinates
    coords_mat <- st_coordinates(x)
    if (ncol(coords_mat) > 2) {
      coords_mat <- coords_mat[, 1:2, drop = FALSE]
    }

    # Get features
    x_df <- st_drop_geometry(x)

    if (is.null(features)) {
      # Use all numeric columns
      numeric_cols <- sapply(x_df, is.numeric)
      if (sum(numeric_cols) == 0) {
        stop("No numeric features found in sf object", call. = FALSE)
      }
      features <- names(x_df)[numeric_cols]
    }

    feature_mat <- as.matrix(x_df[, features, drop = FALSE])
    features_used <- features

  } else if (is.matrix(x) || is.data.frame(x)) {
    # x is a feature matrix
    feature_mat <- as.matrix(x)
    features_used <- colnames(feature_mat)

    if (is.null(coords)) {
      stop("'coords' must be provided when x is a matrix", call. = FALSE)
    }

    coords_mat <- as.matrix(coords)
    crs <- NA

  } else {
    stop("'x' must be an sf object or a matrix", call. = FALSE)
  }

  # Add coordinates as features if requested (before validation)
  if (add_coords) {
    feature_mat <- cbind(feature_mat, coords_mat)
    features_used <- c(features_used, "X", "Y")
    if (verbose) {
      message("Added coordinates as features")
    }
  }

  # Validate inputs (after adding coordinates)
  m <- nrow(feature_mat)
  n_features <- ncol(feature_mat)

  if (n < n_features) {
    stop(
      "'n' must be >= number of features (", n_features, ")",
      call. = FALSE
    )
  }

  if (n > m) {
    stop(
      "'n' (", n, ") cannot exceed number of locations (", m, ")",
      call. = FALSE
    )
  }

  # Normalize features if requested
  if (normalize) {
    norm_result <- .normalize_features(feature_mat)
    feature_mat <- norm_result$X_norm
    if (verbose) {
      message("Normalized features")
    }
  }

  # Run rect_maxvol algorithm
  if (verbose) {
    message("Running maxvol algorithm...")
  }

  maxvol_result <- .rect_maxvol(
    A = feature_mat,
    k = n,
    tol = tol,
    max_iters = max_iters,
    dist_coords = if (!is.null(min_dist)) coords_mat else NULL,
    min_dist = min_dist
  )

  selected_indices <- maxvol_result$index

  if (verbose) {
    message(
      "Maxvol ", if (maxvol_result$converged) "converged" else "did not converge",
      " in ", maxvol_result$iterations, " iterations"
    )
  }

  # Create samples sf object
  if (inherits(x, c("sf", "sfc"))) {
    samples_sf <- x[selected_indices, ]
    samples_sf$sample_id <- seq_len(n)
  } else {
    # Create sf from coordinates
    samples_df <- data.frame(
      sample_id = seq_len(n),
      X = coords_mat[selected_indices, 1],
      Y = coords_mat[selected_indices, 2]
    )
    samples_sf <- st_as_sf(samples_df, coords = c("X", "Y"), crs = crs)
  }

  # Create output object
  out <- list(
    samples = samples_sf,
    method = "maxvol",
    n_samples = n,
    features_used = features_used,
    converged = maxvol_result$converged,
    iterations = maxvol_result$iterations,
    crs = crs,
    strata = NULL
  )

  class(out) <- "ss_samples"
  out
}


#' Summary Method for Maxvol Sampling Results
#'
#' @param object An object of class `ss_samples` from `ss_maxvol()`
#' @param ... Additional arguments (ignored)
#' @export
ss_summary.maxvol <- function(object, ...) {
  if (object$method != "maxvol") {
    return(NextMethod())
  }

  cat("Maxvol Optimal Design Sampling - Summary\n")
  cat("=========================================\n\n")

  cat("Samples:\n")
  cat("  Total samples:", object$n_samples, "\n")

  coords <- st_coordinates(object$samples)
  cat(
    "  X range:", round(min(coords[, 1]), 2), "-",
    round(max(coords[, 1]), 2), "\n"
  )
  cat(
    "  Y range:", round(min(coords[, 2]), 2), "-",
    round(max(coords[, 2]), 2), "\n"
  )

  cat("\nMaxvol Algorithm:\n")
  cat("  Features used:", length(object$features_used), "\n")
  cat("  Feature names:", paste(object$features_used, collapse = ", "), "\n")
  cat("  Converged:", object$converged, "\n")
  cat("  Iterations:", object$iterations, "\n")

  invisible(object)
}
