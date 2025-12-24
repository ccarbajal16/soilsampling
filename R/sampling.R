#' Soil Sampling Functions
#'
#' Functions for generating sampling points using different sampling methods.
#'
#' @name sampling
NULL


#' Simple Random Sampling
#'
#' Generates sampling points using simple random sampling within a study area.
#'
#' @param x An `sf` object representing the study area (polygon geometry).
#' @param n Integer, the number of sampling points to generate.
#' @param seed Optional integer seed for reproducibility.
#'
#' @return An object of class `ss_samples` containing:
#'   \describe{
#'     \item{samples}{An `sf` object with the sampling points.}
#'     \item{method}{Character, "simple_random".}
#'     \item{n_samples}{Integer, number of samples.}
#'     \item{crs}{The coordinate reference system.}
#'   }
#'
#' @details
#' Simple random sampling selects locations uniformly at random within the
#' study area. Each location has an equal probability of being selected.
#'
#' @examples
#' \dontrun{
#' library(sf)
#' poly <- st_polygon(list(rbind(c(0, 0), c(100, 0), c(100, 50), c(0, 50), c(0, 0))))
#' study_area <- st_sf(geometry = st_sfc(poly))
#'
#' samples <- ss_random(study_area, n = 20)
#' plot(samples)
#' }
#'
#' @seealso [ss_stratified()], [ss_coverage()]
#' @export
ss_random <- function(x, n, seed = NULL) {
  # Validate inputs
  if (!inherits(x, c("sf", "sfc"))) {
    stop("'x' must be an sf or sfc object", call. = FALSE)
  }

  geom <- st_geometry(x)
  geom_type <- as.character(st_geometry_type(geom, by_geometry = FALSE))

  if (!geom_type %in% c("POLYGON", "MULTIPOLYGON")) {
    stop("'x' must have polygon geometry for simple random sampling",
      call. = FALSE
    )
  }

  if (length(n) != 1L || n < 1L) {
    stop("'n' must be a positive integer", call. = FALSE)
  }
  n <- as.integer(n)

  crs <- st_crs(x)

  # Set seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Generate random points within the polygon
  samples <- st_sample(st_union(geom), size = n, type = "random")

  # Convert to sf with sample IDs
  samples_sf <- st_sf(
    sample_id = seq_len(length(samples)),
    geometry = samples,
    crs = crs
  )

  # Create output object
  out <- list(
    samples = samples_sf,
    method = "simple_random",
    n_samples = length(samples),
    crs = crs,
    strata = NULL
  )

  class(out) <- "ss_samples"
  out
}


#' Stratified Random Sampling
#'
#' Generates sampling points using stratified random sampling, where random
#' samples are taken within each stratum.
#'
#' @param strata An object of class `ss_strata` created by [ss_stratify()].
#' @param n_per_stratum Integer, number of samples per stratum (default 1).
#' @param seed Optional integer seed for reproducibility.
#'
#' @return An object of class `ss_samples` containing:
#'   \describe{
#'     \item{samples}{An `sf` object with sampling points and stratum assignments.}
#'     \item{method}{Character, "stratified_random".}
#'     \item{n_samples}{Integer, total number of samples.}
#'     \item{strata}{The input strata object.}
#'     \item{crs}{The coordinate reference system.}
#'   }
#'
#' @details
#' Stratified random sampling first divides the study area into strata, then
#' takes random samples within each stratum. This ensures spatial coverage
#' while maintaining the randomness required for design-based inference.
#'
#' @examples
#' \dontrun{
#' library(sf)
#' poly <- st_polygon(list(rbind(c(0, 0), c(100, 0), c(100, 50), c(0, 50), c(0, 0))))
#' study_area <- st_sf(geometry = st_sfc(poly))
#'
#' strata <- ss_stratify(study_area, n_strata = 20)
#' samples <- ss_stratified(strata, n_per_stratum = 1)
#' plot(samples)
#' }
#'
#' @seealso [ss_stratify()], [ss_random()], [ss_coverage()]
#' @export
ss_stratified <- function(strata, n_per_stratum = 1L, seed = NULL) {
  # Validate inputs
  if (!inherits(strata, "ss_strata")) {
    stop("'strata' must be an ss_strata object from ss_stratify()", call. = FALSE)
  }

  if (length(n_per_stratum) != 1L || n_per_stratum < 1L) {
    stop("'n_per_stratum' must be a positive integer", call. = FALSE)
  }
  n_per_stratum <- as.integer(n_per_stratum)

  # Set seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }

  cell_coords <- st_coordinates(strata$cells)
  cell_stratum <- strata$cells$stratum_id
  n_strata <- strata$n_strata
  crs <- strata$crs
  cell_size <- strata$cell_size

  # Sample within each stratum
  sample_list <- vector("list", n_strata)

  for (k in seq_len(n_strata)) {
    stratum_cells <- which(cell_stratum == k)
    n_cells_in_stratum <- length(stratum_cells)

    if (n_cells_in_stratum == 0) {
      warning("Stratum ", k, " has no cells", call. = FALSE)
      next
    }

    # Select cells with replacement if needed
    selected_cells <- if (n_cells_in_stratum >= n_per_stratum) {
      sample(stratum_cells, n_per_stratum, replace = TRUE)
    } else {
      sample(stratum_cells, n_per_stratum, replace = TRUE)
    }

    # Get cell centers
    coords <- cell_coords[selected_cells, , drop = FALSE]

    # Add random offset within cell if cell size is available
    if (!all(is.na(cell_size))) {
      offset_x <- runif(n_per_stratum, -0.5, 0.5) * cell_size[1]
      offset_y <- runif(n_per_stratum, -0.5, 0.5) * cell_size[2]
      coords[, 1] <- coords[, 1] + offset_x
      coords[, 2] <- coords[, 2] + offset_y
    }

    sample_list[[k]] <- data.frame(
      X = coords[, 1],
      Y = coords[, 2],
      stratum_id = k
    )
  }

  # Combine all samples
  all_samples <- do.call(rbind, sample_list)
  all_samples$sample_id <- seq_len(nrow(all_samples))

  samples_sf <- st_as_sf(all_samples, coords = c("X", "Y"), crs = crs)

  # Create output object
  out <- list(
    samples = samples_sf,
    method = "stratified_random",
    n_samples = nrow(samples_sf),
    n_per_stratum = n_per_stratum,
    crs = crs,
    strata = strata
  )

  class(out) <- "ss_samples"
  out
}


#' Spatial Coverage Sampling
#'
#' Generates sampling points at the centroids of compact geographical strata.
#' This is purposive sampling designed for optimal spatial coverage.
#'
#' @param x An `sf` object representing the study area, OR an `ss_strata`
#'   object from [ss_stratify()].
#' @param n_strata Integer, number of strata (and samples) to create. Only
#'   used if `x` is an sf object.
#' @param prior_points Optional `sf` object with prior sampling locations.
#' @param n_try Integer, number of random initializations (default 1).
#' @param n_cells Integer, approximate number of grid cells (default 2500).
#' @param verbose Logical, whether to print progress (default FALSE).
#'
#' @return An object of class `ss_samples` containing:
#'   \describe{
#'     \item{samples}{An `sf` object with sampling points at stratum centroids.}
#'     \item{method}{Character, "coverage".}
#'     \item{n_samples}{Integer, number of samples.}
#'     \item{strata}{The stratification object.}
#'     \item{crs}{The coordinate reference system.}
#'   }
#'
#' @details
#' Spatial coverage sampling places sampling points at the centroids of
#' compact geographical strata. This is optimal for model-based inference
#' (kriging) as it provides even spatial coverage.
#'
#' If a centroid falls outside the study area boundary, it is moved to the
#' nearest cell center within the corresponding stratum.
#'
#' @examples
#' \dontrun{
#' library(sf)
#' poly <- st_polygon(list(rbind(c(0, 0), c(100, 0), c(100, 50), c(0, 50), c(0, 0))))
#' study_area <- st_sf(geometry = st_sfc(poly))
#'
#' # Direct usage
#' samples <- ss_coverage(study_area, n_strata = 25, n_try = 5)
#'
#' # Or using pre-computed strata
#' strata <- ss_stratify(study_area, n_strata = 25, n_try = 5)
#' samples <- ss_coverage(strata)
#' }
#'
#' @seealso [ss_stratify()], [ss_random()], [ss_stratified()]
#' @export
ss_coverage <- function(x, n_strata = NULL, prior_points = NULL,
                        n_try = 1L, n_cells = 2500L, verbose = FALSE) {
  # Check if x is already a strata object

  if (inherits(x, "ss_strata")) {
    strata <- x
  } else if (inherits(x, c("sf", "sfc"))) {
    if (is.null(n_strata)) {
      stop("'n_strata' must be specified when 'x' is an sf object",
        call. = FALSE
      )
    }
    strata <- ss_stratify(
      x = x,
      n_strata = n_strata,
      prior_points = prior_points,
      n_try = n_try,
      n_cells = n_cells,
      verbose = verbose
    )
  } else {
    stop("'x' must be an sf object or ss_strata object", call. = FALSE)
  }

  cell_coords <- st_coordinates(strata$cells)
  cell_stratum <- strata$cells$stratum_id
  n_strata <- strata$n_strata
  crs <- strata$crs
  cell_size <- strata$cell_size
  has_prior <- !is.null(strata$prior_points)

  # Get centroids
  centroid_coords <- st_coordinates(strata$centroids)

  # Check if centroids are inside their strata
  for (k in seq_len(n_strata)) {
    stratum_cells <- which(cell_stratum == k)
    stratum_cell_coords <- cell_coords[stratum_cells, , drop = FALSE]

    centroid <- centroid_coords[k, ]
    dists <- (stratum_cell_coords[, 1] - centroid[1])^2 +
      (stratum_cell_coords[, 2] - centroid[2])^2

    nearest_idx <- which.min(dists)
    min_dist <- sqrt(dists[nearest_idx])

    # Move centroid to nearest cell if outside stratum
    if (!all(is.na(cell_size))) {
      cell_diag <- sqrt(sum(cell_size^2))
      if (min_dist > cell_diag) {
        centroid_coords[k, ] <- stratum_cell_coords[nearest_idx, ]
      }
    }
  }

  # Handle prior points
  is_prior <- rep(FALSE, n_strata)
  if (has_prior) {
    prior_coords <- st_coordinates(strata$prior_points)
    n_prior <- nrow(prior_coords)
    centroid_coords[seq_len(n_prior), ] <- prior_coords
    is_prior[seq_len(n_prior)] <- TRUE
  }

  # Create samples sf
  samples_df <- data.frame(
    X = centroid_coords[, 1],
    Y = centroid_coords[, 2],
    sample_id = seq_len(n_strata),
    stratum_id = seq_len(n_strata),
    is_prior = is_prior
  )

  samples_sf <- st_as_sf(samples_df, coords = c("X", "Y"), crs = crs)

  # Create output object
  out <- list(
    samples = samples_sf,
    method = "coverage",
    n_samples = n_strata,
    crs = crs,
    strata = strata
  )

  class(out) <- "ss_samples"
  out
}


#' Spatial Coverage Sampling with Equal-Area Strata
#'
#' Generates sampling points using spatial coverage with equal-area strata.
#' Useful when strata need to have similar sizes.
#'
#' @param x An `sf` object representing the study area.
#' @param n_strata Integer, number of strata to create.
#' @param n_try Integer, number of random initializations (default 1).
#' @param n_cells Integer, approximate number of grid cells (default 2500).
#' @param verbose Logical, whether to print progress (default FALSE).
#'
#' @return An object of class `ss_samples`.
#'
#' @details
#' This function creates strata of approximately equal area using the
#' swopping algorithm, then returns centroid samples.
#'
#' @examples
#' \dontrun{
#' library(sf)
#' poly <- st_polygon(list(rbind(c(0, 0), c(100, 0), c(100, 50), c(0, 50), c(0, 0))))
#' study_area <- st_sf(geometry = st_sfc(poly))
#'
#' samples <- ss_coverage_equal_area(study_area, n_strata = 25)
#' }
#'
#' @seealso [ss_coverage()], [ss_composite()]
#' @export
ss_coverage_equal_area <- function(x, n_strata, n_try = 1L,
                                   n_cells = 2500L, verbose = FALSE) {
  # Create equal-area strata
  strata <- ss_stratify(
    x = x,
    n_strata = n_strata,
    n_try = n_try,
    n_cells = n_cells,
    equal_area = TRUE,
    verbose = verbose
  )

  # Get coverage samples
  result <- ss_coverage(strata)
  result$method <- "coverage_equal_area"

  result
}


#' Composite Sampling
#'
#' Generates sampling points for composite sampling from equal-area strata.
#' Multiple samples per stratum are grouped into composites.
#'
#' @param x An `sf` object representing the study area, OR an `ss_strata`
#'   object with `equal_area = TRUE`.
#' @param n_strata Integer, number of strata. Only used if `x` is an sf object.
#' @param n_composites Integer, number of composite samples to create.
#' @param n_try Integer, number of random initializations (default 1).
#' @param n_cells Integer, approximate number of grid cells (default 2500).
#' @param seed Optional integer seed for reproducibility.
#' @param verbose Logical, whether to print progress (default FALSE).
#'
#' @return An object of class `ss_samples` with a `composite` column
#'   indicating composite group membership.
#'
#' @details
#' Composite sampling is used when individual samples are combined before
#' analysis. This function creates equal-area strata and generates multiple
#' random samples per stratum, grouped into composite samples.
#'
#' Each composite includes one sample from each stratum.
#'
#' @examples
#'
#' library(sf)
#' poly <- st_polygon(list(rbind(c(0, 0), c(100, 0), c(100, 50), c(0, 50), c(0, 0))))
#' study_area <- st_sf(geometry = st_sfc(poly))
#'
#' # Create 3 composite samples from 20 strata
#' samples <- ss_composite(study_area, n_strata = 20, n_composites = 3)
#'
#' @seealso [ss_coverage_equal_area()], [ss_stratified()]
#' @export
ss_composite <- function(x, n_strata = NULL, n_composites,
                         n_try = 1L, n_cells = 2500L,
                         seed = NULL, verbose = FALSE) {
  # Create or validate strata
  if (inherits(x, "ss_strata")) {
    if (!x$equal_area) {
      stop("Composite sampling requires equal-area strata. ",
        "Use ss_stratify() with equal_area = TRUE",
        call. = FALSE
      )
    }
    strata <- x
  } else if (inherits(x, c("sf", "sfc"))) {
    if (is.null(n_strata)) {
      stop("'n_strata' must be specified when 'x' is an sf object",
        call. = FALSE
      )
    }
    strata <- ss_stratify(
      x = x,
      n_strata = n_strata,
      n_try = n_try,
      n_cells = n_cells,
      equal_area = TRUE,
      verbose = verbose
    )
  } else {
    stop("'x' must be an sf object or ss_strata object", call. = FALSE)
  }

  if (length(n_composites) != 1L || n_composites < 1L) {
    stop("'n_composites' must be a positive integer", call. = FALSE)
  }
  n_composites <- as.integer(n_composites)

  # Set seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Get stratified samples with n_composites per stratum
  result <- ss_stratified(strata, n_per_stratum = n_composites)

  # Add composite IDs
  n_strata <- strata$n_strata
  composite_id <- rep(seq_len(n_composites), times = n_strata)
  result$samples$composite <- composite_id

  result$method <- "composite"
  result$n_composites <- n_composites

  result
}


#' Print Method for ss_samples Objects
#'
#' @param x An object of class `ss_samples`
#' @param ... Additional arguments (ignored)
#' @export
print.ss_samples <- function(x, ...) {
  method_names <- c(
    simple_random = "Simple Random Sampling",
    stratified_random = "Stratified Random Sampling",
    coverage = "Spatial Coverage Sampling",
    coverage_equal_area = "Spatial Coverage Sampling (Equal Area)",
    composite = "Composite Sampling"
  )

  cat(method_names[x$method], "\n")
  cat(strrep("=", nchar(method_names[x$method])), "\n")

  cat("Number of samples:", x$n_samples, "\n")

  if (!is.null(x$strata)) {
    cat("Number of strata:", x$strata$n_strata, "\n")
  }

  if (!is.null(x$n_composites)) {
    cat("Number of composites:", x$n_composites, "\n")
  }

  if (!is.null(x$n_per_stratum)) {
    cat("Samples per stratum:", x$n_per_stratum, "\n")
  }

  if (!is.na(x$crs)) {
    cat("CRS:", x$crs$input, "\n")
  }

  invisible(x)
}


#' Summary Method for ss_samples Objects
#'
#' @param object An object of class `ss_samples`
#' @param ... Additional arguments (ignored)
#' @export
summary.ss_samples <- function(object, ...) {
  method_names <- c(
    simple_random = "Simple Random Sampling",
    stratified_random = "Stratified Random Sampling",
    coverage = "Spatial Coverage Sampling",
    coverage_equal_area = "Spatial Coverage Sampling (Equal Area)",
    composite = "Composite Sampling"
  )

  cat(method_names[object$method], " - Summary\n")
  cat(strrep("=", nchar(method_names[object$method]) + 10), "\n\n")

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

  if (!is.null(object$strata)) {
    cat("\nStratification:\n")
    cat("  Number of strata:", object$strata$n_strata, "\n")
    cat("  Equal area:", object$strata$equal_area, "\n")
    cat("  MSSD:", format(object$strata$mssd, digits = 6), "\n")
  }

  if ("is_prior" %in% names(object$samples)) {
    n_prior <- sum(object$samples$is_prior)
    if (n_prior > 0) {
      cat("\nPrior points:", n_prior, "\n")
    }
  }

  invisible(object)
}


#' Get Number of Samples
#'
#' @param x An object of class `ss_samples`
#' @return Integer, the number of samples
#' @export
ss_n_samples <- function(x) {
  if (!inherits(x, "ss_samples")) {
    stop("'x' must be an ss_samples object", call. = FALSE)
  }
  x$n_samples
}


#' Get Samples as sf Object
#'
#' @param x An object of class `ss_samples`
#' @return An `sf` object with the sampling points
#' @export
ss_get_samples <- function(x) {
  if (!inherits(x, "ss_samples")) {
    stop("'x' must be an ss_samples object", call. = FALSE)
  }
  x$samples
}


#' Convert to sf Object
#'
#' @param x An object of class `ss_samples` or `ss_strata`
#' @param what Character, what to extract: "samples", "cells", or "centroids"
#' @return An `sf` object
#' @export
ss_to_sf <- function(x, what = "samples") {
  if (inherits(x, "ss_samples")) {
    if (what == "samples") {
      return(x$samples)
    } else if (what == "strata" || what == "cells") {
      if (is.null(x$strata)) {
        stop("No strata available for this sampling object", call. = FALSE)
      }
      return(x$strata$cells)
    } else if (what == "centroids") {
      if (is.null(x$strata)) {
        stop("No strata available for this sampling object", call. = FALSE)
      }
      return(x$strata$centroids)
    }
  } else if (inherits(x, "ss_strata")) {
    if (what == "cells" || what == "strata") {
      return(x$cells)
    } else if (what == "centroids") {
      return(x$centroids)
    }
  }

  stop("Unknown 'what' argument: ", what, call. = FALSE)
}


#' Convert to Data Frame
#'
#' @param x An object of class `ss_samples` or `ss_strata`
#' @param what Character, what to extract (default "samples")
#' @return A data frame with coordinates
#' @export
ss_to_data_frame <- function(x, what = "samples") {
  sf_obj <- ss_to_sf(x, what = what)
  coords <- st_coordinates(sf_obj)
  df <- st_drop_geometry(sf_obj)
  df$X <- coords[, 1]
  df$Y <- coords[, 2]
  df
}
