#' Create Spatial Strata for Soil Sampling
#'
#' Partitions a study area into compact geographical strata using k-means
#' clustering. This is the foundation for spatial coverage sampling.
#'
#' @param x An `sf` object representing the study area. Can be a polygon
#'   (POLYGON or MULTIPOLYGON) or point geometry (POINT or MULTIPOINT).
#' @param n_strata Integer, the number of strata to create.
#' @param prior_points Optional `sf` object with point geometry containing
#'   locations that should be used as fixed cluster centers.
#' @param max_iter Integer, maximum number of iterations for the k-means
#'   algorithm (default 1000).
#' @param n_try Integer, number of random initial configurations to try
#'   (default 1). Higher values reduce risk of suboptimal local minima.
#' @param n_cells Integer, approximate number of grid cells to use when
#'   discretizing a polygon (default 2500).
#' @param cell_size Numeric vector of length 1 or 2, cell size for
#'   discretization. If provided, `n_cells` is ignored.
#' @param equal_area Logical, if `TRUE`, use the swopping algorithm to
#'   create strata of equal area (default `FALSE`). Cannot be used with
#'   prior points.
#' @param verbose Logical, whether to print progress messages (default `FALSE`).
#'
#' @return An object of class `ss_strata` containing:
#'   \describe{
#'     \item{cells}{An `sf` object with the grid cells and their stratum assignments.}
#'     \item{centroids}{An `sf` object with the stratum centroids.}
#'     \item{cell_size}{Numeric vector with cell dimensions.}
#'     \item{n_strata}{Integer, number of strata.}
#'     \item{mssd}{Numeric, mean squared shortest distance (objective function).}
#'     \item{converged}{Logical, whether the algorithm converged.}
#'     \item{prior_points}{The prior points if provided, otherwise NULL.}
#'     \item{equal_area}{Logical, whether equal-area stratification was used.}
#'     \item{crs}{The coordinate reference system.}
#'   }
#'
#' @details
#' The function partitions the study area into compact spatial strata by
#' minimizing the mean squared shortest distance (MSSD) between grid cells
#' and their assigned cluster centroids.
#'
#' Two algorithms are available:
#' \itemize{
#'   \item **Transfer algorithm** (default): Standard k-means, creates compact
#'     strata of potentially unequal sizes.
#'   \item **Swop algorithm** (`equal_area = TRUE`): Creates compact strata
#'     of equal size by swapping cells between clusters.
#' }
#'
#' @examples
#' \dontrun{
#' library(sf)
#'
#' # Create a study area
#' poly <- st_polygon(list(rbind(c(0, 0), c(100, 0), c(100, 50), c(0, 50), c(0, 0))))
#' study_area <- st_sf(geometry = st_sfc(poly))
#'
#' # Create 20 strata
#' strata <- ss_stratify(study_area, n_strata = 20, n_try = 5)
#' plot(strata)
#' }
#'
#' @seealso [ss_coverage()], [ss_plot()]
#' @export
ss_stratify <- function(x, n_strata, prior_points = NULL,
                        max_iter = 1000L, n_try = 1L,
                        n_cells = 2500L, cell_size = NULL,
                        equal_area = FALSE, verbose = FALSE) {
  # Validate inputs
  if (!inherits(x, c("sf", "sfc"))) {
    stop("'x' must be an sf or sfc object", call. = FALSE)
  }

  if (length(n_strata) != 1L || n_strata < 1L) {
    stop("'n_strata' must be a positive integer of length 1", call. = FALSE)
  }
  n_strata <- as.integer(n_strata)

  # Get geometry info

  geom <- st_geometry(x)
  geom_type <- as.character(st_geometry_type(geom, by_geometry = FALSE))
  crs <- st_crs(x)

  # Check for lat/lon
  is_latlon <- !is.na(crs) && isTRUE(st_is_longlat(x))
  if (is_latlon && equal_area) {
    stop("Equal-area strata are not supported for lat/lon coordinates",
      call. = FALSE
    )
  }

  # Process prior points
  has_prior <- !is.null(prior_points)
  if (has_prior) {
    if (equal_area) {
      stop("'equal_area' must be FALSE when using prior points", call. = FALSE)
    }

    if (!inherits(prior_points, c("sf", "sfc"))) {
      stop("'prior_points' must be an sf or sfc object", call. = FALSE)
    }

    prior_type <- as.character(st_geometry_type(st_geometry(prior_points),
      by_geometry = FALSE
    ))
    if (!prior_type %in% c("POINT", "MULTIPOINT")) {
      stop("'prior_points' must have point geometry", call. = FALSE)
    }
  }

  # Convert polygon to grid if necessary
  cell_size_out <- c(NA_real_, NA_real_)

  if (geom_type %in% c("POLYGON", "MULTIPOLYGON")) {
    bbox <- st_bbox(geom)

    if (is.null(cell_size)) {
      area <- as.numeric((bbox["xmax"] - bbox["xmin"]) *
        (bbox["ymax"] - bbox["ymin"]))
      cell_area <- area / n_cells
      cs <- sqrt(cell_area)
      cell_size <- c(cs, cs)
    } else {
      if (length(cell_size) == 1) {
        cell_size <- c(cell_size, cell_size)
      }
    }
    cell_size_out <- cell_size

    # Create regular grid
    grid <- st_make_grid(geom, cellsize = cell_size, what = "centers")

    # Keep only points inside the polygon
    inside <- st_intersects(grid, st_union(geom), sparse = FALSE)[, 1]
    grid <- grid[inside]

    if (length(grid) == 0) {
      stop("No grid cells inside the study area. Try reducing 'cell_size'.",
        call. = FALSE
      )
    }

    cell_coords <- st_coordinates(grid)
  } else if (geom_type %in% c("POINT", "MULTIPOINT")) {
    cell_coords <- st_coordinates(geom)
    grid <- geom
  } else {
    stop("Unsupported geometry type: ", geom_type, call. = FALSE)
  }

  n_grid_cells <- nrow(cell_coords)
  if (n_grid_cells < n_strata) {
    stop("Study area has insufficient cells (", n_grid_cells,
      ") for ", n_strata, " strata",
      call. = FALSE
    )
  }

  # Process prior points if present
  prior_coords <- NULL
  if (has_prior) {
    prior_geom <- st_geometry(prior_points)
    prior_coords <- st_coordinates(prior_geom)

    # Create sf for intersection checks
    prior_sf <- st_as_sf(as.data.frame(prior_coords),
      coords = c("X", "Y"), crs = crs
    )
    grid_sf <- st_as_sf(as.data.frame(cell_coords),
      coords = c("X", "Y"), crs = crs
    )

    # Check which prior points are inside study area
    if (geom_type %in% c("POLYGON", "MULTIPOLYGON")) {
      inside <- st_intersects(prior_sf, st_union(geom), sparse = FALSE)[, 1]
    } else {
      inside <- rep(TRUE, nrow(prior_coords))
    }

    # Remove coinciding points
    nearest_cell <- st_nearest_feature(prior_sf, grid_sf)
    is_coinciding <- duplicated(nearest_cell)

    keep <- inside & !is_coinciding
    if (any(!keep)) {
      if (any(!inside)) {
        warning(sum(!inside), " prior point(s) outside study area removed",
          call. = FALSE
        )
      }
      if (any(is_coinciding[inside])) {
        warning("Coinciding prior points removed", call. = FALSE)
      }
    }

    prior_coords <- prior_coords[keep, , drop = FALSE]
    prior_points <- prior_points[keep, ]

    n_prior <- nrow(prior_coords)
    if (n_strata < n_prior) {
      stop("'n_strata' must be >= number of prior points (", n_prior, ")",
        call. = FALSE
      )
    }
  }

  # Run k-means
  result <- .kmeans_coverage(
    cell_coords = cell_coords,
    n_strata = n_strata,
    prior_coords = prior_coords,
    max_iter = max_iter,
    n_try = n_try,
    equal_area = equal_area,
    is_latlon = is_latlon,
    verbose = verbose
  )

  # Create output sf objects
  cells_df <- data.frame(
    X = cell_coords[, 1],
    Y = cell_coords[, 2],
    stratum_id = result$cluster_id + 1L
  )
  cells_sf <- st_as_sf(cells_df, coords = c("X", "Y"), crs = crs)

  centroids_df <- data.frame(
    X = result$centroids[, 1],
    Y = result$centroids[, 2],
    stratum_id = seq_len(n_strata)
  )
  centroids_sf <- st_as_sf(centroids_df, coords = c("X", "Y"), crs = crs)

  # Create output object
  out <- list(
    cells = cells_sf,
    centroids = centroids_sf,
    cell_size = cell_size_out,
    n_strata = n_strata,
    mssd = result$mssd,
    converged = result$converged,
    prior_points = if (has_prior) prior_points else NULL,
    equal_area = equal_area,
    crs = crs
  )

  class(out) <- "ss_strata"
  out
}


#' Print Method for ss_strata Objects
#'
#' @param x An object of class `ss_strata`
#' @param ... Additional arguments (ignored)
#' @export
print.ss_strata <- function(x, ...) {
  cat("Soil Sampling Stratification\n")
  cat("============================\n")

  cat("Number of strata:", x$n_strata, "\n")
  cat("Number of cells:", nrow(x$cells), "\n")

  if (!all(is.na(x$cell_size))) {
    cat("Cell size:", paste(round(x$cell_size, 2), collapse = " x "), "\n")
  }

  cat("MSSD:", format(x$mssd, digits = 6), "\n")
  cat("Converged:", x$converged, "\n")
  cat("Equal area:", x$equal_area, "\n")

  if (!is.null(x$prior_points)) {
    cat("Prior points:", nrow(x$prior_points), "\n")
  }

  if (!is.na(x$crs)) {
    cat("CRS:", x$crs$input, "\n")
  }

  invisible(x)
}


#' Summary Method for ss_strata Objects
#'
#' @param object An object of class `ss_strata`
#' @param ... Additional arguments (ignored)
#' @export
summary.ss_strata <- function(object, ...) {
  cat("Soil Sampling Stratification Summary\n")
  cat("=====================================\n\n")

  cat("Strata:\n")
  cat("  Number of strata:", object$n_strata, "\n")
  cat("  Total cells:", nrow(object$cells), "\n")
  cat("  Cells per stratum (mean):", round(nrow(object$cells) / object$n_strata, 1), "\n")

  counts <- table(object$cells$stratum_id)
  cat("  Cells per stratum (range):", min(counts), "-", max(counts), "\n")

  cat("\nAlgorithm:\n")
  cat("  Equal area:", object$equal_area, "\n")
  cat("  Converged:", object$converged, "\n")
  cat("  MSSD:", format(object$mssd, digits = 6), "\n")

  if (!all(is.na(object$cell_size))) {
    total_area <- nrow(object$cells) * prod(object$cell_size)
    cat("\nArea:\n")
    cat("  Cell size:", paste(round(object$cell_size, 2), collapse = " x "), "\n")
    cat("  Total area:", round(total_area, 2), "\n")
  }

  invisible(object)
}


#' Get Number of Strata
#'
#' @param x An object of class `ss_strata`
#' @return Integer, the number of strata
#' @export
ss_n_strata <- function(x) {
  if (!inherits(x, "ss_strata")) {
    stop("'x' must be an ss_strata object", call. = FALSE)
  }
  x$n_strata
}


#' Get Area of Each Stratum
#'
#' @param x An object of class `ss_strata`
#' @return Named numeric vector with area of each stratum
#' @export
ss_area <- function(x) {
  if (!inherits(x, "ss_strata")) {
    stop("'x' must be an ss_strata object", call. = FALSE)
  }

  if (all(is.na(x$cell_size))) {
    warning("Cell size not available; returning cell counts", call. = FALSE)
    counts <- table(x$cells$stratum_id)
    return(as.numeric(counts))
  }

  cell_area <- prod(x$cell_size)
  counts <- table(x$cells$stratum_id)
  area <- as.numeric(counts) * cell_area
  names(area) <- names(counts)
  area
}


#' Get Relative Area of Each Stratum
#'
#' @param x An object of class `ss_strata`
#' @return Named numeric vector with relative area (proportion) of each stratum
#' @export
ss_relative_area <- function(x) {
  if (!inherits(x, "ss_strata")) {
    stop("'x' must be an ss_strata object", call. = FALSE)
  }

  counts <- table(x$cells$stratum_id)
  rel_area <- as.numeric(counts) / sum(counts)
  names(rel_area) <- names(counts)
  rel_area
}
