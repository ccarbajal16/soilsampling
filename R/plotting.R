#' Plotting Functions for Soil Sampling
#'
#' Functions for visualizing stratification and sampling patterns.
#'
#' @name plotting
#' @keywords internal
NULL


#' Plot Stratification
#'
#' Creates a visualization of the spatial stratification.
#'
#' @param x An object of class `ss_strata` or `ss_samples`.
#' @param samples Optional `ss_samples` object to overlay on stratification.
#' @param show_boundaries Logical, whether to show stratum boundaries (default TRUE).
#' @param show_centroids Logical, whether to show stratum centroids (default FALSE).
#' @param fill_color Character, fill color for the study area (default "#80CC80").
#' @param boundary_color Character, color for stratum boundaries (default "#CCE5CC").
#' @param sample_color Character, color for sampling points (default "black").
#' @param sample_alpha Numeric, transparency for sampling points (default 0.7).
#' @param sample_size Numeric, size of sampling points (default 2).
#' @param ... Additional arguments passed to ggplot2 functions.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' \dontrun{
#' library(sf)
#' poly <- st_polygon(list(rbind(c(0, 0), c(100, 0), c(100, 50), c(0, 50), c(0, 0))))
#' study_area <- st_sf(geometry = st_sfc(poly))
#'
#' strata <- ss_stratify(study_area, n_strata = 25)
#' ss_plot(strata)
#'
#' samples <- ss_coverage(strata)
#' ss_plot(strata, samples = samples)
#' }
#'
#' @seealso [ss_plot_samples()]
#' @export
ss_plot <- function(x, samples = NULL,
                    show_boundaries = TRUE,
                    show_centroids = FALSE,
                    fill_color = "#80CC80",
                    boundary_color = "#CCE5CC",
                    sample_color = "black",
                    sample_alpha = 0.7,
                    sample_size = 2,
                    ...) {
  # Extract strata from different object types
  if (inherits(x, "ss_samples")) {
    if (is.null(x$strata)) {
      # No strata available, just plot samples
      return(ss_plot_samples(x,
        color = sample_color, alpha = sample_alpha,
        size = sample_size, ...
      ))
    }
    if (is.null(samples)) {
      samples <- x
    }
    strata <- x$strata
  } else if (inherits(x, "ss_strata")) {
    strata <- x
  } else {
    stop("'x' must be an ss_strata or ss_samples object", call. = FALSE)
  }

  # Get cell coordinates and stratum IDs
  cell_coords <- st_coordinates(strata$cells)
  cell_stratum <- strata$cells$stratum_id
  cell_size <- strata$cell_size

  # Create data frame for plotting
  plot_df <- data.frame(
    x = cell_coords[, 1],
    y = cell_coords[, 2],
    stratum_id = cell_stratum
  )

  # Base plot with raster of cells
  p <- ggplot() +
    geom_raster(
      data = plot_df,
      aes(x = .data$x, y = .data$y),
      fill = fill_color
    ) +
    coord_fixed() +
    theme_minimal() +
    labs(x = "X", y = "Y")

  # Add stratum boundaries if requested
  if (show_boundaries && !all(is.na(cell_size))) {
    boundaries <- .compute_stratum_boundaries(plot_df, cell_size)

    if (nrow(boundaries$horizontal) > 0) {
      p <- p + geom_segment(
        data = boundaries$horizontal,
        aes(
          x = .data$x, y = .data$y,
          xend = .data$xend, yend = .data$yend
        ),
        colour = boundary_color
      )
    }

    if (nrow(boundaries$vertical) > 0) {
      p <- p + geom_segment(
        data = boundaries$vertical,
        aes(
          x = .data$x, y = .data$y,
          xend = .data$xend, yend = .data$yend
        ),
        colour = boundary_color
      )
    }
  }

  # Add centroids if requested
  if (show_centroids) {
    centroid_coords <- st_coordinates(strata$centroids)
    centroid_df <- data.frame(
      x = centroid_coords[, 1],
      y = centroid_coords[, 2]
    )
    p <- p + geom_point(
      data = centroid_df,
      aes(x = .data$x, y = .data$y),
      colour = "red",
      shape = 3,
      size = 2
    )
  }

  # Add sampling points if provided
  if (!is.null(samples)) {
    p <- .add_samples_to_plot(p, samples, sample_color, sample_alpha, sample_size)
  }

  p
}


#' Plot Sampling Points
#'
#' Creates a simple plot of sampling points.
#'
#' @param x An object of class `ss_samples`.
#' @param color Character, point color (default "black").
#' @param alpha Numeric, point transparency (default 0.7).
#' @param size Numeric, point size (default 2).
#' @param ... Additional arguments (currently ignored).
#'
#' @return A `ggplot` object.
#'
#' @examples
#' \dontrun{
#' library(sf)
#' poly <- st_polygon(list(rbind(c(0, 0), c(100, 0), c(100, 50), c(0, 50), c(0, 0))))
#' study_area <- st_sf(geometry = st_sfc(poly))
#'
#' samples <- ss_random(study_area, n = 30)
#' ss_plot_samples(samples)
#' }
#'
#' @seealso [ss_plot()]
#' @export
ss_plot_samples <- function(x, color = "black", alpha = 0.7, size = 2, ...) {
  if (!inherits(x, "ss_samples")) {
    stop("'x' must be an ss_samples object", call. = FALSE)
  }

  sample_coords <- st_coordinates(x$samples)
  sample_df <- data.frame(
    x = sample_coords[, 1],
    y = sample_coords[, 2]
  )

  # Check for special columns
  has_prior <- "is_prior" %in% names(x$samples) && any(x$samples$is_prior)
  has_composite <- "composite" %in% names(x$samples)

  p <- ggplot()

  if (has_prior) {
    sample_df$type <- ifelse(x$samples$is_prior, "Prior point", "New point")
    p <- p + geom_point(
      data = sample_df,
      aes(x = .data$x, y = .data$y, shape = .data$type),
      colour = color,
      alpha = alpha,
      size = size
    ) +
      scale_shape_manual(values = c("Prior point" = 17, "New point" = 16)) +
      labs(shape = "Point type")
  } else if (has_composite) {
    sample_df$composite <- factor(x$samples$composite)
    p <- p + geom_point(
      data = sample_df,
      aes(x = .data$x, y = .data$y, shape = .data$composite),
      colour = color,
      alpha = alpha,
      size = size
    ) +
      labs(shape = "Composite")
  } else {
    p <- p + geom_point(
      data = sample_df,
      aes(x = .data$x, y = .data$y),
      colour = color,
      alpha = alpha,
      size = size
    )
  }

  p <- p +
    coord_fixed() +
    theme_minimal() +
    labs(x = "X", y = "Y")

  p
}


#' Plot Method for ss_strata Objects
#'
#' @param x An object of class `ss_strata`
#' @param ... Additional arguments passed to [ss_plot()]
#' @return A ggplot object
#' @export
plot.ss_strata <- function(x, ...) {
  ss_plot(x, ...)
}


#' Plot Method for ss_samples Objects
#'
#' @param x An object of class `ss_samples`
#' @param ... Additional arguments passed to [ss_plot()] or [ss_plot_samples()]
#' @return A ggplot object
#' @export
plot.ss_samples <- function(x, ...) {
  if (!is.null(x$strata)) {
    ss_plot(x, ...)
  } else {
    ss_plot_samples(x, ...)
  }
}


#' Compute Stratum Boundaries (Internal)
#'
#' Simple edge detection algorithm to find boundaries between strata.
#'
#' @param plot_df Data frame with x, y, stratum_id columns
#' @param cell_size Numeric vector of length 2 with cell size
#' @return A list with horizontal and vertical boundary segments
#' @keywords internal
.compute_stratum_boundaries <- function(plot_df, cell_size) {
  # Find unique coordinates
  x_vals <- sort(unique(plot_df$x))
  y_vals <- sort(unique(plot_df$y))

  n_x <- length(x_vals)
  n_y <- length(y_vals)

  if (n_x == 0 || n_y == 0) {
    return(list(
      horizontal = data.frame(
        x = numeric(), y = numeric(),
        xend = numeric(), yend = numeric()
      ),
      vertical = data.frame(
        x = numeric(), y = numeric(),
        xend = numeric(), yend = numeric()
      )
    ))
  }

  # Create index mappings
  x_idx <- match(plot_df$x, x_vals)
  y_idx <- match(plot_df$y, y_vals)

  # Create matrix with stratum IDs
  M <- matrix(NA_integer_, nrow = n_x, ncol = n_y)
  for (i in seq_len(nrow(plot_df))) {
    M[x_idx[i], y_idx[i]] <- plot_df$stratum_id[i]
  }

  # Detect horizontal boundaries
  horiz_list <- list()
  if (n_y > 1) {
    for (j in 2:n_y) {
      for (i in 1:n_x) {
        if (!is.na(M[i, j]) && !is.na(M[i, j - 1])) {
          if (M[i, j] != M[i, j - 1]) {
            horiz_list <- c(horiz_list, list(data.frame(
              x = x_vals[i] - cell_size[1] / 2,
              y = (y_vals[j] + y_vals[j - 1]) / 2,
              xend = x_vals[i] + cell_size[1] / 2,
              yend = (y_vals[j] + y_vals[j - 1]) / 2
            )))
          }
        }
      }
    }
  }

  # Detect vertical boundaries
  vert_list <- list()
  if (n_x > 1) {
    for (i in 2:n_x) {
      for (j in 1:n_y) {
        if (!is.na(M[i, j]) && !is.na(M[i - 1, j])) {
          if (M[i, j] != M[i - 1, j]) {
            vert_list <- c(vert_list, list(data.frame(
              x = (x_vals[i] + x_vals[i - 1]) / 2,
              y = y_vals[j] - cell_size[2] / 2,
              xend = (x_vals[i] + x_vals[i - 1]) / 2,
              yend = y_vals[j] + cell_size[2] / 2
            )))
          }
        }
      }
    }
  }

  # Combine boundaries
  horiz <- if (length(horiz_list) > 0) {
    do.call(rbind, horiz_list)
  } else {
    data.frame(
      x = numeric(), y = numeric(),
      xend = numeric(), yend = numeric()
    )
  }

  vert <- if (length(vert_list) > 0) {
    do.call(rbind, vert_list)
  } else {
    data.frame(
      x = numeric(), y = numeric(),
      xend = numeric(), yend = numeric()
    )
  }

  list(horizontal = horiz, vertical = vert)
}


#' Add Samples to Plot (Internal)
#'
#' @param p A ggplot object
#' @param samples An ss_samples object
#' @param color Point color
#' @param alpha Point transparency
#' @param size Point size
#' @return Modified ggplot object
#' @keywords internal
.add_samples_to_plot <- function(p, samples, color, alpha, size) {
  sample_coords <- st_coordinates(samples$samples)
  sample_df <- data.frame(
    x = sample_coords[, 1],
    y = sample_coords[, 2]
  )

  has_prior <- "is_prior" %in% names(samples$samples) &&
    any(samples$samples$is_prior)
  has_composite <- "composite" %in% names(samples$samples)

  if (has_prior) {
    sample_df$type <- ifelse(
      samples$samples$is_prior,
      "Prior point",
      "New point"
    )

    p <- p + geom_point(
      data = sample_df,
      aes(x = .data$x, y = .data$y, shape = .data$type),
      colour = color,
      alpha = alpha,
      size = size
    ) +
      scale_shape_manual(values = c("Prior point" = 17, "New point" = 16)) +
      labs(shape = "Point type")
  } else if (has_composite) {
    sample_df$composite <- factor(samples$samples$composite)

    p <- p + geom_point(
      data = sample_df,
      aes(x = .data$x, y = .data$y, shape = .data$composite),
      colour = color,
      alpha = alpha,
      size = size
    ) +
      labs(shape = "Composite")
  } else {
    p <- p + geom_point(
      data = sample_df,
      aes(x = .data$x, y = .data$y),
      colour = color,
      alpha = alpha,
      size = size
    )
  }

  p
}


#' Create Summary Statistics Table
#'
#' Creates a summary of the sampling design.
#'
#' @param x An object of class `ss_samples`
#' @return A data frame with summary statistics
#' @export
ss_summary <- function(x) {
  if (!inherits(x, "ss_samples")) {
    stop("'x' must be an ss_samples object", call. = FALSE)
  }

  coords <- st_coordinates(x$samples)

  summary_df <- data.frame(
    method = x$method,
    n_samples = x$n_samples,
    x_min = min(coords[, 1]),
    x_max = max(coords[, 1]),
    y_min = min(coords[, 2]),
    y_max = max(coords[, 2])
  )

  if (!is.null(x$strata)) {
    summary_df$n_strata <- x$strata$n_strata
    summary_df$mssd <- x$strata$mssd
    summary_df$equal_area <- x$strata$equal_area
  }

  if (!is.null(x$n_composites)) {
    summary_df$n_composites <- x$n_composites
  }

  summary_df
}
