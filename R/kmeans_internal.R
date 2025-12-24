#' Internal K-means Implementation for Soil Sampling
#'
#' Pure R implementation of k-means algorithms for spatial coverage sampling.
#' These functions are internal and not exported.
#'
#' @name kmeans_internal
#' @keywords internal
NULL


#' Squared Euclidean Distance Matrix
#'
#' Computes the squared Euclidean distance between each row of X and each row of Y.
#'
#' @param X A matrix of coordinates (n x 2)
#' @param Y A matrix of coordinates (m x 2)
#' @return A matrix of squared distances (n x m)
#' @keywords internal
.squared_distance_matrix <- function(X, Y) {
  n <- nrow(X)
  m <- nrow(Y)

  X1_sq <- rowSums(X^2)
  Y1_sq <- rowSums(Y^2)

  # (x - y)^2 = x^2 - 2xy + y^2
  outer(X1_sq, rep(1, m)) + outer(rep(1, n), Y1_sq) - 2 * tcrossprod(X, Y)
}


#' Compute Squared Haversine Distance
#'
#' Computes squared great circle distance for lat/lon coordinates.
#'
#' @param lon1,lat1 Longitude and latitude of first point(s) in degrees
#' @param lon2,lat2 Longitude and latitude of second point(s) in degrees
#' @return Squared distance in km^2 (approximate)
#' @keywords internal
.haversine_distance_sq <- function(lon1, lat1, lon2, lat2) {
  R <- 6371 # Earth radius in km

  lon1_rad <- lon1 * pi / 180
  lat1_rad <- lat1 * pi / 180
  lon2_rad <- lon2 * pi / 180
  lat2_rad <- lat2 * pi / 180

  dlon <- lon2_rad - lon1_rad
  dlat <- lat2_rad - lat1_rad

  a <- sin(dlat / 2)^2 + cos(lat1_rad) * cos(lat2_rad) * sin(dlon / 2)^2
  c <- 2 * asin(pmin(1, sqrt(a)))

  d <- R * c
  d^2
}


#' Squared Distance Matrix for Lat/Lon Coordinates
#'
#' @param X A matrix of coordinates (n x 2), columns are lon, lat
#' @param Y A matrix of coordinates (m x 2), columns are lon, lat
#' @return A matrix of squared distances (n x m) in km^2
#' @keywords internal
.haversine_distance_matrix <- function(X, Y) {
  n <- nrow(X)
  m <- nrow(Y)
  D <- matrix(0, nrow = n, ncol = m)

  for (i in seq_len(n)) {
    D[i, ] <- .haversine_distance_sq(X[i, 1], X[i, 2], Y[, 1], Y[, 2])
  }
  D
}


#' Compute Cluster Centroid
#'
#' Computes the centroid of points in a cluster.
#'
#' @param coords Matrix of coordinates for points in the cluster
#' @param is_latlon Logical, whether coordinates are lat/lon
#' @return A vector of length 2 with the centroid coordinates
#' @keywords internal
.compute_centroid <- function(coords, is_latlon = FALSE) {
  if (nrow(coords) == 0) {
    return(c(NA, NA))
  }

  if (is_latlon) {
    lon <- coords[, 1] * pi / 180
    lat <- coords[, 2] * pi / 180

    x <- cos(lat) * cos(lon)
    y <- cos(lat) * sin(lon)
    z <- sin(lat)

    x_mean <- mean(x)
    y_mean <- mean(y)
    z_mean <- mean(z)

    lon_c <- atan2(y_mean, x_mean) * 180 / pi
    lat_c <- asin(z_mean / sqrt(x_mean^2 + y_mean^2 + z_mean^2)) * 180 / pi

    c(lon_c, lat_c)
  } else {
    colMeans(coords)
  }
}


#' K-means Transfer Algorithm (Pure R)
#'
#' Implements the k-means transfer algorithm for spatial coverage sampling.
#'
#' @param cell_coords Matrix of cell center coordinates (n x 2)
#' @param n_strata Number of strata (clusters) to create
#' @param prior_coords Optional matrix of prior point coordinates
#' @param max_iter Maximum number of iterations
#' @param is_latlon Logical, whether coordinates are lat/lon
#' @return A list with clustering results
#' @keywords internal
.kmeans_transfer <- function(cell_coords, n_strata, prior_coords = NULL,
                             max_iter = 1000L, is_latlon = FALSE) {
  n_cells <- nrow(cell_coords)
  n_prior <- if (is.null(prior_coords)) 0L else nrow(prior_coords)
  n_free <- n_strata - n_prior

  dist_fun <- if (is_latlon) .haversine_distance_matrix else .squared_distance_matrix

  # Initialize cluster centers
  if (n_prior > 0) {
    dist_to_prior <- dist_fun(cell_coords, prior_coords)
    cells_with_prior <- apply(dist_to_prior, 2, which.min)
    available_cells <- setdiff(seq_len(n_cells), cells_with_prior)

    if (length(available_cells) < n_free) {
      stop("Not enough cells available after excluding prior point locations")
    }

    selected_cells <- sample(available_cells, n_free, replace = FALSE)
    centroids <- rbind(prior_coords, cell_coords[selected_cells, , drop = FALSE])
  } else {
    selected_cells <- sample(seq_len(n_cells), n_strata, replace = FALSE)
    centroids <- cell_coords[selected_cells, , drop = FALSE]
  }

  is_fixed <- c(rep(TRUE, n_prior), rep(FALSE, n_free))
  is_active <- rep(TRUE, n_strata)
  was_active <- rep(TRUE, n_strata)

  # Initial assignment
  dist_matrix <- dist_fun(cell_coords, centroids)
  cluster_id <- apply(dist_matrix, 1, which.min) - 1L

  converged <- FALSE
  iterations <- 0L

  for (iter in seq_len(max_iter)) {
    iterations <- iter
    n_transfers <- 0L

    was_active <- is_active
    is_active <- rep(FALSE, n_strata)

    for (i in seq_len(n_cells)) {
      current_cluster <- cluster_id[i] + 1L
      current_dist <- dist_matrix[i, current_cluster]

      for (k in seq_len(n_strata)) {
        if (k == current_cluster) next
        if (!was_active[current_cluster] && !was_active[k]) next

        if (dist_matrix[i, k] < current_dist) {
          cluster_id[i] <- k - 1L
          n_transfers <- n_transfers + 1L
          is_active[current_cluster] <- TRUE
          is_active[k] <- TRUE
          break
        }
      }
    }

    # Update centroids for non-fixed clusters
    for (k in seq_len(n_strata)) {
      if (!is_fixed[k]) {
        cluster_cells <- which(cluster_id == (k - 1L))
        if (length(cluster_cells) > 0) {
          centroids[k, ] <- .compute_centroid(
            cell_coords[cluster_cells, , drop = FALSE],
            is_latlon
          )
        }
      }
    }

    # Update distance matrix for active clusters
    for (k in which(is_active)) {
      dist_matrix[, k] <- dist_fun(cell_coords, centroids[k, , drop = FALSE])
    }

    if (n_transfers == 0L) {
      converged <- TRUE
      break
    }
  }

  # Compute MSSD
  final_dist <- dist_fun(cell_coords, centroids)
  min_dist <- apply(final_dist, 1, min)
  mssd <- mean(min_dist)

  list(
    cluster_id = cluster_id,
    centroids = centroids,
    mssd = mssd,
    converged = converged,
    iterations = iterations
  )
}


#' K-means Swop Algorithm (Pure R)
#'
#' Implements the k-means swop algorithm for equal-area strata.
#'
#' @param cell_coords Matrix of cell center coordinates (n x 2)
#' @param n_strata Number of strata (clusters) to create
#' @param max_iter Maximum number of iterations
#' @param is_latlon Logical, whether coordinates are lat/lon
#' @return A list with clustering results
#' @keywords internal
.kmeans_swop <- function(cell_coords, n_strata, max_iter = 1000L,
                         is_latlon = FALSE) {
  n_cells <- nrow(cell_coords)

  dist_fun <- if (is_latlon) .haversine_distance_matrix else .squared_distance_matrix

  # Initialize with random assignment (equal-sized clusters)
  cluster_id <- sample(rep(0:(n_strata - 1), length.out = n_cells))

  # Compute initial centroids
  centroids <- matrix(0, nrow = n_strata, ncol = 2)
  for (k in seq_len(n_strata)) {
    cluster_cells <- which(cluster_id == (k - 1L))
    if (length(cluster_cells) > 0) {
      centroids[k, ] <- .compute_centroid(
        cell_coords[cluster_cells, , drop = FALSE],
        is_latlon
      )
    }
  }

  is_active <- rep(TRUE, n_strata)
  was_active <- rep(TRUE, n_strata)

  dist_matrix <- dist_fun(cell_coords, centroids)

  converged <- FALSE
  iterations <- 0L

  for (iter in seq_len(max_iter)) {
    iterations <- iter
    n_swops <- 0L

    was_active <- is_active
    is_active <- rep(FALSE, n_strata)

    for (i in seq_len(n_cells)) {
      cluster1 <- cluster_id[i] + 1L
      if (!was_active[cluster1]) next

      dist11 <- dist_matrix[i, cluster1]

      for (j in seq_len(n_cells)) {
        cluster2 <- cluster_id[j] + 1L
        if (cluster1 == cluster2) next
        if (!was_active[cluster1] && !was_active[cluster2]) next

        dist12 <- dist_matrix[i, cluster2]
        dist21 <- dist_matrix[j, cluster1]
        dist22 <- dist_matrix[j, cluster2]

        if ((dist11 + dist22) > (dist12 + dist21)) {
          cluster_id[i] <- cluster2 - 1L
          cluster_id[j] <- cluster1 - 1L
          n_swops <- n_swops + 1L
          is_active[cluster1] <- TRUE
          is_active[cluster2] <- TRUE

          for (k in c(cluster1, cluster2)) {
            cluster_cells <- which(cluster_id == (k - 1L))
            if (length(cluster_cells) > 0) {
              centroids[k, ] <- .compute_centroid(
                cell_coords[cluster_cells, , drop = FALSE],
                is_latlon
              )
            }
          }

          dist_matrix[, cluster1] <- dist_fun(cell_coords, centroids[cluster1, , drop = FALSE])
          dist_matrix[, cluster2] <- dist_fun(cell_coords, centroids[cluster2, , drop = FALSE])

          break
        }
      }
    }

    if (n_swops == 0L) {
      converged <- TRUE
      break
    }
  }

  final_dist <- dist_fun(cell_coords, centroids)
  min_dist <- apply(final_dist, 1, min)
  mssd <- mean(min_dist)

  list(
    cluster_id = cluster_id,
    centroids = centroids,
    mssd = mssd,
    converged = converged,
    iterations = iterations
  )
}


#' Run K-means with Multiple Tries
#'
#' Runs the k-means algorithm multiple times and returns the best result.
#'
#' @param cell_coords Matrix of cell center coordinates (n x 2)
#' @param n_strata Number of strata (clusters) to create
#' @param prior_coords Optional matrix of prior point coordinates
#' @param max_iter Maximum number of iterations per try
#' @param n_try Number of random initializations to try
#' @param equal_area Logical, whether to use swop algorithm
#' @param is_latlon Logical, whether coordinates are lat/lon
#' @param verbose Logical, whether to print progress
#' @return A list with the best clustering result
#' @keywords internal
.kmeans_coverage <- function(cell_coords, n_strata, prior_coords = NULL,
                             max_iter = 1000L, n_try = 1L,
                             equal_area = FALSE, is_latlon = FALSE,
                             verbose = FALSE) {
  best_result <- NULL
  best_mssd <- Inf

  for (i in seq_len(n_try)) {
    if (verbose) {
      message(format(Sys.time()), " | Optimizing configuration ", i)
    }

    if (equal_area) {
      result <- .kmeans_swop(
        cell_coords = cell_coords,
        n_strata = n_strata,
        max_iter = max_iter,
        is_latlon = is_latlon
      )
    } else {
      result <- .kmeans_transfer(
        cell_coords = cell_coords,
        n_strata = n_strata,
        prior_coords = prior_coords,
        max_iter = max_iter,
        is_latlon = is_latlon
      )
    }

    if (verbose) {
      message("    Current MSSD: ", format(result$mssd, digits = 6))
    }

    if (result$mssd < best_mssd) {
      best_mssd <- result$mssd
      best_result <- result
    }

    if (verbose) {
      message("    Best MSSD: ", format(best_mssd, digits = 6))
    }
  }

  if (!best_result$converged) {
    warning("Convergence not reached after ", max_iter, " iterations and ",
      n_try, " attempts",
      call. = FALSE
    )
  }

  best_result
}
