#' Internal Maxvol Algorithm Implementation
#'
#' Pure R implementation of the maxvol and rect_maxvol algorithms for
#' optimal sampling design. These functions are internal and not exported.
#'
#' @name maxvol_internal
#' @keywords internal
NULL


#' Rectangular Maximum Volume Algorithm
#'
#' Finds a submatrix with approximately maximum volume using greedy
#' row swapping. This is the rect_maxvol variant that allows selecting
#' more rows than columns (rectangular submatrix).
#'
#' @param A Numeric matrix (m x n) where m > n
#' @param k Integer, number of rows to select (must be >= n)
#' @param tol Numeric, tolerance for convergence (default 1.1)
#' @param max_iters Integer, maximum number of iterations (default 100)
#' @param dist_coords Optional matrix (m x 2) of coordinates for distance constraints
#' @param min_dist Numeric, minimum distance between selected points
#'
#' @return List with:
#'   \describe{
#'     \item{index}{Integer vector of selected row indices}
#'     \item{converged}{Logical, whether algorithm converged}
#'     \item{iterations}{Integer, number of iterations performed}
#'   }
#' @keywords internal
.rect_maxvol <- function(A, k, tol = 1.1, max_iters = 100,
                         dist_coords = NULL, min_dist = NULL) {
  m <- nrow(A)
  n <- ncol(A)

  if (k < n) {
    stop("k must be >= number of columns", call. = FALSE)
  }
  if (k > m) {
    stop("k must be <= number of rows", call. = FALSE)
  }

  # Initial selection: first k rows
  index <- seq_len(k)

  # Apply distance constraint to initial selection if provided
  if (!is.null(dist_coords) && !is.null(min_dist)) {
    index <- .apply_distance_constraint_init(
      dist_coords, k, min_dist
    )
  }

  # QR decomposition of initial submatrix
  P <- A[index, , drop = FALSE]
  qr_obj <- qr(P)

  if (qr_obj$rank < n) {
    # If initial selection is rank deficient, try to improve it
    # by selecting rows that are most linearly independent
    warning("Initial submatrix is rank deficient, attempting to improve selection",
            call. = FALSE)

    # Use QR with pivoting to find better initial selection
    qr_pivot <- qr(A, LAPACK = TRUE)
    if (qr_pivot$rank >= n && k >= n) {
      # Select the first k most important rows based on QR pivoting
      index <- sort(qr_pivot$pivot[seq_len(min(k, qr_pivot$rank))])
      if (length(index) < k) {
        # Fill remaining with random rows
        remaining <- setdiff(seq_len(m), index)
        index <- c(index, sample(remaining, k - length(index)))
      }
      P <- A[index, , drop = FALSE]
      qr_obj <- qr(P)
    }

    if (qr_obj$rank < n) {
      stop("Cannot find linearly independent initial submatrix. ",
           "Features may be too correlated or insufficient.",
           call. = FALSE)
    }
  }

  # Compute coefficient matrix B such that A ≈ B * P
  # B[i,j] tells how much row i can be expressed as linear combo of selected rows
  # For each row of A, we solve: B[i,] * P = A[i,]
  # Since P is k×n with k > n, this is underdetermined
  # We use the minimum norm solution: B[i,] = A[i,] * P^+
  # where P^+ is the Moore-Penrose pseudoinverse

  # Use SVD to compute pseudoinverse for numerical stability
  # P = U * D * V^T, then P^+ = V * D^+ * U^T
  # where D^+ has reciprocals of non-zero singular values
  svd_p <- svd(P)

  # Determine rank based on singular values
  tol_svd <- max(dim(P)) * max(svd_p$d) * .Machine$double.eps
  rank_p <- sum(svd_p$d > tol_svd)

  # Compute D^+ (pseudoinverse of diagonal matrix)
  d_inv <- ifelse(svd_p$d > tol_svd, 1 / svd_p$d, 0)

  # Compute P^+ = V * D^+ * U^T
  # For P (k×n): U is k×k, D is k×n, V is n×n
  # P^+ is n×k
  P_pinv <- svd_p$v %*% diag(d_inv, nrow = length(d_inv)) %*% t(svd_p$u)

  # Compute B = A * P^+
  # A is m×n, P^+ is n×k, so B is m×k
  B <- A %*% P_pinv

  converged <- FALSE
  iter <- 0

  for (iter in seq_len(max_iters)) {
    # Find row with maximum absolute value in B
    # This row is most "different" from selected rows
    max_vals <- apply(abs(B), 1, max)

    # Apply distance constraint if provided
    if (!is.null(dist_coords) && !is.null(min_dist)) {
      max_vals <- .apply_distance_constraint(
        max_vals, dist_coords, index, min_dist
      )
    }

    i_max <- which.max(max_vals)
    val_max <- max_vals[i_max]

    # Check convergence
    if (val_max <= tol) {
      converged <- TRUE
      break
    }

    # Find which selected row to swap out
    # Choose the row in index that is least important
    j_max <- which.max(abs(B[i_max, ]))
    i_old <- index[j_max]

    # Swap rows
    index[j_max] <- i_max

    # Update B matrix efficiently using Sherman-Morrison formula
    # Instead of recomputing from scratch
    b_row <- B[i_max, ]
    B_old_row <- B[i_old, ]

    v <- b_row / b_row[j_max]

    for (i in seq_len(m)) {
      if (i != i_max) {
        B[i, ] <- B[i, ] - B[i, j_max] * v
      }
    }
    B[i_max, ] <- v
  }

  list(
    index = index,
    converged = converged,
    iterations = iter
  )
}


#' Apply Distance Constraint to Initial Selection
#'
#' Selects initial k points with minimum distance constraints.
#'
#' @param coords Matrix (m x 2) of coordinates
#' @param k Number of points to select
#' @param min_dist Minimum distance between points
#' @return Integer vector of k indices
#' @keywords internal
.apply_distance_constraint_init <- function(coords, k, min_dist) {
  m <- nrow(coords)
  selected <- integer(k)

  # Start with a random point
  selected[1] <- sample.int(m, 1)

  # Greedily select remaining points
  for (i in 2:k) {
    # Calculate distances from all points to selected points
    valid <- rep(TRUE, m)
    valid[selected[1:(i-1)]] <- FALSE

    for (j in 1:(i-1)) {
      s_idx <- selected[j]
      dists_sq <- (coords[, 1] - coords[s_idx, 1])^2 +
                  (coords[, 2] - coords[s_idx, 2])^2
      valid <- valid & (dists_sq >= min_dist^2)
    }

    candidates <- which(valid)
    if (length(candidates) == 0) {
      warning("Cannot satisfy distance constraint for all points",
              call. = FALSE)
      # Fill remaining with any unused points
      remaining <- setdiff(seq_len(m), selected[1:(i-1)])
      selected[i:k] <- sample(remaining, k - i + 1)
      break
    }

    selected[i] <- sample(candidates, 1)
  }

  selected
}


#' Apply Distance Constraint During Iteration
#'
#' Zeros out the selection weights for candidate points that are too close
#' to already selected points.
#'
#' @param weights Numeric vector of selection weights
#' @param coords Matrix (m x 2) of coordinates
#' @param selected Integer vector of currently selected indices
#' @param min_dist Minimum allowed distance
#' @return Modified weights vector
#' @keywords internal
.apply_distance_constraint <- function(weights, coords, selected, min_dist) {
  m <- length(weights)
  min_dist_sq <- min_dist^2

  for (s_idx in selected) {
    dists_sq <- (coords[, 1] - coords[s_idx, 1])^2 +
                (coords[, 2] - coords[s_idx, 2])^2

    # Zero out weights for points too close to this selected point
    too_close <- dists_sq < min_dist_sq
    weights[too_close] <- 0
  }

  # Don't select already selected points
  weights[selected] <- 0

  weights
}


#' Compute Terrain Features from Raster
#'
#' Helper function to compute standard terrain features from a DEM or
#' raster stack.
#'
#' @param x A raster object or sf object with attributes
#' @return Matrix of features (n_cells x n_features)
#' @keywords internal
.extract_features <- function(x) {
  # This is a placeholder - actual implementation would depend on
  # whether we want to compute terrain features or use provided attributes

  if (inherits(x, "sf")) {
    # Extract attributes as features
    coords <- st_coordinates(x)
    attrs <- st_drop_geometry(x)

    if (ncol(attrs) == 0) {
      # Only coordinates available
      return(coords)
    }

    # Combine coordinates with attributes
    feature_mat <- cbind(coords, as.matrix(attrs))
    return(feature_mat)
  }

  stop("Feature extraction not yet implemented for this object type",
       call. = FALSE)
}


#' Normalize Feature Matrix
#'
#' Normalizes features to have zero mean and unit variance.
#'
#' @param X Numeric matrix
#' @return List with normalized matrix and scaling parameters
#' @keywords internal
#' @importFrom stats sd
.normalize_features <- function(X) {
  means <- colMeans(X, na.rm = TRUE)
  sds <- apply(X, 2, stats::sd, na.rm = TRUE)

  # Avoid division by zero
  sds[sds == 0] <- 1

  X_norm <- scale(X, center = means, scale = sds)

  list(
    X_norm = X_norm,
    means = means,
    sds = sds
  )
}
