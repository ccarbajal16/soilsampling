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
