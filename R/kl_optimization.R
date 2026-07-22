#' KL Divergence Sample Size Optimization
#'
#' Functions supporting sample size optimization for cLHS designs based on
#' Kullback-Leibler divergence between population and sample distributions,
#' following Malone et al. (2019).
#'
#' @name kl_optimization
#' @keywords internal
NULL


#' Load Predictor Rasters
#'
#' Loads either a multi-layer raster file or a directory of single-layer
#' raster files into a `SpatRaster` stack.
#'
#' @param path Character, path to a `.tif` file or a directory containing
#'   `.tif` files.
#'
#' @return A `SpatRaster` stack.
#'
#' @examples
#' \dontrun{
#' predictors <- ss_load_rasters("data/predictors.tif")
#' }
#'
#' @export
ss_load_rasters <- function(path) {
  if (file.exists(path) && grepl("\\.tif$", path, ignore.case = TRUE)) {
    return(terra::rast(path))
  } else if (dir.exists(path)) {
    return(.load_raster_dir(path))
  } else {
    stop("'path' is neither an existing .tif file nor a directory: ", path,
      call. = FALSE
    )
  }
}


#' Load All Rasters From a Directory
#'
#' @param dir Character, directory containing `.tif` predictor rasters.
#' @param pattern Character, filename pattern. Default `"*.tif"`.
#' @param recursive Logical, search subdirectories. Default `FALSE`.
#'
#' @return A `SpatRaster` stack.
#'
#' @keywords internal
.load_raster_dir <- function(dir, pattern = "*.tif", recursive = FALSE) {
  if (!dir.exists(dir)) {
    stop("Directory does not exist: ", dir, call. = FALSE)
  }

  files <- list.files(dir, pattern = pattern, full.names = TRUE, recursive = recursive)
  if (length(files) == 0) {
    stop("No raster files found in ", dir, " matching pattern ", pattern,
      call. = FALSE
    )
  }

  terra::rast(files)
}


#' Extract Raster Values at Sample Locations
#'
#' @param rasters A `SpatRaster` stack of ancillary variables.
#' @param sample_points Spatial points, either an `sf` object or a matrix
#'   of x, y coordinates.
#'
#' @return A data frame of extracted values.
#'
#' @keywords internal
.extract_sample_values <- function(rasters, sample_points) {
  coords <- if (inherits(sample_points, "sf")) {
    sf::st_coordinates(sample_points)
  } else {
    sample_points
  }
  coords <- as.data.frame(coords)

  as.data.frame(terra::extract(rasters, coords, ID = FALSE))
}


#' KL Divergence Between Population and Sample Distributions
#'
#' Computes the mean Kullback-Leibler divergence across all numeric
#' variables shared by a population data set and a sample drawn from it.
#'
#' @param population_data Data frame of population ancillary data.
#' @param sample_data Data frame of sample ancillary data.
#' @param n_bins Integer, number of histogram bins used to estimate each
#'   variable's distribution. Default `25`.
#'
#' @return Numeric, the mean KL divergence across all numeric variables.
#'
#' @details
#' For each numeric variable, population and sample distributions are
#' estimated as histograms over the same bins (defined by the population
#' range). KL divergence is then \eqn{\sum O_i \log(O_i / E_i)}, where
#' \eqn{O_i} is the sample density and \eqn{E_i} the population density in
#' bin \eqn{i}. The result is averaged across variables.
#'
#' @examples
#' pop <- data.frame(x = rnorm(200), y = runif(200))
#' samp <- pop[sample(nrow(pop), 30), ]
#' ss_kl_divergence(pop, samp)
#'
#' @importFrom graphics hist
#' @export
ss_kl_divergence <- function(population_data, sample_data, n_bins = 25) {
  kl_values <- numeric()

  for (col in names(population_data)) {
    if (is.numeric(population_data[[col]])) {
      pop_range <- range(population_data[[col]], na.rm = TRUE)
      breaks <- seq(pop_range[1], pop_range[2], length.out = n_bins + 1)

      pop_hist <- hist(population_data[[col]], breaks = breaks, plot = FALSE)
      pop_density <- pop_hist$counts / sum(pop_hist$counts)

      sample_hist <- hist(sample_data[[col]], breaks = breaks, plot = FALSE)
      sample_density <- sample_hist$counts / sum(sample_hist$counts)

      pop_density <- pmax(pop_density, 1e-10)
      sample_density <- pmax(sample_density, 1e-10)

      kl <- sum(sample_density * (log(sample_density) - log(pop_density)))
      kl_values <- c(kl_values, kl)
    }
  }

  mean(kl_values, na.rm = TRUE)
}


#' Optimize cLHS Sample Size Using KL Divergence
#'
#' Determines the sample size at which a conditioned Latin hypercube sample
#' (cLHS) best represents the population distribution, following the
#' KL-divergence-based approach of Malone et al. (2019). Runs cLHS at a
#' range of sample sizes, fits an exponential decay curve to the mean KL
#' divergence, and reports the smallest sample size that reaches a target
#' proportion of the maximum achievable improvement.
#'
#' @param population_data Data frame of population ancillary data (one row
#'   per population unit, one column per covariate).
#' @param min_samples Integer, minimum sample size to test. Default `10`.
#' @param max_samples Integer, maximum sample size to test. Default `500`.
#' @param step_size Integer, increment between tested sample sizes.
#'   Default `10`.
#' @param n_replicates Integer, number of cLHS replicates per sample size.
#'   Default `10`.
#' @param n_bins Integer, number of histogram bins used by
#'   [ss_kl_divergence()]. Default `25`.
#' @param probability_threshold Numeric in (0, 1], CDF threshold used to
#'   pick the optimal sample size. Default `0.95`.
#'
#' @return A list with:
#'   \describe{
#'     \item{raw_results}{Data frame, one row per replicate.}
#'     \item{summary_results}{Data frame, mean/sd KL divergence per sample size.}
#'     \item{fitted_model}{The fitted `nls` exponential decay model, or `NULL`.}
#'     \item{fitted_curve}{Data frame of fitted KL divergence per sample size.}
#'     \item{optimal_sample_size}{Integer, the recommended sample size.}
#'     \item{plot_kl}{A `ggplot` of KL divergence vs. sample size.}
#'     \item{plot_cdf}{A `ggplot` of the CDF used to pick the optimal size.}
#'   }
#'
#' @details
#' The relationship between sample size \eqn{n} and KL divergence is modeled
#' as \eqn{KL(n) = b_1 e^{-kn} + b_0}. The optimal sample size is the
#' smallest \eqn{n} for which the cumulative proportion of improvement,
#' \eqn{(max(KL) - KL(n)) / (max(KL) - min(KL))}, reaches
#' `probability_threshold`.
#'
#' @examples
#' \dontrun{
#' pop <- data.frame(a = rnorm(2000), b = runif(2000))
#' res <- ss_kl_optimize(pop, min_samples = 10, max_samples = 60,
#'   step_size = 10, n_replicates = 3)
#' res$optimal_sample_size
#' }
#'
#' @seealso [ss_kl_divergence()]
#' @export
ss_kl_optimize <- function(population_data,
                            min_samples = 10,
                            max_samples = 500,
                            step_size = 10,
                            n_replicates = 10,
                            n_bins = 25,
                            probability_threshold = 0.95) {
  sample_sizes <- seq(min_samples, max_samples, by = step_size)

  results <- data.frame(
    sample_size = integer(),
    replicate = integer(),
    kl_divergence = numeric(),
    stringsAsFactors = FALSE
  )

  for (n_samples in sample_sizes) {
    for (rep in seq_len(n_replicates)) {
      tryCatch({
        clhs_idx <- clhs::clhs(population_data, size = n_samples, iter = 10000)
        sample_data <- population_data[clhs_idx, ]

        kl_div <- ss_kl_divergence(population_data, sample_data, n_bins)

        results <- rbind(results, data.frame(
          sample_size = n_samples,
          replicate = rep,
          kl_divergence = kl_div,
          stringsAsFactors = FALSE
        ))
      }, error = function(e) {
        warning("cLHS failed at sample size ", n_samples, ", replicate ", rep,
          ": ", conditionMessage(e),
          call. = FALSE
        )
      })
    }
  }

  if (nrow(results) == 0) {
    return(list(
      raw_results = results,
      summary_results = data.frame(),
      fitted_model = NULL,
      fitted_curve = NULL,
      optimal_sample_size = NA,
      plot_kl = NULL,
      plot_cdf = NULL
    ))
  }

  summary_results <- dplyr::summarise(
    dplyr::group_by(results, .data$sample_size),
    mean_kl = mean(.data$kl_divergence, na.rm = TRUE),
    sd_kl = stats::sd(.data$kl_divergence, na.rm = TRUE),
    .groups = "drop"
  )

  exp_model <- NULL
  fitted_curve <- NULL
  cdf_values <- NULL
  optimal_sample_size <- NA

  if (nrow(summary_results) > 3) {
    exp_model <- tryCatch({
      minpack.lm::nlsLM(
        mean_kl ~ b1 * exp(-k * sample_size) + b0,
        data = summary_results,
        start = list(
          b0 = min(summary_results$mean_kl),
          b1 = max(summary_results$mean_kl) - min(summary_results$mean_kl),
          k = 0.01
        )
      )
    }, error = function(e) {
      warning("Exponential decay model fit failed: ", conditionMessage(e), call. = FALSE)
      NULL
    })

    if (!is.null(exp_model)) {
      fitted_curve <- data.frame(
        sample_size = sample_sizes,
        fitted_kl = stats::predict(exp_model, newdata = data.frame(sample_size = sample_sizes))
      )

      max_improvement <- max(fitted_curve$fitted_kl) - min(fitted_curve$fitted_kl)
      if (max_improvement > 1e-10) {
        cdf_values <- (max(fitted_curve$fitted_kl) - fitted_curve$fitted_kl) / max_improvement
        optimal_idx <- which(cdf_values >= probability_threshold)[1]
        optimal_sample_size <- if (is.na(optimal_idx)) max_samples else sample_sizes[optimal_idx]
      } else {
        optimal_sample_size <- max_samples
      }
    }
  }

  plot_kl <- tryCatch(.plot_kl_divergence(summary_results, fitted_curve, step_size), error = function(e) NULL)

  plot_cdf <- NULL
  if (!is.null(cdf_values) && !is.na(optimal_sample_size)) {
    plot_cdf <- tryCatch(
      .plot_kl_cdf(sample_sizes, cdf_values, probability_threshold, optimal_sample_size, step_size),
      error = function(e) NULL
    )
  }

  list(
    raw_results = results,
    summary_results = summary_results,
    fitted_model = exp_model,
    fitted_curve = fitted_curve,
    optimal_sample_size = optimal_sample_size,
    plot_kl = plot_kl,
    plot_cdf = plot_cdf
  )
}


#' Plot KL Divergence vs. Sample Size
#'
#' @param summary_results Data frame with `sample_size`, `mean_kl`, `sd_kl`.
#' @param fitted_curve Data frame with `sample_size`, `fitted_kl`, or `NULL`.
#' @param step_size Integer, used to size error bar width.
#'
#' @return A `ggplot` object.
#'
#' @keywords internal
.plot_kl_divergence <- function(summary_results, fitted_curve, step_size) {
  p <- ggplot2::ggplot(summary_results, ggplot2::aes(x = .data$sample_size, y = .data$mean_kl)) +
    ggplot2::geom_point(size = 2, color = "blue") +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = pmax(.data$mean_kl - .data$sd_kl, 0),
        ymax = .data$mean_kl + .data$sd_kl
      ),
      width = step_size / 2, alpha = 0.7
    ) +
    ggplot2::labs(title = "KL Divergence vs Sample Size", x = "Number of Samples", y = "KL Divergence") +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  if (!is.null(fitted_curve)) {
    p <- p + ggplot2::geom_line(
      data = fitted_curve,
      ggplot2::aes(x = .data$sample_size, y = .data$fitted_kl),
      color = "red", linewidth = 1
    )
  }

  p
}


#' Plot Cumulative Density Function Used to Pick the Optimal Sample Size
#'
#' @param sample_sizes Numeric vector of tested sample sizes.
#' @param cdf_values Numeric vector, CDF of (1 - KL divergence).
#' @param probability_threshold Numeric, CDF threshold.
#' @param optimal_sample_size Integer, the selected sample size.
#' @param step_size Integer, used to position the annotation.
#'
#' @return A `ggplot` object.
#'
#' @keywords internal
.plot_kl_cdf <- function(sample_sizes, cdf_values, probability_threshold, optimal_sample_size, step_size) {
  cdf_data <- data.frame(sample_size = sample_sizes, cdf = cdf_values)

  ggplot2::ggplot(cdf_data, ggplot2::aes(x = .data$sample_size, y = .data$cdf)) +
    ggplot2::geom_line(linewidth = 1, color = "darkgreen") +
    ggplot2::geom_hline(yintercept = probability_threshold, color = "red", linetype = "dashed") +
    ggplot2::geom_vline(xintercept = optimal_sample_size, color = "red", linetype = "dashed") +
    ggplot2::labs(
      title = "Cumulative Density Function of (1 - KL Divergence)",
      x = "Number of Samples", y = "CDF of (1 - KL divergence)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::annotate("text",
      x = optimal_sample_size + step_size * 2, y = probability_threshold - 0.05,
      label = paste("Optimal size:", optimal_sample_size), color = "red"
    )
}


#' End-to-End cLHS Sample Size Optimization
#'
#' Runs [ss_kl_optimize()] starting from predictor rasters (a file path, a
#' directory of `.tif` files, or an already-loaded `SpatRaster`) or directly
#' from a population data frame. Optionally writes results and plots to
#' disk.
#'
#' @param x One of: a character path to a `.tif` file or a directory of
#'   `.tif` files, a `SpatRaster` stack, or a data frame of population
#'   ancillary data (used as-is, skipping raster extraction).
#' @param output_dir Character, directory to write CSV/PNG outputs to. If
#'   `NULL` (default), nothing is written to disk.
#' @param max_population Integer, if the population has more rows than this,
#'   a random subsample of this size is used to keep runtime reasonable.
#'   Default `100000`.
#' @param min_samples,max_samples,step_size,n_replicates,n_bins,probability_threshold
#'   Passed to [ss_kl_optimize()].
#'
#' @return The list returned by [ss_kl_optimize()], plus a `file_paths`
#'   element (only when `output_dir` is supplied) listing the files written.
#'
#' @examples
#' \dontrun{
#' res <- ss_kl_size("data/predictors.tif", output_dir = "outputs")
#' res$optimal_sample_size
#' }
#'
#' @seealso [ss_kl_optimize()], [ss_load_rasters()]
#' @export
ss_kl_size <- function(x,
                        output_dir = NULL,
                        max_population = 100000,
                        min_samples = 10,
                        max_samples = 500,
                        step_size = 10,
                        n_replicates = 10,
                        n_bins = 25,
                        probability_threshold = 0.95) {
  population_data <- if (is.data.frame(x)) {
    x
  } else {
    rasters <- if (inherits(x, "SpatRaster")) x else ss_load_rasters(x)
    population_values <- terra::values(rasters, na.rm = TRUE)
    as.data.frame(population_values)
  }

  population_data <- population_data[stats::complete.cases(population_data), ]
  if (nrow(population_data) == 0) {
    stop("No complete cases found in population data", call. = FALSE)
  }

  if (nrow(population_data) > max_population) {
    idx <- sample(nrow(population_data), max_population)
    population_data <- population_data[idx, ]
  }

  results <- ss_kl_optimize(
    population_data = population_data,
    min_samples = min_samples,
    max_samples = max_samples,
    step_size = step_size,
    n_replicates = n_replicates,
    n_bins = n_bins,
    probability_threshold = probability_threshold
  )

  if (!is.null(output_dir)) {
    results$file_paths <- .write_kl_outputs(results, output_dir)
  }

  results
}


#' Write KL Optimization Results and Plots to Disk
#'
#' @param results List returned by [ss_kl_optimize()].
#' @param output_dir Character, directory to write outputs to.
#'
#' @return A named list of file paths written (`NA` for any output that was
#'   not available or failed to save).
#'
#' @keywords internal
.write_kl_outputs <- function(results, output_dir) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  raw_path <- file.path(output_dir, "kl_raw_results.csv")
  summary_path <- file.path(output_dir, "kl_summary_results.csv")
  utils::write.csv(results$raw_results, raw_path, row.names = FALSE)
  utils::write.csv(results$summary_results, summary_path, row.names = FALSE)

  curve_path <- NA
  if (!is.null(results$fitted_curve)) {
    curve_path <- file.path(output_dir, "kl_fitted_curve.csv")
    utils::write.csv(results$fitted_curve, curve_path, row.names = FALSE)
  }

  kl_plot_path <- NA
  if (!is.null(results$plot_kl)) {
    kl_plot_path <- file.path(output_dir, "kl_divergence_vs_sample_size.png")
    tryCatch(
      ggplot2::ggsave(kl_plot_path, plot = results$plot_kl, width = 7, height = 5, dpi = 300),
      error = function(e) {
        warning("Could not save KL divergence plot: ", conditionMessage(e), call. = FALSE)
        kl_plot_path <<- NA
      }
    )
  }

  cdf_plot_path <- NA
  if (!is.null(results$plot_cdf)) {
    cdf_plot_path <- file.path(output_dir, "kl_cdf_threshold.png")
    tryCatch(
      ggplot2::ggsave(cdf_plot_path, plot = results$plot_cdf, width = 7, height = 5, dpi = 300),
      error = function(e) {
        warning("Could not save CDF plot: ", conditionMessage(e), call. = FALSE)
        cdf_plot_path <<- NA
      }
    )
  }

  list(
    raw = raw_path,
    summary = summary_path,
    curve = curve_path,
    kl_plot = kl_plot_path,
    cdf_plot = cdf_plot_path
  )
}


#' Save KL Optimization Plots
#'
#' Saves the KL-divergence and CDF plots from a [ss_kl_optimize()] (or
#' [ss_kl_size()]) result to disk, without writing the CSV outputs. Useful
#' for re-saving plots on their own, e.g. with a different prefix.
#'
#' @param results List returned by [ss_kl_optimize()] or [ss_kl_size()].
#' @param output_dir Character, directory to save plots to. Created if it
#'   does not exist.
#' @param prefix Character, file name prefix. Default `"kl"`.
#'
#' @return Character vector of file paths written (invisibly, if none were
#'   saved, a zero-length character vector).
#'
#' @examples
#' \dontrun{
#' res <- ss_kl_optimize(population_data)
#' ss_kl_save_plots(res, "outputs", prefix = "clhs")
#' }
#'
#' @seealso [ss_kl_optimize()], [ss_kl_size()]
#' @export
ss_kl_save_plots <- function(results, output_dir = "outputs", prefix = "kl") {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  saved_files <- character()

  if (!is.null(results$plot_kl)) {
    kl_path <- file.path(output_dir, paste0(prefix, "_divergence_plot.png"))
    saved <- tryCatch({
      ggplot2::ggsave(kl_path, plot = results$plot_kl, width = 8, height = 6, dpi = 300)
      TRUE
    }, error = function(e) {
      warning("Could not save KL divergence plot: ", conditionMessage(e), call. = FALSE)
      FALSE
    })
    if (saved) saved_files <- c(saved_files, kl_path)
  }

  if (!is.null(results$plot_cdf)) {
    cdf_path <- file.path(output_dir, paste0(prefix, "_cdf_plot.png"))
    saved <- tryCatch({
      ggplot2::ggsave(cdf_path, plot = results$plot_cdf, width = 8, height = 6, dpi = 300)
      TRUE
    }, error = function(e) {
      warning("Could not save CDF plot: ", conditionMessage(e), call. = FALSE)
      FALSE
    })
    if (saved) saved_files <- c(saved_files, cdf_path)
  }

  if (length(saved_files) == 0) {
    warning("No plots were saved: results$plot_kl and results$plot_cdf are both NULL", call. = FALSE)
  }

  invisible(saved_files)
}
