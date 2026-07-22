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
