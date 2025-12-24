#' soilsampling: Soil Sampling Design Methods
#'
#' @description
#' The soilsampling package provides methods for designing soil sampling schemes
#' including spatial coverage sampling, simple random sampling, stratified random
#' sampling, and composite sampling. The package uses sf for spatial operations
#' and does not require Java or the sp package.
#'
#' @details
#' The package implements three main sampling approaches:
#'
#' \itemize{
#'   \item **Simple Random Sampling** ([ss_random()]): Selects sampling locations
#'     uniformly at random within the study area.
#'   \item **Stratified Random Sampling** ([ss_stratified()]): Divides the study
#'     area into compact strata and samples randomly within each stratum.
#'   \item **Spatial Coverage Sampling** ([ss_coverage()]): Places samples at the
#'     centroids of compact geographical strata for optimal spatial coverage.
#' }
#'
#' The stratification is based on k-means clustering as described in
#' Walvoort et al. (2010). Two k-means algorithms are available:
#'
#' \itemize{
#'   \item **Transfer algorithm**: Standard k-means that creates compact strata
#'     of potentially unequal sizes.
#'   \item **Swop algorithm**: Creates compact strata of equal size, useful for
#'     composite sampling.
#' }
#'
#' @section Main Functions:
#'
#' **Stratification:**
#' \itemize{
#'   \item [ss_stratify()] - Create spatial strata using k-means
#' }
#'
#' **Sampling:**
#' \itemize{
#'   \item [ss_random()] - Simple random sampling
#'   \item [ss_stratified()] - Stratified random sampling
#'   \item [ss_coverage()] - Spatial coverage sampling (centroids)
#'   \item [ss_coverage_equal_area()] - Coverage sampling with equal-area strata
#'   \item [ss_composite()] - Composite sampling from equal-area strata
#' }
#'
#' **Visualization:**
#' \itemize{
#'   \item [ss_plot()] - Plot stratification and samples
#'   \item [ss_plot_samples()] - Plot sampling points only
#' }
#'
#' **Utilities:**
#' \itemize{
#'   \item [ss_to_sf()] - Convert results to sf object
#'   \item [ss_to_data_frame()] - Convert results to data frame
#'   \item [ss_summary()] - Get summary statistics
#'   \item [ss_n_strata()] - Get number of strata
#'   \item [ss_n_samples()] - Get number of samples
#'   \item [ss_area()] - Get stratum areas
#'   \item [ss_relative_area()] - Get relative stratum areas
#' }
#'
#' @section Workflow:
#'
#' A typical workflow for spatial coverage sampling:
#'
#' \preformatted{
#' library(soilsampling)
#' library(sf)
#'
#' # Load or create study area
#' study_area <- st_read("study_area.shp")
#'
#' # Option 1: Direct coverage sampling
#' samples <- ss_coverage(study_area, n_strata = 50, n_try = 10)
#'
#' # Option 2: Create strata first, then sample
#' strata <- ss_stratify(study_area, n_strata = 50, n_try = 10)
#' samples <- ss_coverage(strata)
#'
#' # Visualize
#' ss_plot(strata, samples = samples)
#'
#' # Export coordinates
#' coords <- ss_to_data_frame(samples)
#' write.csv(coords, "sampling_points.csv")
#' }
#'
#' @section When to Use Each Method:
#'
#' \itemize{
#'   \item **Spatial coverage sampling** ([ss_coverage()]): Best for model-based
#'     inference (kriging). Provides optimal spatial coverage for interpolation.
#'   \item **Stratified random sampling** ([ss_stratified()]): Best for design-based
#'     inference. Provides valid probability samples while ensuring coverage.
#'   \item **Simple random sampling** ([ss_random()]): Use when no prior
#'     stratification is desired. Valid for design-based inference.
#'   \item **Composite sampling** ([ss_composite()]): Use when samples will be
#'     physically combined before analysis to reduce costs.
#' }
#'
#' @references
#' de Gruijter, J.J., Brus, D.J., Bierkens, M.F.P., and Knotters, M. (2006).
#' \emph{Sampling for Natural Resource Monitoring}. Springer, Berlin.
#'
#' Walvoort, D.J.J., Brus, D.J., and de Gruijter, J.J. (2010). An R package
#' for spatial coverage sampling and random sampling from compact geographical
#' strata by k-means. \emph{Computers & Geosciences} 36, 1261-1267.
#' \doi{10.1016/j.cageo.2010.04.005}
#'
#' @docType package
#' @name soilsampling-package
#' @aliases soilsampling
#'
#' @importFrom sf st_geometry st_geometry_type st_crs st_is_longlat st_bbox
#'   st_make_grid st_union st_intersects st_coordinates st_as_sf st_sf
#'   st_sample st_nearest_feature st_sfc st_drop_geometry
#' @importFrom ggplot2 ggplot aes geom_raster geom_point geom_segment
#'   coord_fixed theme_minimal labs scale_shape_manual .data
#' @importFrom stats runif var quantile na.omit
#' @importFrom methods is
#'
"_PACKAGE"
