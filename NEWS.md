# soilsampling News

## Version 0.2.0 (2026-07-21)

### New Features

#### KL Sample Size Optimization

Sample size optimization for cLHS designs based on Kullback-Leibler
divergence between population and sample distributions
(Malone et al. 2019).

* `ss_load_rasters()`: Load a multi-layer raster or a directory of
  single-layer rasters
* `ss_kl_divergence()`: KL divergence between population and sample
  distributions
* `ss_kl_optimize()`: Optimize cLHS sample size via KL divergence,
  with exponential decay curve fitting
* `ss_kl_size()`: End-to-end workflow from raster/data frame to
  optimal sample size, with optional CSV/PNG export
* `ss_kl_save_plots()`: Save KL divergence and CDF plots independently

#### cLHS + Random Forest Optimization

Two-stage sampling design workflow: a cLHS baseline refined by
Random Forest simulated annealing (Wadoux et al. 2019).

* `ss_clhs_sample()`: Conditioned Latin hypercube baseline sampling
  from a raster stack
* `ss_rf_mse()`: Cross-validated Random Forest MSE as an objective
  function
* `ss_rf_optimize()`: Simulated annealing refinement of a sample
  design to minimize RF MSE
* `ss_rf_size()`: End-to-end workflow with comparison plots and
  optional CSV/PNG export

#### Alternative Site Selection

Find environmentally similar alternative sites when original
sampling locations become inaccessible.

* `ss_alt_similarity()`: Environmental similarity via Mahalanobis,
  Euclidean, or Gower distance
* `ss_alt_candidates()`: Generate a candidate site pool (random or
  systematic grid)
* `ss_alt_filter_buffer()`: Exclude candidates within a minimum
  distance of target sites
* `ss_alt_rank()`: Rank and select the top similar alternatives
* `ss_alt_standardize_sites()`: Standardize site coordinate data to
  the package's `site_id`/`x`/`y`/`type` layout
* `ss_alt_sites()`: End-to-end alternative site selection workflow,
  with optional CSV export

### Dependencies

* Adds **terra**, **clhs**, **dplyr**, **minpack.lm**,
  **randomForest**, **gridExtra**, **utils**, and **grDevices** to
  Imports

## Version 0.1.0 (2025-01-15)

### Initial Release

This is the first release of the **soilsampling** package, providing methods
for designing soil sampling schemes.

### Features

#### Stratification

* `ss_stratify()`: Create compact geographical strata using k-means clustering
  - Transfer algorithm for compact strata (default)
  - Swop algorithm for equal-area strata (`equal_area = TRUE`)
  - Support for prior points (existing sampling locations)
  - Pure R implementation, no Java required

#### Sampling Methods

* `ss_random()`: Simple random sampling within a study area
* `ss_stratified()`: Stratified random sampling with compact strata
* `ss_coverage()`: Spatial coverage sampling at stratum centroids
* `ss_coverage_equal_area()`: Coverage sampling with equal-area strata
* `ss_maxvol()`: Maxvol optimal design sampling using D-optimal experimental design
  - Feature-based sample selection for maximum diversity
  - SVD-based pseudoinverse for numerical stability
  - Optional minimum distance constraint
  - Feature normalization support
  - Deterministic point selection based on feature space
* `ss_composite()`: Composite sampling from equal-area strata

#### Visualization

* `ss_plot()`: Plot stratification with optional sampling points
* `ss_plot_samples()`: Plot sampling points only
* S3 `plot()` methods for `ss_strata` and `ss_samples` objects

#### Utilities

* `ss_to_sf()`: Convert results to sf object
* `ss_to_data_frame()`: Convert results to data frame with coordinates
* `ss_summary()`: Get summary statistics
* `ss_get_samples()`: Extract samples as sf object
* `ss_n_strata()`: Get number of strata
* `ss_n_samples()`: Get number of samples
* `ss_area()`: Get stratum areas
* `ss_relative_area()`: Get relative stratum areas

### Dependencies

* Requires R >= 4.1.0
* Uses **sf** for spatial operations (no sp or Java required)
* Uses **ggplot2** for visualization

### Notes

The algorithms are based on the methods described in:

* **K-means stratification**: Walvoort, D.J.J., Brus, D.J., and de Gruijter, J.J. (2010).
  An R package for spatial coverage sampling and random sampling from compact
  geographical strata by k-means. *Computers & Geosciences* 36, 1261-1267.
  DOI: 10.1016/j.cageo.2010.04.005

* **Maxvol optimal design**: Petrovskaia, N., Korveh, K., and Maas, E. (2021).
  Optimal soil sampling design based on the maxvol algorithm. *Geoderma* 381, 114733.
  DOI: 10.1016/j.geoderma.2020.114733