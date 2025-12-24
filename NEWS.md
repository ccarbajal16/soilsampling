# soilsampling News

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