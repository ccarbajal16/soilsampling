# soilsampling: Soil Sampling Design Methods

The soilsampling package provides methods for designing soil sampling
schemes including spatial coverage sampling, simple random sampling,
stratified random sampling, and composite sampling. The package uses sf
for spatial operations and does not require Java or the sp package.

## Details

The package implements three main sampling approaches:

- **Simple Random Sampling**
  ([`ss_random()`](https://ccarbajal16.github.io/soilsampling/reference/ss_random.md)):
  Selects sampling locations uniformly at random within the study area.

- **Stratified Random Sampling**
  ([`ss_stratified()`](https://ccarbajal16.github.io/soilsampling/reference/ss_stratified.md)):
  Divides the study area into compact strata and samples randomly within
  each stratum.

- **Spatial Coverage Sampling**
  ([`ss_coverage()`](https://ccarbajal16.github.io/soilsampling/reference/ss_coverage.md)):
  Places samples at the centroids of compact geographical strata for
  optimal spatial coverage.

The stratification is based on k-means clustering as described in
Walvoort et al. (2010). Two k-means algorithms are available:

- **Transfer algorithm**: Standard k-means that creates compact strata
  of potentially unequal sizes.

- **Swop algorithm**: Creates compact strata of equal size, useful for
  composite sampling.

## Main Functions

**Stratification:**

- [`ss_stratify()`](https://ccarbajal16.github.io/soilsampling/reference/ss_stratify.md) -
  Create spatial strata using k-means

**Sampling:**

- [`ss_random()`](https://ccarbajal16.github.io/soilsampling/reference/ss_random.md) -
  Simple random sampling

- [`ss_stratified()`](https://ccarbajal16.github.io/soilsampling/reference/ss_stratified.md) -
  Stratified random sampling

- [`ss_coverage()`](https://ccarbajal16.github.io/soilsampling/reference/ss_coverage.md) -
  Spatial coverage sampling (centroids)

- [`ss_coverage_equal_area()`](https://ccarbajal16.github.io/soilsampling/reference/ss_coverage_equal_area.md) -
  Coverage sampling with equal-area strata

- [`ss_composite()`](https://ccarbajal16.github.io/soilsampling/reference/ss_composite.md) -
  Composite sampling from equal-area strata

**Visualization:**

- [`ss_plot()`](https://ccarbajal16.github.io/soilsampling/reference/ss_plot.md) -
  Plot stratification and samples

- [`ss_plot_samples()`](https://ccarbajal16.github.io/soilsampling/reference/ss_plot_samples.md) -
  Plot sampling points only

**Utilities:**

- [`ss_to_sf()`](https://ccarbajal16.github.io/soilsampling/reference/ss_to_sf.md) -
  Convert results to sf object

- [`ss_to_data_frame()`](https://ccarbajal16.github.io/soilsampling/reference/ss_to_data_frame.md) -
  Convert results to data frame

- [`ss_summary()`](https://ccarbajal16.github.io/soilsampling/reference/ss_summary.md) -
  Get summary statistics

- [`ss_n_strata()`](https://ccarbajal16.github.io/soilsampling/reference/ss_n_strata.md) -
  Get number of strata

- [`ss_n_samples()`](https://ccarbajal16.github.io/soilsampling/reference/ss_n_samples.md) -
  Get number of samples

- [`ss_area()`](https://ccarbajal16.github.io/soilsampling/reference/ss_area.md) -
  Get stratum areas

- [`ss_relative_area()`](https://ccarbajal16.github.io/soilsampling/reference/ss_relative_area.md) -
  Get relative stratum areas

## Workflow

A typical workflow for spatial coverage sampling:

    library(soilsampling)
    library(sf)

    # Load or create study area
    study_area <- st_read("study_area.shp")

    # Option 1: Direct coverage sampling
    samples <- ss_coverage(study_area, n_strata = 50, n_try = 10)

    # Option 2: Create strata first, then sample
    strata <- ss_stratify(study_area, n_strata = 50, n_try = 10)
    samples <- ss_coverage(strata)

    # Visualize
    ss_plot(strata, samples = samples)

    # Export coordinates
    coords <- ss_to_data_frame(samples)
    write.csv(coords, "sampling_points.csv")

## When to Use Each Method

- **Spatial coverage sampling**
  ([`ss_coverage()`](https://ccarbajal16.github.io/soilsampling/reference/ss_coverage.md)):
  Best for model-based inference (kriging). Provides optimal spatial
  coverage for interpolation.

- **Stratified random sampling**
  ([`ss_stratified()`](https://ccarbajal16.github.io/soilsampling/reference/ss_stratified.md)):
  Best for design-based inference. Provides valid probability samples
  while ensuring coverage.

- **Simple random sampling**
  ([`ss_random()`](https://ccarbajal16.github.io/soilsampling/reference/ss_random.md)):
  Use when no prior stratification is desired. Valid for design-based
  inference.

- **Composite sampling**
  ([`ss_composite()`](https://ccarbajal16.github.io/soilsampling/reference/ss_composite.md)):
  Use when samples will be physically combined before analysis to reduce
  costs.

## References

de Gruijter, J.J., Brus, D.J., Bierkens, M.F.P., and Knotters, M.
(2006). *Sampling for Natural Resource Monitoring*. Springer, Berlin.

Walvoort, D.J.J., Brus, D.J., and de Gruijter, J.J. (2010). An R package
for spatial coverage sampling and random sampling from compact
geographical strata by k-means. *Computers & Geosciences* 36, 1261-1267.
[doi:10.1016/j.cageo.2010.04.005](https://doi.org/10.1016/j.cageo.2010.04.005)

## See also

Useful links:

- <https://ccarbajal16.github.io/soilsampling/>

- <https://github.com/ccarbajal16/soilsampling>

- Report bugs at <https://github.com/ccarbajal16/soilsampling/issues>

## Author

**Maintainer**: Carlos Carbajal <ccarbajal@educagis.com>
