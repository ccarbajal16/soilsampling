# Package index

## Package Overview

Overview of the soilsampling package

- [`soilsampling-package`](https://ccarbajal16.github.io/soilsampling/reference/soilsampling-package.md)
  [`soilsampling`](https://ccarbajal16.github.io/soilsampling/reference/soilsampling-package.md)
  : soilsampling: Design and Optimization of Soil Sampling Schemes

## Sampling Methods

Functions for generating sampling designs

- [`ss_random()`](https://ccarbajal16.github.io/soilsampling/reference/ss_random.md)
  : Simple Random Sampling
- [`ss_stratified()`](https://ccarbajal16.github.io/soilsampling/reference/ss_stratified.md)
  : Stratified Random Sampling
- [`ss_coverage()`](https://ccarbajal16.github.io/soilsampling/reference/ss_coverage.md)
  : Spatial Coverage Sampling
- [`ss_coverage_equal_area()`](https://ccarbajal16.github.io/soilsampling/reference/ss_coverage_equal_area.md)
  : Spatial Coverage Sampling with Equal-Area Strata
- [`ss_composite()`](https://ccarbajal16.github.io/soilsampling/reference/ss_composite.md)
  : Composite Sampling
- [`ss_maxvol()`](https://ccarbajal16.github.io/soilsampling/reference/ss_maxvol.md)
  : Maxvol Optimal Design Sampling
- [`ss_get_samples()`](https://ccarbajal16.github.io/soilsampling/reference/ss_get_samples.md)
  : Get Samples as sf Object

## Stratification

Functions for creating and analyzing spatial strata

- [`ss_stratify()`](https://ccarbajal16.github.io/soilsampling/reference/ss_stratify.md)
  : Create Spatial Strata for Soil Sampling
- [`ss_n_strata()`](https://ccarbajal16.github.io/soilsampling/reference/ss_n_strata.md)
  : Get Number of Strata
- [`ss_area()`](https://ccarbajal16.github.io/soilsampling/reference/ss_area.md)
  : Get Area of Each Stratum
- [`ss_relative_area()`](https://ccarbajal16.github.io/soilsampling/reference/ss_relative_area.md)
  : Get Relative Area of Each Stratum
- [`ss_coverage_efficiency()`](https://ccarbajal16.github.io/soilsampling/reference/ss_coverage_efficiency.md)
  : Assess Coverage Efficiency of Stratification
- [`ss_distance_summary()`](https://ccarbajal16.github.io/soilsampling/reference/ss_distance_summary.md)
  : Compute Distance Summary Statistics by Stratum

## Visualization

Plotting functions

- [`ss_plot()`](https://ccarbajal16.github.io/soilsampling/reference/ss_plot.md)
  : Plot Stratification
- [`ss_plot_samples()`](https://ccarbajal16.github.io/soilsampling/reference/ss_plot_samples.md)
  : Plot Sampling Points
- [`plot(`*`<ss_samples>`*`)`](https://ccarbajal16.github.io/soilsampling/reference/plot.ss_samples.md)
  : Plot Method for ss_samples Objects
- [`plot(`*`<ss_strata>`*`)`](https://ccarbajal16.github.io/soilsampling/reference/plot.ss_strata.md)
  : Plot Method for ss_strata Objects

## Utilities

Helper functions and conversion utilities

- [`ss_to_sf()`](https://ccarbajal16.github.io/soilsampling/reference/ss_to_sf.md)
  : Convert to sf Object
- [`ss_to_data_frame()`](https://ccarbajal16.github.io/soilsampling/reference/ss_to_data_frame.md)
  : Convert to Data Frame
- [`ss_summary()`](https://ccarbajal16.github.io/soilsampling/reference/ss_summary.md)
  : Create Summary Statistics Table
- [`ss_n_samples()`](https://ccarbajal16.github.io/soilsampling/reference/ss_n_samples.md)
  : Get Number of Samples

## S3 Methods

Standard S3 methods for package objects

- [`summary(`*`<ss_samples>`*`)`](https://ccarbajal16.github.io/soilsampling/reference/summary.ss_samples.md)
  : Summary Method for ss_samples Objects
- [`summary(`*`<ss_strata>`*`)`](https://ccarbajal16.github.io/soilsampling/reference/summary.ss_strata.md)
  : Summary Method for ss_strata Objects
- [`ss_summary.maxvol()`](https://ccarbajal16.github.io/soilsampling/reference/ss_summary.maxvol.md)
  : Summary Method for Maxvol Sampling Results
- [`print(`*`<ss_samples>`*`)`](https://ccarbajal16.github.io/soilsampling/reference/print.ss_samples.md)
  : Print Method for ss_samples Objects
- [`print(`*`<ss_strata>`*`)`](https://ccarbajal16.github.io/soilsampling/reference/print.ss_strata.md)
  : Print Method for ss_strata Objects
- [`print(`*`<ss_coverage_efficiency>`*`)`](https://ccarbajal16.github.io/soilsampling/reference/print.ss_coverage_efficiency.md)
  : Print Method for ss_coverage_efficiency Objects

## KL Sample Size Optimization

Sample size optimization for cLHS based on KL divergence

- [`ss_load_rasters()`](https://ccarbajal16.github.io/soilsampling/reference/ss_load_rasters.md)
  : Load Predictor Rasters
- [`ss_kl_divergence()`](https://ccarbajal16.github.io/soilsampling/reference/ss_kl_divergence.md)
  : KL Divergence Between Population and Sample Distributions
- [`ss_kl_optimize()`](https://ccarbajal16.github.io/soilsampling/reference/ss_kl_optimize.md)
  : Optimize cLHS Sample Size Using KL Divergence
- [`ss_kl_size()`](https://ccarbajal16.github.io/soilsampling/reference/ss_kl_size.md)
  : End-to-End cLHS Sample Size Optimization
- [`ss_kl_save_plots()`](https://ccarbajal16.github.io/soilsampling/reference/ss_kl_save_plots.md)
  : Save KL Optimization Plots

## cLHS + Random Forest Optimization

cLHS baseline sampling refined by Random Forest simulated annealing

- [`ss_clhs_sample()`](https://ccarbajal16.github.io/soilsampling/reference/ss_clhs_sample.md)
  : Conditioned Latin Hypercube Sampling From a Raster Stack
- [`ss_rf_mse()`](https://ccarbajal16.github.io/soilsampling/reference/ss_rf_mse.md)
  : Cross-Validated Random Forest MSE for a Sample Design
- [`ss_rf_optimize()`](https://ccarbajal16.github.io/soilsampling/reference/ss_rf_optimize.md)
  : Optimize a Sample Design With Random Forest Simulated Annealing
- [`ss_rf_size()`](https://ccarbajal16.github.io/soilsampling/reference/ss_rf_size.md)
  : End-to-End cLHS + Random Forest Sampling Optimization

## Alternative Site Selection

Environmentally similar alternative sites for inaccessible locations

- [`ss_alt_similarity()`](https://ccarbajal16.github.io/soilsampling/reference/ss_alt_similarity.md)
  : Environmental Similarity Between a Target Site and Candidate Sites
- [`ss_alt_candidates()`](https://ccarbajal16.github.io/soilsampling/reference/ss_alt_candidates.md)
  : Generate Candidate Sites for Alternative Site Selection
- [`ss_alt_filter_buffer()`](https://ccarbajal16.github.io/soilsampling/reference/ss_alt_filter_buffer.md)
  : Exclude Candidate Sites Within a Distance Buffer of Target Sites
- [`ss_alt_rank()`](https://ccarbajal16.github.io/soilsampling/reference/ss_alt_rank.md)
  : Rank and Select the Top Similar Alternative Sites
- [`ss_alt_standardize_sites()`](https://ccarbajal16.github.io/soilsampling/reference/ss_alt_standardize_sites.md)
  : Standardize Site Coordinate Data
- [`ss_alt_sites()`](https://ccarbajal16.github.io/soilsampling/reference/ss_alt_sites.md)
  : Find Alternative Sampling Sites for Inaccessible Locations
