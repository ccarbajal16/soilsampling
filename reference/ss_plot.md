# Plot Stratification

Creates a visualization of the spatial stratification.

## Usage

``` r
ss_plot(
  x,
  samples = NULL,
  show_boundaries = TRUE,
  show_centroids = FALSE,
  fill_color = "#80CC80",
  boundary_color = "#CCE5CC",
  sample_color = "black",
  sample_alpha = 0.7,
  sample_size = 2,
  ...
)
```

## Arguments

- x:

  An object of class `ss_strata` or `ss_samples`.

- samples:

  Optional `ss_samples` object to overlay on stratification.

- show_boundaries:

  Logical, whether to show stratum boundaries (default TRUE).

- show_centroids:

  Logical, whether to show stratum centroids (default FALSE).

- fill_color:

  Character, fill color for the study area (default "#80CC80").

- boundary_color:

  Character, color for stratum boundaries (default "#CCE5CC").

- sample_color:

  Character, color for sampling points (default "black").

- sample_alpha:

  Numeric, transparency for sampling points (default 0.7).

- sample_size:

  Numeric, size of sampling points (default 2).

- ...:

  Additional arguments passed to ggplot2 functions.

## Value

A `ggplot` object.

## See also

[`ss_plot_samples()`](https://ccarbajal16.github.io/soilsampling/reference/ss_plot_samples.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(sf)
poly <- st_polygon(list(rbind(c(0, 0), c(100, 0), c(100, 50), c(0, 50), c(0, 0))))
study_area <- st_sf(geometry = st_sfc(poly))

strata <- ss_stratify(study_area, n_strata = 25)
ss_plot(strata)

samples <- ss_coverage(strata)
ss_plot(strata, samples = samples)
} # }
```
