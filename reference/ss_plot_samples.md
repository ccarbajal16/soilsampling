# Plot Sampling Points

Creates a simple plot of sampling points.

## Usage

``` r
ss_plot_samples(x, color = "black", alpha = 0.7, size = 2, ...)
```

## Arguments

- x:

  An object of class `ss_samples`.

- color:

  Character, point color (default "black").

- alpha:

  Numeric, point transparency (default 0.7).

- size:

  Numeric, point size (default 2).

- ...:

  Additional arguments (currently ignored).

## Value

A `ggplot` object.

## See also

[`ss_plot()`](https://ccarbajal16.github.io/soilsampling/reference/ss_plot.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(sf)
poly <- st_polygon(list(rbind(c(0, 0), c(100, 0), c(100, 50), c(0, 50), c(0, 0))))
study_area <- st_sf(geometry = st_sfc(poly))

samples <- ss_random(study_area, n = 30)
ss_plot_samples(samples)
} # }
```
