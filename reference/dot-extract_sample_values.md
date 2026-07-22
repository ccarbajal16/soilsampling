# Extract Raster Values at Sample Locations

Extract Raster Values at Sample Locations

## Usage

``` r
.extract_sample_values(rasters, sample_points)
```

## Arguments

- rasters:

  A `SpatRaster` stack of ancillary variables.

- sample_points:

  Spatial points, either an `sf` object or a matrix of x, y coordinates.

## Value

A data frame of extracted values.
