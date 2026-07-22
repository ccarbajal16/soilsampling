# Filter Candidate Coordinates to Valid (Non-NA) Raster Cells

Filter Candidate Coordinates to Valid (Non-NA) Raster Cells

## Usage

``` r
.alt_filter_valid_coords(layer, coords)
```

## Arguments

- layer:

  A single-layer `SpatRaster`.

- coords:

  Data frame with `x`, `y` columns.

## Value

The subset of `coords` falling on non-`NA` cells of `layer`.
