# Load Predictor Rasters

Loads either a multi-layer raster file or a directory of single-layer
raster files into a `SpatRaster` stack.

## Usage

``` r
ss_load_rasters(path)
```

## Arguments

- path:

  Character, path to a `.tif` file or a directory containing `.tif`
  files.

## Value

A `SpatRaster` stack.

## Examples

``` r
if (FALSE) { # \dontrun{
predictors <- ss_load_rasters("data/predictors.tif")
} # }
```
