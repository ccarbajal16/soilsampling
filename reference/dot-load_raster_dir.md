# Load All Rasters From a Directory

Load All Rasters From a Directory

## Usage

``` r
.load_raster_dir(dir, pattern = "*.tif", recursive = FALSE)
```

## Arguments

- dir:

  Character, directory containing `.tif` predictor rasters.

- pattern:

  Character, filename pattern. Default `"*.tif"`.

- recursive:

  Logical, search subdirectories. Default `FALSE`.

## Value

A `SpatRaster` stack.
