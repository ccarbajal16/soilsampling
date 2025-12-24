## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## ----eval=FALSE---------------------------------------------------------------
# # Install from local source
# install.packages(".", repos = NULL, type = "source")
# 
# # Or using devtools
# devtools::install()

## ----message=FALSE------------------------------------------------------------
library(soilsampling)
library(sf)

# Create a study area
poly <- st_polygon(list(rbind(
  c(0, 0), c(100, 0), c(100, 50), c(0, 50), c(0, 0)
)))
study_area <- st_sf(geometry = st_sfc(poly))

## -----------------------------------------------------------------------------
# Set seed for reproducibility
set.seed(42)

# Create spatial coverage sampling design
samples_coverage <- ss_coverage(study_area, n_strata = 25, n_try = 5)

# View summary
print(samples_coverage)

# Plot
ss_plot(samples_coverage)

## -----------------------------------------------------------------------------
# First create strata
strata <- ss_stratify(study_area, n_strata = 25, n_try = 5)

# Take random samples within each stratum
samples_stratified <- ss_stratified(strata, n_per_stratum = 1)

# Plot
ss_plot(strata, samples = samples_stratified)

## -----------------------------------------------------------------------------
# Simple random sampling
samples_random <- ss_random(study_area, n = 25)

# Plot
ss_plot_samples(samples_random)

## -----------------------------------------------------------------------------
# Get coordinates as data frame
coords <- ss_to_data_frame(samples_coverage)
head(coords)

## ----eval=FALSE---------------------------------------------------------------
# # Export to CSV
# write.csv(coords, "sampling_points.csv", row.names = FALSE)

## ----eval=FALSE---------------------------------------------------------------
# # Get as sf object
# samples_sf <- ss_to_sf(samples_coverage)
# 
# # Export to GeoPackage
# st_write(samples_sf, "sampling_points.gpkg")
# 
# # Export to Shapefile
# st_write(samples_sf, "sampling_points.shp")

## ----eval=FALSE---------------------------------------------------------------
# # Read study area from shapefile
# study_area <- st_read("path/to/study_area.shp")
# 
# # Create sampling design
# set.seed(42)
# samples <- ss_coverage(study_area, n_strata = 50, n_try = 10)
# 
# # Export coordinates
# coords <- ss_to_data_frame(samples)
# write.csv(coords, "field_sampling_points.csv", row.names = FALSE)

