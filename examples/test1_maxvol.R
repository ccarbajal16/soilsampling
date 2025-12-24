library(terra)  # For raster processing
library(sf)     # For handling spatial vector data
library(soilsampling)  # For ss_maxvol function

# Load DEM
dem <- rast("data/dem_basin.tif")
names(dem) <- "dem"

# Compute terrain features
slope <- terrain(dem, "slope")
aspect <- terrain(dem, "aspect")
tpi <- terrain(dem, "TPI")
tri <- terrain(dem, "TRI")

# Add more features
ndvi <- rast("data/ndvi_basin.tif")
names(ndvi) <- "ndvi"
savi <- rast("data/savi_basin.tif")
names(savi) <- "savi"

# Resample to match DEM resolution and extent
ndvi <- resample(ndvi, dem, method = "bilinear")
savi <- resample(savi, dem, method = "bilinear")

# Stack features
features_stack <- c(dem, slope, aspect, tpi, tri, ndvi, savi)

# Convert to points
feature_points <- as.points(features_stack)
feature_sf <- st_as_sf(feature_points)

## Run Maxvol

# Run Maxvol with the validated feature names
samples_maxvol <- ss_maxvol(
  feature_sf,
  n = 100,
  features = c("dem", "slope", "aspect", "TPI", "TRI", "ndvi", "savi"),
  normalize = TRUE,
  add_coords = TRUE,
  verbose = TRUE
)

## Export results

# Get coordinates
coords_maxvol <- ss_to_data_frame(samples_maxvol)

# Export for field work
write.csv(coords_maxvol, "outputs/maxvol_sampling_points.csv", row.names = FALSE)

# Export as GeoPackage
st_write(ss_to_sf(samples_maxvol), "outputs/maxvol_points_basin.gpkg")

# Plot results with study area boundary
boundary <- st_read("data/basin.gpkg")

library(ggplot2)
dem_df <- as.data.frame(dem, xy = TRUE)
ggplot() +
  geom_raster(data = dem_df, aes(x = x, y = y, fill = dem)) +
  scale_fill_viridis_c() +
  geom_sf(data = boundary, fill = NA, color = "blue", size = 1) +
  geom_sf(data = samples_maxvol$samples, color = "red", size = 2) +
  labs(title = "Maxvol Sampling Points on DEM") +
  theme_minimal()

# Save plot
ggsave("outputs/maxvol_sampling_points_plot.png", width = 10, height = 8, dpi = 300)