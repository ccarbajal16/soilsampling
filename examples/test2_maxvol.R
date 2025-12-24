library(terra)  # For raster processing
library(sf)     # For handling spatial vector data
library(soilsampling)  # For ss_maxvol function

# Load DEM
dem <- rast("data/dem_basin.tif")
names(dem) <- "dem"

# Load land cover raster
landcover <- rast("data/landcover_basin.tif")
names(landcover) <- "landcover"

# Check resolutions
cat("DEM resolution:", res(dem), "meters\n")
cat("Land cover resolution:", res(landcover), "meters\n")

# Resample land cover to match DEM resolution
# Using "near" method to preserve categorical values
if (!identical(res(dem), res(landcover))) {
  cat("\nResampling land cover to match DEM resolution...\n")
  landcover_resampled <- resample(landcover, dem, method = "near")
  landcover <- landcover_resampled
  cat("Land cover resampled to:", res(landcover), "meters\n")
} else {
  cat("\nResolutions match - no resampling needed\n")
}

# Compute terrain features (same resolution as DEM)
slope <- terrain(dem, "slope")
aspect <- terrain(dem, "aspect")

# Stack features (all now have same resolution)
features_stack <- c(dem, slope, aspect, landcover)

# Convert to points
feature_points <- as.points(features_stack)
feature_sf <- st_as_sf(feature_points)

# Filter to Grassland or Cropland areas only (value 30 or 40)
# Land cover values:
# 10 = Trees, 20 = Shrubland, 30 = Grassland, 40 = Cropland
# 50 = Built-up, 60 = Sparse vegetation, 70 = Snow and ice
# 80 = Open water, 90 = Herbaceous wetland
cropland_sf <- feature_sf[feature_sf$landcover == 40, ]
grassland_sf <- feature_sf[feature_sf$landcover == 30, ]

cat("Total feature points:", nrow(feature_sf), "\n")
cat("Cropland points:", nrow(cropland_sf), "\n")
cat("Percentage cropland:", round(100 * nrow(cropland_sf) / nrow(feature_sf), 2), "%\n")

## Run Maxvol on cropland areas only
samples_land <- ss_maxvol(
  cropland_sf,
  n = 100,
  features = c("dem", "slope", "aspect"),
  normalize = TRUE,
  add_coords = TRUE,
  verbose = TRUE
)

cat("\nSampling completed. Number of samples:", nrow(ss_to_data_frame(samples)), "\n")

## Export results

# Get coordinates and attributes (includes land cover)
coords <- ss_to_data_frame(samples_land)

# Export for field work (CSV with all attributes including land cover)
write.csv(coords, "outputs/maxvol_sampling_points_cropland.csv", row.names = FALSE)

# Export as GeoPackage (preserves spatial information)
st_write(ss_to_sf(samples_land), "outputs/maxvol_points_cropland.gpkg", delete_dsn = TRUE)
cat("GeoPackage exported: maxvol_points_cropland_altomantaro.gpkg\n")

# Plotting using ggplot2 to visualize the sampling points on the DEM
library(ggplot2)
dem_df <- as.data.frame(dem, xy = TRUE)
colnames(dem_df) <- c("x", "y", "elevation")

ggplot() +
  geom_raster(data = dem_df, aes(x = x, y = y, fill = elevation)) +
  scale_fill_viridis_c() +
  geom_sf(data = samples_land$samples, color = "red", size = 2) +
  labs(title = "Maxvol Sampling Points on DEM") +
  theme_minimal()

# Export to PNG
ggsave("outputs/maxvol_sampling_points_cropland.png", width = 10, height = 8, dpi = 300)
