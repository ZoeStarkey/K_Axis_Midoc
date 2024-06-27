library(ggplot2)



#CHECKING SST 
# Convert raster to dataframe for ggplot
raster_df <- as.data.frame(tmp, xy = TRUE)
colnames(raster_df) <- c("longitude", "latitude", "SST")

# Plot the raster and points
ggplot() +
  geom_raster(data = raster_df, aes(x = longitude, y = latitude, fill = SST)) +
  geom_point(data = st_as_sf(km_sf), aes(x = lon_end, y = lat_end, color = SST), size = 2) +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  theme_minimal() +
  labs(title = "SST and Biomass Data Points",
       fill = "SST (°C)",
       color = "SST (°C)")

# Select a few points for manual verification
selected_points <- km_sf[1:5, ]

# Extract the SST values for these points from the raster
manual_sst_values <- extract(sst[[idate]], as(selected_points, "Spatial"))

# Convert the manually extracted SST values from Kelvin to Celsius
manual_sst_values_celsius <- manual_sst_values - 273.15

# Print the manually extracted and converted SST values
print(manual_sst_values_celsius)

# Compare with the values in km_sf
print(selected_points$SST)



###CHECKING CHLA 

ggplot() +
  geom_raster(data = R_df, aes(x = x, y = y, fill = value)) +
  geom_point(data = st_as_sf(km_sf), aes(x = lon_end, y = lat_end, fill = CHLA), size = 2, shape = 21) +
  scale_fill_viridis_c(name = "CHLA (log-transformed)") +
  theme_minimal() +
  labs(title = "CHLA and Biomass Data Points")

# Manual verification for a few points
selected_points <- km_sf[1:5, ]
manual_chla_values <- extract(R, as(selected_points, "Spatial"))

# Print the manually extracted CHLA values
print(manual_chla_values)

# Compare with the values in km_sf
print(selected_points$CHLA)

