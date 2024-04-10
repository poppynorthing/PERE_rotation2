# Creating Climate Maps for PERE
# Poppy Northing
# pcnorthing@arizona.edu
# 10apr2024

library(raster)
library(geodata)
library(ggplot2)
library(usmap)

# Load US map from geodata, save in current directory
us <- gadm(country = "USA", level = 1, resolution = 2,
           path = ".")

# Load worldclim data for monthly average temp & precip in the USA from 1970-2000
tavg <- worldclim_country("USA", var="tavg", res = 0.5, path=tempdir())
prec <- worldclim_country("USA", var="prec", res = 0.5, path=tempdir())

# Create vector of points to add sites
lat <- c(32.44, 32.24, 32.34, 34.28, 33.84, 33.85, 32.77, 31.93, 34.24, 34.60, 34.72)
long <- c(-111.10, -111.16, -112.90, -113.10, -112.32, -111.83, -112.26, -109.09, -114.64, -113.54, -113.61)
lat_name <- "latitude"
long_name <- "longitude"

sites <- data.frame(lat, long)
names(sites) <- c(lat_name, long_name)

# Plot just the data for February (month 2)
plot(tavg$USA_wc2.1_30s_tavg_12, xlim = c(-116,-108), ylim = c(31,37.8))
points(sites$longitude, sites$latitude, pch = 16, col = "black")
plot(us, border = "black", lwd = 1, add = TRUE)


plot(prec$USA_wc2.1_30s_prec_12, xlim = c(-116,-108), ylim = c(31,37.8), legend = TRUE)
plot(us, border = "black", lwd = 1, add = TRUE)
points(sites$longitude, sites$latitude, pch = 8, col = "black")
mtext("Precipitation (mm)", side = 4)

# Convert raster to data frame for geom_raster
tavg_df <- as.data.frame(tavg$USA_wc2.1_30s_tavg_2, xy = TRUE)

ggplot() + geom_raster(data = tavg_df, aes(x = x, y = y, fill = USA_wc2.1_30s_tavg_2)) +
  coord_quickmap()
