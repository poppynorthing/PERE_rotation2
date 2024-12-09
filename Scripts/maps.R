# Creating Climate Maps for PERE
# Poppy Northing
# pcnorthing@arizona.edu
# 10apr2024

library(raster)
library(geodata)
library(ggplot2)
library(usmap)
library(rgbif)
library(tidyverse)
library(dismo)

# Load US map from geodata, save in current directory
us <- gadm(country = "USA", level = 1, resolution = 2,
           path = ".")

usmex <- gadm(country = c("USA", "MEX"), level = 1, resolution = 2,
              path = ".")

# Load worldclim data for monthly average temp & precip in the USA from 1970-2000
tavg <- worldclim_country("USA", var="tavg", res = 0.5, path=tempdir())
prec <- worldclim_country("USA", var="prec", res = 0.5, path=tempdir())

# Create PCN site data frame with site coordinates
lat <- c(32.44, 32.24, 32.34, 34.28, 33.84, 33.85, 32.77, 31.93, 34.24, 34.60, 34.72)
long <- c(-111.10, -111.16, -112.90, -113.10, -112.32, -111.83, -112.26, -109.09, -114.64, -113.54, -113.61)
lat_name <- "latitude"
long_name <- "longitude"

sites <- data.frame(lat, long)
names(sites) <- c(lat_name, long_name)

# Create XYG site dataframe with additional site coordinates

lat2 <- c(32.22, 32.47, 31.91, 32.77, 32.17, 31.91, 32.58, 34.71, 34.77, 34.25, 33.85, 33.83, 34.28, 31.93, 31.93)
long2 <- c(-111.01, -111.51, -110.99, -112.26, -112.76, -111.95, -111.33, -113.61, -114.62, -114.65, -111.82, -112.28, -113.1, -111.01, -109.08)

monica_sites <- data.frame(lat2, long2)
names(monica_sites) <- c(lat_name, long_name)

# Create dataframe with PERE occurances from gbif

pere_taxon_key <- name_backbone("Pectocarya recurvata")

occ_download(pred("taxonKey", pere_taxon_key$usageKey),
             user = "pcnorthing",
             pwd = "5Mp4!R@u8XdnKez",
             email = "pcnorthing@arizona.edu",
             format = "SIMPLE_CSV")

d <- occ_download_get('0001217-241107131044228') %>%
  occ_download_import()

pere_occs <- d # rename variable

pere_occs <- pere_occs %>% drop_na(decimalLatitude) # remove rows with missing lat/long data

lat3 <- as.vector(pere_occs['decimalLatitude']) # make vector of all latitude values
long3 <-as.vector(pere_occs['decimalLongitude']) # make vector of all longitude values

occs <- data.frame(lat3,long3) # combine into data frame
names(occs) <- c(lat_name, long_name) # add column names

# Plot all of the GBIF PERE occurrences

plot(usmex, border = "#3b3b3b", xlim = c(-120,-106.5), ylim = c(25.5,37.8), background="#F7F3EA", legend = T)
scalebar(500, type = "bar", divs = 4, below = "kilometers", xy=click(), adj=c(0.5, -1.5))
points(occs$longitude, occs$latitude, pch = 20, col = "lightblue")
points(monica_sites$longitude, monica_sites$latitude, pch = 17, col = "#E9847A")
points(sites$longitude, sites$latitude, pch = 8, col = "darkblue")

plot(usmex, border = "#3b3b3b", xlim = c(-116,-108), ylim = c(31,37.8), background="#F7F3EA", legend = T)
scalebar(100, type = "bar", divs = 4, below = "kilometers", xy=click(), adj=c(0.5, -1.5))
points(occs$longitude, occs$latitude, pch = 20, col = "lightblue")
points(monica_sites$longitude, monica_sites$latitude, pch = 17, col = "#E9847A", cex = 2)


# Plot the temp and precip data for December (month 2) with sites

# Avg Winter Temp
plot(tavg$USA_wc2.1_30s_tavg_12, xlim = c(-120,-106.5), ylim = c(25.5,37.8), col=terrain.colors(50))
points(occs$longitude, occs$latitude, pch = 1, col = "darkgrey", cex = 0.5)
points(sites$longitude, sites$latitude, pch = 8, col = "black", alpha = 0.5)
plot(usmex, border = "black", lwd = 1, add = TRUE)

?points

# Total Precip (large region)
plot(prec$USA_wc2.1_30s_prec_12, xlim = c(-120,-106.5), ylim = c(25.5,37.8), legend = TRUE, col=terrain.colors(50))
plot(usmex, border = "black", lwd = 1, add = TRUE)
points(occs$longitude, occs$latitude, pch = 1, col = "darkgrey", cex = 0.5)
points(sites$longitude, sites$latitude, pch = 8, col = "black")

?plot

# Avg Winter Temp (small region)
plot(tavg$USA_wc2.1_30s_tavg_12, xlim = c(-116,-108), ylim = c(31,37.8), legend = TRUE, col=terrain.colors(50))
plot(usmex, border = "black", lwd = 1, add = TRUE)
points(occs$longitude, occs$latitude, pch = 1, col = "darkgrey", cex = 0.5)
points(monica_sites$longitude, monica_sites$latitude, pch = 8, col = "black")
#mtext("Temperature (C)", side = 4)

# Total Precip (small region)
plot(prec$USA_wc2.1_30s_prec_12, xlim = c(-116,-108), ylim = c(31,37.8), legend = TRUE, col=terrain.colors(50))
plot(usmex, border = "black", lwd = 1, add = TRUE)
points(occs$longitude, occs$latitude, pch = 1, col = "darkgrey", cex = 0.5)
points(monica_sites$longitude, monica_sites$latitude, pch = 8, col = "black")
#mtext("Precipitation (mm)", side = 4)

?options







# Get elevation data from the internet for specified coordinates
elev <- geodata_path("SRTM", download=T, lon=-76.6, lat=18.1)

# Specify desired extent
ext <- c(-76.68, -76.65, 18.08, 18.11)
ext2 <- c(-76.675, -76.655, 18.088, 18.10) # for plotting


# Crop raster to desired extent
ele <- crop(elev, ext)


## hillshading
slope <- terrain(ele, opt = "slope")
aspect <- terrain(ele, opt = "aspect")
hill <- hillShade(slope, aspect, 40, 180)

## plot
plot(hill, col = grey(0:100/100), legend = FALSE, interpolate=T,
     xlim=ext2[1:2], ylim=ext2[3:4],
     axes=F, box=F)
plot(ele, col=topo.colors(16), alpha = 0.5, add = T, interpolate=T,
     xlim=ext2[1:2], ylim=ext2[3:4], legend=F)
contour(crop(ele, ext2), levels=seq(1000, 2000, 50), add=T)
axis(1, at=ext2[1:2])
axis(2, at=c(ext2[3]+0.0004, ext2[4]), pos=ext2[1], las=1)