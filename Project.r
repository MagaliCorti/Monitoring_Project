# R Project for Monitoring Ecosystems

# With this project I want to highligt the differences in Snow Cover Extent in the region where I live between the winter 2020/2021 and winter 2021/2022

# setting the working directory
setwd("/Users/magalicorti/Desktop/project/")

# recalling the libraries for already installed packaged we will need
library(raster)        # to import files in R, and stacking operaytions
library(ggplot2)       # to plot data with ggplot function
library(gridExtra) 
library (RStoolbox)    # for remote sensing data processing
library(ncdf4)         # to open Copernicus data with nc extention
library(viridis)       # to use viridis palette
library(patchwork)     # to plot together plots made with ggplot
 


# importing Copernicus data about december 2021
snow20211217 <- raster("c_gls_SCE500_202112170000_CEURO_MODIS_V1.0.1.nc")

# visualizing the image imported
plot(snow20211217)

# cropping the image focusing on the area of interest (Northern Itlay)
ext <- c(8, 11, 45, 47)
snow21 <- crop(snow20211217, ext)

# visualizing new image
plot(snow21)

# changing the colors
cl <- colorRampPalette(c("dark blue", "blue", "light blue"))(100)
plot(snow21, col=cl)



# importing Copernicus data
snow20201217 <- raster("c_gls_SCE500_202012190000_CEURO_MODIS_V1.0.1.nc")
# cropping the image focusing on the area of interest
snow20 <- crop(snow20201219, ext)
plot(snow20)



