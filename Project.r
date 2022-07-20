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
 

# importing Copernicus data for winter 2022
snow20220205 <- raster("c_gls_SCE500_202202050000_CEURO_MODIS_V1.0.1.nc")
# visualizing the image imported
plot(snow20220205)
# cropping the image focusing on the area of interest (Northern Itlay)
ext <- c(8, 11, 45, 47)
snow22 <- crop(snow20220205, ext)
# visualizing new image
plot(snow22)

# importing Copernicus data for winter 2021
snow20210111 <- raster("c_gls_SCE500_202101110000_CEURO_MODIS_V1.0.1.nc")
# cropping the image focusing on the same area of interest
snow21 <- crop(snow20210111, ext)
plot(snow21)

# using ggplot function with viridis (changing colorRampPalette)
p21 <- ggplot() + geom_raster(snow21, mapping = aes(x=x, y=y, fill = Snow.Cover.Extent)) + scale_fill_viridis() + ggtitle("Snow Cover in winter 2021")
p22 <- ggplot() + geom_raster(snow22, mapping = aes(x=x, y=y, fill = Snow.Cover.Extent)) + scale_fill_viridis() + ggtitle("Snow Cover in winter 2022")

# to visualize the two plots together in an horizontal sequence
# if I wanted to visualize them in a vertical sequence Ishuold have used / instead of +
p21 + p22


# otherwise to import multiple data with with the same pattern in the name I can create a list and use the lapply function
rlist <- list.files(pattern = "SCE")
rlist # list of 2 images
list_rast <- lapply(rlist, brick)   # can also use raster function 
list_rast
# creating a stack
snowstack <- stack(list_rast)
snowstack



# plotting frequency distribution -> plot all histograms together
par(mfrow=c(1,2))
hist(snow21, xlim = c(0,200))
hist(snow22, xlim = c(0,200))

# plotting values of 2021 and 2022
# comparing data one in function of the other
plot(snow21, snow22) # making line  passing trough 0
abline(0, 1, col="red") # plotting line 

pairs(snowstack)


plotRGB(snowstack$X2021.01.11, r=1, g=2, b=3, stretch = "Lin")

s21 <- unsuperClass(snow21, nClasses=2)
plot(s21$map)

s22 <- unsuperClass(snow22, nClasses=2)
plot(s22$map)





