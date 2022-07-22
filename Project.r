# R Project for Monitoring Ecosystems

# With this project I want to highligt the differences in Snow Cover Extent in the region where I live between the winter 2020/2021 and winter 2021/2022

# setting the working directory
setwd("/Users/magalicorti/Desktop/project/")

# recalling the libraries for already installed packaged we will need
library(raster)        # to import files in R, and stacking operaytions
library(ggplot2)       # to plot data with ggplot function
library(gridExtra)     # to plot together plots made with ggplot
library (RStoolbox)    # for remote sensing data analysis -> to make the classification
library(ncdf4)         # to open Copernicus data with nc extention
library(viridis)       # to use viridis palette
library(patchwork)     # to plot together plots made with ggplot
 

# SCE - Snow Cover Extent
# Copernicus data with geometric resolution of 500m x 500m per pixel
# importing Copernicus data for winter 2022
snow20220125 <- raster("c_gls_SCE500_202201250000_CEURO_MODIS_V1.0.1.nc")
# visualizing the image imported
plot(snow20220125)
# cropping the image focusing on the area of interest (Central Alps in Northern Itlay)
ext <- c(7, 13, 45.5, 47)
snow22 <- crop(snow20220125, ext)
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

# to visualize the two plots together in a vertical sequence
# if I wanted to visualize them in an horizontal sequence Ishuold have used + instead of /
p21 / p22


# otherwise to import multiple data with with the same pattern in the name I can create a list and use the lapply function
rlist <- list.files(pattern = "SCE")
rlist # list of 2 images
list_rast <- lapply(rlist, brick)   # can also use raster function 
list_rast
# creating a stack
snowstack <- stack(list_rast)
snowstack


# computing differece in snow cover between 2021 and 2022
SCEdif <- (snow22 - snow21)
plot(SCEdif)
# changing color palette for a more clear visualization
cldif = colorRampPalette(c("blue", "white", "red"))(100)
plot(SCEdif, col=cldif) # in blue the snow missing in 2022 with respect to 2021



# qualitative analysis of proportions and frequency distributions
# plotting frequency distribution of snow cover values -> plot all histograms together
par(mfrow=c(1,2))
hist(snow21, xlim = c(0,200))
hist(snow22, xlim = c(0,200))


# PERCHE DISTRIBUZIONE DEI. DATI LUNGO LINEE?? GUARDO MAPPE
# plotting values of 2022 against 2021
# comparing data one in function of the other
plot(snow21, snow22, xlab = "Snow Cover Extent in 2021", ylab = "Snow Cover Extent in 2022") 
abline(0, 1, col="red") # plotting line, making it passing trough 0

# plotting automatically all graphs together, very usefull when we have many graphs
pairs(snowstack)



# computing proportiuons of snow cover in winter 2021
# passing from a layers with values ranging 0-200 to 3 values (1 - 2 - 3)
s21 <- unsuperClass(snow21, nClasses=3)
# plotting the two maps, the original one and the new one after running the unsupervised classification (in one column and two rows) -> identifying the three classes
par(mfrow=c(2,1))
plot(snow21)
plot(s21$map)
# computing the frequency for each class
freq(s21$map)
total <- 360000 # tot amount of pixels -> run s21 -> look at tird value of dimension (ncell)
# compute percentage per type of cover (frequency of class / total)
propsnow21 <- 239706/total # class 2
propbare21 <- 94315/total # class 1
propwater21 <- 25979/total # class 3

propbare21 # 0.2619861. = 26.2%
propsnow21 # 0.66585 = 66.6%
propwater21 # 0.07216389 = 7.2%

# building a dataframe with type of cover and proportion of pixels
cover <- c("Snow", "Bare", "Water/Cloud")
prop21 <- c(propsnow21, propbare21, propwater21)
proportion21 <- data.frame(cover, prop21) # proportion of pixels in 2021
proportion21 # quantitative data

# plotting data with ggplot2
# ggplot function -> first argument = dataset, other arguments = aesthetic, color stored in cover
# geom_bar function explainig type of graph
# stat - statistics used, identity bc we're using data as they are (no median or mean)
# for changing limit from 0 to 1 use ylim()
PR21 <- ggplot(proportion21, aes(x=cover, y=prop21, color=cover)) + geom_bar(stat="identity", fill="white") + ylim(0,1)


# let's do the same thing for winter 2022
s22 <- unsuperClass(snow22, nClasses=3)
par(mfrow=c(2,1))
plot(s22$map)
plot(snow22)

freq(s22$map)
total <- 360000
# compute percentage per type of cover (frequency of class / total)
propsnow22 <- 118301/total # class 3
propbare22 <- 176219/total # class 1
propwater22 <- 65480/total # class 2
# dataframe with proportion per cover type
prop22 <- c(propsnow22, propbare22, propwater22)
proportion22 <- data.frame(cover, prop22) 
proportion22

PR22 <- ggplot(proportion22, aes(x=cover, y=prop22, color=cover)) + geom_bar(stat="identity", fill="white") + ylim(0,1)

# plotting the 2 ggplot graph together in one row using a different package tha patchwork
grid.arrange(PR21, PR22, nrow=1)










# SWI messeggio di warning NON VA BENE



# LST (bassa qualità immagine)
# importing Copernicus data for land surface temperature winter 2021
lst2021 <- raster("c_gls_LST_202101181400_GLOBE_GEO_V2.0.1.nc")
# cropping the image focusing on the same area of interest
lst21 <- crop(lst2021, ext)
plot(lst21)

# importing Copernicus data for land surface temperature winter 2021
lst2022 <- raster("c_gls_LST_202202051400_GLOBE_GEO_V2.0.1.nc")
# cropping the image focusing on the same area of interest
lst22 <- crop(lst2022, ext)
plot(lst22)

par(mfrow=c(1,2)) # strani e unità di misura diverse
plot(lst21)
plot(lst22)
dev.off()




# NDVI da errore nel caricare il file!!!
# importing Copernicus data for land surface temperature winter 2021
ndvi2021 <- raster("c_gls_NDVI300_202103210000_GLOBE_OLCI_V2.0.1.nc")
# cropping the image focusing on the same area of interest
ndvi21 <- crop(ndvi2021, ext)
plot(ndvi21)

# importing Copernicus data for land surface temperature winter 2021
ndvi2022 <- raster("c_gls_NDVI300_202203210000_GLOBE_OLCI_V2.0.1.nc")
# cropping the image focusing on the same area of interest
ndvi22 <- crop(ndvi2022, ext)
plot(ndvi22)

par(mfrow=c(1,2)) # strani e unità di misura diverse
plot(ndvi21)
plot(ndvi22)
dev.off()







