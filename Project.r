# R Project for Monitoring Ecosystems


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
 


##### With this project I want to highligt the differences in Snow Cover Extent in the Cetral Alps in Nortern Itlay between the winter 2021 and winter 2022 ##### 

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
# if I wanted to visualize them in an horizontal sequence I shuold have used + instead of /
p21 / p22


# computing differece in snow cover between 2021 and 2022
SCEdif <- (snow22 - snow21)
plot(SCEdif)
# changing color palette for a more clear visualization
cldif = colorRampPalette(c("blue", "white", "red"))(100) # 100 number of color from blue to red
plot(SCEdif, col=cldif) # in blue the snow missing in 2022 with respect to 2021


# qualitative analysis of proportions and frequency distributions
# plotting frequency distribution of snow cover values -> plot all histograms together
par(mfrow=c(1,2))
hist(snow21, xlim = c(0,200))
hist(snow22, xlim = c(0,200))


# PERCHE DISTRIBUZIONE DEI. DATI LUNGO LINEE?? GUARDO MAPPE -> risoluzione, cropping ??
# plotting values of 2022 against 2021
# comparing data one in function of the other
plot(snow21, snow22, xlab = "Snow Cover Extent in 2021", ylab = "Snow Cover Extent in 2022") 
abline(0, 1, col="red") # plotting line, making it passing trough 0

# plotting automathically all graphs together, very usefull when we have many graphs
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
# compute proportion per type of cover (frequency of class / total)
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
# geom_bar function explainig type of graph
# stat - statistics used = identity because we're using data as they are (no median or mean)
# for changing limit from 0 to 1 use ylim()
PR21 <- ggplot(proportion21, aes(x=cover, y=prop21, color=cover)) + geom_bar(stat="identity", fill="white") + ylim(0,1)


# let's do the same thing for winter 2022
s22 <- unsuperClass(snow22, nClasses=3)
par(mfrow=c(2,1))
plot(s22$map)
plot(snow22)

freq(s22$map)
total <- 360000
# compute proportion per type of cover (frequency of class / total)
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



##### Now let's see if we can see differences in the temperature of the lakes present in the region #####

# checking the snow cover extent in the January, February and March for both the year considered
# when the snow is consistenly molten?
# does the melting of snow affect the surface water temperature?

# to import multiple data with the same pattern in the name I can create a list and use the lapply function
rlist <- list.files(pattern = "SCE")
rlist # list of 8 images
# applying to all the objects in the list the rester function
list_rast <- lapply(rlist, raster)
list_rast
# creating a stack
snowstack <- stack(list_rast)
snowstack

# plotting together all the images
plot(snowstack)


# LSWT - Lake Surface Water Temperature
# Copernicus data with geometric resolution of 1000m x 1000m per pixel

# importing Copernicus data for land surface temperature winter 2021
lswt2021 <- raster("c_gls_LSWT_202102210000_GLOBE_SLSTRAB_v1.1.0.nc")
# cropping the image focusing on the same area of interest
lswt21 <- crop(lswt2021, ext)
plot(lswt21)

# importing Copernicus data for land surface temperature winter 2021
lswt2022 <- raster("c_gls_LSWT_202202210000_GLOBE_SLSTRAB_v1.1.0.nc")
# cropping the image focusing on the same area of interest
lswt22 <- crop(lswt2022, ext)
plot(lswt22)

par(mfrow=c(2,1)) 
plot(lswt21)
plot(lswt22)

# new cropping focusing on the lake area
ext1 <- c(8, 12, 45, 46.5)
lswt21 <- crop(lswt2021, ext1)
plot(lswt21)
lswt22 <- crop(lswt2022, ext1)
plot(lswt22)

par(mfrow=c(2,1)) 
plot(lswt21, main = "Lake Surface Water Temperature in 2021")
plot(lswt22, main = "Lake Surface Water Temperature in 2022")

# using ggplot function with viridis (changing colorRampPalette)
lt21 <- ggplot() + geom_raster(lswt21, mapping = aes(x=x, y=y, fill = lake.surface.skin.temperature)) + scale_fill_viridis() + ggtitle("Lake Surface Water Temperature in 2021")
lt22 <- ggplot() + geom_raster(lswt22, mapping = aes(x=x, y=y, fill = lake.surface.skin.temperature)) + scale_fill_viridis() + ggtitle("Lake Surface Water Temperature in 2022")

# to visualize the two plots together in a vertical sequence
lt21 / lt22

# computing differece in lake surface temperature between 2021 and 2022
LSWTdif <- (lswt22 - lswt21)
plot(LSWTdif)
# changing color palette for a more clear visualization
plot(LSWTdif, col=cldif) # in red the higest temperature in 2022 with respect to 2021
# let's use ggplot
ldif22 <- ggplot() + geom_raster(LSWTdif, mapping = aes(x=x, y=y, fill = layer))+ scale_fill_viridis(option="magma") + ggtitle("Difference in Lake Surface Water Temperature")

# plotting frequency distribution histogrames
par(mfrow=c(1,2))
hist(lswt21, xlim = c(275,285), ylim = c(0,450))
hist(lswt22, xlim = c(275,285), ylim = c(0,450))

# plotting values of 2022 against 2021
# comparing data one in function of the other
plot(lswt21, lswt22, xlab = "Lake Surface Water Temperature in 2021", ylab = "Lake Surface Water Temperature in 2022", xlim = c(275,285), ylim = c(275,285)) 
abline(0, 1, col="red") # plotting line, making it passing trough 0










# Use diverging colors to plot the positive and negative change in LAI
# from diverging palette from the colorspace package
red_blue <- diverging_hcl(5, "Red-Blue")
red_blue # recall the variable to see the color palette
# "#841859" "#F398C4" "#F6F6F6" "#7CC57D" "#005600"

# this palette does not center in 0 with white color

# palette for the top half of the image, with positive values
red <- colorRampPalette(colors = c("red", "white"))(70)

# Palette for the bottom half of the image, with negative values
blue <- colorRampPalette(colors = c("white", "blue"))(30)

# Combine the two color palettes
red_blue <- c(red, blue)

# Plot!
plot(LSWTdif, col=red_blue) # now the midpoint of the palette is in 0!

# it doesn't seems to be many differences in the surface temperature of the lakes
# probably the melting of snow doesn't affect the surface water temperature







####

# importing Copernicus data for land surface temperature winter 2021
swe2021 <- raster("c_gls_SWE5K_202201250000_NHEMI_SSMIS_V1.0.2.nc")
# cropping the image focusing on the same area of interest
swe21 <- crop(swe2021, ext)

par(mfrow=c(1,2))
plot(swe21)
plot(snow22)



# SWI messeggio di warning NON VA BENE
# SSM NON VA BENE dati a fascie
# LST (bassa qualità immagine)
# NDVI da errore nel caricare il file!!!



# importing Copernicus data for land surface temperature winter 2021
lst2021 <- raster("c_gls_LST10-DC_202101110000_GLOBE_GEO_V2.0.1.nc")
# cropping the image focusing on the same area of interest
lst21 <- crop(lst2021, ext)
plot(lst21)
# importing Copernicus data for land surface temperature winter 2021
lst2022 <- raster("c_gls_LST10-DC_202201110000_GLOBE_GEO_V2.0.1.nc")
# cropping the image focusing on the same area of interest
lst22 <- crop(lst2022, ext)
plot(lst22)

par(mfrow=c(2,1)) # strani e unità di misura diverse
plot(lst21)
plot(lst22)

dev.off()


# NDVI da errore nel caricare il file!!!
setwd("/Users/magalicorti/Desktop/ndvi/")
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

par(mfrow=c(1,2))
plot(ndvi21)
plot(ndvi22)
dev.off()





