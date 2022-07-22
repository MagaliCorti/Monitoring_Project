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
plot(snow21, snow22) # making line  passing trough 0
abline(0, 1, col="red") # plotting line 

# plotting automatically all graphs together, very usefull when we have many graphs
pairs(snowstack)



# computing proportiuons of snow cover in winter 2021
# passing from a layers with values ranging 0-200 to 4 values (1 - 2 - 3 - 4)
s21 <- unsuperClass(snow21, nClasses=4)
# plotting the new map
plot(s21$map)
# computing the frequency for each class
freq(s21$map)
total <- 240000 # tot amount of pixels -> run s21 -> look at tird value of dimension (ncell)
# compute percentage of snow cover (frequency of class 4 / total)
propsnow21 <- 106071/total
propsnow21 # 0.4419625 = 44%

# let's do the same thing for winter 2022
s22 <- unsuperClass(snow22, nClasses=4)
plot(s22$map)
freq(s22$map)
propsnow22 <- 11938/total
propsnow22 # 0.04974167 = 5%


par(mfrow=c(1,2))
plot(s21$map)
plot(s22$map)






# SWI messeggio di warning
# importing Copernicus data for surface water index spring 2021
swi2021 <- raster("c_gls_SWI1km_202104021200_CEURO_SCATSAR_V1.0.1.nc")
# cropping the image focusing on the same area of interest
swi21 <- crop(swi2021, ext)
plot(swi21)

# importing Copernicus data for surface water index spring 2022
swi2022 <- raster("c_gls_SWI1km_202204021200_CEURO_SCATSAR_V1.0.2.nc")
# cropping the image focusing on the same area of interest
swi22 <- crop(swi2022, ext)
plot(swi22)

par(mfrow=c(1,2)) # sono ugualiiii
plot(swi21)
plot(swi22)
dev.off()



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







