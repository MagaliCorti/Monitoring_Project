setwd("/Users/magalicorti/Desktop/project/")
> library(raster)
> library(ggplot2) 
> library(gridExtra) 
> library (RStoolbox)
> library(ncdf4)
> 
> # importing copernicus data
> snow20211217 <- raster("c_gls_SCE500_202112170000_CEURO_MODIS_V1.0.1.nc")
> plot(snow20211217)
> ext <- c(8, 11, 45, 47)
> plot(crop(snow20211217, ext))
> cl <- colorRampPalette(c("dark blue", "blue", "light blue"))(100)
> plot(crop(snow20211217, ext), col=cl)
> 
