# # climetrics package for RTS objects 

# https://cran.r-project.org/web/packages/climetrics/vignettes/climetrics.pdf
# code from https://github.com/shirintaheri/climetrics

# Standardized local anomalies (sed);
# Changes in probability of local climate extremes (localExtreme);
# Change in area of analogous climates (aaClimate);
# Change in the distance to analogous climates (daClimate),
# Novel climates (novelClimate);
# Climate Change Velocity (ve; dVelocity; gVelocity).

library(climetrics)
library(terra)
library(rts)


# get data with file 

filePath <- system.file("external/", package="climetrics") # path to the dataset folder

pr <- rast(paste0(filePath,'/precip.tif')) # precipitation
tmin <- rast(paste0(filePath,'/tmin.tif')) # minimum_temperature
tmax <- rast(paste0(filePath,'/tmax.tif')) # maximum temperature
tmean <- rast(paste0(filePath,'/tmean.tif')) # mean temperature

pr 

# color palette:
cl1 <- colorRampPalette (c("Maroon","Khaki","yellow","lightblue","blue","MidnightBlue"))

plot(pr[[1]], main='Precipitation',col=cl1(100))


# "The climetrics works based on both raster or raster time series objects. 
# It would, however, easier to use raster time series. 
# Here, you can see how a raster time series can be created using the rts function in the rts package. 
# To do so, we need a vector holding (Date/time) with the same length as the number of layers in the raster objects 
# (i.e., the dates correspond to layers). 
# The class of the object should be “Date” or “POCIXxx”. 
# The corresponding dates to the above layers are already available in the package: "
         
# create date object from raster  
n <- readRDS(paste0(filePath,'/dates.rds'))

class(n)
length(n)
head(n)


# create Raster Time Series (RTS) obj
pr.t <- rts(pr,n) 
tmin.t <- rts(tmin,n)
tmax.t <- rts(tmax,n)
tmean.t <- rts(tmean,n)
#------

plot(pr.t[[1:4]])

# This code generates a map of probability of extreme climate events. 
lce <- localExtreme(tmean.t , pr.t,
                    t1 = '1991/2000', 
                    t2 = '2010/2020', 
                    extreme = c(0.95,0.05)) 

# as you can see from the code, when the input climate variables are raster time series,
# you can easily specify the range of time periods in t1 and t2

# Extreme is 95th and 5th percentiles corresponds to tmean.t, and pr.t (first and second arguments)

# let's first specify an appropriate color Pallete:
cl <- colorRampPalette(c("MidnightBlue","Turquoise","lightblue","gray","khaki","orange","red","Maroon"))


plot(lce,col=cl(100),main='Changes in Probability of Local Climate Extremes')
