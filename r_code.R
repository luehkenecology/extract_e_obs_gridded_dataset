###################################################
# clear memory
###################################################
rm(list = ls())

#============================================================
# set working directory
#============================================================
RPROJ <- list(PROJHOME = normalizePath(getwd()))
attach(RPROJ)
rm(RPROJ)
setwd(PROJHOME)

###################################################
# load libraries
###################################################
library(plyr)
library(ggplot2)
library(scales) # to access breaks/formatting functions
require(lubridate)
library(zoo)
require(fields)
library(raster)
library(maptools)
library(spatstat) 
library(raster)
library(ncdf)
library(RNetCDF)
library(ncdf.tools)
library(fields)
library(colorRamps) 
library(rworldmap)

###################################################
# extents
###################################################
# extent Italy
#state.map3 <- readShapeSpatial("data/ITA_adm1")
#e<-extent(c(6.630879, 18.52069, 35.49292, 47.09096))

# extent Ukraine
#state.map3 <- readShapeSpatial("data/UKR_adm1")
#e<-c(extent(state.map3)[1:4])

# Moldavao
#state.map3 <- readShapeSpatial("data/MDA_adm1")
#e<-c(extent(state.map3)[1:4])

#state.map3 <- readShapeSpatial("data/DEU_adm1")
#e<-c(extent(state.map3)[1:4])


temp_func <- function(nc, start_dataset = '1950-01-01',
                      year_start, day_start = "-01-01",
                      year_end = year_start, day_end = "-12-31",
                      extent_v = 0,
                      var = "tg"){
  
  # Time
  A1<-paste(year_start, day_start, sep = "")
  A2<-paste(year_end, day_end, sep = "")
  time.s=as.POSIXct(A1,tz='UTC')
  time.e=as.POSIXct(A2,tz='UTC')
  tseq=seq(time.s, time.e, by='24 hours')
  times=as.POSIXct(nc$dim$time$vals*86400, origin=start_dataset, tz='UTC')
  t1=which(times==time.s)
  tfull1=which(times==time.s)
  t2=which(times==time.e)
  tfull2=which(times==time.e)
  dt = t2-t1+1
  
  afi<-get.var.ncdf(nc, var,start=c(1,1,t1), count=c(-1,-1,dt))
  
  nc$dim$longitude$vals -> lon
  nc$dim$latitude$vals -> lat
  
  TEST<-lapply(1:dt, function(x)  m <-   t((afi[,,x])))
  TEST1<-lapply(TEST, function(x) x[nrow(x):1,])
  TEST2<-lapply(TEST1, function(x) raster(x,xmn=min(lon),xmx=max(lon),ymn=min(lat),ymx=max(lat)))
  
  if(sum(extent_v) > 0  | sum(extent_v) < 0){
    TEST3<-lapply(TEST2, function(x) crop(x, extent_v))
    brick(unlist(TEST3))
  } else{
    brick(unlist(TEST2))
  }
}


#============================================================
# daily mean temperature until 2015
#============================================================

# loop through years
for(i in 1950:2015){
  # convert *.nc to raster
  data <- temp_func(open.ncdf("data/tg_0.25deg_reg_v13.0.nc"),
                    year_start = i)
  
  # save raster
  writeRaster(data, paste("output/mean_temperature_europe_", 
                          i, ".grd", sep = ""), overwrite = T)
}