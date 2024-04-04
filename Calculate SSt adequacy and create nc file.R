#Calculate SST adequacy and create nc file

#Author: Enrique Sánchez-Fabrés

#created: 18/01/2023

#last version:

library(ggplot2)
library(mapdata)
library(maptools)
library(rnaturalearth)
library(gdata)
library(raster)
library(ncdf4)
library(dplyr)
library(lubridate)
library(reshape2)
#library(RNetCDF)



setwd("C:/Users/Usuario/TFM/SST and Taggging/")



#function to calculate sst adequacy from nc file with SST data
#funcion para crear una tabla a partir del NC con valores de SST, coordenadas y fechas y caclular SST adequacy
fun_sstadeq_in<- function(ncfile, denisty.function)
  { 
  #ncfile=ncdata_10
  t1<- nc_open(ncfile)
  t2<-list()
  t2$lat<-ncvar_get(t1, "lat")
  t2$lon<-ncvar_get(t1, "lon")
  t2$time<-ncvar_get(t1, "time")
  t2$SST<-ncvar_get(t1, "analysed_sst")- 273.15
  t2<-na.omit(t2)
  nc_close (t1)
  
  dimnames(t2$SST) <- list(long = t2$lon, lat = t2$lat, date = t2$time)
  t3 <- melt(t2$SST, value.name = "sst")
  
  a<-rep(NA, length(t3$sst))
  t3$sst_adequacy<- a
  
  sst_adequacy<-approx(denisty.function$x,denisty.function$y,xout=t3$sst)
  
  str(sst_adequacy)
  
  sst_adequacy<-as.data.frame(sst_adequacy)
  names(sst_adequacy)<-c("sst", "sst_adequacy")
  
  t3$sst_adequacy<-sst_adequacy$sst_adequacy
  #sustitute SST_adecuay NAs values for 0
  
  t3$sst_adequacy[is.na(t3$sst_adequacy)]<-0
  
  summary(t3$sst_adequacy)
  
  #to transform date format
  t3$ddmmyy<-as.Date(as.POSIXct(t3$date, origin="1981-01-01 00:00:00"))
  #t3$yday<-yday(t3$ddmmyy)
  
  #t4<-subset(t3, yday< 235) #subset for dates of interest (bejore 15th June)
  #t4<-t4[, 1:5]
  
  return(t3)
  
} #funcion para crear una tabla a partir del NC con valores de SST, SST adequacy, coordenadas y fechas


#directory to nc file with sst data (x): by year, dates from 15 april to 15 august

ncdata_10 <- "C:/Users/Usuario/TFM/SST and Taggging/datos SST/SST_MED_SST_L4_NRT_OBSERVATIONS_010_004_c_V2_2010.nc"
ncdata_11 <- "C:/Users/Usuario/TFM/SST and Taggging/datos SST/SST_MED_SST_L4_NRT_OBSERVATIONS_010_004_c_V2_2011.nc"
ncdata_12 <- "C:/Users/Usuario/TFM/SST and Taggging/datos SST/SST_MED_SST_L4_NRT_OBSERVATIONS_010_004_c_V2_2012.nc"
ncdata_13 <- "C:/Users/Usuario/TFM/SST and Taggging/datos SST/SST_MED_SST_L4_NRT_OBSERVATIONS_010_004_c_V2_2013.nc"
ncdata_14 <- "C:/Users/Usuario/TFM/SST and Taggging/datos SST/SST_MED_SST_L4_NRT_OBSERVATIONS_010_004_c_V2_2014.nc"
ncdata_15 <- "C:/Users/Usuario/TFM/SST and Taggging/datos SST/SST_MED_SST_L4_NRT_OBSERVATIONS_010_004_c_V2_2015.nc"
ncdata_16 <- "C:/Users/Usuario/TFM/SST and Taggging/datos SST/SST_MED_SST_L4_NRT_OBSERVATIONS_010_004_c_V2_2016.nc"
ncdata_17 <- "C:/Users/Usuario/TFM/SST and Taggging/datos SST/SST_MED_SST_L4_NRT_OBSERVATIONS_010_004_c_V2_2017.nc"
ncdata_18 <- "C:/Users/Usuario/TFM/SST and Taggging/datos SST/SST_MED_SST_L4_NRT_OBSERVATIONS_010_004_c_V2_2018.nc"
ncdata_19 <- "C:/Users/Usuario/TFM/SST and Taggging/datos SST/SST_MED_SST_L4_NRT_OBSERVATIONS_010_004_c_V2_2019.nc"
ncdata_20 <- "C:/Users/Usuario/TFM/SST and Taggging/datos SST/SST_MED_SST_L4_NRT_OBSERVATIONS_010_004_c_V2_2020.nc"
ncdata_21 <- "C:/Users/Usuario/TFM/SST and Taggging/datos SST/SST_MED_SST_L4_NRT_OBSERVATIONS_010_004_c_V2_2021.nc"
ncdata_22 <- "C:/Users/Usuario/TFM/SST and Taggging/datos SST/SST_MED_SST_L4_NRT_OBSERVATIONS_010_004_c_V2_2022.nc"

#IN------

#lim.date="235"

#SST IN TRACKS-> to produce density function and calculate SST adequacy
head(track_in_2013)
track_in_2011<-read.csv("2011_SST_tracks_IN.csv")
track_in_2012<-read.csv("2012_SST_tracks_IN.csv")
track_in_2013<-read.csv("2013_SST_tracks_IN.csv")

totaltrack_in <- rbind(track_in_2011, track_in_2012, track_in_2013)

range(track_in_2013$df_dates)
total_sst_in<-totaltrack_in$Obs.SST


summary(totaltrack_in)
#generating denisty function 
dx=density(total_sst_in)
plot(dx)

#calculating SSt adfeqauacy for each year with "in" tracks density function
sst_adeq_10<-fun_sstadeq_in(ncdata_10, dx)


sst_adeq_10<-subset(sst_adeq_10, date<929491200) #subset fechas 15 abril-15Jun


nc_open(ncdata_10)


sst_adeq_11<-fun_sstadeq_in(ncdata_11, dx)
sst_adeq_12<-fun_sstadeq_in(ncdata_12, dx)
sst_adeq_13<-fun_sstadeq_in(ncdata_13, dx)
sst_adeq_14<-fun_sstadeq_in(ncdata_14, dx)
sst_adeq_15<-fun_sstadeq_in(ncdata_15, dx)
sst_adeq_16<-fun_sstadeq_(ncdata_16, dx)
sst_adeq_17<-fun_sstadeq(ncdata_17, dx)
sst_adeq_18<-fun_sstadeq(ncdata_18, dx)
sst_adeq_19<-fun_sstadeq(ncdata_19, dx)
sst_adeq_20<-fun_sstadeq(ncdata_20, dx)

#create nc file method 1 (ncdf4) -----

#define variables for cordinates

y<-ncdim_def(name="lat", units="degrees_north", longname = "latitude", vals=as.double(sst_adeq_10$lat))
x<-ncdim_def(name="lon", units="degrees_east", longname = "longitude", vals=as.double(sst_adeq_10$long))
t<- ncdim_def(name="time", units="seconds since 1981-01-01 00:00:00", vals=sst_adeq_10$date, unlim = T)

?ncdim_def #unlim	-> If TRUE, this dimension is unlimited. Unlimited dimensions are convenient for storing, for example, data that extends over time; the time dimension can be made unlimited, and extended as needed. 

#make variable with those dimensions
?ncvar_def()

fillvalue <- -999
dim()
?ncvar_get()
sst<- ncvar_def(name="SST", units="Celsius degrees", dim=list(x,y,t), fillvalue, longname = "Sea Surface Temperature",  prec="double")

print(sst)

sst_adequacy<- ncvar_def(name="SST adequacy", units="degree of adequacy", dim=list(x,y,t), fillvalue, longname = "Sea Surface Temperature adequacy",  prec="double")


# create netCDF file and put arrays
#ncout <- nc_create(ncfname,list(tmp_def,mtco.def,mtwa.def,mat.def),force_v4=TRUE)

ncfname= "C:/Users/Usuario/TFM/SST and Taggging/datos SST adequacy/ABFT_SST_adequacy_2010.nc"

ncsst10<-nc_create(ncfname, list(sst, sst_adequacy),force_v4=TRUE)

ancsst10<-nc_create("C:/Users/Usuario/TFM/SST and Taggging/datos SST adequacy/ABFT_SST_2010.nc", sst, force_v4=TRUE)

?var.def.nc

split

# put variables
ncvar_put(ncsst10,sst,sst_adeq_10$sst, start=c(1,1,1), count=c(x,y,t))

?ncvar_put

#ncvar_put(ncsst10,mtwa.def,mtwa_array3)

#create nc file method Adding data to a existing nc?-----

mync<-nc_open(ncdata_10)
sst_adequacy

sst_adequacy<- ncvar_def(name="SST adequacy", units="degree of adequacy", dim=list(x, y, t), fillvalue, longname = "Sea Surface Temperature adequacy",  prec="single")

?ncvar_def

nc_adeq<-ncvar_add(mync, sst_adequacy, verbose=FALSE, indefine=FALSE )

mync$dim


# Rnetcdf ------

a<-open.nc(ncdata_10)
print(a)

detach(package:ncdf4)

nc<-open.nc()



nc<- create.nc("sst_adequacy_IN_2010.nc")

#Dimensions

dim.def.nc(nc, "Lon", 481)
dim.def.nc(nc, "Lat", 301)
dim.def.nc(nc, "time", 123)
print(mync)

#Varible

var.def.nc(nc, "Time", "NC_INT", "time")
var.def.nc(nc, "Longitude", "NC_DOUBLE", "Lon" )
var.def.nc(nc, "Latitude", "NC_DOUBLE", "Lat" )
var.def.nc(nc, "SST", "NC_DOUBLE", c("Lon", "Lat", "time"))
var.def.nc(nc, "SST_adeq", "NC_DOUBLE", c("Lon", "Lat", "time"))


var.put.nc(nc, "Longitude", 481)
var.put.nc(nc, "Latitude", 301)
var.put.nc(nc, "time", 123)
var.put.nc(nc, "SST", )
?var.put.nc
