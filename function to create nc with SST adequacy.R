#function to create NC with SST adequacy to atlantic bluefin tuna

library(ggplot2)
library(mapdata)
library(maptools)
library(rnaturalearth)
library(gdata)
library(raster)
library(RNetCDF)
library(dplyr)
library(lubridate)
library(reshape2)
library(beepr)

setwd("C:/Users/Usuario/TFM/SST and Taggging/")

aa<-open.nc("SST adequacy ncs/SST_adequacy_2022_out.nc")

print.nc(aa)

aaatemp<-var.get.nc(aa,"analysed_sst")

att.get.nc(aa)

rm(aa)

#directory to nc file with sst data (x)----

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

#
#SST IN eletronic tags-> to produce density function and calculate SST adequacy----
head(track_in_2013)
track_in_2011<-read.csv("2011_SST_tracks_IN.csv")
track_in_2012<-read.csv("2012_SST_tracks_IN.csv")
track_in_2013<-read.csv("2013_SST_tracks_IN.csv")

totaltrack_in <- rbind(track_in_2011, track_in_2012, track_in_2013)

range(track_in_2013$df_dates)
range(track_in_2012$df_dates)
range(track_in_2011$df_dates)
total_sst_in<-totaltrack_in$Obs.SST


#generating denisty function 
dx=density(total_sst_in)
plot(dx)


#SST in Study area from L4 NRT product out-----
OUT_2010<-read.csv("2010_SST_OUT.csv")
OUT_2011<-read.csv("2011_SST_OUT.csv")
OUT_2012<-read.csv("2012_SST_OUT.csv")
OUT_2013<-read.csv("2013_SST_OUT.csv")

total_out<- rbind(OUT_2010, OUT_2011, OUT_2012, OUT_2013)

total_out<-na.omit(total_out)

#SST from electronic tagging

track_out_2010<-read.csv("2010_SST_tracks_OUT.csv")
track_out_2011<-read.csv("2011_SST_tracks_OUT.csv")
track_out_2012<-read.csv("2012_SST_tracks_OUT.csv")
track_out_2013<-read.csv("2013_SST_tracks_OUT.csv")

totaltrack_out<-c(track_out_2010$Observed.SST, track_out_2011$x, track_out_2012$Obs.SST, track_out_2013$Obs.SST)

range(track_out_2013$df_dates)

total_sst_out<-totaltrack_out

#generating denisty function 
dx=density(total_sst_out)
plot(dx)

#Create NC-------

nc_create_fun <-function(nc_file, newnc_dir, adeq_fun){ 
  #function to create a nc including the estimated sst adequacy to presence of bluefin tuna
  #print.nc(nc)
  nctime=var.get.nc(nc,"time")
  table(nctime)
  #as.Date(as.POSIXct(nctime, origin="1981-01-01 00:00:00")) #31-05 = nctime[47] #15-Jun = nctime[62] 
  #as.Date(as.POSIXct(nctime[62:123], origin="1981-01-01 00:00:00"))
  #nctime<-nctime[62:123]
  nclat=var.get.nc(nc,"lat")
  nclon=var.get.nc(nc,"lon")
  
  
  SST<-var.get.nc(nc, "analysed_sst") #- 273.15
  #str(SST)# parecen los valores en grados celsius pero x100...
  ?var.get.nc
  SST<-SST/100
  
  
  myaproxfun<-approxfun(adeq_fun$x, adeq_fun$y)
  
  testvar= apply(SST, 1:3, myaproxfun)
  
  #dim(testvar)
  #str(testvar)
  
  #sustitute SST_adecuay NAs values for 0 because NAs come from 
  
  testvar[is.na(testvar)]<-0
  #summary(testvar[1:100,100,1])
  #dim(nctime)
  #dim(SST)
  #print.nc(nc)
  
  nc2 <- create.nc(newncdir)
  dim.def.nc(nc2, "time",47)
  dim.def.nc(nc2, "lat",301)
  dim.def.nc(nc2, "lon",481)
  var.def.nc(nc2, "time", "NC_INT",c("time"))
  var.def.nc(nc2, "lat", "NC_DOUBLE",c("lat"))
  var.def.nc(nc2, "lon", "NC_DOUBLE",c("lon"))
  var.def.nc(nc2, "adecuacy", "NC_DOUBLE",c("lon","lat","time"))
  var.def.nc(nc2, "analysed_sst", "NC_DOUBLE",c("lon","lat","time"))
  #print.nc(nc2)
  var.put.nc(nc2,"time", nctime, start=1, count=47)
  var.put.nc(nc2,"lat", nclat, start=1, count=301)
  var.put.nc(nc2,"lon", nclon, start=1, count=481)
  var.put.nc(nc2, "adecuacy", testvar,start=c(1,1,1),count=c(481,301,47))
  var.put.nc(nc2, "analysed_sst", SST,start=c(1,1,1),count=c(481,301,47))
  #print.nc(nc2)
  
  #testing:
  #as.Date(as.POSIXct(var.get.nc(nc2,"time"), origin="1981-01-01 00:00:00")) #checking dates
  #var.get.nc(nc2,"time")
  #var.get.nc(nc2,"adecuacy",start=c(1,100,30),count=c(1,1,1))
  #var.get.nc(nc2,"analysed_sst",start=c(1,100,62),count=c(1,1,1))
  #var.get.nc(nc2,"lat",start=c(1,100,30),count=c(1,1,1))
  ##  Put some attributes
  att.put.nc(nc2, "time", "units", "NC_CHAR", "seconds since 1981-01-01 00:00:00")
  att.put.nc(nc2, "lat", "long_name", "NC_CHAR", "Latitude")
  att.put.nc(nc2, "lat", "units", "NC_CHAR", "degrees_east")
  att.put.nc(nc2, "lat", "comment", "NC_CHAR", "geographical coordinates, WGS84 projection")
  att.put.nc(nc2, "lon", "long_name", "NC_CHAR", "Longitude")
  att.put.nc(nc2, "lon", "units", "NC_CHAR", "degrees_north")
  att.put.nc(nc2, "lon", "comment", "NC_CHAR", "geographical coordinates, WGS84 projection")
  att.put.nc(nc2, "analysed_sst", "long_name", "NC_CHAR", "Sea surface temperature")
  att.put.nc(nc2, "analysed_sst", "units", "NC_CHAR", "Celsius degrees")
  att.put.nc(nc2, "adecuacy", "long_name", "NC_CHAR", "Sea surface temperature adequacy to ABFT presence")
  print.nc(nc2)
  
  close.nc(nc)
  close.nc(nc2)
  beep(6)
}

rm(nc)
rm(nc2)
nc<- open.nc(ncdata_10)
rm(newncdir)
newncdir<-"SST adequacy ncs/SST_adequacy_2010_out.nc"
adeq_fun<- dx

nc_create_fun(nc, newncdir)

#test
aaa<-open.nc("SST adequacy ncs//SST_adequacy_2010_out.nc")

(aaa)

var.get.nc(aaa,"adecuacy",start=c(1,100,62),count=c(1,1,1))
var.get.nc(aaa,"analysed_sst",start=c(1,100,62),count=c(1,1,1)) 
var.get.nc(aaa,"lat")

print.nc(aaa)

rm(aaa)
