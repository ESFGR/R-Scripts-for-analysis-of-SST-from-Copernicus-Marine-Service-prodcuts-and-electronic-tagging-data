#### Analysis of GPE3 data, tracking of bluefin tunas ####

library(stringr)
library(ncdf4)
library(reshape2)
library(ggplot2)
library(ggrepel)
library(mapdata)
library(maptools)
library(rnaturalearth)
library(gdata)
library(raster)
library(scatterpie)
library (openxlsx)
library(dplyr)
library(lubridate)


setwd("C:/Users/Usuario/Desktop/marcas migratun/")

#includes coordintaes and dates. Coordinates are used to subset individuals in the study area and dates to be the nexus with files contining SST data ----

AllGPE <- read.csv("GPE data from all individuals.csv")
head(AllGPE)
table(AllGPE$DeployID)

#dates change
AllGPE$Date<-parse_date_time(AllGPE$Date, orders="dmy hms")
AllGPE$ddmmyy <- as.Date(AllGPE$Date)

tapply(AllGPE$DeployID, paste(AllGPE$DeployID, AllGPE$ddmmyy, sep = ""), length)

#study area filtering

GPEdata <- subset(AllGPE,Most.Likely.Longitude<(-5) & Most.Likely.Longitude>(-9) & Most.Likely.Latitude>35 & Most.Likely.Latitude<37.5) 

hist(GPEdata$Observed.SST, breaks = 100 , xlim = c(14,27),  main ="", xlab="")

summary(GPEdata$Observed.SST) 


#### subseting only rows with data of Obs.SST
GPEsst <-GPEdata[!is.na(GPEdata$Observed.SST),] #removing rows with NA in the Observed.SST
length(GPEsst$Observed.SST)
GPEsst$year <-format(as.Date(GPEsst$ddmmyy), format="%Y")
GPEsst$month <-format(as.Date(GPEsst$ddmmyy), format="%m")
GPEsst<- subset(GPEsst, !GPEsst$month == "12" ) # removing data from december (out of the period of interest)

write.csv(GPEsst, "GPEsst study area.csv")
write.csv2(GPEsst, "GPEsst study area.csv2")
write.xlsx(GPEsst, "GPEsst study area.xlsx")

read.csv2("GPEsst study area.csv2")

#### Remove repeated dates

SA.GPE.table<-GPEdata[!duplicated(GPEdata$ddmmyy), ] 
SA.GPE.table$DeployID<-as.character(SA.GPE.table$DeployID)



## id-SST.csv data-----

SSTdata <- read.csv("All SST data.csv") #there is no coordinates data, is not possible to subset directly to the study area

#

SSTdata$Date<-parse_date_time(SSTdata$Date, orders="hms dmy")
SSTdata$ddmmyy <- as.Date(SSTdata$Date)


(tapply(SSTdata$DeployID, paste(SSTdata$DeployID, SSTdata$ddmmyy, sep = "-"), length) )#one SST value per day in most of the cases


SA.GPE.table2 <- SA.GPE.table[, c(1,3,4,14)]

SA.SST <- merge(SSTdata, SA.GPE.table2, by = c("DeployID", "ddmmyy"))

aaa<- merge(SSTdata, SA.GPE.table2, by = c( "ddmmyy", "DeployID"))


SA.SST<-right_join(SSTdata, SA.GPE.table2, by=c("ddmmyy","DeployID"))

summary(SA.SST) #

length(na.omit(SA.SST))

write.csv(SA.SST, "datos ficheros SST area de estudio.csv")

#### Satelite data -----
dates<- SA.SST$ddmmyy # hay datos de 2011 y 2010

### Colours

c1 <- rgb(216,238,192, max = 255, alpha = 120, names = "lt.green") #light colours for histogram
c2 <- rgb(255,100,100, max = 255, alpha = 100, names = "lt.red")
c3 <- rgb(0, 100, 255, max=255, alpha = 100, names = "lt.bule")

f1<- function(x){ #function to create a table with satellite data of SST, coordinates and dates from .nc files provided by CMEMS
  
  t1<- nc_open(x)
  t2<-list()
  t2$lat<-ncvar_get(t1, "lat")
  t2$lon<-ncvar_get(t1, "lon")
  t2$time<-ncvar_get(t1, "time")
  t2$SST<-ncvar_get(t1, "analysed_sst")- 273.15
  t2<-na.omit(t2)
  nc_close (t1)
  
  dimnames(t2$SST) <- list(long = t2$lon, lat = t2$lat, date = t2$time)
  t3 <- melt(t2$SST, value.name = "sst")} 

#2010 July: 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 21, 22----

#tracking data -SST.csv

SA.SST$year <- format(as.Date(SA.SST$ddmmyy), format="%Y")

track2010 <- subset(SA.SST, year=="2010") #6 observaciones

#From GPE.csv = obs.SST

GPEtrack2010 <- subset(GPEsst, year=="2010") #18 observaciones

#Satellite data
j2 <- "C:/Users/Usuario/Desktop/TFM/SST and Taggging/datos SST/2010_July_10_19_L4_NRT.nc"
j3 <- "C:/Users/Usuario/Desktop/TFM/SST and Taggging/datos SST/2010_July_19_21_L4_NRT.nc"

gpej2010<- f1(j3)
gpej2010$ddmmyy<-as.Date(as.POSIXct(gpej2010$date, origin="1981-01-01 00:00:00"))
gpej2010$yday<-yday(gpej2010$ddmmyy)

j2010 <- f1(j2)
j2010$ddmmyy<-as.Date(as.POSIXct(j2010$date, origin="1981-01-01 00:00:00"))
j2010$yday<-yday(j2010$ddmmyy)

july10_10<- subset(j2010, yday=="191")
july10_11<- subset(j2010, yday=="192")
july10_12<- subset(j2010, yday=="193")
july10_13<- subset(j2010, yday=="194")
july10_14<- subset(j2010, yday=="195")
july10_19<- subset(j2010, yday=="200")

july2010<- cbind(july10_10, july10_11, july10_12, july10_13, july10_14, july10_19)

alljuly2010 <- rbind(j2010, gpej2010)

par(mar=c(4, 4, 2, 0.1))

hist(alljuly2010$sst, col = c2, breaks = 100, xlim = c(14,27), main ="July 2010 | OUT", xlab="")
rug(track2010$Temperature, ticksize = 0.05, side = 1,lwd = 5, col = "red")
rug(GPEtrack2010$Observed.SST, ticksize = 0.02, side = 1,lwd = 5, col = "blue")
mtext("SST",side=1,col="black",line=2.5)
legend(14,50000, c("satellite SST ", "SST file", "obs.SST GPE file"), fill=c( c2, "red", "blue"))
mtext("SST",side=1,col="black",line=2.5)

#???2011 July: 3, 4, 5, 6, 7, 9-----

#tracking data

#From GPE.csv = obs.SST

GPEsst$year <-format(as.Date(GPEsst$ddmmyy), format="%Y")
GPEtrack2011 <- subset(GPEsst, year=="2011") #18 observaciones

#From -SST.csv
SA.SST$year <- format(as.Date(SA.SST$ddmmyy), format="%Y")
track2011 <- subset(SA.SST, year=="2011")



#Satellite data
j1 <- "C:/Users/Usuario/Desktop/TFM/SST and Taggging/datos SST/2011_July_01_10_L4_NRT.nc"

j2011 <- f1(j1)
j2011$ddmmyy<-as.Date(as.POSIXct(j2011$date, origin="1981-01-01 00:00:00"))
j2011$yday<-yday(j2011$ddmmyy)
table(j2011$ddmmyy)

july3_11<- subset(j2011, yday=="184")
july4_11<- subset(j2011, yday=="185")
july5_11<- subset(j2011, yday=="186")
july6_11<- subset(j2011, yday=="187")
july7_11<- subset(j2011, yday=="188")
july9_11<- subset(j2011, yday=="190")

july2011<-cbind(july3_11, july4_11, july5_11, july6_11, july7_11, july9_11)



hist(july2011$sst, col = c2, breaks = 70, xlim = c(14,27), main ="July 2011 | OUT", xlab="")
rug(track2011$Temperature, ticksize = 0.05, side = 1,lwd = 5, col = "red")
rug(GPEtrack2011$Observed.SST, ticksize = 0.02, side = 1,lwd = 5, col = "blue")
mtext("SST",side=1,col="black",line=2.5)
legend(14,7000, c("satellite SST ", "SST file", "obs.SST GPE file"), fill=c( c2, "red", "blue"))
mtext("SST",side=1,col="black",line=2.5)
