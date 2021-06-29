##### Analyisis with data from L4 ... ------

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


setwd("C:/Users/Usuario/Desktop/TFM/SST and Taggging/")
load("C:/Users/Usuario/Desktop/TFM/SST and Taggging/.RData")

#SST data from  tracking----

tracking_data <-read.csv("Tracking data.csv")
table(tracking_data$inout)


##preparando mapa de la zona de estudio----

mapBuffer <-0.1
worldMap <- ne_download(scale = "medium", returnclass = "sp")

worldMapCroped <- crop(worldMap,extent(-9,-5,35,37.5)) #coordenadas del área de estudio

mainMap <- ggplot() + geom_polygon(data = worldMapCroped, aes(x = long, y = lat, group = group)) + coord_fixed() + theme(axis.ticks=element_blank()) + ylab("Latitude") + xlab("Longitude")#mapa del area de estudio

#colourss-
col.pal<- rainbow(150, end=0.8, rev=TRUE)

# DATES IN AND OUT PER YEAR ----

dates<-read.csv("dates_inout.csv") #dates of tagging

#arrange(dates, dates$inout) #order data by direction (in/out)

dates<-arrange(dates,  dates$df_dates) #order data by date

#View(dates) #observe the dates of tagging --> dates for which SST values will be obtained

dates2011 <-subset(dates, dates$Year=="2011")
dates2011_in<-subset(dates2011, dates2011$inout== "in")
dates2011_out<-subset(dates2011, dates2011$inout== "out")

dates2012 <-subset(dates, dates$Year=="2012")
dates2012_in<-subset(dates2012, dates2012$inout== "in")
dates2012_out<-subset(dates2012, dates2012$inout== "out")

table(dates2012_in$df_dates)


dates2013 <-subset(dates, dates$Year=="2013")
dates2013_in<-subset(dates2013, dates2013$inout== "in")
dates2013_out<-subset(dates2013, dates2013$inout== "out")

### Colours

c1 <- rgb(216,238,192, max = 255, alpha = 120, names = "lt.green") #light colours for histogram
c2 <- rgb(255,100,100, max = 255, alpha = 100, names = "lt.red")
c3 <- rgb(0, 100, 255, max=255, alpha = 100, names = "lt.blue")
c4 <- rgb(100, 0, 100, max=255, alpha = 100, names = "lt.purple")



##----

f1<- function(x){ #Function to create a table based on a .nc file including SST, dates and coordinates data
  
  t1<- nc_open(x)
  t2<-list()
  t2$lat<-ncvar_get(t1, "lat")
  t2$lon<-ncvar_get(t1, "lon")
  t2$time<-ncvar_get(t1, "time")
  t2$SST<-ncvar_get(t1, "analysed_sst")- 273.15
  t2<-na.omit(t2)
  nc_close (t1)
  
  dimnames(t2$SST) <- list(long = t2$lon, lat = t2$lat, date = t2$time)
  t3 <- melt(t2$SST, value.name = "sst")} ##Function to create a table based on a .nc file including SST, dates and coordinates data

#2010 ----

GPEsst <- read.csv2("C:/Users/Usuario/Desktop/marcas migratun/GPEsst study area.csv2")


#2010 ----

#From GPE.csv = obs.SST

GPEtrack2010 <- subset(GPEsst, year=="2010") #18 observations

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

hist(alljuly2010$sst, col = c4, breaks = 100, xlim = c(14,27), main ="July 2010 | OUT", xlab="")
rug(GPEtrack2010$Observed.SST, ticksize = 0.02, side = 1,lwd = 5, col = "blue")
mtext("SST",side=1,col="black",line=2.5)
legend(14,50000, c("satellite SST ", " GPE obs.SST"), fill=c( c4, "blue"))
mtext("SST",side=1,col="black",line=2.5)

summary(na.omit(alljuly2010$sst))

#daily Maps ----

range(na.omit(alljuly2010$sst))

#??? 10july
mainMap + geom_raster(data = july10_10, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data =july10_10, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(17, 26), colours = col.pal, na.value = NA) +
  theme_bw()  +
  labs(title = "Mean SST | 10/07/2010", fill = "SST") 

#??? 11july
mainMap + geom_raster(data = july10_11, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data =july10_11, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(17, 26), colours = col.pal, na.value = NA) +
  theme_bw()  +
  labs(title = "Mean SST | 11/07/2010", fill = "SST") 

#??? 12july
mainMap + geom_raster(data = july10_12, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data =july10_12, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(17, 26), colours = col.pal, na.value = NA) +
  theme_bw()  +
  labs(title = "Mean SST | 12/07/2010", fill = "SST") 

#??? 13july
mainMap + geom_raster(data = july10_13, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data =july10_13, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(17, 26), colours = col.pal, na.value = NA) +
  theme_bw()  +
  labs(title = "Mean SST | 13/07/2010", fill = "SST") 

#??? 14july
mainMap + geom_raster(data = july10_14, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data =july10_14, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(17, 26), colours = col.pal, na.value = NA) +
  theme_bw()  +
  labs(title = "Mean SST | 14/07/2010", fill = "SST") 

#??? 19july
mainMap + geom_raster(data = july10_19, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data =july10_19, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(17, 26), colours = col.pal, na.value = NA) +
  theme_bw()  +
  labs(title = "Mean SST | 19/07/2010", fill = "SST") 

# 2011 ----

# GPE data --> 2011 July: 3, 4, 5, 6, 7, 9-----

#tracking data

#From GPE.csv = obs.SST

GPEtrack2011 <- subset(GPEsst, year=="2011") #

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

july2011GPE<-rbind(july3_11, july4_11, july5_11, july6_11, july7_11, july9_11)



hist(july2011GPE$sst, col = c2, breaks = 70, xlim = c(14,27), main ="July 2011 | OUT", xlab="")
rug(GPEtrack2011$Observed.SST, ticksize = 0.02, side = 1,lwd = 5, col = "blue")
mtext("SST",side=1,col="black",line=2.5)
legend(14,7000, c("satellite SST ", "GPE obs.SST "), fill=c( c2, "blue"))

#daily maps----

range(na.omit(july2011GPE$sst))

#3 july
mainMap + geom_raster(data = july3_11, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data =july3_11, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(17, 26), colours = col.pal, na.value = NA) +
  theme_bw()  +
  labs(title = "Mean SST | 03/07/2011", fill = "SST") 

#4 july
mainMap + geom_raster(data = july4_11, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data =july4_11, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(17, 26), colours = col.pal, na.value = NA) +
  theme_bw()  +
  labs(title = "Mean SST | 04/07/2011", fill = "SST") 

#5 july
mainMap + geom_raster(data = july5_11, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data =july5_11, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(17, 26), colours = col.pal, na.value = NA) +
  theme_bw()  +
  labs(title = "Mean SST | 05/07/2011", fill = "SST") 

#6 july
mainMap + geom_raster(data = july6_11, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data =july6_11, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(17, 26), colours = col.pal, na.value = NA) +
  theme_bw()  +
  labs(title = "Mean SST | 06/07/2011", fill = "SST") 

#7 july
mainMap + geom_raster(data = july7_11, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data =july7_11, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(17, 26), colours = col.pal, na.value = NA) +
  theme_bw()  +
  labs(title = "Mean SST | 07/07/2011", fill = "SST") 

#9 july
mainMap + geom_raster(data = july9_11, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data =july9_11, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(17, 26), colours = col.pal, na.value = NA) +
  theme_bw()  +
  labs(title = "Mean SST | 09/07/2011", fill = "SST") 



#-----

trackData2011<- subset(tracking_data, Year=="2011")
track_in_2011 <- subset(trackData2011, inout == "in")
track_out_2011 <- subset(trackData2011, inout == "out")
track26 <- subset(trackData2011, yday== "146") #obs.SST = 19.1 
track27 <- subset(trackData2011, yday== "147") #obs.SST = 19.2-19.7
track28 <- subset(trackData2011, yday== "148") #obs.SST = 19.1-20.7
track23july<- subset(trackData2011, yday== "204") #21.7
track28july <-subset(trackData2011, yday== "209") #20.5
track30july <-subset(trackData2011, yday== "211")

#May: 26, 27, 28, 29  

may1 <- "datos SST/2011_26_29_May_SST_MED_SST_L4.nc"
may1<-f1(may1)#need to subset per day (days of interes = 21, 22, 23, 25, 26, 27)
may1$ddmmyy<-as.Date(as.POSIXct(may1$date, origin="1981-01-01 00:00:00"))
may1$yday<-yday(may1$ddmmyy)


may26_11<-subset(may1, may1$yday=="146") #26
range(na.omit(may26_11$sst)) #17.54999 22.70999

may27_11<-subset(may1, may1$yday=="147") #27
range(na.omit(may27_11$sst)) #18.96999 22.89999

may28_11<-subset(may1, may1$yday=="148") #28 = only date for in 
range(na.omit(may28_11$sst)) #18.94999 24.01999

may29_11<-subset(may1, may1$yday=="149") #
range(na.omit(may29_11$sst)) #18.83999 23.37999


#histograms and maps----
hist(may26_11$sst, col = c2, breaks = 50, xlim = c(14,27), main ="SST 26/05/2011 | Direction: IN", xlab="")
rug(track26$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "red")
mtext("SST",side=1,col="black",line=2.5)



mainMap + geom_raster(data = may26_11, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data =may26_11, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(14, 24), colours = col.pal, na.value = NA) +
  theme_bw() + labs(title = "Mean SST | 26/05/2011", fill = "SST") 
  
  
  
hist(may27_11$sst, col = c2, breaks = 50, xlim = c(14,27), main ="SST 27/05/2011 | Direction: IN", xlab="")
rug(track27$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "red")
mtext("SST",side=1,col="black",line=2.5)


mainMap + geom_raster(data = may27_11, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data =may27_11, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(14, 24), colours = col.pal, na.value = NA) +
  theme_bw() + labs(title = "Mean SST | 27/05/2011", fill = "SST")


hist(may28_11$sst, col = c2, breaks = 50, xlim = c(14,27), main ="SST 28/05/2011 | Direction: IN", xlab="")
rug(track28$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "red")
mtext("SST",side=1,col="black",line=2.5)

mainMap + geom_raster(data = may28_11, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data =may28_11, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(18, 24), colours = col.pal, na.value = NA) +
  theme_bw() + labs(title = "Mean SST | 28/05/2011", fill = "SST")

#July: 23, 28, 30----

july1 <- "datos SST/2011_July_23_L4_NRT.nc"
july23_11<- f1(july1)
july23_11$ddmmyy<-as.Date(as.POSIXct(july23_11$date, origin="1981-01-01 00:00:00"))
july23_11$yday<-yday(july23_11$ddmmyy)
range(na.omit(july23_11$sst)) #17.35999 23.88999

july2 <- "datos SST/2011_July_28_L4_NRT.nc"
july28_11 <- f1(july2)
july28_11$ddmmyy<-as.Date(as.POSIXct(july28_11$date, origin="1981-01-01 00:00:00"))
july28_11$yday<-yday(july28_11$ddmmyy)
range(na.omit(july28_11$sst)) # 17.76999 25.29999

july3 <- "datos SST/2011_July_30_L4_NRT.nc"
july30_11 <- f1(july3)
july30_11$ddmmyy<-as.Date(as.POSIXct(july30_11$date, origin="1981-01-01 00:00:00"))
july30_11$yday<-yday(july30_11$ddmmyy)


# daily maps histograms----


hist(july23_11$sst, col = c2, breaks = 50, xlim = c(14,27), main ="SST 23/07/2011 | Direction: IN", xlab="")
rug(track23july$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "red")
mtext("SST",side=1,col="black",line=2.5)

mainMap + geom_raster(data = july23_11, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data =july23_11, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(17, 25), colours = col.pal, na.value = NA) +
  theme_bw() + labs(title = "Mean SST | 23/07/2011", fill = "SST")


hist(july28_11$sst, col = c2, breaks = 100, xlim = c(14,27), main ="SST 28/07/2011 | Direction: OUT", xlab="")
rug(track28july$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "red")
mtext("SST",side=1,col="black",line=2.5)

mainMap + geom_raster(data = july28_11, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data =july28_11, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(17, 25), colours = col.pal, na.value = NA) +
  theme_bw() + labs(title = "Mean SST | 28/07/2011", fill = "SST")


hist(july30_11$sst, col = c2, breaks = 100, xlim = c(14,27), main ="SST 30/07/2011 | Direction: OUT", xlab="")
rug(track30july$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "red")
mtext("SST",side=1,col="black",line=2.5)

mainMap + geom_raster(data = july30_11, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data =july30_11, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(17, 25), colours = col.pal, na.value = NA) +
  theme_bw() + labs(title = "Mean SST | 30/07/2011", fill = "SST")


#2011 IN----

IN_2011 <- na.omit(may28_11) #total 2011 in
range(IN_2011$sst) # 18.94999 24.01999 --> IBI model: 14.450 19.242

hist(may28_11$sst, col = c2, breaks = 50, xlim = c(14,27), main ="SST 28/05/2011 | Direction: IN", xlab="")
rug(track_in_2011$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
legend(24,10000, c("Model", "BF tracks"), fill=c("red", "blue"))


mainMap + geom_raster(data = may28_11, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data =may28_11, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(15, 24), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = track_in_2011, aes(x = Long, y = Lat, color =factor(track_in_2011$id)), size=3 , show.legend=F) + 
  geom_point(data =track_in_2011, aes(x = Long, y = Lat, color = factor(track_in_2011$id)), size=12, alpha=0.5, show.legend=F) + geom_text_repel(data = track_in_2011, aes(x = Long, y = Lat, label= track_in_2011$id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 28/05/2011  |  Direction: In") 


# 2011 OUT----
sstmay11_out <- rbind(may26_11, may27_11, may28_11, may29_11) 
sstmay11_out <-data.frame(sstmay11_out)
sstmay11_out<-na.omit(sstmay11_out)
range(sstmay11_out$sst) #17.54999 24.01999 ---- IBI model -> 14.672 19.568 

sstjuly11_out <- rbind(july23_11, july28_11, july30_11)
range(na.omit(sstjuly11_out$sst)) # 17.01999 25.29999 -- IBI model -> 15.347 25.028 

OUT_2011 <- rbind(sstjuly11_out, july2011GPE)#total 2011 out
#WEST_2011 <- sstmay11_out

track_out_2011 <- subset(trackData2011, inout == "out")
tout2011_may <- subset(track_out_2011, track_out_2011$Month=="5")
range(tout2011_may$Obs.SST) #19.1 20.9 # does not coincide wth max sst from IBI model

tout2011_july <- subset(track_out_2011, track_out_2011$Month=="7")
range(tout2011_july$Obs.SST) #19.2 21.7

tracks2011all<- c(GPEtrack2011$Observed.SST, tout2011_july$Obs.SST)


par(mar=c(4, 4, 2, 1))

hist(sstjuly11_out$sst, col = c1, breaks = 100, xlim = c(14,27), main ="SST 2011 | Direction: OUT", xlab="")
rug(tout2011_july$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "green")
#rug(tout2011_may$Obs.SST, ticksize = 0.05, side = 1,lwd = 2, col = "red")
legend(14,250, c( "SST July", "tracks july"), fill=c(c1, "green"))
mtext("SST",side=1,col="black",line=2.5)

#MAY WEST
hist(sstmay11_out$sst, col = c2, breaks = 50, xlim = c(14,27), main ="SST 2011 | Direction: WEST", xlab="")
rug(tout2011_may$Obs.SST, ticksize = 0.05, side = 1,lwd = 2, col = "red")
legend(14,250, c( "SST May", "tracks May"), fill=c(c2, "red"))
mtext("SST",side=1,col="black",line=2.5)

#Maps-----

mainMap + geom_raster(data = sstmay11_out , aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data =sstmay11_out, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(15, 24), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = tout2011_may, aes(x = Long, y = Lat, color =factor(tout2011_may$id)), size=3 , show.legend=F) + 
  geom_point(data = tout2011_may, aes(x = Long, y = Lat, color = factor(tout2011_may$id)), size=12, alpha=0.5, show.legend=F) + geom_text_repel(data = tout2011_may, aes(x = Long, y = Lat, label= tout2011_may$id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 26-29/05/2011  |  Direction: West") #con tracks


#Daily maps May WEST

track26may <- subset(track_out_2011, yday=="146")
track27may <- subset(track_out_2011, yday=="147")
track28may <- subset(track_out_2011, yday=="148")
track29may <- subset(track_out_2011, yday=="148")

#26 may

mainMap + geom_raster(data = may26_11, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data =may26_11, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(17, 24), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = track26may, aes(x = Long, y = Lat, color =factor(track26may$id)), size=3 , show.legend=F) + 
  geom_point(data = track26may, aes(x = Long, y = Lat, color = factor(track26may$id)), size=12, alpha=0.5, show.legend=F) + geom_text_repel(data = track26may, aes(x = Long, y = Lat, label= track26may$id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 26/05/2011  |  Direction: West") #con tracks


hist( may26_11$sst, col = c2, breaks = 100 , xlim = c(14,27), main ="SST 26/05/2011 | Direction: WEST", xlab="")
rug(track26may$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "red")
mtext("SST",side=1,col="black",line=2.5)

#27

mainMap + geom_raster(data = may27_11, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data =may27_11, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(17, 24), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = track27may, aes(x = Long, y = Lat, color =factor(track27may$id)), size=3 , show.legend=F) + 
  geom_point(data = track27may, aes(x = Long, y = Lat, color = factor(track27may$id)), size=12, alpha=0.5, show.legend=F) + geom_text_repel(data = track27may, aes(x = Long, y = Lat, label= track27may$id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 27/05/2011  |  Direction: West") #con tracks

hist( may27_11$sst, col = c2, breaks = 100 , xlim = c(14,27), main ="SST 27/05/2011 | Direction: OUT", xlab="")
rug(track27may$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "red")
legend(24,5000, c("SST" ,"track" ), fill=c(c2, "red"))
mtext("SST",side=1,col="black",line=2.5)

#28

mainMap + geom_raster(data = may28_11, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data =may28_11, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(17, 24), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = track28may, aes(x = Long, y = Lat, color =factor(track28may$id)), size=3 , show.legend=F) + 
  geom_point(data = track28may, aes(x = Long, y = Lat, color = factor(track28may$id)), size=12, alpha=0.5, show.legend=F) + geom_text_repel(data = track28may, aes(x = Long, y = Lat, label= track28may$id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 28/05/2011  |  Direction: West") #con tracks

hist( may28_11$sst, col = c2, breaks = 100 , xlim = c(14,27), main ="SST 28/05/2011 | Direction: West", xlab="")
rug(track28may$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "red")
legend(24,5000, c("SST" ,"track" ), fill=c(c2, "red"))
mtext("SST",side=1,col="black",line=2.5)


#maps July OUT-----
tout2011_july
sstjuly11_out <- data.frame(sstjuly11_out)

mainMap + geom_raster(data = sstjuly11_out, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data =sstjuly11_out, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(15, 25), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = tout2011_july, aes(x = Long, y = Lat, color =factor(tout2011_july$id)), size=3 , show.legend=F) + 
  geom_point(data = tout2011_july, aes(x = Long, y = Lat, color = factor(tout2011_july$id)), size=12, alpha=0.5, show.legend=F) + geom_text_repel(data = tout2011_july, aes(x = Long, y = Lat, label= tout2011_july$id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 23, 28, 30/07/2011  |  Direction: out") #con tracks

#July 23

july23 <- subset(tout2011_july, yday == "204")
july23_11

mainMap + geom_raster(data = july23_11 , aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data =july23_11 , aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(15, 25), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = july23, aes(x = Long, y = Lat, color = "red", size=3 , show.legend=F)) + 
  geom_point(data = july23, aes(x = Long, y = Lat, color = "red", size=12, alpha=0.5, show.legend=F)) + geom_text_repel(data = july23, aes(x = Long, y = Lat, label= july23$id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 23/07/2011  |  Direction: out") #con tracks

hist(july23_11$sst, col = c2, breaks = 100 , xlim = c(14,27), main ="SST 23/07/2011 | Direction: OUT", xlab="")
rug(july23$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "red")
mtext("SST",side=1,col="black",line=2.5)

#July 28

july28 <- subset(tout2011_july, yday == "209")
july28_11

mainMap + geom_raster(data = july28_11 , aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data =july28_11 , aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(15, 25), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = july28, aes(x = Long, y = Lat, color =factor(july28$id)), size=3 , show.legend=F) + 
  geom_point(data = july28, aes(x = Long, y = Lat, color = factor(july28$id)), size=12, alpha=0.5, show.legend=F) + geom_text_repel(data = july28, aes(x = Long, y = Lat, label= july28$id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 28/07/2011  |  Direction: out") #con tracks


hist(july28_11$sst, col = c2, breaks = 100 , xlim = c(14,27), main ="SST 28/07/2011 | Direction: OUT", xlab="")
rug(july28$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "red")
mtext("SST",side=1,col="black",line=2.5)

# July 30

july30 <- subset(tout2011_july, yday == "211")
july30_11

mainMap + geom_raster(data = july30_11 , aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data =july30_11 , aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(15, 25), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = july30, aes(x = Long, y = Lat, color =factor(july30$id)), size=3 , show.legend=F) + 
  geom_point(data = july30, aes(x = Long, y = Lat, color = factor(july30$id)), size=12, alpha=0.5, show.legend=F) + geom_text_repel(data = july30, aes(x = Long, y = Lat, label= july30$id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 30/07/2011  |  Direction: out") #con tracks

hist(july30_11$sst, col = c2, breaks = 100 , xlim = c(14,27), main ="SST 30/07/2011 | Direction: OUT", xlab="")
rug(july30$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "red")
mtext("SST",side=1,col="black",line=2.5)


#28-30

july2830 <- rbind(july28, july30)
sstjuly2830 <- rbind(july28_11, july30_11)
sstjuly2830 <- data.frame(sstjuly2830)

mainMap + geom_raster(data = sstjuly2830 , aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data =sstjuly2830 , aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(15, 25), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = july2830, aes(x = Long, y = Lat, color =factor(july2830$id)), size=3 , show.legend=F) + 
  geom_point(data = july2830, aes(x = Long, y = Lat, color = factor(july2830$id)), size=12, alpha=0.5, show.legend=F) + geom_text_repel(data = july2830, aes(x = Long, y = Lat, label= july2830$id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 28-30/07/2011  |  Direction: out") #con tracks


#2012 ----


#SST #15-18 May 

may2012<- "datos SST/2012_May_15-18_L4_NRT.nc"
may12<-f1(may2012)
may12$ddmmyy<-as.Date(as.POSIXct(may12$date, origin="1981-01-01 00:00:00"))
may12$yday<-yday(may12$ddmmyy)

may15<-subset(may12, may12$yday=="136")
may16<-subset(may12, may12$yday=="137")
may17<-subset(may12, may12$yday=="138")
may18<-subset(may12, may12$yday=="139")

#may19<-subset(may12, may12$yday=="140") no hay tracks ese dia



#??? July: 10, 14, 16, 17
july4<- "datos SST/2012_July_10_L4_NRT.nc"
july10_12<- f1(july4)
july10_12$ddmmyy<-as.Date(as.POSIXct(july10_12$date, origin="1981-01-01 00:00:00"))
july10_12$yday<-yday(july10_12$ddmmyy)
july10_12 <- subset(july10_12, yday == "192")

july5<- "datos SST/2012_July_14-17_L4_NRT.nc"
july2012<-f1(july5)
july2012$ddmmyy<-as.Date(as.POSIXct(july2012$date, origin="1981-01-01 00:00:00"))
july2012$yday<-yday(july2012$ddmmyy)
table(july2012$ddmmyy)

head(july10_12)
head(july2012)

sstjuly12_out<- rbind(july10_12, july2012)

#Tracking data 

trackData2012<- subset(tracking_data, Year=="2012")
track_in_2012 <- subset(trackData2012, inout == "in")
track_out_2012 <- subset(trackData2012, inout == "out")
tout2012_may <- subset(track_out_2012, track_out_2012$Month=="5")
tout2012_july <- subset(track_out_2012, track_out_2012$Month=="7")

#2012 IN----

in2012<- rbind(may15, may16, may17, may18)
IN_2012 <- na.omit(in2012)

range(IN_2012$sst) #  16.66999 23.37999   ---- IBI model -->16.690 22.079
range(track_in_2012$Obs.SST) #17.9 20.2

hist(in2012$sst, col = c2, breaks = 50, xlim = c(14,27), main ="SST May 15-18th 2012 | Direction: IN", xlab="")
rug(track_in_2012$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
legend(24,25000, c("Model", "BF tracks"), fill=c(c2, "blue"))
mtext("SST",side=1,col="black",line=2.5)

#
par(new=TRUE) ## Allow a second plot on the same graph
hist(track_in_2012$Obs.SST, breaks =20, xlab="", ylab="", ylim=c(0,10), xlim=c(14, 27), main = "", axes=FALSE,  col = "blue")
mtext("Tuna Obs.",side=4,col="blue",line=0) 
axis(4, col="blue",col.axis="blue",line=-3)
mtext("SST",side=1,col="black",line=2.5)
legend("topleft", c("Model", "BF tracks"), fill=c("red", "blue"))

# mapss----

track_in_2012

mainMap + geom_raster(data =in2012 , aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data =in2012 , aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(16, 21), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = track_in_2012, aes(x = Long, y = Lat, color =factor(track_in_2012$id)), size=3 , show.legend=F) + 
  geom_point(data = track_in_2012, aes(x = Long, y = Lat, color = factor(track_in_2012$id)), size=12, alpha=0.5, show.legend=F) + geom_text_repel(data = track_in_2012, aes(x = Long, y = Lat, label= track_in_2012$id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 15-18/05/2012  |  Direction: in") #con tracks


#15 may

track15may <- subset(track_in_2012, yday=="136")

mainMap + geom_raster(data = may15, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data =may15, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") +  scale_fill_gradientn(limits=c(16, 23), colours = col.pal, na.value = NA) +
  theme_bw() + labs(title = "Mean SST | 15/05/2012", fill = "SST")

hist(may15$sst, col = c2, breaks = 100 , xlim = c(14,27), main ="SST 15/05/2012 | Direction: in", xlab="")
rug(track15may$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "red")
mtext("SST",side=1,col="black",line=2.5)

#16 may

track16may <- subset(track_in_2012, yday=="137")

mainMap + geom_raster(data = may16, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data =may16, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + scale_fill_gradientn(limits=c(16, 23), colours = col.pal, na.value = NA) +
  theme_bw() + labs(title = "Mean SST | 16/05/2012", fill = "SST")

hist(may16$sst, col = c2, breaks = 100 , xlim = c(14,27), main ="SST 16/05/2012 | Direction: in", xlab="")
rug(track16may$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "red")
mtext("SST",side=1,col="black",line=2.5)


#17 may

track17may <- subset(track_in_2012, yday=="138")

mainMap + geom_raster(data = may17, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data =may17, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + scale_fill_gradientn(limits=c(16, 23), colours = col.pal, na.value = NA) +
  theme_bw() + labs(title = "Mean SST | 17/05/2012", fill = "SST")

hist(may17$sst, col = c2, breaks = 100 , xlim = c(14,27), main ="SST 17/05/2012 | Direction: in", xlab="")
rug(track17may$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "red")
mtext("SST",side=1,col="black",line=2.5)



#18 may

track18may <- subset(track_in_2012, yday=="139")

mainMap + geom_raster(data = may18, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data =may18, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + scale_fill_gradientn(limits=c(16, 23), colours = col.pal, na.value = NA) +
  theme_bw() + labs(title = "Mean SST | 18/05/2012", fill = "SST")


hist(may18$sst, col = c2, breaks = 100 , xlim = c(14,27), main ="SST 18/05/2012 | Direction: IN", xlab="")
rug(track18may$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "red")
mtext("SST",side=1,col="black",line=2.5)




#2012 OUT ----

dates2012_out # may 15, 16, 17, 18,  ???| July: 10, 14, 16, 17

may12_out <- rbind(may15,may16, may17, may18)
range(na.omit(may12_out$sst)) # 16.66999 23.37999 ----- IBI model -> 16.690 22.079
range(na.omit(sstjuly12_out$sst)) # 16.67999 23.36999 ---- IBI model -> 14.814 24.712 

range(tout2012_may$Obs.SST) #18.4 20.0
range(tout2012_july$Obs.SST) #21.0 21.9


OUT_2012 <- sstjuly12_out
range(na.omit(OUT_2012$sst)) # 16.66999 23.37999 ---- IBI model -->14.814 24.712

WEST_2012 <-may12_out


par(mar=c(4, 4, 2, 0.5 ))

hist(sstjuly12_out$sst, col = c1, breaks = 100 , xlim = c(14,27), main ="SST July 2012 | Direction: OUT", xlab="")
rug(tout2012_july$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "green")
mtext("SST",side=1,col="black",line=2.5)
legend( 24, 250, c( "SST July",  "tracks july"), fill=c( c1, "green"))

#WEST may
hist(may12_out$sst, col = c2, breaks = 70, xlim = c(14,27), main ="SST May 2012 | Direction: West", xlab="")
rug(tout2012_may$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "red")
mtext("SST",side=1,col="black",line=2.5)
legend( 24, 250, c("SST May","tracks may"), fill=c(c2,  "red"))


####
par(new=TRUE) ## Allow a second plot on the same graph
hist(tout2012_may$Obs.SST, breaks =15, xlab="", ylab="", ylim=c(0,10), xlim=c(14, 27), main = "", axes=FALSE,  col = "red")

par(new=TRUE)
hist(tout2012_july$Obs.SST, breaks =15, xlab="", ylab="", ylim=c(0,10), xlim=c(14, 27), main = "", axes=FALSE,  col = "green")


mtext("Tuna Obs.",side=4,col="black",line=0) 
axis(4, col="black",col.axis="black",line=-2)
mtext("SST",side=1,col="black",line=2.5)
legend("topleft", c("SST May", "SST July", "tracks may", "tracks july"), fill=c(c2, c1, "red", "green"))

#map----

dates2012_out
track18may <- subset(track_out_2012, yday=="139")

mainMap + geom_raster(data = may12_out, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = may12_out, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(16, 22), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = tout2012_may, aes(x = Long, y = Lat, color =factor(tout2012_may$id)), size=3 , show.legend=F) + 
  geom_point(data = tout2012_may, aes(x = Long, y = Lat), size=12, color = "red", alpha=0.5, show.legend=F) + geom_text_repel(data = tout2012_may, aes(x = Long, y = Lat, label= tout2012_may$id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 15-18/05/2012  |  Direction: West") #con tracks


# daily maps
# 15 may 

track15may <- subset(track_out_2012, yday=="136")

mainMap + geom_raster(data = may15, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data =may15, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(16, 23.5), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = track15may, aes(x = Long, y = Lat, color =factor(track15may$id)), size=3 , show.legend=F) + 
  geom_point(data = track15may, aes(x = Long, y = Lat), size=12, color = "red", alpha=0.5, show.legend=F) + geom_text_repel(data = track15may, aes(x = Long, y = Lat, label= track15may$id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 15/05/2012  |  Direction: West") #con tracks


hist(may15$sst, col = c2, breaks = 100 , xlim = c(14,27), main ="SST 15/05/2012 | Direction: WEST", xlab="")
rug(track15may$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "red")
mtext("SST",side=1,col="black",line=2.5)

# 16 may 

track16may <- subset(track_out_2012, yday=="137")

mainMap + geom_raster(data = may16, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data =may16, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(16, 22), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = track16may, aes(x = Long, y = Lat, color =factor(id)), size=3 , show.legend=F) + 
  geom_point(data = track16may, aes(x = Long, y = Lat), size=12, color = "red", alpha=0.5, show.legend=F) + geom_text_repel(data = track16may, aes(x = Long, y = Lat, label= id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 16/05/2012  |  Direction: West") #con tracks

hist(may16$sst, col = c2, breaks = 100 , xlim = c(14,27), main ="SST 16/05/2012 | Direction: WEST", xlab="")
rug(track16may$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "red")
mtext("SST",side=1,col="black",line=2.5)


# 17 may 

track17may <- subset(track_out_2012, yday=="138")

mainMap + geom_raster(data = may17, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data =may17, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(16, 22), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = track17may, aes(x = Long, y = Lat, color =factor(id)), size=3 , show.legend=F) + 
  geom_point(data = track17may, aes(x = Long, y = Lat), size=12, color = "red", alpha=0.5, show.legend=F) + geom_text_repel(data = track17may, aes(x = Long, y = Lat, label= id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 17/05/2012  |  Direction: West") #con tracks


hist(may17$sst, col = c2, breaks = 100 , xlim = c(14,27), main ="SST 17/05/2012 | Direction: WEST", xlab="")
rug(track17may$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "red")
mtext("SST",side=1,col="black",line=2.5)

# 18 may 

track18may <- subset(track_out_2012, yday=="139")

mainMap + geom_raster(data = may18, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data =may18, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(16, 22), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = track18may, aes(x = Long, y = Lat, color =factor(id)), size=3 , show.legend=F) + 
  geom_point(data = track18may, aes(x = Long, y = Lat), size=12, color = "red", alpha=0.5, show.legend=F) + geom_text_repel(data = track18may, aes(x = Long, y = Lat, label= id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 18/05/2012  |  Direction: West") #con tracks

hist(may18$sst, col = c2, breaks = 100 , xlim = c(14,27), main ="SST 18/05/2012 | Direction: WEST", xlab="")
rug(track18may$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "red")
mtext("SST",side=1,col="black",line=2.5)

# 10 july 
dates2012_out
track10july <- subset(track_out_2012, yday=="192")

mainMap + geom_raster(data = july10_12, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = july10_12, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + scale_fill_gradientn(limits=c(16, 23), colours = col.pal, na.value = NA) +
  theme_bw() + labs(title = "Mean SST | 10/07/2012", fill = "SST")


hist(july10_12$sst, col = c2, breaks = 100 , xlim = c(14,27), main ="SST 10/07/2012 | Direction: OUT", xlab="")
rug(track10july$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "red")


#14-17 july


track14july <- subset(track_out_2012, yday=="196")
track16july <- subset(track_out_2012, yday=="198")
track17july <- subset(track_out_2012, yday=="199")
track1417 <- rbind(track14july, track16july, track17july)



mainMap + geom_raster(data = july2012, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = july2012, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(16, 23), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = track1417, aes(x = Long, y = Lat, color =factor(id)), size=3 , show.legend=F) + 
  geom_point(data = track1417, aes(x = Long, y = Lat), size=12, color = "red", alpha=0.5, show.legend=F) + geom_text_repel(data = track1417, aes(x = Long, y = Lat, label= id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 14-17/07/2012  |  Direction: out") #con tracks



#14

july14 <- subset(july2012, july2012$yday=="196")

mainMap + geom_raster(data = july14, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = july14, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + scale_fill_gradientn(limits=c(16, 23), colours = col.pal, na.value = NA) +
  theme_bw() + labs(title = "Mean SST | 14/07/2012", fill = "SST")

hist(july14$sst, col = c2, breaks = 100 , xlim = c(14,27), main ="SST 14/07/2012 | Direction: OUT", xlab="")
rug(track14july$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "red")

# 16 july

july16 <- subset(july2012, july2012$yday=="198")

mainMap + geom_raster(data = july16, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = july16, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + scale_fill_gradientn(limits=c(16, 23), colours = col.pal, na.value = NA) +
  theme_bw() + labs(title = "Mean SST | 16/07/2012", fill = "SST")

hist(july16$sst, col = c2, breaks = 100 , xlim = c(14,27), main ="SST 16/07/2012 | Direction: OUT", xlab="")
rug(track16july$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "red")


# 17 july

july17 <- subset(july2012, july2012$yday=="199")

mainMap + geom_raster(data = july17, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = july17, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(16, 23), colours = col.pal, na.value = NA) +
  theme_bw() + labs(title = "Mean SST | 17/07/2012", fill = "SST")

hist(july17$sst, col = c2, breaks = 100 , xlim = c(14,27), main ="SST 17/07/2012 | Direction: OUT", xlab="")
rug(track17july$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "red")


#
#2013----


#SST 
dates2013

#May: 21-27th, 29th, 31st (in) 

may1 <- "datos SST/2013_May_21-27_L4_NRT.nc"
may1<-f1(may1)#need to subset per day (days of interes = 21, 22, 23, 25, 26, 27)
may1<-na.omit(may1)
may1$ddmmyy<-as.Date(as.POSIXct(may1$date, origin="1981-01-01 00:00:00"))
may1$yday<-yday(may1$ddmmyy)


may2931 <- "datos SST/2013_May_29-31_L4_NRT.nc"
may2931<- f1(may2931) 
may2931$ddmmyy<-as.Date(as.POSIXct(may2931$date, origin="1981-01-01 00:00:00"))
may2931$yday<-yday(may2931$ddmmyy)


may21<-subset(may1, may1$yday=="141")
may22<-subset(may1, may1$yday=="142")
may23<-subset(may1, may1$yday=="143")
may24<-subset(may1, may1$yday=="144")
may25<-subset(may1, may1$yday=="145")
may26<-subset(may1, may1$yday=="146")
may27<-subset(may1, may1$yday=="147")
may29 <- subset(may2931, yday=="149")
may31 <- subset(may2931, yday=="151")




#June 2013: 2, 4, 6, 8, 16, 18, 20, 21, 23, 24  

dates2013
june <-"datos SST/2013_June_2-8_L4_NRT.nc"
june2_8 <-f1(june)
june2_8$ddmmyy<-as.Date(as.POSIXct(june2_8$date, origin="1981-01-01 00:00:00"))
june2_8$yday<-yday(june2_8$ddmmyy)

june1624 <- "datos SST/2013_June_16-24_L4_NRT.nc"
june16_24<- f1(june1624)
june16_24$ddmmyy<-as.Date(as.POSIXct(june16_24$date, origin="1981-01-01 00:00:00"))
june16_24$yday<-yday(june16_24$ddmmyy)


june2 <-subset(june2_8, yday=="153")
june4 <-subset(june2_8, yday=="155")
june6 <-subset(june2_8, yday=="157")
june8 <-subset(june2_8, yday=="159")

june16 <- subset(june16_24, yday=="167")
june18 <- subset(june16_24, yday=="169")
june20 <- subset(june16_24, yday=="171")
june21 <- subset(june16_24, yday=="172")
june23 <- subset(june16_24, yday=="174")
june24 <- subset(june16_24, yday=="175")

june2013<- rbind(june16, june18, june20, june21, june23, june24)
june2013 <- na.omit(june2013)
range(june2013$sst) #15.13999 22.74999

#July: 5, 7, 9, 13, 17, 19, 21 (out)

july1 <- "datos SST/2013_July_5-13_L4_NRT.nc"
july5_13<- f1(july1)
#july5_13 <- subset(july5_13,long<(-5.5) & long>(-9) & lat>35 & lat<37.5)
july5_13$ddmmyy<-as.Date(as.POSIXct(july5_13$date, origin="1981-01-01 00:00:00"))
july5_13$yday<-yday(july5_13$ddmmyy)

july2 <- "datos SST/2013_July_17-21_L4_NRT.nc"
july13_21<- f1(july2)
#july13_21 <- subset(july13_21,long<(-5.5) & long>(-9) & lat>35 & lat<37.5)
july13_21$ddmmyy<-as.Date(as.POSIXct(july13_21$date, origin="1981-01-01 00:00:00"))
july13_21$yday<-yday(july13_21$ddmmyy)

july5 <- subset(july5_13, yday=="186")
july7_13 <- subset(july5_13, yday=="188")
july7_13 <- subset(july5_13, yday=="188")
july9_13 <- subset(july5_13, yday=="190")
july13_13 <- subset(july5_13, yday=="194")

july17_13 <- subset(july13_21, yday == "198")
july19_13 <- subset(july13_21, yday == "200")
july21_13 <- subset(july13_21, yday == "202")





#july: 5, 7, 9, 13, 17, 19, 21
july13 <- rbind(july5, july7_13, july9_13, july13_13, july17_13, july19_13, july21_13)
summary(july13$sst)
range(july13$long)

#???tracking data 2013

trackData2013<- subset(tracking_data, Year=="2013")
track_in_2013 <- subset(trackData2013, inout == "in")
track_out_2013 <- subset(trackData2013, inout == "out") #los de mayo no son out sino dirección oeste
track_nd_2013 <-  subset(trackData2013, inout == "nd")
tout2013_may <- subset(track_out_2013, track_out_2013$Month=="5")
tout2013_june <- subset(track_out_2013, track_out_2013$Month=="6")
tout2013_july <- subset(track_out_2013, track_out_2013$Month=="7")


#May 21, 22, 24, 25, 26, 29, 31----

dates2013_in

in2013 <- rbind(may21, may22, may24, may25, may26, may29, may31)

IN_2013 <- na.omit(in2013)
range(IN_2013$sst) # 14.61999 19.24999
range(track_in_2013$Obs.SST) #16.9 18.6

hist(in2013$sst, col = c2, breaks = 50, xlim = c(14,27), main ="SST May 2013 | Direction: IN", xlab="")
rug(track_in_2013$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
legend(24,50000, c("Model", "BF tracks"), fill=c(c2, "blue"))
mtext("SST",side=1,col="black",line=2.5)

#
par(new=TRUE) ## Allow a second plot on the same graph
hist(track_in_2013$Obs.SST, breaks =20, xlab="", ylab="", ylim=c(0,10), xlim=c(14, 27), main = "", axes=FALSE,  col = "blue")

mtext("Tuna Obs.",side=4,col="blue",line=-1) 
axis(4, col="blue",col.axis="blue",line=-3)

#Maps----

#may21
arrange(track_in_2013, yday)

track141 <-subset(track_in_2013, yday=="141")


mainMap + geom_raster(data = may21, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = may21, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + scale_fill_gradientn(limits=c(14, 19), colours = col.pal, na.value = NA) +
  theme_bw() + labs(title = "Mean SST | 21/05/2013", fill = "SST")

hist(may21$sst, col = c2, breaks = 100 , xlim = c(14,27), main ="21/05/2013  |  Direction: in", xlab="")
rug(track141$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "red")


#may22

track142 <-subset(track_in_2013, yday=="142")


mainMap + geom_raster(data = may22, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = may22, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + scale_fill_gradientn(limits=c(14, 19), colours = col.pal, na.value = NA) +
  theme_bw() + labs(title = "Mean SST | 22/05/2013", fill = "SST")

hist(may22$sst, col = c2, breaks = 100 , xlim = c(14,27), main ="22/05/2013  |  Direction: in", xlab="")
rug(track142$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "red")


#may24

track144 <-subset(track_in_2013, yday=="144")


mainMap + geom_raster(data = may24, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = may24, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + scale_fill_gradientn(limits=c(14, 19), colours = col.pal, na.value = NA) +
  theme_bw() + labs(title = "Mean SST | 24/05/2013", fill = "SST")

hist(may24$sst, col = c2, breaks = 100 , xlim = c(14,27), main ="24/05/2013  |  Direction: in", xlab="")
rug(track144$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "red")

#may25

track145 <-subset(track_in_2013, yday=="145")


mainMap + geom_raster(data = may25, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = may25, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + scale_fill_gradientn(limits=c(14, 19), colours = col.pal, na.value = NA) +
  theme_bw() + labs(title = "Mean SST | 25/05/2013", fill = "SST")

hist(may25$sst, col = c2, breaks = 100 , xlim = c(14,27), main ="25/05/2013  |  Direction: in", xlab="")
rug(track145$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "red")


#may26

track146 <-subset(track_in_2013, yday=="146")


mainMap + geom_raster(data = may26, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = may26, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + scale_fill_gradientn(limits=c(14, 19), colours = col.pal, na.value = NA) +
  theme_bw() + labs(title = "Mean SST | 26/05/2013", fill = "SST")

hist(may26$sst, col = c2, breaks = 100 , xlim = c(14,27), main ="26/05/2013  |  Direction: in", xlab="")
rug(track146$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "red")


#29 may


track149 <-subset(track_in_2013, yday=="149")

mainMap + geom_raster(data = may29, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = may29, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + scale_fill_gradientn(limits=c(14, 19), colours = col.pal, na.value = NA) +
  theme_bw() + labs(title = "Mean SST | 29/05/2013", fill = "SST")

hist(may29$sst, col = c2, breaks = 100 , xlim = c(14,27), main ="29/05/2013  |  Direction: in", xlab="")
rug(track149$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "red")

#31 may


track151 <-subset(track_in_2013, yday=="151")

mainMap + geom_raster(data = may31, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = may31, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + scale_fill_gradientn(limits=c(14, 19), colours = col.pal, na.value = NA) +
  theme_bw() + labs(title = "Mean SST | 31/05/2013", fill = "SST")

hist(may31$sst, col = c2, breaks = 100 , xlim = c(14,27), main ="31/05/2013  |  Direction: in", xlab="")
rug(track151$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "red")


#Out: ----

dates2013_out

#may: 21, 23, 25, 27

may2013_out <- rbind(may21, may23, may25, may27)
may2013_out <- na.omit(may2013_out)
head(may2013_out)
range(range(may2013_out$sst))
range(tout2013_may$Obs.SST)

par(mar=c(4, 4, 2, 0.5))
hist(may2013_out$sst, col = c2, breaks = 100 , xlim = c(14,27),  main ="SST May 2013 | Direction: West", xlab="")
rug(tout2013_may$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
mtext("SST",side=1,col="black",line=2.5)
legend(24, 30000, c("Model", "BF tracks"), fill=c(c2, "blue"))



#.par(new=TRUE) ## Allow a second plot on the same graph
hist(tout2013_may$Obs.SST, breaks =10, xlab="", ylab="", ylim=c(0,10), xlim=c(14,26), main = "", axes=FALSE, col = "blue")
mtext("Tuna Obs.",side=4,col="blue",line=2) 
axis(4, col="blue",col.axis="blue",las=1)
mtext("SST",side=1,col="black",line=2.5)

#june: 16, 18, 20, 21, 23, 24
june2013<- rbind(june16, june18, june20, june21, june23, june24)
june2013 <- na.omit(june2013)
range(june2013$sst) #15.13999 22.74999
range(tout2013_june$Obs.SST)

hist(june2013$sst, col = c3, breaks = 100 , xlim = c(14,27),  main ="SST June 2013 | Direction: Out", xlab="")
rug(tout2013_june$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
mtext("SST",side=1,col="black",line=2.5)
legend(24, 30000, c("Model", "BF tracks"), fill=c(c3, "blue"))


#july: 5, 7, 9, 13, 17, 19, 21

july13 <- data.frame(july13)
july13 <-na.omit(july13)
range(july13$sst)# 18.40999 25.45999
range(tout2013_july$Obs.SST) #18.4 23.1

par(mar=c(4, 4, 2, 0.5 ))

hist(july13$sst, col = c1, breaks = 100 , xlim = c(14,27),  main ="SST July 2013 | Direction: OUT", xlab="")
rug(tout2013_july$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
mtext("SST",side=1,col="black",line=2.5)
legend(24, 35000, c("Model", "BF tracks"), fill=c(c1, "blue"))


#

OUT_2013 <- rbind(july13, june2013)
WEST_2013 <- rbind(may2013_out)


#map -----


mainMap + geom_raster(data =july13, aes(x = long, y = lat, fill = july13$sst), interpolate = TRUE) + 
  scale_fill_gradientn(limits=c(14, 24), colours = col.pal, na.value = NA) + stat_contour(data =july13, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") +
  theme_bw() + geom_point(data = tout2013_july, aes(x = Long, y = Lat, color =factor(tout2013_july$id)), size=3 , show.legend=F) + 
  geom_point(data = tout2013_july, aes(x = Long, y = Lat, color = factor(tout2013_july$id)), size=12, alpha=0.5, show.legend=F) + geom_text_repel(data = tout2013_july, aes(x = Long, y = Lat, label= tout2013_july$id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " July 2013  |  Direction: out") #con tracks

#???daily maps----


#MAY

range(may2013_out$sst) #14.82999 18.94999

#21 May
track141 <-subset(track_out_2013, yday=="141")

mainMap + geom_raster(data = may21, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = may21, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(14, 20), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = track141, aes(x = Long, y = Lat, color =factor(id)), size=3 , show.legend=F) + 
  geom_point(data = track141, aes(x = Long, y = Lat), size=12, color = "red", alpha=0.5, show.legend=F) + geom_text_repel(data = track141, aes(x = Long, y = Lat, label= id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 21/05/2013  |  Direction: west") #con tracks

hist(may21$sst, col = c2, breaks = 100 , xlim = c(14,27), main ="21/05/2013  |  Direction: west", xlab="")
rug(track141$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")

#23 May
track143 <-subset(track_out_2013, yday=="143")

mainMap + geom_raster(data = may23, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = may23, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(14, 20), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = track143, aes(x = Long, y = Lat, color =factor(id)), size=3 , show.legend=F) + 
  geom_point(data = track143, aes(x = Long, y = Lat), size=12, color = "red", alpha=0.5, show.legend=F) + geom_text_repel(data = track143, aes(x = Long, y = Lat, label= id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 23/05/2013  |  Direction: west") #con tracks

hist(may23$sst, col = c2, breaks = 100 , xlim = c(14,27), main ="23/05/2013  |  Direction: west", xlab="")
rug(track143$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")

#25 May
track145 <-subset(track_out_2013, yday=="145")

mainMap + geom_raster(data = may25, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = may25, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(14, 20), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = track145, aes(x = Long, y = Lat, color =factor(id)), size=3 , show.legend=F) + 
  geom_point(data = track145, aes(x = Long, y = Lat), size=12, color = "red", alpha=0.5, show.legend=F) + geom_text_repel(data = track145, aes(x = Long, y = Lat, label= id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 25/05/2013  |  Direction: west") #con tracks

hist(may25$sst, col = c2, breaks = 100 , xlim = c(14,27), main ="25/05/2013  |  Direction: west", xlab="")
rug(track145$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")

#27 May
track147 <-subset(track_out_2013, yday=="147")

mainMap + geom_raster(data = may27, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = may27, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(14, 20), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = track147, aes(x = Long, y = Lat, color =factor(id)), size=3 , show.legend=F) + 
  geom_point(data = track147, aes(x = Long, y = Lat), size=12, color = "red", alpha=0.5, show.legend=F) + geom_text_repel(data = track147, aes(x = Long, y = Lat, label= id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 27/05/2013  |  Direction: West") #con tracks

hist(may25$sst, col = c2, breaks = 100 , xlim = c(14,27), main ="27/05/2013  |  Direction: west", xlab="")
rug(track145$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")

## June 

#june: 16, 18, 20, 21, 23, 24

range(june2013$sst) #15.13999 22.74999

#16 june


track167 <- subset(track_out_2013, yday=="167")

mainMap + geom_raster(data = june16, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = june16, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + scale_fill_gradientn(limits=c(15, 23), colours = col.pal, na.value = NA) +
  theme_bw() + labs(title = "Mean SST | 16/06/2013", fill = "SST")

hist(june16$sst, col = c3, breaks = 100 , xlim = c(14,27), main ="16/06/2013 | Direction: out", xlab="")
rug(track167$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
mtext("SST",side=1,col="black",line=2.5)


#18 june

track169 <- subset(track_out_2013, yday=="169")

mainMap + geom_raster(data = june18, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = june18, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + scale_fill_gradientn(limits=c(15, 23), colours = col.pal, na.value = NA) +
  theme_bw() + labs(title = "Mean SST | 18/06/2013", fill = "SST")

hist(june18$sst, col = c3, breaks = 100 , xlim = c(14,27), main ="18/06/2013 | Direction: out", xlab="")
rug(track169$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
mtext("SST",side=1,col="black",line=2.5)


#Ä 20 june

track171 <- subset(track_out_2013, yday=="171")

mainMap + geom_raster(data = june20, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = june20, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + scale_fill_gradientn(limits=c(15, 23), colours = col.pal, na.value = NA) +
  theme_bw() + labs(title = "Mean SST | 20/06/2013", fill = "SST")

hist(june20$sst, col = c3, breaks = 100 , xlim = c(14,27), main ="20/06/2013 | Direction: out", xlab="")
rug(track171$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
mtext("SST",side=1,col="black",line=2.5)


# 21 june

track172 <- subset(track_out_2013, yday=="172")

mainMap + geom_raster(data = june21, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = june21, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + scale_fill_gradientn(limits=c(15, 23), colours = col.pal, na.value = NA) +
  theme_bw() + labs(title = "Mean SST | 21/06/2013", fill = "SST")

hist(june21$sst, col = c3, breaks = 100 , xlim = c(14,27), main ="21/06/2013 | Direction: out", xlab="")
rug(track172$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
mtext("SST",side=1,col="black",line=2.5)


# 23 june

track174 <- subset(track_out_2013, yday=="174")

mainMap + geom_raster(data = june23, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = june23, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + scale_fill_gradientn(limits=c(15, 23), colours = col.pal, na.value = NA) +
  theme_bw() + labs(title = "Mean SST | 23/06/2013", fill = "SST")

hist(june23$sst, col = c3, breaks = 100 , xlim = c(14,27), main ="23/06/2013 | Direction: out", xlab="")
rug(track174$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
mtext("SST",side=1,col="black",line=2.5)

# 24 june

track175 <- subset(track_out_2013, yday=="175")

mainMap + geom_raster(data = june24, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = june24, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + scale_fill_gradientn(limits=c(15, 23), colours = col.pal, na.value = NA) +
  theme_bw() + labs(title = "Mean SST | 24/06/2013", fill = "SST")


hist(june24$sst, col = c3, breaks = 100 , xlim = c(14,27), main ="24/06/2013 | Direction: out", xlab="")
rug(track175$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
mtext("SST",side=1,col="black",line=2.5)

##JULY

#july: 5, 7, 9, 13, 17, 19, 21
july13 <- rbind(july5_13, july7_13, july9_13, july13_13, july17_13, july19_13, july21_13)
range(na.omit(july13$sst)) #18.40999 25.45999

#july 5

track186 <- subset(track_out_2013, yday=="186")

mainMap + geom_raster(data = july5_13, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = july5_13, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + scale_fill_gradientn(limits=c(18, 25.5), colours = col.pal, na.value = NA) +
  theme_bw() + labs(title = "Mean SST | 05/07/2013", fill = "SST")

hist(july5_13$sst, col = c1, breaks = 100 , xlim = c(14,27), main =" 05/07/2013 | Direction: out", xlab="")
rug(track186$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
mtext("SST",side=1,col="black",line=2.5)


#july 7

track188 <- subset(track_out_2013, yday=="188")

mainMap + geom_raster(data = july7_13, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = july7_13, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + scale_fill_gradientn(limits=c(18, 25.5), colours = col.pal, na.value = NA) +
  theme_bw() + labs(title = "Mean SST | 07/07/2013", fill = "SST")

hist(july7_13$sst, col = c1, breaks = 100 , xlim = c(14,27), main =" 07/07/2013 | Direction: out", xlab="")
rug(track188$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
mtext("SST",side=1,col="black",line=2.5)


#july 9

track190 <- subset(track_out_2013, yday=="190")

mainMap + geom_raster(data = july9_13, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = july9_13, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") +  scale_fill_gradientn(limits=c(18, 25.5), colours = col.pal, na.value = NA) +
  theme_bw() + labs(title = "Mean SST | 09/07/2013", fill = "SST")

hist(july7_13$sst, col = c1, breaks = 100 , xlim = c(14,27), main =" 09/07/2013 | Direction: out", xlab="")
rug(track190$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
mtext("SST",side=1,col="black",line=2.5)



#july 13

track194 <- subset(track_out_2013, yday=="194")

mainMap + geom_raster(data = july13_13, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = july13_13, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") +  scale_fill_gradientn(limits=c(18, 25.5), colours = col.pal, na.value = NA) +
  theme_bw() + labs(title = "Mean SST | 13/07/2013", fill = "SST")

hist(july13_13$sst, col = c1, breaks = 100 , xlim = c(14,27), main =" 13/07/2013 | Direction: out", xlab="")
rug(track194$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
mtext("SST",side=1,col="black",line=2.5)


#july 17

track198 <- subset(track_out_2013, yday=="198")

mainMap + geom_raster(data = july17_13, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = july17_13, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") +  scale_fill_gradientn(limits=c(18, 25.5), colours = col.pal, na.value = NA) +
  theme_bw() + labs(title = "Mean SST | 17/07/2013", fill = "SST")

hist(july17_13$sst, col = c1, breaks = 100 , xlim = c(14,27), main =" 17/07/2013 | Direction: out", xlab="")
rug(track198$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
mtext("SST",side=1,col="black",line=2.5)


#july 19

track200 <- subset(track_out_2013, yday=="200")

mainMap + geom_raster(data = july19_13, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = july19_13, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") +  scale_fill_gradientn(limits=c(18, 25.5), colours = col.pal, na.value = NA) +
  theme_bw() + labs(title = "Mean SST | 19/07/2013", fill = "SST")

hist(july19_13$sst, col = c1, breaks = 100 , xlim = c(14,27), main =" 19/07/2013 | Direction: out", xlab="")
rug(track200$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
mtext("SST",side=1,col="black",line=2.5)

#july 21

track202 <- subset(track_out_2013, yday=="202")

mainMap + geom_raster(data = july21_13, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = july21_13, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") +  scale_fill_gradientn(limits=c(18, 25.5), colours = col.pal, na.value = NA) +
  theme_bw() + labs(title = "Mean SST | 21/07/2013", fill = "SST")

hist(july21_13$sst, col = c1, breaks = 100 , xlim = c(14,27), main =" 21/07/2013 | Direction: out", xlab="")
rug(track202$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
mtext("SST",side=1,col="black",line=2.5)

#No direction (ND)
subset(dates2013, dates2013$inout=="nd")

id130543 <- rbind(may21, may23, may25, may27, may29, may31, june4, june6, june8)


hist(id130543$sst, col = c2, breaks = 100, xlim = c(14,27), main ="SST May- June 2013 | Direction: nd", xlab="")
rug(track_nd_2013$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
legend(24,25000, c("Model", "BF tracks"), fill=c(c2, "blue"))
mtext("SST",side=1,col="black",line=2.5)

#map

id130543<-data.frame(id130543)

mainMap + geom_raster(data =id130543, aes(x = id130543$long, y = id130543$lat, fill = id130543$sst), interpolate = TRUE) + 
  scale_fill_gradientn(limits=c(14, 20), colours = col.pal, na.value = NA) + stat_contour(data =id130543, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") +
  theme_bw() +
  geom_point(data = track_nd_2013, aes(x = track_nd_2013$Long, y = track_nd_2013$Lat, color ="black"), color= "black", size=3 , show.legend=F) + 
  geom_label_repel(data = track_nd_2013, aes(x = track_nd_2013$Long, y = track_nd_2013$Lat, label = track_nd_2013$yday, colour = "black"),  show.legend=F, box.padding   = 0.8,  point.padding = 0.3, label.size =0.2, segment.color = 'grey50', label.r = 0.2, direction= "x") +
  geom_point(data = track_nd_2013, aes(x = Long, y = Lat), color = "blue", size=12, alpha=0.5, show.legend=F) + 
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " May-June 2013  |  Direction: nd") #con tracks


#### All out and it --------

#IN
par(mar=c(4, 4, 2, 0.5))

hist(IN_2013$sst, col = c1, breaks = 90, xlim = c(14,27), main ="SST 2011, 2012 & 2013 | Direction: IN", xlab="")
hist(IN_2012$sst, col = c2, breaks = 50, xlim = c(14,27), main = "",  add=T)
hist(IN_2011$sst, col = c3, breaks = 50, xlim = c(14,27), main = "",  add=T)
rug(track_in_2013$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "green")
rug(track_in_2011$Obs.SST, ticksize = 0.03, side = 1,lwd = 5, col = "blue")
rug(track_in_2012$Obs.SST, ticksize = 0.01, side = 1,lwd = 5, col = "red")

legend(23,25000, c("2011", "2012", "2013", "tracks 2011", "tracks 2012", "tracks 2013"), fill=c(c3, c2,c1, "blue", "red", "green"))
mtext("SST",side=1,col="black",line=2.5)

#summary IN
summary(IN_2011$sst) #solo hay un día "2011-05-28" 
range(IN_2011$ddmmyy)
summary(track_in_2011$Obs.SST)

summary(IN_2012$sst) #"2012-05-15" "2012-05-18"
range(IN_2012$ddmmyy) ##
track_in_2012$Obs.SST

summary(IN_2013$sst)
range(IN_2013$ddmmyy)# "2013-05-21" "2013-05-31"
summary(track_in_2013$Obs.SST)

total_in <- rbind(IN_2011, IN_2012, IN_2013)

totaltrack_in <- rbind(track_in_2011, track_in_2012, track_in_2013)

ggplot() + 
  geom_density(aes(total_in$sst), alpha=0.75, fill = c1) + 
  
  geom_density(aes(totaltrack_in$Obs.SST), alpha=0.2,fill=c3) + 
  
  xlim(13,27)+ scale_fill_manual( values = c("SST" = c1, "Tuna track" = c3)) + theme(legend.position="right") +
  theme(axis.title = element_text(vjust = 0.25), plot.title = element_text(vjust = 1)) +labs(title = "Tuna Tracking vs Satellite SST |IN", x = "SST")+labs(subtitle = "Blue = SST | Green = tuna track Obs. SST")


#OUT
subset(dates,inout=="out")



OUT_2011<-na.omit(OUT_2011) #<- sstjuly11_out#total 2011 out
summary(OUT_2011$sst)
tout2011_july
tout2011_july$Obs.SST

WEST_2011 #<- sstmay11_out #mayo out no salen del mediterraneo, empeizan en Marruecos (larache)
tout2011_may

OUT_2012 <- na.omit(OUT_2012)#<- sstjuly12_out
summary(OUT_2012$sst) # 16.66999 23.37999 ---- IBI model -->14.814 24.712
tout2012_july$df_dates
tout2012_july$Obs.SST

WEST_2012 #<-may12_out
tout2012_may


OUT_2013 <- na.omit(OUT_2013) # <- rbind(july13, june2013)
summary(OUT_2013$sst)
tout2013 <- rbind(tout2013_june, tout2013_july)
tout2013$df_dates
tout2013$Obs.SST

WEST_2013 #<- rbind(may2013_out, june2013)
tout2013_may #<- subset(track_out_2013, track_out_2013$Month=="5") #dirección WEST


par(mar=c(4, 4, 4, 0.5))

hist(alljuly2010$sst, col = "yellow", breaks = 100, xlim = c(14,27), main ="SST 2011, 2012 & 2013 | Direction: OUT", xlab="")
hist(OUT_2012$sst, col = c2, breaks = 50, xlim = c(14,27), main = "",  add=T)
hist(OUT_2011$sst, col = c3, breaks = 100,  xlim = c(14,27), main = "",  add=T)
hist(OUT_2013$sst, col = c1, breaks = 100, xlim = c(14,27), main = "",  add=T)
rug(tout2013$Obs.SST, ticksize = 0.07, side = 1,lwd = 3, col = "green")
rug(GPEtrack2010$Observed.SST, ticksize = 0.05, side = 1,lwd = 5, col = "yellow")
rug(tout2011_july$Obs.SST, ticksize = 0.03, side = 1,lwd = 5, col = "blue")
rug(GPEtrack2011$Observed.SST, ticksize = 0.03, side = 1,lwd = 5, col = "blue")
rug(tout2012_july$Obs.SST, ticksize = 0.01, side = 1,lwd = 5, col = "red")

legend(14,58000, c("SST 2010","SST 2011", "SST 2012", "SST 2013","tracks 2010", "tracks 2011", "tracks 2012", "tracks 2013"), fill=c("yellow", c3, c2,c1,"yellow", "blue", "red", "green"))
mtext("SST",side=1,col="black",line=2.5)


summary(OUT_2011$sst)
summary(OUT_2012$sst)
summary(OUT_2013$sst)

total_out<- rbind(alljuly2010, OUT_2011, OUT_2012, OUT_2013)
totaltrack_out <- c(GPEtrack2010$Observed.SST, tracks2011all, GPEtrack2011$Observed.SST, tout2012_july$Obs.SST, tout2013$Obs.SST)
summary(na.omit(total_out$sst))

length(total_out$sst)

density(plot(total_out$sst))


  
ggplot() + 
  geom_density(aes(total_out$sst), alpha=0.75, fill = c1) + 
  
  geom_density(aes(totaltrack_out), alpha=0.2,fill=c3) + 
 
  xlim(13,27)+ scale_fill_manual( values = c("SST" = c1, "Tuna track" = c3)) + theme(legend.position="right") +
  theme(axis.title = element_text(vjust = 0.25), plot.title = element_text(vjust = 1)) +labs(title = "Tuna Tracking vs Satellite SST | OUT", x = "SST")+labs(subtitle = "Blue = SST | Green = tuna track Obs. SST")

### WEST----

hist(WEST_2013$sst, col = c1, breaks = 100,xlim = c(14,27), main ="SST 2011, 2012 & 2013 (May)| Direction: West", xlab="")
hist(WEST_2012$sst, col = c2, breaks = 100, xlim = c(14,27), main = "",  add=T)
hist(WEST_2011$sst, col = c3, breaks = 100,  xlim = c(14,27), main = "",  add=T)
rug(tout2013_may$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "green")
rug(tout2011_may$Obs.SST, ticksize = 0.03, side = 1,lwd = 5, col = "blue")
rug(tout2012_may$Obs.SST, ticksize = 0.01, side = 1,lwd = 5, col = "red")

legend(24,15000, c("SST 2011", "SST 2012", "SST 2013", "tracks 2011", "tracks 2012", "tracks 2013"), fill=c(c3, c2,c1, "blue", "red", "green"))
mtext("SST",side=1,col="black",line=2.5)


summary(OUT_2011$sst)
summary(OUT_2012$sst)
summary(OUT_2013$sst)

summary(tout2011_july$Obs.SST)



#### Reclasification of May tracks from Out to West ----

tracking_data <-read.csv("Tracking data.csv")

tracking_data[c(tracking_data$Month=="5" & tracking_data$inout=="out"), 25 ]<- "west"

tracking_data<-data.frame(tracking_data)

#write.csv(tracking_data, "Tracking data in out west.csv") #
a <-read.csv("Tracking data in out west.csv")
table(a$id, a$Year)

####TEST DE KOLMOGOROV-SMIRNOV ----

#???Hay que tener en cuenta que para poder ejecutatr un test Kolmogorov-Smirnov en R es necesario conocer la media y desviación estándar de los datos.

meansst_in <- mean(total_in$sst)
meantrack_in <- mean(totaltrack_in$SST)

sdsst_in <- sd(total_in$sst)
sdtrack_in <- sd(totaltrack_in$SST)

meansst_out <- mean(na.omit(total_out$sst))
meantrack_out <- mean(totaltrack_out)

sdsst_out <- sd(na.omit(total_out$sst))
sdtrack_out <- sd(totaltrack_out)



ecdf_track_in<- ecdf(totaltrack_in$Obs.SST)
ecdf_sst_in <-ecdf(total_in$sst)

plot(ecdf_sst_in())

ecdf_track_out<- ecdf(totaltrack_out)
ecdf_sst_out <-ecdf(total_out$sst)


# IN
par(mar=c(4, 4, 4, 1))
plot.ecdf(ecdf(total_in$sst), col="red",lwd=0.5, main="All years IN", ylab="cummulative prob", xlab="")
plot.ecdf(ecdf(totaltrack_in$Obs.SST),col="blue",lwd=0.5,ylab="",add=TRUE)
mtext("SST",side=1,col="black",line=2.5)
mtext("D = 0.20439, p-value = 0.3736", side = 3)
legend(14, 1, c("Tuna tracks IN", "SST"), c("blue", "red"))


# Se calcula la diferencia absoluta entre las probabilidades acumuladas de cada
# función.

# La distancia Kolmogorov-Smirnov es el máximo de las distancias absolutas.
ks.test(total_in$sst, totaltrack_in$Obs.SST) # D = 0.20439, p-value = 0.3736

#Una vez calculada la distancia de Kolmogorov-Smirnov, hay que determinar si el valor de esta distancia es suficientemente grande, teniendo en cuenta las muestras disponibles, como para considerar que las dos distribuciones son distintas (p-value).

install.packages("Matching")
library(Matching)
ks.boot(total_in$sst, totaltrack_in$Obs.SST)

###aplicando normalidad

ks.test(rnorm(total_in$sst), rnorm(totaltrack_in$Obs.SST))

#out
ks.test(total_out$sst, totaltrack_out)
?ks.test

plot.ecdf(ecdf(total_out$sst), col="red",lwd=0.5, main="All years OUT", ylab="cummulative prob", xlab="")
plot.ecdf(ecdf(totaltrack_out),col="blue",lwd=0.5,ylab="",add=TRUE)
mtext("SST",side=1,col="black",line=2.5)
mtext("D = 0.30455, p-value = 0.0008268", side = 3)
legend(14, 1, c("Tuna tracks out", "SST"), c("blue", "red"))


#NORMALITY
ks.test(rnorm(total_out$sst),rnorm(totaltrack_out))




#Year by Year -----

#2010

ks.test(alljuly2010$sst, GPEtrack2010$Observed.SST)

plot.ecdf(ecdf(alljuly2010$sst), col="red",lwd=0.5, ylab="cummulative prob", xlab="", main="2010 | out")
plot.ecdf(ecdf(GPEtrack2010$Observed.SST),col="blue",lwd=0.5,ylab="",add=TRUE)
mtext("SST",side=1,col="black",line=2.5)
mtext("D = 0.47956, p-value = 0.0005075", side = 3)
legend(16.5, 1, c("Tuna tracks out", "SST"), c("blue", "red"))

#2011 in #no computa, solo un dato de SST

ks.test(OUT_2011$sst, tracks2011all) # 3 datos de track
ks.test(IN_2011$sst, track_in_2011$Obs.SST)

plot.ecdf(ecdf(OUT_2011$sst), col="red",lwd=0.5, ylab="cummulative prob", xlab="", main="2011 | out")
plot.ecdf(ecdf(tracks2011all),col="blue",lwd=0.5,ylab="",add=TRUE)
mtext("SST",side=1,col="black",line=2.5)
mtext("D = 0.67275, p-value = 0.003542", side = 3)
legend(16, 1, c("Tuna tracks out", "SST"), c("blue", "red"))


ks.test(OUT_2012$sst, tout2012_july$Obs.SST)

plot.ecdf(ecdf(OUT_2012$sst), col="red",lwd=0.5, ylab="cummulative prob", xlab="", main="2012 | out")
plot.ecdf(ecdf(tout2012_july$Obs.SST),col="blue",lwd=0.5,ylab="",add=TRUE)
mtext("SST",side=1,col="black",line=2.5)
mtext("D = 0.3944, p-value = 0.5625", side = 3)
legend(16, 1, c("Tuna tracks out", "SST"), c("blue", "red"))

ks.test(IN_2012$sst, track_in_2012$Obs.SST)

plot.ecdf(ecdf(IN_2012$sst), col="red",lwd=0.5, ylab="cummulative prob", xlab="", main="2012 | IN")
plot.ecdf(ecdf(track_in_2012$Obs.SST),col="blue",lwd=0.5,ylab="",add=TRUE)
mtext("SST",side=1,col="black",line=2.5)
mtext("D = 0.42934, p-value = 0.05011", side = 3)
legend(16, 1, c("Tuna tracks out", "SST"), c("blue", "red"))


ks.test(OUT_2013$sst, tout2013$Obs.SST)

plot.ecdf(ecdf(OUT_2013$sst), col="red",lwd=0.5, ylab="cummulative prob", xlab="", main="2013 | out")
plot.ecdf(ecdf(tout2013_july$Obs.SST),col="blue",lwd=0.5,ylab="",add=TRUE)
mtext("SST",side=1,col="black",line=2.5)
mtext("D = 0.30253, p-value = 0.185", side = 3)
legend(14, 1, c("Tuna tracks out", "SST"), c("blue", "red"))


ks.test(IN_2013$sst, track_in_2013$Obs.SST)

plot.ecdf(ecdf(IN_2013$sst), col="red",lwd=0.5, ylab="cummulative prob", xlab="", main="2013 | IN")
plot.ecdf(ecdf(track_in_2013$Obs.SST),col="blue",lwd=0.5,ylab="",add=TRUE)
mtext("SST",side=1,col="black",line=2.5)
mtext("D = 0.26174, p-value = 0.5684", side = 3)
legend(14.5, 1, c("Tuna tracks out", "SST"), c("blue", "red"))


####mas analisis k-s ----

##tracks entre años

#out
ks.test(tracks2011all, GPEtrack2010$Observed.SST ) #2010 vs 2011
ks.test(tout2012_july$SST, GPEtrack2010$Observed.SST ) #2010 vs 2012
ks.test(tout2013_july$SST, GPEtrack2010$Observed.SST ) #2010 vs 2013
ks.test(tracks2011all, tout2012_july$SST) #2011 vs 2012
ks.test(tracks2011all, tout2013_july$SST) #2011 vs 2013
ks.test(tout2013_july$SST, tout2012_july$SST) #2012 vs 2013

#in
ks.test(track_in_2011$Obs.SST, track_in_2012$Obs.SST)
ks.test(track_in_2011$Obs.SST, track_in_2013$Obs.SST)
ks.test(track_in_2013$Obs.SST, track_in_2012$Obs.SST)

#ninguna distribucion es significativamente distinta == no hay variabilidad interanual en los tracks

##SST ambiental entra años

#in
ks.test(IN_2011$sst, IN_2012$sst)
ks.test(IN_2011$sst, IN_2013$sst)
ks.test(IN_2012$sst, IN_2013$sst)

#out
ks.test(OUT_2011$sst, july2010$sst)
ks.test(OUT_2012$sst, july2010$sst)
ks.test(OUT_2013$sst, july2010$sst)
ks.test(OUT_2011$sst, OUT_2012$sst)
ks.test(OUT_2011$sst, OUT_2013$sst)
ks.test(OUT_2012$sst, OUT_2013$sst)


###

plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE)
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
file.copy(from=plots.png.paths, to="C:/Users/Usuario/Desktop/maps/")

