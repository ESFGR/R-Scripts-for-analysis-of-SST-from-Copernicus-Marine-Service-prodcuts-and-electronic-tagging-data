
####Mapping daily SST + distibution ####

library(stringr)
library(ncdf4)
library(reshape2)
library(ggplot2)
library(mapdata)
library(maptools)
library(rnaturalearth)
library(gdata)
library(raster)
library(scatterpie)
library (openxlsx)
library(dplyr)
library(lubridate)

#SST data from IBI model ----

setwd("C:/Users/Usuario/Desktop/TFM/SST and Taggging")

tracking_data <-read.csv("Tracking data.csv")
table(tracking_data$inout)

dates<-read.csv("dates_inout.csv") #dates of tagging

dates<-arrange(dates, dates$inout, dates$df_dates) #order data by direction (in/out)

#dates<-arrange(dates, dates$df_dates) #order data by date
#View(dates) #observe the dates of tagging --> dates for which SST values will be obtained


dates2011 <-subset(dates, dates$Year=="2011")
dates2012 <-subset(dates, dates$Year=="2012")
dates2013 <-subset(dates, dates$Year=="2013")

f1<- function(x){ 
  
  t1<- nc_open(x)
  t2<-list()
  t2$lat<-ncvar_get(t1, "latitude")
  t2$lon<-ncvar_get(t1, "longitude")
  t2$time<-ncvar_get(t1, "time")
  t2$SST<-ncvar_get(t1, "thetao")
  t2<-na.omit(t2)
  nc_close (t1)
  
  dimnames(t2$SST) <- list(long = t2$lon, lat = t2$lat, date = t2$time)
  t3 <- melt(t2$SST, value.name = "sst")
  
}#funcion para crear una tabla a partir del NC con valores de SST, coordenadas y fechas

#colourss-
col.pal<- rainbow(150, end=0.8, rev=TRUE)



##preparando mapa de la zona de estudio----

mapBuffer <-0.1
worldMap <- ne_download(scale = "medium", returnclass = "sp")

worldMapCroped <- crop(worldMap,extent(-9,-5,35,37.5)) #coordenadas del área de estudio

mainMap <- ggplot() + geom_polygon(data = worldMapCroped, aes(x = long, y = lat, group = group)) + coord_fixed() + theme(axis.ticks=element_blank()) + ylab("Latitude") + xlab("Longitude")#mapa del area de estudio


##### 2013 ####

####track_in_2013

###IN : #May: 21-28th, 29th, 31st (in) June: 4th (in)----

##subsetting tracking data per day
trackData2013<- subset(tracking_data, Year=="2013")
arrange(trackData2013,trackData2013$id, trackData2013$yday)

?arrange


track_in_2013 <- subset(trackData2013, inout == "in")


head(track_in_2013)
arrange(track_in_2013,yday)
range(track_in_2013$Obs.SST)
range(track_in_2013$yday)
# IN 141 = 21/05
trackmay21<- subset(track_in_2013,track_in_2013$yday=="141") #1
trackmay22<- subset(track_in_2013,track_in_2013$yday=="142") #2
trackmay23<- subset(track_in_2013, track_in_2013$yday=="143") #3 
trackmay24<- subset(track_in_2013, track_in_2013$yday=="144") #1
trackmay25<- subset(track_in_2013, track_in_2013$yday=="145") #3
trackmay26<- subset(track_in_2013, track_in_2013$yday=="146") #2
trackmay27<- subset(track_in_2013, track_in_2013$yday=="147") #2
trackmay28<- subset(track_in_2013, track_in_2013$yday=="148") #0
trackmay29<- subset(track_in_2013, track_in_2013$yday=="149") #2
trackmay31<- subset(track_in_2013, track_in_2013$yday=="151") #2

trackjune4<- subset(track_in_2013, track_in_2013$yday=="153") #1


#IBI model data



#IN
#May: 21-28th, 29th, 31st (in) 
 
may1 <- "datos SST/21-28may2013.nc"
may29 <- "datos SST/29may2013.nc"
may31 <- "datos SST/31may2013.nc"

#june <-"datos SST/2june2013.nc"


may1<-f1(may1)#need to subset per day (days of interes = 21, 22, 23, 25, 26, 27)
sstmay29<- f1(may29) 
sstmay31<-f1(may31)

#june4 <-f1(june)

sstin2013 <- c(may1, sstmay29, sstmay31)


#21-28 May
may1$ddmmyy<-as.Date(as.POSIXct(may1$date*60*60, origin="1950-01-01"))
may1$yday<-yday(may1$ddmmyy)

may21<-subset(may1, may1$yday=="141")
may22<-subset(may1, may1$yday=="142")
may23<-subset(may1, may1$yday=="143")
may25<-subset(may1, may1$yday=="145")
may26<-subset(may1, may1$yday=="146")
may27<-subset(may1, may1$yday=="147")






#mapping SST and tracks + plotting SST distribution----
?stat_contour

#may 21
mainMap + geom_raster(data =may21, aes(x = long, y = lat, fill = sst), interpolate = TRUE) + stat_contour(data =may21, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(14, 19.5), colours = col.pal, na.value = NA) + 
  theme_bw() +  
  geom_point(data = trackmay21, aes(x = Long, y = Lat, color =factor(trackmay21$id)), size=3 , show.legend=F) + 
  geom_point(data = trackmay21, aes(x = Long, y = Lat, color = factor(trackmay21$id)), size=12, alpha=0.5, show.legend=F) +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 21/05/2013  |  Direction: IN") #con tracks

hist(may21$sst, col = "red", breaks = 50, xlim = c(14,19), main = "2013 SST May 21th", xlab= "SST")
rug(trackmay21$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
legend("topright", c("Model", "BF tracks"), fill=c("red", "blue"))


#may 22
mainMap + geom_raster(data =may22, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data =may22, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") +
  scale_fill_gradientn(limits=c(14, 19.5), colours = col.pal, na.value = NA) + theme_bw() + 
  geom_point(data = trackmay22, aes(x = Long, y = Lat, color =factor(trackmay22$id)), size=3 , show.legend=F) + 
  geom_point(data = trackmay22, aes(x = Long, y = Lat, color = factor(trackmay22$id)), size=12, alpha=0.5, show.legend=F) +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 22/05/2013  |  Direction: IN") #con tracks

hist(may22$sst, col = "red", breaks = 50, xlim = c(14,20), main = "2013 SST May 22th", xlab= "SST")
rug(trackmay22$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
legend("topright", c("Model", "BF tracks"), fill=c("red", "blue"))

#may 23
mainMap + geom_raster(data =may23, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data =may23, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(14, 19.5), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = trackmay23, aes(x = Long, y = Lat, color =factor(trackmay23$id)), size=3 , show.legend=F) + 
  geom_point(data = trackmay23, aes(x = Long, y = Lat, color = factor(trackmay23$id)), size=12, alpha=0.5, show.legend=F) + geom_text(data = trackmay23, aes(x = Long, y = Lat, label= trackmay23$id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 23/05/2013  |  Direction: IN") #con tracks

hist(may23$sst, col = "red", breaks = 50, xlim = c(14,20), main = "2013 SST May 23th", xlab= "SST")
rug(trackmay23$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
legend("topright", c("Model", "BF tracks"), fill=c("red", "blue"))

#may 25
mainMap + geom_raster(data =may25, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data =may25, aes(x = long, y = lat, z= sst), color="black", binwidth= 1) +
  scale_fill_gradientn(limits=c(14, 19.5), colours = col.pal, na.value = NA) + theme_bw() + 
  geom_point(data = trackmay25, aes(x = Long, y = Lat, color =factor(trackmay25$id)), size=3 , show.legend=F) + 
  geom_point(data = trackmay25, aes(x = Long, y = Lat, color = factor(trackmay25$id)), size=12, alpha=0.5, show.legend=F) + geom_text(data = trackmay25, aes(x = Long, y = Lat, label= trackmay25$id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 25/05/2013  |  Direction: IN") #con tracks

hist(may25$sst, col = "red", breaks = 50, xlim = c(14,20), main = "2013 SST May 25th", xlab= "SST")
rug(trackmay25$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
legend("topright", c("Model", "BF tracks"), fill=c("red", "blue"))


#may 26
mainMap + geom_raster(data =may26, aes(x = long, y = lat, fill = sst), interpolate = TRUE) + 
  scale_fill_gradientn(limits=c(14, 19.5), colours = col.pal, na.value = NA) +
  theme_bw() +  stat_contour(data =may26, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") +
  geom_point(data = trackmay26, aes(x = Long, y = Lat, color =factor(trackmay26$id)), size=3 , show.legend=F) + 
  geom_point(data = trackmay26, aes(x = Long, y = Lat, color = factor(trackmay26$id)), size=12, alpha=0.5, show.legend=F) +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 26/05/2013  |  Direction: IN") #con tracks

hist(may26$sst, col = "red", breaks = 50, xlim = c(14,20), main = "2013 SST May 26th", xlab= "SST")
rug(trackmay26$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
legend("topright", c("Model", "BF tracks"), fill=c("red", "blue"))


#may 27
mainMap + geom_raster(data =may27, aes(x = long, y = lat, fill = sst), interpolate = TRUE) + 
  scale_fill_gradientn(limits=c(14, 19.5), colours = col.pal, na.value = NA) + theme_bw() + stat_contour(data =may27, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") +
  geom_point(data = trackmay27, aes(x = Long, y = Lat, color =factor(trackmay27$id)), size=3 , show.legend=F) + 
  geom_point(data = trackmay27, aes(x = Long, y = Lat, color = factor(trackmay27$id)), size=12, alpha=0.5, show.legend=F) +
  labs(title = "SST and BF tuna locations", fill = "SST") + 
  labs(subtitle = " 27/05/2013  |  Direction: IN") #con tracks

hist(may27$sst, col = "red", breaks = 50, xlim = c(14,20), main = "2013 SST May 27th", xlab= "SST")
rug(trackmay27$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
legend("topright", c("Model", "BF tracks"), fill=c("red", "blue"))


#may 29
mainMap + geom_raster(data =sstmay29, aes(x = long, y = lat, fill = sstmay29$sst), interpolate = TRUE) +  stat_contour(data =sstmay29, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") +
  scale_fill_gradientn(limits=c(14, 19.5), colours = col.pal, na.value = NA) +
  theme_bw() + geom_point(data = trackmay29, aes(x = Long, y = Lat), size=3 , color = trackmay29$id, alpha=3) + 
  geom_point(data = trackmay29, aes(x = Long, y = Lat), size=12, alpha=0.5 , color = trackmay29$id) +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 29/05/2013  |  Direction: IN") #con tracks

hist(sstmay29$sst, col = "red", breaks = 50, xlim = c(14,20), main = "2013 SST May 29th", xlab= "SST")
rug(trackmay29$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
legend("topright", c("Model", "BF tracks"), fill=c("red", "blue"))


#may 31

mainMap + geom_raster(data =sstmay31, aes(x = long, y = lat, fill = sstmay31$sst), interpolate = TRUE) + stat_contour(data =sstmay31, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") +
  scale_fill_gradientn(limits=c(14, 19.5), colours = col.pal, na.value = NA) +
  theme_bw() + geom_point(data = trackmay31, aes(x = Long, y = Lat), size=3 , color = trackmay31$id) + 
  geom_point(data = trackmay31, aes(x = Long, y = Lat), size=12, alpha=0.5 , color = trackmay31$id) +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 31/05/2013  |  Direction: IN") #con tracks

hist(sstmay31$sst, col = "red", breaks = 50, xlim = c(14,20), main = "2013 SST May 31st", xlab= "SST")
rug(trackmay29$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
legend("topright", c("Model", "BF tracks"), fill=c("red", "blue"))


#### total 2013 in
sstin2013<-data.frame(sstin2013)

mainMap + geom_raster(data =sstin2013, aes(x = long, y = lat, fill = sstin2013$sst), interpolate = TRUE) + 
  scale_fill_gradientn(limits=c(14, 19.5), colours = col.pal, na.value = NA) + stat_contour(data =sstin2013, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") +
  theme_bw() + geom_point(data = track_in_2013, aes(x = Long, y = Lat), size=3, colour = track_in_2013$id, alpha=5) + geom_point(data = track_in_2013, aes(x = Long, y = Lat), size=7, alpha=0.5 , color = track_in_2013$id) +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 2013  |  Direction: IN") #con tracks



hist(sstin2013$sst, col = "red", breaks = 100, xlim = c(14,20), main = "SST 2013", xlab= "SST")
rug(track_in_2013$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
legend("topright", c("Model", "BF tracks"), fill=c("red", "blue"))

###
c1 <- rgb(216,238,192, max = 255, alpha = 120, names = "lt.green")
c2 <- rgb(255,100,100, max = 255, alpha =100, names = "lt.red")

par(mar=c(4, 4, 2, 4))

hist(sstin2013$sst, col = "red", breaks = 70, xlim = c(14,26), main = "SST 2013 | Direction: IN", xlab= "")
par(new=TRUE) ## Allow a second plot on the same graph

hist(track_in_2013$Obs.SST, breaks =50, xlab="", ylab="", ylim=c(0,10), xlim=c(14, 26), main = "", axes=FALSE,  col = "blue")
mtext("Tuna Obs.",side=4,col="blue",line=2) 
axis(4, col="blue",col.axis="blue",las=1)
mtext("SST",side=1,col="black",line=2.5)
legend("topleft", c("Model", "BF tracks"), fill=c("red", "blue"))

#### OUT ----
subset(dates2013, dates2013$inout=="out")
#subsetting tracking data per day
trackData2013<- subset(tracking_data, Year=="2013")
track_out_2013 <- subset(trackData2013, inout == "out")
head(track_out_2013)
arrange(track_out_2013,yday)

##May: 21-28th, 29th, 31st



#June: 16, 18, 20-21, 23-24 (out)

 #june2 <- "datos SST/6june2013.nc"
 #june3 <- "datos SST/8june2013.nc"
 #june4 <- "datos SST/14june2013.nc"
june5 <- "datos SST/16june2013.nc"
june6 <- "datos SST/18june2013.nc"
june7 <- "datos SST/20&21june2013.nc"
june8 <- "datos SST/23&24june2013.nc"

 #june6 <- f1(june2)
 #june8 <- f1(june3)
 #june14 <- f1(june4)
june16 <- f1(june5)
june18 <- f1(june6)
june2021 <- f1(june7)
june2324 <- f1(june8)


#July: 5, 7, 9, 13, 17, 19, 21 (out)

july1 <- "datos SST/5july2013.nc"
july2 <- "datos SST/7july2013.nc"
july3 <- "datos SST/9july2013.nc"
july4 <- "datos SST/13july2013.nc"
july5 <- "datos SST/17july2013.nc"
july6 <- "datos SST/19july2013.nc"
july7 <- "datos SST/21july2013.nc"


july5<- f1(july1)
july7<- f1(july2)
july9<- f1(july3)
july13<- f1(july4)
july17<- f1(july5)
july19<- f1(july6)
july21<- f1(july7)

out_2013 <- c(may1, june16, june18, june2021, june2324, july5, july7, july9, july13, july17, july19, july21)

range(out_2013$sst)

head(out_2013)

total_2013 <- c(out_2013, sstin2013)



#histogram with all dates and tracks out

out_2013 <- data.frame(out_2013)

mainMap + geom_raster(data =out_2013, aes(x = long, y = lat, fill = out_2013$sst), interpolate = TRUE) + 
  scale_fill_gradientn(limits=c(14, 19.5), colours = col.pal, na.value = NA) + stat_contour(data =out_2013, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") +
  theme_bw() + geom_point(data = track_out_2013, aes(x = Long, y = Lat), size=3, colour = track_out_2013$id, alpha=5) + geom_point(data = track_out_2013, aes(x = Long, y = Lat), size=7, alpha=0.5 , color = track_out_2013$id) +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 2013  |  Direction: OUT") #con tracks


###
c1 <- rgb(216,238,192, max = 255, alpha = 120, names = "lt.green")
c2 <- rgb(255,100,100, max = 255, alpha =100, names = "lt.red")

par(mar=c(4, 4, 2, 4))
hist(out_2013$sst, col = "red", breaks = 100, xlim = c(14,26), main = "SST 2013 | Direction: OUT", xlab= "")
par(new=TRUE) ## Allow a second plot on the same graph

hist(track_out_2013$Obs.SST, breaks =50, xlab="", ylab="", ylim=c(0,10), xlim=c(14,26), main = "", axes=FALSE, col = "blue")
mtext("Tuna Obs.",side=4,col="blue",line=2) 
axis(4, col="blue",col.axis="blue",las=1)
mtext("SST",side=1,col="black",line=2.5)


legend("topleft", c("Model", "BF tracks"), fill=c("red", "blue"))

#.


hist(out_2013$sst, col = "red", breaks = 1000, xlim = c(14,24), main = "2013 SST | OUT", xlab= "SST")
rug(track_out_2013$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
legend("topright", c("Model", "BF tracks"), fill=c("red", "blue"))

### total 2013----



par(mar=c(4, 4, 2, 4))
hist(total_2013$sst, col = "red", breaks = 100, xlim = c(14,26), main = "SST 2013", xlab= "")
par(new=TRUE) ## Allow a second plot on the same graph

hist(trackData2013$Obs.SST, breaks =50, xlab="", ylab="", ylim=c(0,10), xlim=c(14,26), main = "", axes=FALSE, col = "blue")
mtext("Tuna Obs.",side=4,col="blue",line=2) 
axis(4, col="blue",col.axis="blue",las=1)
mtext("SST",side=1,col="black",line=2.5)

arrange(trackData2013, by_group= df_dates)

# 2012 ----

## IN ----
#15-19 May (in) 

#tracks
trackData2012<- subset(tracking_data, Year=="2012")
track_in_2012 <- subset(trackData2012, inout == "in")
head(track_in_2012)
arrange(track_in_2012,yday) # 2012-05-15 =  136 


track136 <- subset(track_in_2012, track_in_2012$yday=="136") #6
track137 <- subset(track_in_2012, track_in_2012$yday=="137") #9
track138 <- subset(track_in_2012, track_in_2012$yday=="138") #5
track139 <- subset(track_in_2012, track_in_2012$yday=="139") #4
track140 <- subset(track_in_2012, track_in_2012$yday=="140") #1


#SST #15-19 May (in) 

may2012<- "datos SST/15-19may2012.nc"

may12<-f1(may2012)

may12$ddmmyy<-as.Date(as.POSIXct(may12$date*60*60, origin="1950-01-01"))
may12$yday<-yday(may12$ddmmyy)

may15<-subset(may12, may12$yday=="136")
may16<-subset(may12, may12$yday=="137")
may17<-subset(may12, may12$yday=="138")
may18<-subset(may12, may12$yday=="139")
may19<-subset(may12, may12$yday=="140")


# SST distribution for 2012 + sst from tuna tracks

par(mar=c(4, 4, 2, 4))
hist(may12$sst, col = "red", breaks = 50, xlim = c(14,26), main = "SST 2012 | Direction: IN", xlab= "")
par(new=TRUE) ## Allow a second plot on the same graph

hist(track_in_2012$Obs.SST, breaks =50, xlab="", ylab="", ylim=c(0,10), xlim=c(14, 26), main = "", axes=FALSE,  col = "blue")
mtext("Tuna Obs.",side=4,col="blue",line=2) 
axis(4, col="blue",col.axis="blue",las=1)
mtext("SST",side=1,col="black",line=2.5)
legend("topleft", c("Model", "BF tracks"), fill=c("red", "blue"))

#mapping SST and tracks + plotting SST distribution for 2012 ----

#may 15


mainMap + geom_raster(data =may15, aes(x = long, y = lat, fill = sst), interpolate = TRUE) + 
  scale_fill_gradientn(limits=c(16, 22), colours = col.pal, na.value = NA) +
  theme_bw() +  stat_contour(data =may15, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  geom_point(data = track136, aes(x = Long, y = Lat, color =factor(track136$id)), size=3 , show.legend=F) + 
   geom_point(data = track136, aes(x = Long, y = Lat, color = factor(track136$id)), size=12, alpha=0.5, show.legend=F) +
  labs(title = "SST and BF tuna locations", fill = "SST")+ labs(subtitle = " 15/05/2012  |  Direction: IN") #con tracks


hist(may15$sst, col = "red", breaks = 50, xlim = c(16,22), main = "2012 SST May 15th", xlab= "SST")
rug(track136$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
legend("topright", c("Model", "BF tracks"), fill=c("red", "blue"))

#may 16

mainMap + geom_raster(data =may16, aes(x = long, y = lat, fill = sst), interpolate = TRUE) + 
  scale_fill_gradientn(limits=c(16, 22), colours = col.pal, na.value = NA) +
  theme_bw() +  stat_contour(data =may16, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") +
  geom_point(data = track137, aes(x = Long, y = Lat, color =factor(track137$id)), size=3 , show.legend=F) + 
  geom_point(data = track137, aes(x = Long, y = Lat, color = factor(track137$id)), size=12, alpha=0.5, show.legend=F) +
  labs(title = "SST and BF tuna locations", fill = "SST")+ labs(subtitle = " 16/05/2012  |  Direction: IN") #con tracks

hist(may16$sst, col = "red", breaks = 50, xlim = c(16,22), main = "2012 SST May 16th", xlab= "SST")
rug(track137$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
legend("topright", c("Model", "BF tracks"), fill=c("red", "blue"))

#may 17

mainMap + geom_raster(data =may17, aes(x = long, y = lat, fill = sst), interpolate = TRUE) + 
  scale_fill_gradientn(limits=c(16, 22), colours = col.pal, na.value = NA) +
  theme_bw() +  stat_contour(data =may17, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") +
  geom_point(data = track138, aes(x = Long, y = Lat, color =factor(track138$id)), size=3 , show.legend=F) + 
  geom_point(data = track138, aes(x = Long, y = Lat, color = factor(track138$id)), size=12, alpha=0.5, show.legend=F) +
  labs(title = "SST and BF tuna locations", fill = "SST")+ labs(subtitle = " 17/05/2012  |  Direction: IN") #con tracks

hist(may17$sst, col = "red", breaks = 50, xlim = c(16,22), main = "2012 SST May 17th", xlab= "SST")
rug(track138$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
legend("topright", c("Model", "BF tracks"), fill=c("red", "blue"))

#may 18

mainMap + geom_raster(data =may18, aes(x = long, y = lat, fill = sst), interpolate = TRUE) + 
  scale_fill_gradientn(limits=c(16, 22), colours = col.pal, na.value = NA) +
  theme_bw() +  stat_contour(data =may18, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") +
  geom_point(data = track139, aes(x = Long, y = Lat, color =factor(track139$id)), size=3 , show.legend=F) + 
  geom_point(data = track139, aes(x = Long, y = Lat, color = factor(track139$id)), size=12, alpha=0.5, show.legend=F) +
  labs(title = "SST and BF tuna locations", fill = "SST")+ labs(subtitle = " 18/05/2012  |  Direction: IN") #con tracks

hist(may18$sst, col = "red", breaks = 50, xlim = c(16,22), main = "2012 SST May 18th", xlab= "SST")
rug(track139$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
legend("topright", c("Model", "BF tracks"), fill=c("red", "blue"))


#may 19

mainMap + geom_raster(data =may19, aes(x = long, y = lat, fill = sst), interpolate = TRUE) + 
  scale_fill_gradientn(limits=c(16, 22), colours = col.pal, na.value = NA) +
  theme_bw() +  stat_contour(data =may19, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") +
  geom_point(data = track140, aes(x = Long, y = Lat, color =factor(track140$id)), size=3 , show.legend=F) + 
  geom_point(data = track140, aes(x = Long, y = Lat, color = factor(track140$id)), size=12, alpha=0.5, show.legend=F) +
  labs(title = "SST and BF tuna locations", fill = "SST")+ labs(subtitle = " 19/05/2012  |  Direction: IN") #con tracks

hist(may19$sst, col = "red", breaks = 50, xlim = c(16,22), main = "2012 SST May 19th", xlab= "SST")
rug(track140$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
legend("topright", c("Model", "BF tracks"), fill=c("red", "blue"))


#all days


mainMap + geom_raster(data =may12, aes(x = long, y = lat, fill = sst), interpolate = TRUE) + 
  scale_fill_gradientn(limits=c(16, 22), colours = col.pal, na.value = NA) +
  theme_bw() +  stat_contour(data =may12, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") +
  geom_point(data = track_in_2012, aes(x = Long, y = Lat, color =factor(track_in_2012$id)), size=3 , show.legend=F) + 
  geom_point(data = track_in_2012, aes(x = Long, y = Lat, color = factor(track_in_2012$id)), size=12, alpha=0.5, show.legend=F) +
  labs(title = "SST and BF tuna locations", fill = "SST")+ labs(subtitle = " 19/05/2012  |  Direction: IN") #con tracks

hist(may12$sst, col = "red", breaks = 50, xlim = c(16,22), main = "2012 SST May  2012", xlab= "SST")
rug(track_in_2012$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
legend("topright", c("Model", "BF tracks"), fill=c("red", "blue"))

range(track_in_2012$Obs.SST)
# OUT ----


#July: 10, 14-17 (out)


july_10_12<- "datos SST/10july2012.nc"
july_2012<- "datos SST/14-17july2012.nc"
j10<-f1(july_10_12)
j2012<- f1(july_2012)

july2012out <- c(j10, j2012 )

track_out_2012 <- subset(trackData2012, inout == "out")
head(track_out_2012)
arrange(track_out_2012,yday) # 


track192 <- subset(track_in_2012, track_in_2012$yday=="192") 
track196 <- subset(track_in_2012, track_in_2012$yday=="196") 
track197 <- subset(track_in_2012, track_in_2012$yday=="197") 
track198 <- subset(track_in_2012, track_in_2012$yday=="198") 
track199 <- subset(track_in_2012, track_in_2012$yday=="199") 


# distributions out


par(mar=c(4, 4, 2, 4))
hist(july2012out$sst, col = "red", breaks = 100, xlim = c(14,26), main = "SST 2012 | Direction: OUT", xlab= "")
par(new=TRUE) ## Allow a second plot on the same graph

hist(track_out_2012$Obs.SST, breaks =50, xlab="", ylab="", ylim=c(0,10), xlim=c(14, 26), main = "", axes=FALSE,  col = "blue")
mtext("Tuna Obs.",side=4,col="blue",line=2) 
axis(4, col="blue",col.axis="blue",las=1)
mtext("SST",side=1,col="black",line=2.5)
legend("topleft", c("Model", "BF tracks"), fill=c("red", "blue"))
 




# 2011 ----

trackData2011<- subset(tracking_data, Year=="2011")
track_in_2011 <- subset(trackData2011, inout == "in")
track_out_2011 <- subset(trackData2011, inout == "out")


SST_fun<-function(x){ # x= dataset in format nc including SST values (variable name = "thetao") 
  SST<- ncvar_get(x, "thetao") 
  SST<- as.vector(SST) 
  SST<- na.omit(SST) 
  return(SST) 
  
}

may2011<- nc_open("datos SST/SST_2011_05_26-29.nc")
may2011_IN<-SST_fun(may2011) #in


#|2011| July 23rd, 28th and 30th | OUT |

july_23_11<- nc_open("datos SST/23july2011.nc")
july_28_11<- nc_open("datos SST/28july2011.nc")
july_30_11<- nc_open("datos SST/30july2011.nc")

SST_july_23_11<-SST_fun(july_23_11)
SST_july_28_11<-SST_fun(july_28_11)
SST_july_30_11<-SST_fun(july_30_11)

july2011_OUT<- c(SST_july_23_11, SST_july_28_11, SST_july_30_11)

#### Histrograms


#IN
par(mar=c(4, 4, 2, 4))
hist(may2011_IN, col = "red", breaks = 50, xlim = c(14,26), main = "SST 2011 | Direction: IN", xlab= "")
par(new=TRUE) ## Allow a second plot on the same graph

hist(track_in_2011$Obs.SST, breaks =50, xlab="", ylab="", ylim=c(0,10), xlim=c(14, 24), main = "", axes=FALSE,  col = "blue")
mtext("Tuna Obs.",side=4,col="blue",line=2) 
axis(4, col="blue",col.axis="blue",las=1)
mtext("SST",side=1,col="black",line=2.5)
legend("topleft", c("Model", "BF tracks"), fill=c("red", "blue"))

#???OUT
par(mar=c(4, 4, 2, 4))
hist(july2011_OUT, col = "red", breaks = 100, xlim = c(14,26), main = "SST 2011 | Direction: OUT", xlab= "")
par(new=TRUE) ## Allow a second plot on the same graph

hist(track_out_2011$Obs.SST, breaks =25, xlab="", ylab="", ylim=c(0,10), xlim=c(14, 26), main = "", axes=FALSE,  col = "blue")
mtext("Tuna Obs.",side=4,col="blue",line=2) 
axis(4, col="blue",col.axis="blue",las=1)
mtext("SST",side=1,col="black",line=2.5)
legend("topleft", c("Model", "BF tracks"), fill=c("red", "blue"))


