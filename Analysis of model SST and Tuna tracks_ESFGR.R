
# Analysis of SST distribution from IBI model and tuna tracking----


##### libraries

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

#SST data from model and tracking----


setwd("C:/Users/Usuario/Desktop/...")

tracking_data <-read.csv("Tracking data.csv")
table(tracking_data$inout)


##Creating map of study area----

mapBuffer <-0.1
worldMap <- ne_download(scale = "medium", returnclass = "sp")

worldMapCroped <- crop(worldMap,extent(-9,-5,35,37.5)) #coordenadas del área de estudio

mainMap <- ggplot() + geom_polygon(data = worldMapCroped, aes(x = long, y = lat, group = group)) + coord_fixed() + theme(axis.ticks=element_blank()) + ylab("Latitude") + xlab("Longitude")#mapa del area de estudio

#colourss-
col.pal<- rainbow(150, end=0.8, rev=TRUE)


# TRACKING DATES IN AND OUT PER YEAR ----

dates<-read.csv("dates_inout.csv") #dates of tracking

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
c3 <- rgb(0, 100, 255, max=255, alpha = 100, names = "lt.bule")

##----

f1<- function(x){ #function to create a table from a .nc file including  SST values, coordinates and dates 
  
  t1<- nc_open(x)
  t2<-list()
  t2$lat<-ncvar_get(t1, "latitude")
  t2$lon<-ncvar_get(t1, "longitude")
  t2$time<-ncvar_get(t1, "time")
  t2$SST<-ncvar_get(t1, "thetao")
  t2<-na.omit(t2)
  nc_close (t1)
  
  dimnames(t2$SST) <- list(long = t2$lon, lat = t2$lat, date = t2$time)
  t3 <- melt(t2$SST, value.name = "sst")} ##function to create a table from a .nc file including  SST values, coordinates and dates


#Yearly SST of the study area from IBI model: maps and distributions
# 2011 ----

#May: 26, 27, 28, 29  

may1 <- "datos SST/21-28may2013.nc"
may1<-f1(may1)#need to subset per day (days of interes = 21, 22, 23, 25, 26, 27)
may1$ddmmyy<-as.Date(as.POSIXct(may1$date*60*60, origin="1950-01-01"))
may1$yday<-yday(may1$ddmmyy)


may26_11<-subset(may1, may1$yday=="146") #26

may27_11<-subset(may1, may1$yday=="147") #27
may28_11<-subset(may1, may1$yday=="148") #28 = only date for in 

may29_11 <- "datos SST/29may2013.nc" #29
sstmay29<- f1(may29_11) 
sstmay29$ddmmyy<-as.Date(as.POSIXct(sstmay29$date*60*60, origin="1950-01-01"))
sstmay29$yday<-yday(sstmay29$ddmmyy)

#July: 23, 28, 30

july1 <- "datos SST/23july2011.nc"
july23_11<- f1(july1)
july23_11$ddmmyy<-as.Date(as.POSIXct(july23_11$date*60*60, origin="1950-01-01"))
july23_11$yday<-yday(july23_11$ddmmyy)
range(na.omit(july23_11$sst)) #15.347 25.028


july2 <- "datos SST/28july2011.nc"
july28_11 <- f1(july2)
july28_11$ddmmyy<-as.Date(as.POSIXct(july28_11$date*60*60, origin="1950-01-01"))
july28_11$yday<-yday(july28_11$ddmmyy)

july3 <- "datos SST/30july2011.nc"
july30_11 <- f1(july3)
july30_11$ddmmyy<-as.Date(as.POSIXct(july30_11$date*60*60, origin="1950-01-01"))
july30_11$yday<-yday(july30_11$ddmmyy)


#Tracks

trackData2011<- subset(tracking_data, Year=="2011")
track_in_2011 <- subset(trackData2011, inout == "in")
track_out_2011 <- subset(trackData2011, inout == "out")


#2011 IN----

IN_2011 <- na.omit(may28_11) #total 2011 in
range(IN_2011$sst) #14.450 19.242

hist(may28_11$sst, col = c2, breaks = 50, xlim = c(14,27), main ="SST 28/05/2011 | Direction: IN", xlab="")
rug(track_in_2011$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
legend(24,10000, c("Model", "BF tracks"), fill=c("red", "blue"))


mainMap + geom_raster(data = may28_11, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data =may28_11, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(14.5, 24), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = track_in_2011, aes(x = Long, y = Lat, color =factor(track_in_2011$id)), size=3 , show.legend=F) + 
  geom_point(data =track_in_2011, aes(x = Long, y = Lat, color = factor(track_in_2011$id)), size=12, alpha=0.5, show.legend=F) + geom_text_repel(data = track_in_2011, aes(x = Long, y = Lat, label= track_in_2011$id) , color= "black") +
  labs(title = "Mean SST | 28/05/2011", fill = "SST")+labs(subtitle = " 28/05/2011  |  Direction: In") 


#####----

range(na.omit(may26_11$sst)) #14.672 19.568
range(na.omit(may27_11$sst)) #14.463 19.479
range(na.omit(may28_11$sst)) #14.450 19.242

track26 <- subset(trackData2011, yday == "146")
track27 <- subset(trackData2011, yday== "147")
track28 <- subset(trackData2011, yday== "148")


hist(may26_11$sst, col = c2, breaks = 50, xlim = c(14,27), main ="SST 26/05/2011 | Direction: West", xlab="")
rug(track26$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "red")
mtext("SST",side=1,col="black",line=2.5)



mainMap + geom_raster(data = may26_11, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data =may26_11, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(14.45, 19.242), colours = col.pal, na.value = NA) +
  theme_bw() + geom_point(data = track26, aes(x = Long, y = Lat, color = "red"), size=3 , show.legend=F) + 
  geom_point(data = track26, aes(x = Long, y = Lat, color = "red"), size=12, alpha=0.7, show.legend=F) + geom_text_repel(data = track26, aes(x = Long, y = Lat, label= track26$id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 26/05/2011") #con tracks




hist(may27_11$sst, col = c2, breaks = 50, xlim = c(14,27), main ="SST 27/05/2011 | Direction: West", xlab="")
rug(track27$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "red")
mtext("SST",side=1,col="black",line=2.5)


mainMap + geom_raster(data = may27_11, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data =may27_11, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(14, 24), colours = col.pal, na.value = NA) +
  theme_bw()  + geom_point(data = track27, aes(x = Long, y = Lat, color = "red"), size=3 , show.legend=F) + geom_point(data = track27, aes(x = Long, y = Lat, color = "red"), size=12, alpha=0.7, show.legend=F) + geom_text_repel(data = track27, aes(x = Long, y = Lat, label= track27$id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 27/05/2011") #con tracks

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








# 2011 OUT ####
sstmay11_out <- rbind(may26_11, may27_11, may28_11, sstmay29) 
sstmay11_out <-data.frame(sstmay11_out)
sstmay11_out<-na.omit(sstmay11_out)
range(sstmay11_out$sst) #14.672 19.568 

sstjuly11_out <- rbind(july23_11, july28_11, july30_11)
range(na.omit(sstjuly11_out$sst)) # 17.01999 25.29999 -- IBI model -> 15.347 25.028 

OUT_2011 <- sstjuly11_out#total 2011 out
WEST_2011 <- sstmay11_out

track_out_2011 <- subset(trackData2011, inout == "out")
tout2011_may <- subset(track_out_2011, track_out_2011$Month=="5")
range(tout2011_may$Obs.SST) #19.1 20.9 # does not coincide wth max sst from IBI model

tout2011_july <- subset(track_out_2011, track_out_2011$Month=="7")
range(tout2011_july$Obs.SST) #19.2 21.7



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

##
par(new=TRUE) ## Allow a second plot on the same graph
hist(tout2011_may$Obs.SST, breaks =10, xlab="", ylab="", ylim=c(0,10), xlim=c(14, 26), main = "", axes=FALSE,  col = "red")

par(new=TRUE)
hist(tout2011_july$Obs.SST, breaks =10, xlab="", ylab="", ylim=c(0,10), xlim=c(14, 26), main = "", axes=FALSE,  col = "green")

mtext("Tuna Obs.",side=4,col="black",line=0) 
axis(4, col="black",col.axis="black",line=-2)
mtext("SST",side=1,col="black",line=2.5)
legend("topleft", c("SST May", "SST July", "tracks may", "tracks july"), fill=c(c2, c1, "red", "green"))


#Maps-----



mainMap + geom_raster(data = sstmay11_out , aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data =sstmay11_out, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(14, 20), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = tout2011_may, aes(x = Long, y = Lat, color =factor(tout2011_may$id)), size=3 , show.legend=F) + 
  geom_point(data = tout2011_may, aes(x = Long, y = Lat, color = factor(tout2011_may$id)), size=12, alpha=0.5, show.legend=F) + geom_text_repel(data = tout2011_may, aes(x = Long, y = Lat, label= tout2011_may$id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 26-29/05/2011  |  Direction: West") #con tracks


#Daily maps May OUT

track26may <- subset(track_out_2011, yday=="146")
track27may <- subset(track_out_2011, yday=="147")
track28may <- subset(track_out_2011, yday=="148")
track29may <- subset(track_out_2011, yday=="148")

#26 may

mainMap + geom_raster(data = may26_11, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data =may26_11, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(14, 20), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = track26may, aes(x = Long, y = Lat, color =factor(track26may$id)), size=3 , show.legend=F) + 
  geom_point(data = track26may, aes(x = Long, y = Lat, color = factor(track26may$id)), size=12, alpha=0.5, show.legend=F) + geom_text_repel(data = track26may, aes(x = Long, y = Lat, label= track26may$id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 26/05/2011  |  Direction: West") #con tracks


hist( may26_11$sst, col = c2, breaks = 50 , xlim = c(14,27), main ="SST 26/05/2011 | Direction: WEST", xlab="")
rug(track26may$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "red")
mtext("SST",side=1,col="black",line=2.5)

#27

mainMap + geom_raster(data = may27_11, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data =may27_11, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(14, 20), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = track27may, aes(x = Long, y = Lat, color =factor(track27may$id)), size=3 , show.legend=F) + 
  geom_point(data = track27may, aes(x = Long, y = Lat, color = factor(track27may$id)), size=12, alpha=0.5, show.legend=F) + geom_text_repel(data = track27may, aes(x = Long, y = Lat, label= track27may$id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 27/05/2011  |  Direction: West") #con tracks

hist( may27_11$sst, col = c2, breaks = 50 , xlim = c(14,27), main ="SST 27/05/2011 | Direction: WEST", xlab="")
rug(track27may$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "red")
legend(24,80, c("SST" ,"track" ), fill=c(c2, "red"))
mtext("SST",side=1,col="black",line=2.5)

#28

mainMap + geom_raster(data = may28_11, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data =may28_11, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(14, 20), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = track28may, aes(x = Long, y = Lat, color =factor(track28may$id)), size=3 , show.legend=F) + 
  geom_point(data = track28may, aes(x = Long, y = Lat, color = factor(track28may$id)), size=12, alpha=0.5, show.legend=F) + geom_text_repel(data = track28may, aes(x = Long, y = Lat, label= track28may$id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 28/05/2011  |  Direction: West") #con tracks

hist( may28_11$sst, col = c2, breaks = 50 , xlim = c(14,27), main ="SST 28/05/2011 | Direction: WEST", xlab="")
rug(track28may$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "red")
legend(24,50, c("SST" ,"track" ), fill=c(c2, "red"))
mtext("SST",side=1,col="black",line=2.5)


#maps July OUT
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
  geom_point(data = july23, aes(x = Long, y = Lat, color =factor(july23$id)), size=3 , show.legend=F) + 
  geom_point(data = july23, aes(x = Long, y = Lat, color = factor(july23$id)), size=12, alpha=0.5, show.legend=F) + geom_text_repel(data = july23, aes(x = Long, y = Lat, label= july23$id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 23/07/2011  |  Direction: out") #con tracks

hist(july23_11$sst, col = c2, breaks = 100 , xlim = c(14,27), main ="SST 23/07/2011 | Direction: OUT", xlab="")
rug(july23$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "red")


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

may2012<- "datos SST/15-19may2012.nc"
may12<-f1(may2012)
may12$ddmmyy<-as.Date(as.POSIXct(may12$date*60*60, origin="1950-01-01"))
may12$yday<-yday(may12$ddmmyy)

may15<-subset(may12, may12$yday=="136")
may16<-subset(may12, may12$yday=="137")
may17<-subset(may12, may12$yday=="138")
may18<-subset(may12, may12$yday=="139")

#may19<-subset(may12, may12$yday=="140")



#??? July: 10, 14, 16, 17
july4<- "datos SST/10july2012.nc"
july10_12<- f1(july4)
july10_12$ddmmyy<-as.Date(as.POSIXct(july10_12$date*60*60, origin="1950-01-01"))
july10_12$yday<-yday(july10_12$ddmmyy)

july5<- "datos SST/14-17july2012.nc"
july2012<-f1(july5)
july2012$ddmmyy<-as.Date(as.POSIXct(july2012$date*60*60, origin="1950-01-01"))
july2012$yday<-yday(july2012$ddmmyy)

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

range(IN_2012$sst) #16.690 22.079
range(track_in_2012$Obs.SST) #17.9 20.2

hist(in2012$sst, col = c2, breaks = 50, xlim = c(14,27), main ="SST May 15-18th 2012 | Direction: IN", xlab="")
rug(track_in_2012$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
legend(24,250, c("Model", "BF tracks"), fill=c(c2, "blue"))
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

mainMap + geom_raster(data = may15, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data =may15, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(16, 22), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = track15may, aes(x = Long, y = Lat, color =factor(track15may$id)), size=3 , show.legend=F) + 
  geom_point(data = track15may, aes(x = Long, y = Lat, color =factor(track15may$id)), size=12, alpha=0.8, show.legend=F) + geom_text_repel(data = track15may, aes(x = Long, y = Lat, label= track15may$id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 15/05/2012  |  Direction: in") #con tracks


hist(may15$sst, col = c2, breaks = 50 , xlim = c(14,27), main ="SST 15/05/2012 | Direction: in", xlab="")
rug(track15may$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "red")
mtext("SST",side=1,col="black",line=2.5)

#16 may

track16may <- subset(track_in_2012, yday=="137")

mainMap + geom_raster(data = may16, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data =may16, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(16, 22), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = track16may, aes(x = Long, y = Lat, color =factor(track16may$id)), size=3 , show.legend=F) + 
  geom_point(data = track16may, aes(x = Long, y = Lat), size=12, color = "red", alpha=0.5, show.legend=F) + geom_text_repel(data = track16may, aes(x = Long, y = Lat, label= track16may$id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 16/05/2012  |  Direction: in") #con tracks

hist(may16$sst, col = c2, breaks = 50 , xlim = c(14,27), main ="SST 16/05/2012 | Direction: in", xlab="")
rug(track16may$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "red")
mtext("SST",side=1,col="black",line=2.5)


#17 may

track17may <- subset(track_in_2012, yday=="138")

mainMap + geom_raster(data = may17, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data =may17, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(16, 22), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = track17may, aes(x = Long, y = Lat, color =factor(track17may$id)), size=3 , show.legend=F) + 
  geom_point(data = track17may, aes(x = Long, y = Lat), size=12, color = "red", alpha=0.5, show.legend=F) + geom_text_repel(data = track17may, aes(x = Long, y = Lat, label= track17may$id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 17/05/2012  |  Direction: in") #con tracks

hist(may17$sst, col = c2, breaks = 100 , xlim = c(14,27), main ="SST 17/05/2012 | Direction: in", xlab="")
rug(track17may$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "red")
mtext("SST",side=1,col="black",line=2.5)



#18 may

track18may <- subset(track_in_2012, yday=="139")

mainMap + geom_raster(data = may18, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data =may18, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(16, 22), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = track18may, aes(x = Long, y = Lat, color =factor(track18may$id)), size=3 , show.legend=F) + 
  geom_point(data = track18may, aes(x = Long, y = Lat), size=12, color = "red", alpha=0.5, show.legend=F) + geom_text_repel(data = track18may, aes(x = Long, y = Lat, label= track18may$id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 18/05/2012  |  Direction: in") #con tracks


hist(may18$sst, col = c2, breaks = 50 , xlim = c(14,27), main ="SST 17/05/2012 | Direction: IN", xlab="")
rug(track18may$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "red")
mtext("SST",side=1,col="black",line=2.5)



#2012 OUT ----

dates2012_out # may 15, 16, 17, 18,  ???| July: 10, 14, 16, 17

may12_out <- rbind(may15,may16, may17, may18)
range(na.omit(may12_out$sst)) # 16.690 22.079 #west
range(na.omit(sstjuly12_out$sst)) # 14.814 24.712

range(tout2012_may$Obs.SST) #18.4 20.0
range(tout2012_july$Obs.SST) #21.0 21.9


OUT_2012 <- sstjuly12_out
range(na.omit(OUT_2012$sst)) #14.814 24.712

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


hist(may15$sst, col = c2, breaks = 50 , xlim = c(14,27), main ="SST 15/05/2012 | Direction: WEST", xlab="")
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

hist(may16$sst, col = c2, breaks = 50 , xlim = c(14,27), main ="SST 16/05/2012 | Direction: WEST", xlab="")
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


hist(may17$sst, col = c2, breaks = 50 , xlim = c(14,27), main ="SST 17/05/2012 | Direction: WEST", xlab="")
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

hist(may18$sst, col = c2, breaks = 50 , xlim = c(14,27), main ="SST 18/05/2012 | Direction: WEST", xlab="")
rug(track18may$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "red")
mtext("SST",side=1,col="black",line=2.5)

# 10 july 
dates2012_out
track10july <- subset(track_out_2012, yday=="192")

mainMap + geom_raster(data = july10_12, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = july10_12, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(15, 22.5), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = track10july, aes(x = Long, y = Lat, color =factor(id)), size=3 , show.legend=F) + 
  geom_point(data = track10july, aes(x = Long, y = Lat), size=12, color = "red", alpha=0.5, show.legend=F) + geom_text_repel(data = track10july, aes(x = Long, y = Lat, label= id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 10/07/2012  |  Direction: out") #con tracks


hist(july10_12$sst, col = c2, breaks = 100 , xlim = c(14,27), main ="SST 10/07/2012 | Direction: OUT", xlab="")
rug(track10july$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "red")


#14-17 july


track14july <- subset(track_out_2012, yday=="196")
track16july <- subset(track_out_2012, yday=="198")
track17july <- subset(track_out_2012, yday=="199")
track1417 <- rbind(track14july, track16july, track17july)



mainMap + geom_raster(data = july2012, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = july2012, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(16, 22), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = track1417, aes(x = Long, y = Lat, color =factor(id)), size=3 , show.legend=F) + 
  geom_point(data = track1417, aes(x = Long, y = Lat), size=12, color = "red", alpha=0.5, show.legend=F) + geom_text_repel(data = track1417, aes(x = Long, y = Lat, label= id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 14-17/07/2012  |  Direction: out") #con tracks



#14

july14 <- subset(july2012, july2012$yday=="196")

mainMap + geom_raster(data = july14, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = july14, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(15, 22.5), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = track14july, aes(x = Long, y = Lat, color =factor(id)), size=3 , show.legend=F) + 
  geom_point(data = track14july, aes(x = Long, y = Lat), size=12, color = "red", alpha=0.5, show.legend=F) + geom_text_repel(data = track14july, aes(x = Long, y = Lat, label= id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 14/07/2012  |  Direction: out") #con tracks

hist(july14$sst, col = c2, breaks = 100 , xlim = c(14,27), main ="SST 14/07/2012 | Direction: OUT", xlab="")
rug(track14july$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "red")

# 16 july

july16 <- subset(july2012, july2012$yday=="198")

mainMap + geom_raster(data = july16, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = july16, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(15, 22.5), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = track16july, aes(x = Long, y = Lat, color =factor(id)), size=3 , show.legend=F) + 
  geom_point(data = track16july, aes(x = Long, y = Lat), size=12, color = "red", alpha=0.5, show.legend=F) + geom_text_repel(data = track16july, aes(x = Long, y = Lat, label= id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 16/07/2012  |  Direction: out") #con tracks

hist(july16$sst, col = c2, breaks = 100 , xlim = c(14,27), main ="SST 16/07/2012 | Direction: OUT", xlab="")
rug(track16july$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "red")


# 17 july

july17 <- subset(july2012, july2012$yday=="199")

mainMap + geom_raster(data = july17, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = july17, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(15, 22.5), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = track17july, aes(x = Long, y = Lat, color =factor(id)), size=3 , show.legend=F) + 
  geom_point(data = track17july, aes(x = Long, y = Lat), size=12, color = "red", alpha=0.5, show.legend=F) + geom_text_repel(data = track17july, aes(x = Long, y = Lat, label= id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 17/07/2012  |  Direction: out") #con tracks

hist(july17$sst, col = c2, breaks = 100 , xlim = c(14,27), main ="SST 17/07/2012 | Direction: OUT", xlab="")
rug(track17july$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "red")

#2013----


#SST 
dates2013

#May: 21-27th, 29th, 31st (in) 

may1 <- "datos SST/21-28may2013.nc"
may1<-f1(may1)#need to subset per day (days of interes = 21, 22, 23, 25, 26, 27)
may1$ddmmyy<-as.Date(as.POSIXct(may1$date*60*60, origin="1950-01-01"))
may1$yday<-yday(may1$ddmmyy)


sstmay29 <- "datos SST/29may2013.nc"
may29<- f1(sstmay29) 
may29$ddmmyy<-as.Date(as.POSIXct(may29$date*60*60, origin="1950-01-01"))
may29$yday<-yday(may29$ddmmyy)


sstmay31 <- "datos SST/31may2013.nc"
may31<-f1(sstmay31)
may31$ddmmyy<-as.Date(as.POSIXct(may31$date*60*60, origin="1950-01-01"))
may31$yday<-yday(may31$ddmmyy)



#21-28 May
may1$ddmmyy<-as.Date(as.POSIXct(may1$date*60*60, origin="1950-01-01"))
may1$yday<-yday(may1$ddmmyy)

may21<-subset(may1, may1$yday=="141")
may22<-subset(may1, may1$yday=="142")
may23<-subset(may1, may1$yday=="143")
may24<-subset(may1, may1$yday=="144")
may25<-subset(may1, may1$yday=="145")
may26<-subset(may1, may1$yday=="146")
may27<-subset(may1, may1$yday=="147")



#June 2013: 2, 4, 6, 8, 16, 18, 20, 21, 23, 24  


june <-"datos SST/4june2013.nc"
june4_13 <-f1(june)
june4_13$ddmmyy<-as.Date(as.POSIXct(june4_13$date*60*60, origin="1950-01-01"))
june4_13$yday<-yday(june4_13$ddmmyy)


june2 <- "datos SST/6june2013.nc"
june6 <- f1(june2)
june6$ddmmyy<-as.Date(as.POSIXct(june6$date*60*60, origin="1950-01-01"))
june6$yday<-yday(june6$ddmmyy)


june3 <- "datos SST/8june2013.nc"
june8 <- f1(june3)
june8$ddmmyy<-as.Date(as.POSIXct(june8$date*60*60, origin="1950-01-01"))
june8$yday<-yday(june8$ddmmyy)


june4 <- "datos SST/14june2013.nc"
june14<- f1(june4)
june14$ddmmyy<-as.Date(as.POSIXct(june14$date*60*60, origin="1950-01-01"))
june14$yday<-yday(june14$ddmmyy)


june5 <- "datos SST/16june2013.nc"
june16 <- f1(june5)
june16$ddmmyy<-as.Date(as.POSIXct(june16$date*60*60, origin="1950-01-01"))
june16$yday<-yday(june16$ddmmyy)


june6 <- "datos SST/18june2013.nc"
june18 <- f1(june6)
june18$ddmmyy<-as.Date(as.POSIXct(june18$date*60*60, origin="1950-01-01"))
june18$yday<-yday(june18$ddmmyy)


june7 <- "datos SST/20&21june2013.nc"
june2021 <- f1(june7)
june2021$ddmmyy<-as.Date(as.POSIXct(june2021$date*60*60, origin="1950-01-01"))
june2021$yday<-yday(june2021$ddmmyy)


june8 <- "datos SST/23&24june2013.nc"
june2324 <- f1(june8)
june2324$ddmmyy<-as.Date(as.POSIXct(june2324$date*60*60, origin="1950-01-01"))
june2324$yday<-yday(june2324$ddmmyy)

june2013<- rbind(june16, june18, june2021, june2324)
june2013 <- na.omit(june2013)
range(june2013$sst) #14.396 23.158 ---- Satellite: 15.13999 22.74999

#July: 5, 7, 9, 13, 17, 19, 21 (out)

july1 <- "datos SST/5july2013.nc"
july5_13<- f1(july1)
july5_13$ddmmyy<-as.Date(as.POSIXct(july5_13$date*60*60, origin="1950-01-01"))
july5_13$yday<-yday(july5_13$ddmmyy)


july2 <- "datos SST/7july2013.nc"
july7_13<- f1(july2)
july7_13$ddmmyy<-as.Date(as.POSIXct(july7_13$date*60*60, origin="1950-01-01"))
july7_13$yday<-yday(july7_13$ddmmyy)


july3 <- "datos SST/9july2013.nc"
july9_13<- f1(july3)
july9_13$ddmmyy<-as.Date(as.POSIXct(july9_13$date*60*60, origin="1950-01-01"))
july9_13$yday<-yday(july9_13$ddmmyy)


july4 <- "datos SST/13july2013.nc"
july13_13<- f1(july4)
july13_13$ddmmyy<-as.Date(as.POSIXct(july13_13$date*60*60, origin="1950-01-01"))
july13_13$yday<-yday(july13_13$ddmmyy)


july5 <- "datos SST/17july2013.nc"
july17_13<- f1(july5)
july17_13$ddmmyy<-as.Date(as.POSIXct(july17_13$date*60*60, origin="1950-01-01"))
july17_13$yday<-yday(july17_13$ddmmyy)

july6 <- "datos SST/19july2013.nc"
july19_13<- f1(july6)
july19_13$ddmmyy<-as.Date(as.POSIXct(july19_13$date*60*60, origin="1950-01-01"))
july19_13$yday<-yday(july19_13$ddmmyy)


july7 <- "datos SST/21july2013.nc"
july21_13<- f1(july7)
july21_13$ddmmyy<-as.Date(as.POSIXct(july21_13$date*60*60, origin="1950-01-01"))
july21_13$yday<-yday(july21_13$ddmmyy)


#july: 5, 7, 9, 13, 17, 19, 21
july13 <- rbind(july5_13, july7_13, july9_13, july13_13, july17_13, july19_13, july21_13)
summary(july13$sst)
range(july13$long)

#???tracking data 2013

trackData2013<- subset(tracking_data, Year=="2013")
track_in_2013 <- subset(trackData2013, inout == "in")
track_out_2013 <- subset(trackData2013, inout == "out")
track_nd_2013 <-  subset(trackData2013, inout == "nd")
tout2013_may <- subset(track_out_2013, track_out_2013$Month=="5")
tout2013_june <- subset(track_out_2013, track_out_2013$Month=="6")
tout2013_july <- subset(track_out_2013, track_out_2013$Month=="7")


#In 2013 

#May 21, 22, 24, 25, 26, 29, 31 ----

dates2013_in

in2013 <- rbind(may21, may22, may24, may25, may26, may29, may31)

IN_2013 <- na.omit(in2013)
range(IN_2013$sst)
range(track_in_2013$Obs.SST)

hist(in2013$sst, col = c2, breaks = 50, xlim = c(14,27), main ="SST May 2013 | Direction: IN", xlab="")
rug(track_in_2013$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
legend(24,500, c("Model", "BF tracks"), fill=c(c2, "blue"))
mtext("SST",side=1,col="black",line=2.5)

#
par(new=TRUE) ## Allow a second plot on the same graph
hist(track_in_2013$Obs.SST, breaks =20, xlab="", ylab="", ylim=c(0,10), xlim=c(14, 27), main = "", axes=FALSE,  col = "blue")

mtext("Tuna Obs.",side=4,col="blue",line=-1) 
axis(4, col="blue",col.axis="blue",line=-3)

#Maps ----

#may21
arrange(track_in_2013, yday)

track141 <-subset(track_in_2013, yday=="141")


mainMap + geom_raster(data = may21, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = may21, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(14, 20), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = track141, aes(x = Long, y = Lat, color =factor(id)), size=3 , show.legend=F) + 
  geom_point(data = track141, aes(x = Long, y = Lat), size=12, color = "red", alpha=0.5, show.legend=F) + geom_text_repel(data = track141, aes(x = Long, y = Lat, label= id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 21/05/2013  |  Direction: in") #con tracks

hist(may21$sst, col = c2, breaks = 50 , xlim = c(14,27), main ="21/05/2013  |  Direction: in", xlab="")
rug(track141$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "red")


#may22

track142 <-subset(track_in_2013, yday=="142")


mainMap + geom_raster(data = may22, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = may22, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(14, 20), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = track142, aes(x = Long, y = Lat, color =factor(id)), size=3 , show.legend=F) + 
  geom_point(data = track142, aes(x = Long, y = Lat), size=12, color = "red", alpha=0.5, show.legend=F) + geom_text_repel(data = track142, aes(x = Long, y = Lat, label= id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 22/05/2013  |  Direction: in") #con tracks

hist(may22$sst, col = c2, breaks = 50 , xlim = c(14,27), main ="22/05/2013  |  Direction: in", xlab="")
rug(track142$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "red")


#may24

track144 <-subset(track_in_2013, yday=="144")


mainMap + geom_raster(data = may24, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = may24, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(14, 20), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = track144, aes(x = Long, y = Lat, color =factor(id)), size=3 , show.legend=F) + 
  geom_point(data = track144, aes(x = Long, y = Lat), size=12, color = "red", alpha=0.5, show.legend=F) + geom_text_repel(data = track144, aes(x = Long, y = Lat, label= id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 24/05/2013  |  Direction: in") #con tracks

hist(may24$sst, col = c2, breaks = 50 , xlim = c(14,27), main ="24/05/2013  |  Direction: in", xlab="")
rug(track144$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "red")

#may25

track145 <-subset(track_in_2013, yday=="145")


mainMap + geom_raster(data = may25, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = may25, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(14, 20), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = track145, aes(x = Long, y = Lat, color =factor(id)), size=3 , show.legend=F) + 
  geom_point(data = track145, aes(x = Long, y = Lat), size=12, color = "red", alpha=0.5, show.legend=F) + geom_text_repel(data = track145, aes(x = Long, y = Lat, label= id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 25/05/2013  |  Direction: in") #con tracks

hist(may25$sst, col = c2, breaks = 50 , xlim = c(14,27), main ="25/05/2013  |  Direction: in", xlab="")
rug(track145$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "red")


#may26

track146 <-subset(track_in_2013, yday=="146")


mainMap + geom_raster(data = may26, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = may26, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(14, 20), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = track146, aes(x = Long, y = Lat, color =factor(id)), size=3 , show.legend=F) + 
  geom_point(data = track146, aes(x = Long, y = Lat), size=12, color = "red", alpha=0.5, show.legend=F) + geom_text_repel(data = track146, aes(x = Long, y = Lat, label= id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 26/05/2013  |  Direction: in") #con tracks

hist(may26$sst, col = c2, breaks = 50 , xlim = c(14,27), main ="26/05/2013  |  Direction: in", xlab="")
rug(track146$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "red")


#29 may


track149 <-subset(track_in_2013, yday=="149")

mainMap + geom_raster(data = may29, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = may29, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(14, 20), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = track149, aes(x = Long, y = Lat, color =factor(id)), size=3 , show.legend=F) + 
  geom_point(data = track149, aes(x = Long, y = Lat), size=12, color = "red", alpha=0.5, show.legend=F) + geom_text_repel(data = track149, aes(x = Long, y = Lat, label= id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 29/05/2013  |  Direction: in") #con tracks

hist(may29$sst, col = c2, breaks = 50 , xlim = c(14,27), main ="29/05/2013  |  Direction: in", xlab="")
rug(track149$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "red")

#31 may


track151 <-subset(track_in_2013, yday=="151")

mainMap + geom_raster(data = may31, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = may31, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(14, 20), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = track151, aes(x = Long, y = Lat, color =factor(id)), size=3 , show.legend=F) + 
  geom_point(data = track151, aes(x = Long, y = Lat), size=12, color = "red", alpha=0.5, show.legend=F) + geom_text_repel(data = track151, aes(x = Long, y = Lat, label= id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 31/05/2013  |  Direction: in") #con tracks

hist(may31$sst, col = c2, breaks = 50 , xlim = c(14,27), main ="31/05/2013  |  Direction: in", xlab="")
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
hist(may2013_out$sst, col = c2, breaks = 50 , xlim = c(14,27),  main ="SST May 2013 | Direction: West", xlab="")
rug(tout2013_may$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
mtext("SST",side=1,col="black",line=2.5)
legend(24, 300, c("Model", "BF tracks"), fill=c(c2, "blue"))


#.par(new=TRUE) ## Allow a second plot on the same graph
hist(tout2013_may$Obs.SST, breaks =10, xlab="", ylab="", ylim=c(0,10), xlim=c(14,26), main = "", axes=FALSE, col = "blue")
mtext("Tuna Obs.",side=4,col="blue",line=2) 
axis(4, col="blue",col.axis="blue",las=1)
mtext("SST",side=1,col="black",line=2.5)

#june: 16, 18, 20, 21, 23, 24

june2013 <- na.omit(june2013)
range(june2013$sst) #14.396 23.158 --------> Satellite: 15.13999 22.74999
range(tout2013_june$Obs.SST) #17.3 20.4

hist(june2013$sst, col = c3, breaks = 100 , xlim = c(14,27),  main ="SST June 2013 | Direction: Out", xlab="")
rug(tout2013_june$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
mtext("SST",side=1,col="black",line=2.5)
legend(24, 400, c("Model", "BF tracks"), fill=c(c3, "blue"))


#july: 5, 7, 9, 13, 17, 19, 21

july13 <- data.frame(july13)
july13 <-na.omit(july13)
range(tout2013_july$Obs.SST)

par(mar=c(4, 4, 2, 0.5 ))

hist(july13$sst, col = c1, breaks = 100 , xlim = c(14,27),  main ="SST July 2013 | Direction: OUT", xlab="")
rug(tout2013_july$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
mtext("SST",side=1,col="black",line=2.5)
legend(24, 400, c("Model", "BF tracks"), fill=c(c1, "blue"))


#
OUT_2013 <- rbind(july13, june2013)
WEST_2013 <- rbind(may2013_out)

#map -----


mainMap + geom_raster(data =july13, aes(x = long, y = lat, fill = july13$sst), interpolate = TRUE) + 
  scale_fill_gradientn(limits=c(14, 24), colours = col.pal, na.value = NA) + stat_contour(data =july13, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") +
  theme_bw() + geom_point(data = tout2013_july, aes(x = Long, y = Lat, color =factor(tout2013_july$id)), size=3 , show.legend=F) + 
  geom_point(data = tout2013_july, aes(x = Long, y = Lat, color = factor(tout2013_july$id)), size=12, alpha=0.5, show.legend=F) + geom_text_repel(data = tout2013_july, aes(x = Long, y = Lat, label= tout2013_july$id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " July 2013  |  Direction: out") #con tracks

#???daily

#21 May
track141 <-subset(track_out_2013, yday=="141")

mainMap + geom_raster(data = may21, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = may21, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(14, 20), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = track141, aes(x = Long, y = Lat, color =factor(id)), size=3 , show.legend=F) + 
  geom_point(data = track141, aes(x = Long, y = Lat), size=12, color = "red", alpha=0.5, show.legend=F) + geom_text_repel(data = track141, aes(x = Long, y = Lat, label= id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 21/05/2013  |  Direction: west") #con tracks

hist(may21$sst, col = c2, breaks = 50 , xlim = c(14,27), main ="21/05/2013  |  Direction: west", xlab="")
rug(track141$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")

#23 May
track143 <-subset(track_out_2013, yday=="143")

mainMap + geom_raster(data = may23, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = may23, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(14, 20), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = track143, aes(x = Long, y = Lat, color =factor(id)), size=3 , show.legend=F) + 
  geom_point(data = track143, aes(x = Long, y = Lat), size=12, color = "red", alpha=0.5, show.legend=F) + geom_text_repel(data = track143, aes(x = Long, y = Lat, label= id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 23/05/2013  |  Direction: west") #con tracks

hist(may23$sst, col = c2, breaks = 50 , xlim = c(14,27), main ="23/05/2013  |  Direction: west", xlab="")
rug(track143$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")

#25 May
track145 <-subset(track_out_2013, yday=="145")

mainMap + geom_raster(data = may25, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = may25, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(14, 20), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = track145, aes(x = Long, y = Lat, color =factor(id)), size=3 , show.legend=F) + 
  geom_point(data = track145, aes(x = Long, y = Lat), size=12, color = "red", alpha=0.5, show.legend=F) + geom_text_repel(data = track145, aes(x = Long, y = Lat, label= id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 25/05/2013  |  Direction: west") #con tracks

hist(may25$sst, col = c2, breaks = 50 , xlim = c(14,27), main ="25/05/2013  |  Direction: west", xlab="")
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
june13<- rbind(june16_13, june18_13, june2021_13, june2324_13)

june20 <- subset(june2021_13, yday== "171")
june21 <- subset(june2021_13, yday== "172")
june23 <- subset(june2324_13, yday== "174")
june24 <- subset(june2324_13, yday== "175")

#16 june

june16_13

track167 <- subset(track_out_2013, yday=="167")

mainMap + geom_raster(data = june16_13, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = june16_13, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(14, 22), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = track167, aes(x = Long, y = Lat, color =factor(id)), size=3 , show.legend=F) + 
  geom_point(data = track167, aes(x = Long, y = Lat), size=12, color = "red", alpha=0.5, show.legend=F) + geom_text_repel(data = track167, aes(x = Long, y = Lat, label= id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 16/06/2013  |  Direction: out") #con tracks

hist(june16_13$sst, col = c3, breaks = 100 , xlim = c(14,27), main ="16/06/2013 | Direction: out", xlab="")
rug(track167$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
mtext("SST",side=1,col="black",line=2.5)


#18 june

june18_13

track169 <- subset(track_out_2013, yday=="169")

mainMap + geom_raster(data = june18_13, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = june18_13, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(14, 22), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = track169, aes(x = Long, y = Lat, color =factor(id)), size=3 , show.legend=F) + 
  geom_point(data = track169, aes(x = Long, y = Lat), size=12, color = "red", alpha=0.5, show.legend=F) + geom_text_repel(data = track169, aes(x = Long, y = Lat, label= id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 18/06/2013  |  Direction: out") #con tracks

hist(june18_13$sst, col = c3, breaks = 50 , xlim = c(14,27), main ="18/06/2013 | Direction: out", xlab="")
rug(track169$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
mtext("SST",side=1,col="black",line=2.5)


#Ä 20 june

track171 <- subset(track_out_2013, yday=="171")

mainMap + geom_raster(data = june20, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = june20, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(14, 22), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = track171, aes(x = Long, y = Lat, color =factor(id)), size=3 , show.legend=F) + 
  geom_point(data = track171, aes(x = Long, y = Lat), size=12, color = "red", alpha=0.5, show.legend=F) + geom_text_repel(data = track171, aes(x = Long, y = Lat, label= id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 20/06/2013  |  Direction: out") #con tracks

hist(june20$sst, col = c3, breaks = 50 , xlim = c(14,27), main ="20/06/2013 | Direction: out", xlab="")
rug(track171$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
mtext("SST",side=1,col="black",line=2.5)


# 21 june

track172 <- subset(track_out_2013, yday=="172")

mainMap + geom_raster(data = june21, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = june21, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(14, 22), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = track172, aes(x = Long, y = Lat, color =factor(id)), size=3 , show.legend=F) + 
  geom_point(data = track172, aes(x = Long, y = Lat), size=12, color = "red", alpha=0.5, show.legend=F) + geom_text_repel(data = track172, aes(x = Long, y = Lat, label= id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 21/06/2013  |  Direction: out") #con tracks

hist(june21$sst, col = c3, breaks = 100 , xlim = c(14,27), main ="21/06/2013 | Direction: out", xlab="")
rug(track172$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
mtext("SST",side=1,col="black",line=2.5)


# 23 june

track174 <- subset(track_out_2013, yday=="174")

mainMap + geom_raster(data = june23, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = june23, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(14, 22), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = track174, aes(x = Long, y = Lat, color =factor(id)), size=3 , show.legend=F) + 
  geom_point(data = track174, aes(x = Long, y = Lat), size=12, color = "red", alpha=0.5, show.legend=F) + geom_text_repel(data = track174, aes(x = Long, y = Lat, label= id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 23/06/2013  |  Direction: out") #con tracks

hist(june23$sst, col = c3, breaks = 50 , xlim = c(14,27), main ="23/06/2013 | Direction: out", xlab="")
rug(track174$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
mtext("SST",side=1,col="black",line=2.5)

# 24 june

track175 <- subset(track_out_2013, yday=="175")

mainMap + geom_raster(data = june24, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = june24, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(14, 22), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = track175, aes(x = Long, y = Lat, color =factor(id)), size=3 , show.legend=F) + 
  geom_point(data = track175, aes(x = Long, y = Lat), size=12, color = "red", alpha=0.5, show.legend=F) + geom_text_repel(data = track175, aes(x = Long, y = Lat, label= id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 24/06/2013  |  Direction: out") #con tracks

hist(june24$sst, col = c3, breaks = 50 , xlim = c(14,27), main ="24/06/2013 | Direction: out", xlab="")
rug(track175$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
mtext("SST",side=1,col="black",line=2.5)

##JULY

#july: 5, 7, 9, 13, 17, 19, 21
july13 <- rbind(july5_13, july7_13, july9_13, july13_13, july17_13, july19_13, july21_13)

#july 5

track186 <- subset(track_out_2013, yday=="186")

mainMap + geom_raster(data = july5_13, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = july5_13, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(15, 24), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = track186, aes(x = Long, y = Lat, color =factor(id)), size=3 , show.legend=F) + 
  geom_point(data = track186, aes(x = Long, y = Lat), size=12, color = "red", alpha=0.5, show.legend=F) + geom_text_repel(data = track186, aes(x = Long, y = Lat, label= id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 05/07/2013  |  Direction: out") #con tracks

hist(july5_13$sst, col = c1, breaks = 100 , xlim = c(14,27), main =" 05/07/2013 | Direction: out", xlab="")
rug(track186$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
mtext("SST",side=1,col="black",line=2.5)


#july 7

track188 <- subset(track_out_2013, yday=="188")

mainMap + geom_raster(data = july7_13, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = july7_13, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(15, 24), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = track188, aes(x = Long, y = Lat, color =factor(id)), size=3 , show.legend=F) + 
  geom_point(data = track188, aes(x = Long, y = Lat), size=12, color = "red", alpha=0.5, show.legend=F) + geom_text_repel(data = track188, aes(x = Long, y = Lat, label= id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 07/07/2013  |  Direction: out") #con tracks

hist(july7_13$sst, col = c1, breaks = 100 , xlim = c(14,27), main =" 07/07/2013 | Direction: out", xlab="")
rug(track188$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
mtext("SST",side=1,col="black",line=2.5)


#july 9

track190 <- subset(track_out_2013, yday=="190")

mainMap + geom_raster(data = july9_13, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = july9_13, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(15, 24), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = track190, aes(x = Long, y = Lat, color =factor(id)), size=3 , show.legend=F) + 
  geom_point(data = track190, aes(x = Long, y = Lat), size=12, color = "red", alpha=0.5, show.legend=F) + geom_text_repel(data = track190, aes(x = Long, y = Lat, label= id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 09/07/2013  |  Direction: out") #con tracks

hist(july7_13$sst, col = c1, breaks = 100 , xlim = c(14,27), main =" 09/07/2013 | Direction: out", xlab="")
rug(track190$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
mtext("SST",side=1,col="black",line=2.5)



#july 13

track194 <- subset(track_out_2013, yday=="194")

mainMap + geom_raster(data = july13_13, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = july13_13, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(15, 24), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = track194, aes(x = Long, y = Lat, color =factor(id)), size=3 , show.legend=F) + 
  geom_point(data = track194, aes(x = Long, y = Lat), size=12, color = "red", alpha=0.5, show.legend=F) + geom_text_repel(data = track194, aes(x = Long, y = Lat, label= id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 13/07/2013  |  Direction: out") #con tracks

hist(july13_13$sst, col = c1, breaks = 50 , xlim = c(14,27), main =" 13/07/2013 | Direction: out", xlab="")
rug(track194$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
mtext("SST",side=1,col="black",line=2.5)


#july 17

track198 <- subset(track_out_2013, yday=="198")

mainMap + geom_raster(data = july17_13, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = july17_13, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(15, 25), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = track198, aes(x = Long, y = Lat, color =factor(id)), size=3 , show.legend=F) + 
  geom_point(data = track198, aes(x = Long, y = Lat), size=12, color = "red", alpha=0.5, show.legend=F) + geom_text_repel(data = track198, aes(x = Long, y = Lat, label= id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 17/07/2013  |  Direction: out") #con tracks

hist(july17_13$sst, col = c1, breaks = 100 , xlim = c(14,27), main =" 17/07/2013 | Direction: out", xlab="")
rug(track198$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
mtext("SST",side=1,col="black",line=2.5)


#july 19

track200 <- subset(track_out_2013, yday=="200")

mainMap + geom_raster(data = july19_13, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = july19_13, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(15, 25), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = track200, aes(x = Long, y = Lat, color =factor(id)), size=3 , show.legend=F) + 
  geom_point(data = track200, aes(x = Long, y = Lat), size=12, color = "red", alpha=0.5, show.legend=F) + geom_text_repel(data = track200, aes(x = Long, y = Lat, label= id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 19/07/2013  |  Direction: out") #con tracks

hist(july19_13$sst, col = c1, breaks = 100 , xlim = c(14,27), main =" 19/07/2013 | Direction: out", xlab="")
rug(track200$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
mtext("SST",side=1,col="black",line=2.5)

#july 21

track202 <- subset(track_out_2013, yday=="202")

mainMap + geom_raster(data = july21_13, aes(x = long, y = lat, fill = sst), interpolate = TRUE) +  stat_contour(data = july21_13, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") + 
  scale_fill_gradientn(limits=c(15, 25), colours = col.pal, na.value = NA) +
  theme_bw() + 
  geom_point(data = track202, aes(x = Long, y = Lat, color =factor(id)), size=3 , show.legend=F) + 
  geom_point(data = track202, aes(x = Long, y = Lat), size=12, color = "red", alpha=0.5, show.legend=F) + geom_text_repel(data = track202, aes(x = Long, y = Lat, label= id) , color= "black") +
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 21/07/2013  |  Direction: out") #con tracks

hist(july21_13$sst, col = c1, breaks = 100 , xlim = c(14,27), main =" 21/07/2013 | Direction: out", xlab="")
rug(track202$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
mtext("SST",side=1,col="black",line=2.5)

#No direction (ND)
subset(dates2013, dates2013$inout=="nd")

id130543 <- rbind(may21, may23, may25, may27, sstmay29, sstmay31, june4_13, june6_13, june8_13)


hist(id130543$sst, col = c2, breaks = 100, xlim = c(14,27), main ="SST May- June 2013 | Direction: nd", xlab="")
rug(track_nd_2013$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "blue")
legend(24,600, c("Model", "BF tracks"), fill=c(c2, "blue"))
mtext("SST",side=1,col="black",line=2.5)

#map

id130543<-data.frame(id130543)

mainMap + geom_raster(data =id130543, aes(x = id130543$Long, y = id130543$lat, fill = id130543$sst), interpolate = TRUE) + 
  scale_fill_gradientn(limits=c(14, 19.5), colours = col.pal, na.value = NA) + stat_contour(data =id130543, aes(x = long, y = lat, z= sst), binwidth= 1, color="black") +
  theme_bw() +
  geom_point(data = track_nd_2013, aes(x = Long, y = Lat, color ="black"), color= "black", size=3 , show.legend=F) + geom_label_repel(data = id130543, aes(x = id130543$Long, y = Lat, label = id130543$yday, colour = "black"),  alpha = 0.7,  show.legend=F,  force = 2,
                                                                                                                                      box.padding   = 0.8,  point.padding = 0.3, label.size =0.2, segment.color = 'grey50', label.r = 0.2, direction= "x") +
  geom_point(data = track_nd_2013, aes(x = Long, y = Lat), color = "blue", size=12, alpha=0.5, show.legend=F) + 
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " May-June 2013  |  Direction: nd") #con tracks




#### All out and it --------

#IN
par(mar=c(4, 4, 2, 0.5))

hist(IN_2013$sst, col = c1, breaks = 50, xlim = c(14,27), main ="SST 2011, 2012 & 2013 | Direction: IN", xlab="")
hist(IN_2012$sst, col = c2, breaks = 50, xlim = c(14,27), main = "",  add=T)
hist(IN_2011$sst, col = c3, breaks = 50, xlim = c(14,27), main = "",  add=T)
rug(track_in_2013$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "green")
rug(track_in_2011$Obs.SST, ticksize = 0.03, side = 1,lwd = 5, col = "blue")
rug(track_in_2012$Obs.SST, ticksize = 0.01, side = 1,lwd = 5, col = "red")

legend(23,500, c("2011", "2012", "2013", "tracks 2011", "tracks 2012", "tracks 2013"), fill=c(c3, c2,c1, "blue", "red", "green"))
mtext("SST",side=1,col="black",line=2.5)



#OUT


par(mar=c(4, 4, 2, 0.5))

hist(OUT_2013$sst, col = c1, breaks = 100, xlim = c(14,27), main ="SST 2011, 2012 & 2013 | Direction: OUT", xlab="")
hist(OUT_2012$sst, col = c2, breaks = 100, xlim = c(14,27), main = "",  add=T)
hist(OUT_2011$sst, col = c3, breaks = 100, xlim = c(14,27), main = "",  add=T)
rug(track_out_2013$Obs.SST, ticksize = 0.05, side = 1,lwd = 5, col = "green")
rug(track_out_2011$Obs.SST, ticksize = 0.03, side = 1,lwd = 5, col = "blue")
rug(track_out_2012$Obs.SST, ticksize = 0.01, side = 1,lwd = 5, col = "red")

legend(23,600, c("2011", "2012", "2013", "tracks 2011", "tracks 2012", "tracks 2013"), fill=c(c3, c2,c1, "blue", "red", "green"))
mtext("SST",side=1,col="black",line=2.5)
