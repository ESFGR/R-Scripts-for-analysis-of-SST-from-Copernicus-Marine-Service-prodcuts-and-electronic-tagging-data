
####Mapping SST ####

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
#SST data from model ----

setwd("C:/Users/Usuario/Desktop/TFM/SST and Taggging")

tracking_data <-read.csv("tagging/Tracking data.csv")

dates<-read.csv("dates_inout.csv") #dates of tagging

arrange(dates, dates$inout) #order data by direction (in/out)

dates<-arrange(dates, dates$df_dates) #order data by date
View(dates) #observe the dates of tagging --> dates for which SST values will be obtained



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

#colourss----
col.pal<- rainbow(150, end=0.9, rev=TRUE)
#chose your own colours

cust.col<- colorRampPalette(c("blue","orange", "pink","springgreen",  "skyblue2"))
map(col=cust.col(150), fill=TRUE)

##preparando mapa de la zona de estudio----

mapBuffer <-0.1
worldMap <- ne_download(scale = "medium", returnclass = "sp")

worldMapCroped <- crop(worldMap,extent(-9,-5,35,37.5)) #coordenadas del área de estudio

mainMap <- ggplot() + geom_polygon(data = worldMapCroped, aes(x = long, y = lat, group = group)) + coord_fixed() + theme(axis.ticks=element_blank()) + ylab("Latitude") + xlab("Longitude")#mapa del area de estudio


#|2011|---- 

trackData2011<- subset(tracking_data, Year=="2011")
IN_2011<- subset(trackData2011, inout=="in")
OUT_2011<- subset(trackData2011, inout=="out")

#May 26-29th (IN)

in2011<- "datos SST/SST_2011_05_26-29.nc"

may2011<-f1(in2011)

#|2011| July 23rd, 28th and 30th | OUT |

july_23_11<- "datos SST/23july2011.nc"
july_28_11<- "datos SST/28july2011.nc"
july_30_11<- "datos SST/30july2011.nc"

july_23_11<-f1(july_23_11)
july_28_11<-f1(july_28_11)
july_30_11<-f1(july_30_11)

out2011<- rbind(july_23_11, july_28_11, july_30_11)


##mapping SST + (tracks de atunes)

## IN
mainMap + geom_raster(data =may2011, aes(x = long, y = lat, fill = may2011$sst), interpolate = TRUE) + 
scale_fill_gradientn(limits = c(15,25), colours = rev(rainbow(150)), na.value = NA) +
  theme_bw() + geom_point(data = IN_2011, aes(x = Long, y = Lat), size=3 , color = IN_2011$id)+labs(title = "SST and BF tuna locations", 
    fill = "SST")+labs(subtitle = "Year 2011 | Days: 26-29 May | Direction: IN")


## OUT

mainMap + geom_raster(data =out2011, aes(x = long, y = lat, fill = out2011$sst), interpolate = TRUE) + 
  scale_fill_gradientn(limits = c(15,25), colours = rev(rainbow(150)), na.value = NA) +
  theme_bw() + 
  geom_point(data = OUT_2011, aes(x = Long, y = Lat), size=3 , color = OUT_2011$id)+ 
  labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = "Year 2011 | Days: 23, 28, 30 July| Direction: OUT")

coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97), expand = FALSE)


#


#|2012| ----

#tracking data
trackData2012<- subset(tracking_data, Year=="2012")
track_in_2012 <- subset(trackData2012, inout == "in")
track_out_2012 <- subset(trackData2012, inout == "out")

##SST data
#15-19 May (in)
may2012<- "datos SST/15-19may2012.nc"
in_2012<-f1(may2012)

#|2012| July: 10, 14-17 (out)

july10<- "datos SST/10july2012.nc"
july14<- "datos SST/14-17july2012.nc"

july10<- f1(july10)
july14<- f1(july14)

out_2012<- rbind( july10, july14)

###mapping

## IN
mainMap + geom_raster(data = in_2012, aes(x = long, y = lat, fill = in_2012$sst), interpolate = TRUE) + 
  scale_fill_gradientn(limits = c(15,25),colours = rev(rainbow(150)), na.value = NA) + theme_bw()+labs(fill = "SST") +
geom_point(data = track_in_2012, aes(x = Long, y = Lat), size=3, color = track_in_2012$id) + 
  labs(subtitle = "Year: 2012  |  Days:  15-19 May  |  direction: IN") + labs(title = "SST and BF tuna locations") 

# OUT

mainMap + geom_raster(data = out_2012, aes(x = long, y = lat, fill = sst), interpolate = TRUE) + 
  scale_fill_gradientn(limits = c(15,25), colours = rev(rainbow(150)), na.value = NA) + theme_bw()+labs(fill = "SST") +
  geom_point(data = track_out_2012, aes(x = Long, y = Lat), size=3, color = track_out_2012$id) + 
labs(subtitle = "Year: 2012  |  Days:  10, 14-17 July  |  direction: OUT") + labs(title = "SST and BF tuna locations") 


#|2013| ---- 

###IN
trackData2013<- subset(tracking_data, Year=="2013")
track_in_2013 <- subset(trackData2013, inout == "in")
head(track_in_2013)
arrange(track_in_2013,yday)

#May: 21-28th, 29th, 31st (in) 

may1 <- "datos SST/21-28may2013.nc"
may2 <- "datos SST/29may2013.nc"
may3 <- "datos SST/31may2013.nc"

may1<-f1(may1)
may2<-f1(may2)
may3<-f1(may3)


#June: 4th (in)

june1 <-"datos SST/4june2013.nc"
june1 <-f1(june1)

in_2013<- rbind(may1, may2, may3, june1)


#May: 21-28th, 29th, 31st (in) 



in13<-f1()
in13$ddmmyy<-as.Date(as.POSIXct(in11$date*60*60, origin="1950-01-01"))
in13$yday<-yday(in11$ddmmyy)


#### May:  29th (in) valores de sólo un día ###
trackmay29<- subset(trackData2013, trackData2013$yday=="149") #149 = 29/05

may29 <- "datos SST/29may2013.nc"

#
varmay29<- f1(may29)


#mapping SST (and tracks)
mainMap + geom_raster(data =varmay29, aes(x = long, y = lat, fill = varmay29$sst), interpolate = TRUE) + 
  scale_fill_gradientn(limits=c(10, 25), colours = rev(rainbow(150)), na.value = NA) +
  theme_bw() + geom_point(data = trackmay29, aes(x = Long, y = Lat), size=3 , color = "red")+labs(title = "SST and BF tuna locations", fill = "SST")+labs(subtitle = " 29/05/2013  |  Direction: IN") #con tracks






#####



#.mapping SST

mainMap + geom_raster(data = in_2013, aes(x = long, y = lat, fill = in_2013$sst), interpolate = TRUE) + 
  scale_fill_gradientn(limits = c(15,25),colours = rev(rainbow(150)), na.value = NA) + theme_bw()+labs(fill = "SST") +
  geom_point(data = track_in_2013, aes(x = Long, y = Lat), size=3, color = track_in_2013$id) + 
  labs(subtitle = "Year: 2013  |  Days: 21-28, 29, 31 May; 2 June  |  direction: IN") + labs(title = "SST and BF tuna locations") 
                 
#### OUT

#June: 4, 6, 8, 14, 16, 18, 20-21, 23-24 (out)

june2 <- "datos SST/6june2013.nc"
june3 <- "datos SST/8june2013.nc"
june4 <- "datos SST/14june2013.nc"
june5 <- "datos SST/16june2013.nc"
june6 <- "datos SST/18june2013.nc"
june7 <- "datos SST/20&21june2013.nc"
june8 <- "datos SST/23&24june2013.nc"

june2 <- f1(june2)
june3 <- f1(june3)
june4 <- f1(june4)
june5 <- f1(june5)
june6 <- f1(june6)
june7 <- f1(june7)
june8 <- f1(june8)


#July: 5, 7, 9, 13, 17, 19, 21 (out)

july1 <- "datos SST/5july2013.nc"
july2 <- "datos SST/7july2013.nc"
july3 <- "datos SST/9july2013.nc"
july4 <- "datos SST/13july2013.nc"
july5 <- "datos SST/17july2013.nc"
july6 <- "datos SST/19july2013.nc"
july7 <- "datos SST/21july2013.nc"


july1<- f1(july1)
july2<- f1(july2)
july3<- f1(july3)
july4<- f1(july4)
july5<- f1(july5)
july6<- f1(july6)
july7<- f1(july7)

out_2013 <- rbind(june2, june3, june4, june5, june6, june7, june8, july1, july2, july3, july4, july5, july6, july7)


#
track_out_2013 <- subset(trackData2013, inout == "out")




####.mapping SST

mainMap + geom_raster(data = out_2013, aes(x = long, y = lat, fill = out_2013$sst), interpolate = TRUE) + 
  scale_fill_gradientn(limits = c(15,25),colours = rev(rainbow(150)), na.value = NA) + theme_bw()+labs(fill = "SST") +
  geom_point(data = track_out_2013, aes(x = Long, y = Lat), size=3, color = track_out_2013$id) +
  labs(subtitle = "Year: 2013  | Several days June & July |  direction: OUT") + labs(title = "SST and BF tuna locations") 
