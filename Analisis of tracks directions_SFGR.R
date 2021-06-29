#### Analisis track directions ####

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
library(leaflet)
library(sp)
library(maptools)
library(lubridate)
library(ggrepel)

#tracking data

setwd("C:/Users/Usuario/Desktop/TFM/SST and Taggging/")

tracking_data <-read.csv("Tracking data.csv")

table(tracking_data$inout)

dates<-read.csv("dates_inout.csv") #dates of tagging

arrange(dates, dates$inout) #order data by preliminar direction (in/out of mediterranean)

dates<-arrange(dates, dates$df_dates) #order data by date
View(dates) #observe the dates of tagging 


trackData2011<- subset(tracking_data, Year=="2011")
trackData2012<- subset(tracking_data, Year=="2012")
trackData2013<- subset(tracking_data, Year=="2013")
arrange(trackData2013,trackData2013$id, trackData2013$yday)

table(trackData2011$inout)
table(trackData2012$inout)
table(trackData2013$inout)

directionMay <-subset(tracking_data, Month=="5")
directionMay <- directionMay[, c(2, 4, 5, 6, 25)]

table(directionMay$inout, directionMay$Year, directionMay$id )
##preparing map of study area ----

mapBuffer <-0.1
worldMap <- ne_download(scale = "medium", returnclass = "sp")

worldMapCroped <- crop(worldMap,extent(-9,-5,35,37.5)) #coordinates of study area

mainMap <- ggplot() + geom_polygon(data = worldMapCroped, aes(x = long, y = lat, group = group)) + coord_fixed() + theme(axis.ticks=element_blank()) + ylab("Latitude") + xlab("Longitude")#map of study area


##### 2013 ####

trackjune<-subset(trackData2013, Month=="6")
trackjune_out<-subset(trackjune, inout=="out")
trackjune_out$id

#130543 ---



id130543<-subset(trackData2013, id == "130543")

tracks130543 <- mainMap + geom_point(data = id130543, aes(x = Long, y = Lat), size=1.5) +  geom_label_repel(data = id130543, aes(x = Long, y = Lat, label = id130543$yday, colour = "black"),  alpha = 0.7,  show.legend=F,  force = 2,
box.padding   = 0.8,  point.padding = 0.3, label.size =0.2, segment.color = 'grey50', label.r = 0.2, direction= "x")  +  theme(legend.position="none") +labs(title = "130543 locations")

#130545 ---

trackData2013<- subset(tracking_data, Year=="2013")
id130545<-subset(trackData2013, id == "130545")

tracks130545 <- mainMap + geom_point(data = id130545, aes(x = Long, y = Lat), size=1.5) +  geom_label_repel(data = id130545, aes(x = Long, y = Lat, label = id130545$yday, colour = "black"),  alpha = 0.7,  show.legend=F,  force = 2,
                                                                                                            
                                                                                                            box.padding   = 0.8, 
                                                                                                            point.padding = 0.3,
                                                                                                            label.size =0.2,
                                                                                                            segment.color = 'grey50',
                                                                                                            label.r = 0.5,
                                                                                                            direction= "x")  + 
  theme(legend.position="none") +labs(title = "130545 locations")


#130548 ---


id130548<-subset(trackData2013, id == "130548")

mainMap2 + geom_point(data = id130548, aes(x = Long, y = Lat), size=1.5) +  geom_label_repel(data = id130548, aes(x = Long, y = Lat, label = id130548$yday, colour = "black"),  
                                                                                            
                                                                                            box.padding   = 0.8, 
                                                                                            point.padding = 0.3,
                                                                                            label.size =0.2,
                                                                                            segment.color = 'grey50',
                                                                                            label.r = 0.5,
                                                                                            direction= "x")  + 
  theme(legend.position="none") +labs(title = "1130548 locations")



#IN
track_in_2013 <- subset(trackData2013, inout == "in")
id130548<-subset(track_in_2013, id == "130548")

mainMap + geom_point(data = id130548, aes(x = Long, y = Lat), size=1.5) +  geom_label_repel(data = id130548, aes(x = Long, y = Lat, label = id130548$yday, colour = "black"),  alpha = 0.7,  show.legend=F,  force = 2,
                                                                                                            
                                                                                                            box.padding   = 0.8, 
                                                                                                            point.padding = 0.3,
                                                                                                            label.size =0.2,
                                                                                                            segment.color = 'grey50',
                                                                                                            label.r = 0.5,
                                                                                                            direction= "x")  + 
  theme(legend.position="none") +labs(title = "1130548 locations")


# OUT

track_out_2013 <- subset(trackData2013, inout == "out")
id130548<-subset(track_out_2013, id == "130548")

mainMap + geom_point(data = id130548, aes(x = Long, y = Lat), size=1.5) +  geom_label_repel(data = id130548, aes(x = Long, y = Lat, label = id130548$yday, colour = "black"),  alpha = 0.7,  show.legend=F,  force = 2,
                                                                                                            
                                                                                                            box.padding   = 0.8, 
                                                                                                            point.padding = 0.3,
                                                                                                            label.size =0.2,
                                                                                                            segment.color = 'grey50',
                                                                                                            label.r = 0.5,
                                                                                                            direction= "x")  + 
  theme(legend.position="none") +labs(title = "1130548 locations")




#130550 ---

id130550<-subset(track_out_2013, id == "130550")

mainMap + geom_point(data = id130550, aes(x = Long, y = Lat), size=1.5) +  geom_label_repel(data = id130550, aes(x = Long, y = Lat, label = id130550$yday, colour = "black"),  alpha = 0.7,  show.legend=F,  force = 2, box.padding   = 0.8, point.padding = 0.3, label.size =0.2, segment.color = 'grey50', label.r = 0.5, direction= "x")  + 
  theme(legend.position="none") +labs(title = "1130550 locations")



#### 2012 ----

trackData2012<- subset(tracking_data, Year=="2012")

#118756
id118756<-subset(trackData2013, id == "118756")

mainMap + geom_point(data = id118756, aes(x = Long, y = Lat), size=1.5) +  geom_label_repel(data = id118756, aes(x = Long, y = Lat, label = id118756$yday, colour = "black"),  alpha = 0.7,  show.legend=F,  force = 2,
                                                                                                            
                                                                                                            box.padding   = 0.8, 
                                                                                                            point.padding = 0.3,
                                                                                                            label.size =0.2,
                                                                                                            segment.color = 'grey50',
                                                                                                            label.r = 0.5,
                                                                                                            direction= "x")  + 
  theme(legend.position="none") +labs(title = "118756 locations")




#### 2011 ----

trackData2011<- subset(tracking_data, Year=="2011")
track_out_2011 <- subset(trackData2011, inout == "out")

#97462

id97462<-subset(track_out_2011, id == "97462")

mainMap + geom_point(data = id97462, aes(x = Long, y = Lat), size=1.5) +  geom_label_repel(data = id97462, aes(x = Long, y = Lat, label = id97462$yday, colour = "black"),  alpha = 0.7,  show.legend=F,  force = 2, box.padding   = 0.8, point.padding = 0.3, label.size =0.2, segment.color = 'grey50', label.r = 0.5, direction= "x")  + 
  theme(legend.position="none") +labs(title = "97462 locations")

#97466

id97466<-subset(trackData2011, id == "97466")

mainMap + geom_point(data = id97466, aes(x = Long, y = Lat), size=1.5) +  geom_label_repel(data = id97466, aes(x = Long, y = Lat, label = id97466$yday, colour = "black"),  alpha = 0.7,  show.legend=F,  force = 2, box.padding   = 0.8, point.padding = 0.3, label.size =0.2, segment.color = 'grey50', label.r = 0.5, direction= "x")  + 
  theme(legend.position="none") +labs(title = "97466 locations")






# mapping full tracks of Individuals with only 1 or few occurrences in the study area ----

setwd("C:/Users/Usuario/Desktop/TFM/SST and Taggging/")

t<-read.csv("All Tracking data.csv") #all tracking data
rm(t3)
t3<- read.csv("Tracking data.csv") #only for study area
table(t3$id)
ids <- data.frame(table(t3$id))

subset(t, c(id=="118758" | id=="118758B")) # same in different years

#creating the map
range(t$Long)
range(t$Lat)

mapBuffer <-0.1
worldMap <- ne_download(scale = "medium", returnclass = "sp")

worldMapCroped <- crop(worldMap,extent(min(t$Lon)-mapBuffer,max(t$Long)+mapBuffer,min(t$Lat)-mapBuffer,max(t$Lat)+mapBuffer)) 

mainMap2 <- ggplot() + geom_polygon(data = worldMapCroped, aes(x = long, y = lat, group = group)) + coord_fixed() + theme(axis.ticks=element_blank()) + ylab("Latitude") + xlab("Longitude")#mapa general

trackjune
#### mapping individual tracks

id130545<-subset(t, id == "130545")

mainMap + geom_point(data = id130545, aes(x = Long, y = Lat), size=1.5) +  geom_label_repel(data = id130545, aes(x = Long, y = Lat, label = id130545$yday, colour = "black"),  alpha = 0.7,  show.legend=F,  force = 1, box.padding   = 0.8, point.padding = 0.3, label.size =0.2, segment.color = 'grey50', label.r = 0.5, direction= "x", max.overlaps = 25)  + 
  theme(legend.position="none") +labs(title = "id130545 locations")

#
id114008<-subset(t, id == "114008")

mainMap + geom_point(data = id114008, aes(x = Long, y = Lat), size=1.5) +  geom_label_repel(data = id114008, aes(x = Long, y = Lat, label = id114008$yday, colour = "black"),  alpha = 0.7,  show.legend=F,  force = 1, box.padding   = 0.8, point.padding = 0.3, label.size =0.2, segment.color = 'grey50', label.r = 0.5, direction= "x", max.overlaps = 25)  + 
  theme(legend.position="none") +labs(title = "id114008 locations")


#

id114009<-subset(t, id == "114009")
id114009 <- subset(id114009, id114009$Month=="5")
id114009 <- id114009[2:14, ]

mainMap + geom_point(data = id114009, aes(x = Long, y = Lat), size=1.5) +  geom_label_repel(data = id114009, aes(x = Long, y = Lat, label = id114009$yday, colour = "black"),  alpha = 0.7,  show.legend=F,  force = 2, box.padding   = 0.8, point.padding = 0.3, label.size =0.2, nudge_y = 3,segment.color = 'grey50', label.r = 0.5, direction= "x", max.overlaps = 25)  + 
  theme(legend.position="none") +labs(title = "id114009 locations")

#???114066

id114066<-subset(t, id == "114066")

mainMap + geom_point(data = id114066, aes(x = Long, y = Lat), size=1.5) +  geom_label_repel(data = id114066, aes(x = Long, y = Lat, label = id114066$yday, colour = "black"),  alpha = 0.7,  show.legend=F,  force = 1, box.padding   = 0.8, point.padding = 0.3, label.size =0.2, segment.color = 'grey50', label.r = 0.5, direction= "x", max.overlaps = 25)  + 
  theme(legend.position="none") +labs(title = "id114066 locations")

#
id118756 <- subset(t, id == "118756") 

mainMap + geom_point(data = id118756, aes(x = Long, y = Lat), size=1.5) +  geom_label_repel(data = id118756, aes(x = Long, y = Lat, label = id118756$yday, colour = "black"),  alpha = 0.7,  show.legend=F,  force = 1, box.padding   = 0.8, point.padding = 0.3, label.size =0.2, segment.color = 'grey50', label.r = 0.5, nudge_y = 2,direction= "x", max.overlaps = 25)  + theme(legend.position="none") +labs(title = "id118756 locations")

#118758
id118758 <- subset(t, id == "118758") 

range(id118758$yday)

id118758 <- id118758[2:24, ] 


mainMap + geom_point(data = id118758, aes(x = Long, y = Lat), size=1.5) +  geom_label_repel(data = id118758, aes(x = Long, y = Lat, label = id118758$yday, colour = "black"),  alpha = 0.7,  show.legend=F,  force = 1, box.padding   = 0.8, point.padding = 0.3, label.size =0.2, segment.color = 'red', label.r = 0.5, nudge_y = 2, max.overlaps = 50)  + 
  theme(legend.position="none") +labs(title = "id118758 locations")

# 118758B
id118758B <- subset(t, id == "118758B")
id118758B <- subset(id118758B,yday>152)

mainMap + geom_point(data = id118758B, aes(x = Long, y = Lat), size=1.5) +  geom_label_repel(data = id118758B, aes(x = Long, y = Lat, label = yday, colour = "black"),  alpha = 0.7,  show.legend=F,  force = 1, box.padding   = 0.8, point.padding = 0.3, label.size =0.2, nudge_y = 3, segment.color = 'grey50', label.r = 0.5,  max.overlaps = 25)  + 
  theme(legend.position="none") +labs(title = "id118758B locations")



t3[t3$id=="id118758B", ]


# 118760
id118760 <- subset(t, id == "118760")
id118760  <- subset(id118760, yday > 190)

mainMap + geom_point(data = id118760, aes(x = Long, y = Lat), size=1.5) +  geom_label_repel(data = id118760, aes(x = Long, y = Lat, label = yday, colour = "black"),  alpha = 0.7,  show.legend=F,  force = 1, box.padding   = 0.8, point.padding = 0.3, label.size =0.2, nudge_y =3, segment.color = 'red', label.r = 0.5, direction= "x", max.overlaps = 25)  + 
  theme(legend.position="none") +labs(title = "id118760 locations")



#118817
id118817 <- subset(t, id == "118817")

#120081
id120081 <- subset(t, id == "120081")

mainMap + geom_point(data = id120081, aes(x = Long, y = Lat), size=1.5) +  geom_label_repel(data = id120081, aes(x = Long, y = Lat, label = id120081$yday, colour = "black"),  alpha = 0.7,  show.legend=F,  force = 1, box.padding   = 0.8, point.padding = 0.3, label.size =0.2, nudge_y = 2, segment.color = 'red', label.r = 0.5, direction= "x", max.overlaps = 25)  + 
  theme(legend.position="none") +labs(title = "id120081 locations")

#120086

id120086 <- subset(t, id == "120086")

mainMap + geom_point(data = id120086, aes(x = Long, y = Lat), size=1.5) +  geom_label_repel(data = id120086, aes(x = Long, y = Lat, label = id120086$yday, colour = "black"),  alpha = 0.7,  show.legend=F,  force = 1, box.padding   = 0.8, point.padding = 0.3, label.size =0.2, nudge_y = 2, segment.color = 'red', label.r = 0.5, direction= "x", max.overlaps = 25)  + 
  theme(legend.position="none") +labs(title = "id120086 locations")

#120446

id120446 <- subset(t, id == "120446")
id120446<- subset(id120446, yday>190)

mainMap + geom_point(data = id120446, aes(x = Long, y = Lat), size=1.5) +  geom_label_repel(data = id120446, aes(x = Long, y = Lat, label = id120446$yday, colour = "black"),  alpha = 0.7,  show.legend=F,  force = 1, box.padding   = 0.8, point.padding = 0.3, label.size =0.2, nudge_y = 2, segment.color = 'red', label.r = 0.5, direction= "x", max.overlaps = 25)  + 
  theme(legend.position="none") +labs(title = "id120446 locations")

#130539

id130539 <- subset(t, id == "130539")
id130539 <- subset(id130539, yday <167)

mainMap + geom_point(data = id130539, aes(x = Long, y = Lat), size=1.5) +  geom_label_repel(data = id130539, aes(x = Long, y = Lat, label = id130539$yday, colour = "black"),  alpha = 0.7,  show.legend=F, segment.color = 'red', label.r = 0.5)  + 
  theme(legend.position="none") +labs(title = "id130539 locations")

ids

#130546

id130546 <- subset(t, id == "130546")

mainMap + geom_point(data = id130546, aes(x = Long, y = Lat), size=1.5) +  geom_label_repel(data = id130546, aes(x = Long, y = Lat, label = id130546$yday, colour = "black"),  alpha = 0.7,  show.legend=F,  box.padding   = 0.8, point.padding = 0.3, label.size =0.2,  segment.color = 'red', label.r = 0.5, max.overlaps = 25)  + 
  theme(legend.position="none") +labs(title = "id130546 locations")

#130552

id130552 <- subset(t, id == "130552")

mainMap + geom_point(data = id130552, aes(x = Long, y = Lat), size=1.5) +  geom_label_repel(data = id130552, aes(x = Long, y = Lat, label = id130552$yday, colour = "black"),  alpha = 0.7,  show.legend=F,  force = 1, box.padding   = 0.8, point.padding = 0.3, label.size =0.2, nudge_y = 2, segment.color = 'red', label.r = 0.5, direction= "x", max.overlaps = 25)  + 
  theme(legend.position="none") +labs(title = "id130552 locations")

#66694

id66694 <- subset(t, id == "66694")

mainMap + geom_point(data = id66694, aes(x = Long, y = Lat), size=1.5) +  geom_label_repel(data = id66694, aes(x = Long, y = Lat, label = yday, colour = "black"),  alpha = 0.7,  show.legend=F,   box.padding   = 0.8, point.padding = 0.3, label.size =0.2,  segment.color = 'red', label.r = 0.5,  max.overlaps = 25)  + 
  theme(legend.position="none") +labs(title = "id66694 locations")

#86235C

id86235C <- subset(t, id == "86235C")

mainMap + geom_point(data = id86235C, aes(x = Long, y = Lat), size=1.5) +  geom_label_repel(data = id86235C, aes(x = Long, y = Lat, label = yday, colour = "black"),  alpha = 0.7,  show.legend=F,  force = 1) + 
  theme(legend.position="none") +labs(title = "id86235C locations")

#86238 

id86238  <- subset(t, id == "86238")
id86238 <- subset(id86238, Year == "2012")
id86238 <- subset(id86238, yday>150)  # or id86238 <- subset(id86238, yday<150)

mainMap + geom_point(data = id86238, aes(x = Long, y = Lat), size=1.5) +  geom_label_repel(data = id86238, aes(x = Long, y = Lat, label = yday, colour = "black"),  alpha = 0.7,  show.legend=F, box.padding   = 0.8, point.padding = 0.3, label.size =0.2, nudge_y = 2, segment.color = 'red', label.r = 0.5, max.overlaps = 25)  + 
  theme(legend.position="none") +labs(title = "id86238 locations")

#86243

id86243  <- subset(t, id == "86243")
id86243 <- subset(id86243, yday<170)

mainMap + geom_point(data = id86243, aes(x = Long, y = Lat), size=1.5) +  geom_label_repel(data = id86243, aes(x = Long, y = Lat, label = yday, colour = "black"),  alpha = 0.7,  show.legend=F)  + theme(legend.position="none") + labs(title = "id86243 locations")


# 97466

id97466  <- subset(t, id == "97466")

mainMap + geom_point(data = id97466, aes(x = Long, y = Lat), size=1.5) +  geom_label_repel(data = id97466, aes(x = Long, y = Lat, label = yday, colour = "black"),  alpha = 0.7,  show.legend=F,   box.padding   = 0.8, point.padding = 0.3, label.size =0.2,  segment.color = 'red', label.r = 0.5,  max.overlaps = 25)  + 
  theme(legend.position="none") +labs(title = "id97466 locations")


