##### Depth data analysis ----



##### 1. Mean daily depths values --------
setwd()

tracking_data <-read.csv("Tracking data in out west.csv")


### Colours

c1 <- rgb(216,238,192, max = 255, alpha = 150, names = "lt.green") #light colours for histogram
c2 <- rgb(255,100,100, max = 255, alpha = 150, names = "lt.red")
c3 <- rgb(0, 100, 255, max=255, alpha = 150, names = "lt.blue")
c4 <- rgb(100, 0, 100, max=255, alpha = 150, names = "lt.purple")


#### Analitic plots ----

par(mar=c(2, 4, 1.5, 1))
par(mfrow=c(3,1))

plot(tracking_data$Max.Dive~tracking_data$Long,  ylim=c(500,0), xlim=c(-9, -5), pch = 19, col=factor(tracking_data$id), main = "Bluefin Tuna depths", ylab = "Max dive", xlab= "Long")
rug(c(-5.95, -5.3), col = "red")

plot(track_in$Max.Dive~track_in$Long, ylim=c(500,0), xlim=c(-9, -5), pch = 19, col=factor(track_in$id), main = "Bluefin Tuna depths | Direction 'IN'", ylab = "Max dive", xlab= "Long")
rug(c(-5.95, -5.3), col = "red")

par(mar=c(4, 4, 1.5, 1))

plot(track_out$Max.Dive~track_out$Long,  ylim=c(500,0), xlim=c(-9, -5), pch = 19, col=factor(track_out$id), main = "Bluefin Tuna depths | Direction 'OUT'", ylab = "Max dive", xlab= "Long")
rug(c(-5.95, -5.3), col = "red")


boxplot(tracking_data$Max.Dive~tracking_data$inout, ylab = "depth", xlab = "Direction", main = "Bluefin Tuna depths")
stripchart(tracking_data$Max.Dive~tracking_data$inout, vertical = TRUE, method = "jitter",
           pch = 19, add = TRUE, col = c("blue","red"))

par(mfrow=c(1,1))




library(ggplot2)

ggplot(tracking_data) + 
  geom_point(mapping = aes(x = Long, y = Max.Dive, color = id))

ggplot(tracking_data) + 
  geom_point(mapping = aes(x = inout, y = Max.Dive, color = id))

ggplot(tracking_data) + 
  geom_point(mapping = aes(x = inout, y = Max.Dive, color = id))

ggplot(data = tracking_data) + 
  stat_summary(
    mapping = aes(x = id, y = Max.Dive, col= inout),
    fun.min = min,
    fun.max = max,
    fun = mean) + geom_smooth(data = tracking_data, aes(x = id, y = Max.Dive) ) + labs(title = "Bluefin Tuna depths", y = "Depth (m)")


ggplot(data = tracking_data) + geom_smooth(data = tracking_data, aes(x = Long, y = Max.Dive) )


ggplot(data = tracking_data) + 
  stat_summary(
    mapping = aes(x = inout, y = Max.Dive),
    fun.min = min,
    fun.max = max,
    fun = mean
  ) + geom_smooth(data = tracking_data, aes(x = inout, y = Max.Dive) )



hist(track_in$Max.Dive, col = c1, breaks = 15, main ="Max Dives", xlab="Depth", xlim = c(0, 600))
hist(track_out$Max.Dive, col = c2, breaks = 20, add=TRUE)
legend(0, 4, c("IN", "OUT"), fill = c(c1,c2))

range(tracking_data$Max.Dive)



#### 2. Migratun files (GPE3 data) ----

setwd("C:/Users/Usuario/Desktop/marcas migratun")


#exploring files with diving data---------- 
head(read.csv("61125/61125-Series.csv")) #depth data for each 10 minutes
head(read.csv("61125/61125-MinMaxDepth.csv"))
head(read.csv("61125/61125-1-GPE3.csv", skip = 5))
head(read.csv("61127/61127-Series.csv", skip = 2))
head(read.csv("72498/72498-Series.csv", skip = 2))
head(read.csv("72499/72499-Series.csv"))
head(read.csv("86437/86437-MinMaxDepth.csv"))
head(read.csv("86440/86440-MinMaxDepth.csv"))

#
GPEdata<-read.csv2("GPEsst study area.csv2") #data of tracking corresponding to the study area
GPEdata$Date #dates of occurence in the study area
table(GPEdata$DeployID, GPEdata$ddmmyy ) #dates and resepctive individuals 

#imorting files of interest
depth1<-read.csv("61125/61125-Series.csv")
depth1<-depth1[,1:14]
depth2<-read.csv("61127/61127-Series.csv", skip = 2)
depth3<-read.csv("72498/72498-Series.csv", skip = 2)
depth4<-read.csv("72499/72499-Series.csv")

depthseries<- rbind(depth1, depth2, depth3, depth4)


##editing dates format to make it compatible with the GPE

t1=depthseries
names(t1)
str(t1)
#t1$fecha=as.Date(depth series$Day, format =  "%d-%b-%Y")
library(lubridate)
t1$fecha=as.Date(t1$Day, format=  "%d-%m-%Y")

t1$Day<- gsub("-May-","-05-",t1$Day)
t1$Day<- gsub("-Jun-","-06-",t1$Day)
t1$Day<- gsub("-Jul-","-07-",t1$Day)
t1$Day<- gsub("-Aug-","-08-",t1$Day)
t1$Day<- gsub("-Sep-","-09-",t1$Day)
t1$Day<- gsub("-Oct-","-10-",t1$Day)
t1$Day<- gsub("-Nov-","-11-",t1$Day)

t1$fecha<-as.Date(t1$Day, format=  "%d-%m-%Y")


table(GPEdata$DeployID, GPEdata$ddmmyy) #dates and respective individuals of interest

#filtering dates when individuals occurred in the study area

d1<- subset(t1, DeployID=="61125")
id61125<-subset(d1, Day=="22-07-2010")

d2<- subset(t1, DeployID=="61127")
id61127<-subset(d2, Day=="16-07-2010" | Day== "18-07-2010")

d3<- subset(t1, DeployID=="72498")
id72498<- subset(d3, Day=="03-07-2011"| Day=="04-07-2011" |Day=="05-07-2011")

track_depths <- rbind(id61125, id61127, id72498)



write.csv(track_depths, "Depths series.csv")


#------- depths above thermnocline (=40m depth) ------
track_depths <- read.csv("Depths series.csv")

str(subset(track_depths, Depth < 40))
str(track_depths)

surfacedepths <- subset(track_depths, Depth <40)
range(surfacedepths$Depth)

plot(surfacedepths$Depth, col=surfacedepths$DeployID, ylim = c(40, 0) )
legend("topright", c("id61125", "id61127", "id72498"), fill=surfacedepths$DeployID)

par(mar=c(4, 4, 2, 1))
plot(id61125$Depth, col= c2, pch = 19, ylim = c(40,0), xlim = c(0, 250), ylab = "Depth (m)", xaxt="n", xlab = "Time")
points(id61127$Depth, col= c3, pch = 19, ylim = c(40,0), add=TRUE)
points(id72498$Depth, col= c4, pch = 19, ylim = c(40,0), add=TRUE)
legend( 200, 0, c("id61125", "id61127", "id72498"), fill= c(c2,c3,c4))


surface61125<-subset(id61125, Depth < 40) 
surface61127 <-subset(id61127, Depth < 40)
surface72498 <- subset(id72498, Depth < 40)


##producing analitic plots------

### Colours

c1 <- rgb(216,238,192, max = 255, alpha = 150, names = "lt.green") #light colours for histogram
c2 <- rgb(255,100,100, max = 255, alpha = 150, names = "lt.red")
c3 <- rgb(0, 100, 255, max= 255, alpha = 150, names = "lt.blue")
c4 <- rgb(100, 0, 100, max= 255, alpha = 150, names = "lt.purple")


par(mar=c(4, 4, 2, 1))
hist(track_depths$Depth, breaks = 50, col = c2, xlab = "Depth (m)", main= "Blueifn Tuna Depths")
axis(side=1, at=seq(0,1000, 100), labels=seq(0,1000,100))

?axis

par(mar=c(4, 4, 2, 1))
boxplot(track_depths$Depth~track_depths$DeployID, ylim=c(815, 0), ylab = "depth", xlab = "ID", main = "Bluefin Tuna depths")
stripchart(track_depths$Depth~track_depths$DeployID, vertical = TRUE, method = "jitter", pch = 19, add = TRUE, col = c(c2,c4,c3))
axis(side=2, at=seq(0,1000, 100), labels=seq(0,1000,100))
abline(40,0, lwd = 3, col = "red")
?lines
par(mar=c(1, 4, 2, 1))
boxplot(track_depths$Depth, ylim=c(815, 0), ylab = "Depth", main = "Bluefin Tuna depths")
stripchart(id61127$Depth, vertical = TRUE, method = "jitter", pch = 19, add = TRUE, col = c4)
stripchart(id72498$Depth, vertical = TRUE, method = "jitter", pch = 19, add = TRUE, col = c3)
stripchart(id61125$Depth, vertical = TRUE, method = "jitter", pch = 19, add = TRUE, col = c2)

abline(40,0, lwd = 3, col = "red")
axis(side=2, at=seq(0,1000, 100), labels=seq(0,1000,100))
legend("topright", c("id61125", "id61127", "id72498"), fill= c(c2,c4,c3))

nrow(surface61125)/nrow(id61125) # 94%
nrow(surface61127)/nrow(id61127) # 20%
nrow(surface72498)/nrow(id72498) # 59%

nrow(surfacedepths)/nrow(track_depths) #55%

?legend

abline(h=11.5)
abline(h=31.5)
abline(h=127.5)
abline(h=176.5)



#statistical summaries and analysis-----

summary(id61127$Depth)
summary(id61125$Depth)
summary(id72498$Depth)

summary(track_depths$Depth)

ks.test(id61127$Depth,id72498$Depth) #D = 0.4375, p-value = 2.22e-16
ks.test(id61127$Depth,id61125$Depth) #D = 0.74826, p-value < 2.2e-16
ks.test(id61125$Depth,id72498$Depth) #D = 0.45486, p-value = 3.22e-15


library(RColorBrewer)
library(histogram)


quantile(track_depths$Depth)


?histogram
histogram(quantile(track_depths$Depth), type = "irregular", grid = "quantiles", breaks=20, plot = TRUE, col= factor(quantile(track_depths$Depth)))

axis(side=1, at=seq(0,100, 800))

q1<- rep("NA", length(track_depths$Depth))

funQ <-function(a){ 
  if (a <= 11.5){
    
    y= "Q1"
    return("Q1")
  }
  
  else if (a > 11.5 & a <= 31.5 ){
    
    y= "Q2"
    return("Q2")
  }
  
  else if (a > 31.55 & a <= 176.5 ){
    
    y= "Q3"
    return("Q3")
  }
  
  else if (a > 176.5){
    
    y= "Q4"
    return("Q4")
  }
  
} #function to create groups by quantile

for (i in 1:length(q1)) {
 
  q1[i]<- funQ(track_depths$Depth[i])
}


track_depths$Quantile <- q1

q1<- subset(track_depths, Quantile=="Q1")
q2<- subset(track_depths, Quantile=="Q2")
q3<- subset(track_depths, Quantile=="Q3")
q4<- subset(track_depths, Quantile=="Q4")

#histogram based on quantiles
hist(q1$Depth, breaks = 5, col ="red", xlab = "Depth (m)", main= "Blueifn Tuna Depths", xlim = c(0,180), ylim = c(0,50))
hist(q2$Depth, breaks = 10, col =c1, add=T)
hist(q3$Depth, breaks = 100, col =c3, add=T)
hist(q4$Depth, breaks = 50, col =c4, add=T)
legend(150,40, c("1s interval", "2nd interval ", "3rd interval"), fill= c("red", c1, c3))


#legend(500,120, c("1s interval (<11.5m)", "2nd interval (11.5- 31.5m)", "3rd interval (31.5-176.5m)", "Last interval (176.5-830m"), fill= c("red", c1, c3, c4))

?rug()
axis(side=1, at=seq(0,180, 10), labels=seq(0,180,10),lwd=0.5)
axis()




#Maps with tracks from individuals----

#library(stringr)

library(ncdf4)
library(reshape2)
library(ggplot2)
library(mapdata)
library(maptools)
library(rnaturalearth)
library(gdata)
library(raster)
library(scatterpie)
library(maptools)
library(ggrepel)


mapBuffer <-0.1
worldMap <- ne_download(scale = "medium", returnclass = "sp")

worldMapCroped <- crop(worldMap,extent(-9,-5,35,37.5)) #coordenadas del área de estudio

mainMap <- ggplot() + geom_polygon(data = worldMapCroped, aes(x = long, y = lat, group = group)) + coord_fixed() + theme(axis.ticks=element_blank()) + ylab("Latitude") + xlab("Longitude")#mapa del area de estudio


summary(id$Depth)
summary(id61125$Depth)
summary(id72498$Depth)


#61127 ---

track61127<-subset(GPEdata, DeployID == "61127")
track61127$yday <- yday(track61127$ddmmyy)

mainMap + geom_point(data = track61127, aes(x = Most.Likely.Longitude, y = Most.Likely.Latitude), size=1.5) +  geom_label_repel(data = track61127, aes(x = Most.Likely.Longitude, y = Most.Likely.Latitude, label = track61127$yday, colour = "black"),  alpha = 0.7,  show.legend=F,  force = 2, box.padding   = 0.8,  point.padding = 0.3, label.size =0.2, segment.color = 'grey50', label.r = 0.2, direction= "x")  +  theme(legend.position="none") +labs(title = "61127 locations")


#61125 ---

track61125<-subset(GPEdata, DeployID == "61125")
track61125$yday <- yday(track61125$ddmmyy)

mainMap + geom_point(data = track61125, aes(x = Most.Likely.Longitude, y = Most.Likely.Latitude), size=1.5) +  geom_label_repel(data = track61125, aes(x = Most.Likely.Longitude, y = Most.Likely.Latitude, label = track61125$yday, colour = "black"),  alpha = 0.7,  show.legend=F,  force = 2, box.padding   = 0.8,  point.padding = 0.3, label.size =0.2, segment.color = 'grey50', label.r = 0.2, direction= "x")  +  theme(legend.position="none") +labs(title = "61127 locations")

#72498 ---

track72498<-subset(GPEdata, DeployID == "72498")
track72498$yday <- yday(track72498$ddmmyy)

mainMap + geom_point(data = track72498, aes(x = Most.Likely.Longitude, y = Most.Likely.Latitude), size=1.5) +  geom_label_repel(data = track72498, aes(x = Most.Likely.Longitude, y = Most.Likely.Latitude, label = track72498$yday, colour = "black"),  alpha = 0.7,  show.legend=F,  force = 2, box.padding   = 0.8,  point.padding = 0.3, label.size =0.2, segment.color = 'grey50', label.r = 0.2, direction= "x")  +  theme(legend.position="none") +labs(title = "61127 locations")
