

#Bluefin tuna traking data (2011-2013)
#Study area coordinates: lat=> -5/-9º  | Long => 35/37.5º
#Period: May to July of 2011, 2012 and 2013

setwd("C:/Users/Usuario/Desktop/TFM/SST and Taggging/")

rm(list = ls())

library(leaflet)
library(sp)
library(maptools)
library(lubridate)

load("tagging/dataset_tuna_for_R.RData")

as.Date(data$df_dates, 'GMT')
head(data)
str(data)
summary(data)
data$yday=yday(data$df_dates) #cambiar formato de la fecha 



t2=na.omit(data, cols=seq_along("Lon","Lat")) #eliminar filas sin datos de coordenadas
table(t2$id)


plot(t2$Lon,t2$Lat)

?rm
rm(t3)
t3=subset(t2,Long<(-5.5) & Long>(-9) & Lat>35 & Lat<37.5) #seleccionar tracks dentro del área de estudio

hist(t3$Long, xlim=c(-9, -5), breaks= 84)
hist(t3$Lat, breaks= 84)

head(t3)
nrow(t3)
hist(t3$SST, breaks = 100, xlim = c(16,24))

#filter 
tin=subset(t3,yday<153);nrow(tin) #calssification based on date, need to be modified
plot(density(tin$SST))

tout=subset(t3,yday>153);nrow(tin)
     plot(density(tout$SST))


t3$inout="in";
t3[t3$yday>153,]$inout="out"
str(t3)

#t3<-subset(t3, !t3$id == "130543") #this individual  shows a circular movement, not clear direction in or out


#Fixing errors in directions of individuals based date -----


#2011: 
t2011 <- subset(t3, Year== "2011")
t2011$id

t2011[c(7, 8, 9),24] <- "out" #???97466 change in -> out
t2011[c(1, 2),24] <- "out" #86243 change in -> out
t2011[t2013$id=="86243",]$inout<- "out"

#2012

t2012 <- subset(t3, Year== "2012")
t2012$id

t2012[c(15:17), 24] <- "out"# 114008 change in -> out
t2012[c(15:17),]

t2012[c(20,22:24),24] <- "out" # 118756 change in -> out
t2012[c(20,22:24),]



#2013
t2013 <- subset(t3, Year== "2013")

t2013[t2013$id=="130543",]$inout<- "nd"
t2013[t2013$id=="130545",]$inout<- "out"
t2013[t2013$id=="130539",]$inout<- "out"
t2013[t2013$id=="130546",]$inout<- "out"

t3<- rbind(t2011, t2012, t2013)

table(t3$id)


t4<-t3[!t3$id=="114007", ] #
t4<-t4[!t4$id=="114066", ] #
t4<-t4[!t4$id=="114067", ] #
t4<-t4[!t4$id=="114068", ] #
t4<-t4[!t4$id=="118755", ] #
t4<-t4[!t4$id=="118817", ] #


table(t4$id)
table(t4$inout)

### Distributions of SST from tracking data #### ----


library(ggplot2)
tinout<-subset(t4, !t4$inout == "nd")

ggplot(tinout,aes(x=Obs.SST, fill=inout)) + geom_density(alpha=0.25)+xlim(10,30) + labs(title = "tracking SST", fill = NULL)

#2011

ggplot(t2011,aes(x=Obs.SST, fill=inout)) + geom_density(alpha=0.25)+xlim(10,30)+labs(title = "tracking SST 2011", fill = NULL)


library(MASS)
table(t2011$Year,t2011$inout)
tapply(t2011$yday,t2011$inout, max)
tapply(t2011$yday,t2011$inout, min)

t2011$df_dates[t2011$inout=="in"]
t2011$df_dates[t2011$inout=="out"]

#2012

t2012 <- subset(t4, Year== "2012")
ggplot(t2012,aes(x=Obs.SST, fill=inout)) + geom_density(alpha=0.25)+xlim(10,30)+labs(title = "tracking SST 2012", fill = NULL)

table(t2012$Year,t2012$inout)
tapply(t2012$yday,t2012$inout, max)
tapply(t2012$yday,t2012$inout, min)


t2012$df_dates[t2012$inout=="in"]
t2012$df_dates[t2012$inout=="out"]

#2013

t2013 <- subset(tinout, Year== "2013")
ggplot(t2013,aes(x=SST, fill=inout)) + geom_density(alpha=0.25)+xlim(10,30)+labs(title = "tracking SST 2013", fill = NULL) #el segundo pico de out se corresponde con datos de julio...

table(t2013$Year,t2013$inout)
tapply(t2013$yday,t2013$inout, max)
tapply(t2013$yday,t2013$inout, min)

t2013$df_dates[t2013$inout=="in"]
t2013$df_dates[t2013$inout=="out"]

#summarize data
library(MASS)
table(t4$Year,t4$inout)
tapply(t4$yday,t4$inout, max)
tapply(t4$yday,t4$inout, min)

t4$df_dates[t4$inout=="in"]
t4$df_dates[t4$inout=="out"]


head(t4)

?subset



write.csv(t4[,c(1,3, 21,24)],"dates_inout.csv")

write.csv(t4, "Tracking data.csv")

head(read.csv("Tracking data.csv"))



####
ggplot(t4,aes(x=Max.Dive, fill=inout)) + geom_density(alpha=0.25)
ggplot(t4,aes(x=Min.Dive, fill=inout)) + geom_density(alpha=0.25) + xlim(-10,30)



t3$vlat=t3$Var.lat
t3$vlon=t3$Var.lon
ggplot(t3,aes(x=vlat)) + geom_density(alpha=0.25)
ggplot(t3,aes(x=vlon)) + geom_density(alpha=0.25)







