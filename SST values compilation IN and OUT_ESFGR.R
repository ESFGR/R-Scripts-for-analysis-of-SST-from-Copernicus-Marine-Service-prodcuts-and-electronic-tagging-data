# Enrique Sánchez-Fabrés

#### SST values compilation for study area ####

#Study area coordinates: lat=> -5/-9º  | Long => 35/37.5º
#Period: May to July of 2011, 2012 and 2013

library (raster)
library (rgdal)
library (ncdf4)
library (fields)
library(dplyr) #library for manipulation of dataframes
library(ggplot2)

setwd("C:/Users/Usuario/Desktop/TFM/SST and Taggging")



# DATES IN AND OUT PER YEAR

dates<-read.csv("dates_inout.csv") #dates of tagging

#arrange(dates, dates$inout) #order data by direction (in/out)

dates<-arrange(dates,  dates$df_dates) #order data by date

View(dates) #observe the dates of tagging --> dates for which SST values will be obtained

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

#|2011|---- 



#May 26-29th (IN)

may2011<- nc_open("datos SST/SST_2011_05_26-29.nc")

print(may2011)

names(may2011$var)#name of variable = thetao


SSTmay2011<- ncvar_get(may2011, "thetao") #table with SST values

SSTmay2011<- as.vector(SSTmay2011) #transform matrix into vector (list)
SSTmay2011<- na.omit(SSTmay2011) # remove NAs
range(SSTmay2011) #show min max SST values 




#function to extract values of SST:

SST_fun<-function(x){ # x= dataset in format nc including SST values (variable name = "thetao") 
  SST<- ncvar_get(x, "thetao") 
  SST<- as.vector(SST) 
  SST<- na.omit(SST) 
  return(SST) 
  
}

may2011_IN<-SST_fun(may2011) #in


#Importing data of SST (in nc format) for all the tagging dates

dates

#|2011| July 23rd, 28th and 30th | OUT |

july_23_11<- nc_open("datos SST/23july2011.nc")
july_28_11<- nc_open("datos SST/28july2011.nc")
july_30_11<- nc_open("datos SST/30july2011.nc")

SST_july_23_11<-SST_fun(july_23_11)
SST_july_28_11<-SST_fun(july_28_11)
SST_july_30_11<-SST_fun(july_30_11)

july2011_OUT<- c(SST_july_23_11, SST_july_28_11, SST_july_30_11)

#table with values for in and out
direction_2011 <- c(rep("in", length(SSTmay2011)), rep("out", length(july2011_OUT)))
SST_2011 <- c(may2011_IN, july2011_OUT)
SST_2011<- data.frame(SST_2011, direction_2011)
names(SST_2011) =c("SST", "Direction")

#graph for 2011
ggplot(SST_2011, aes(x=SST, fill=Direction)) + geom_density(alpha=0.25) + xlim(15,30)+labs(title = "SST 2011")
  

#SST_2011 <- c(SSTmay2011 ,SST_july_23_11, SST_july_28_11, SST_july_30_11) #list with all SST values for 2011


#|2012| ----
#15-19 May (in)

may2012<- nc_open("datos SST/15-19may2012.nc")
may2012_IN <- SST_fun(may2012)

#|2012| July: 10, 14-17 (out)

july_20_12<- nc_open("datos SST/10july2012.nc")
SST_july_10_12 <- SST_fun(july_20_12)

july_2012<- nc_open("datos SST/14-17july2012.nc")
SST_july2<- SST_fun(july_2012)

july2012_OUT<- c(SST_july_10_12, SST_july2) 

#table with in/out data for 2012 
direction_2012 <- c(rep("in", length(may2012_IN)), rep("out", length(july2012_OUT)))
SST_2012 <- c(may2012_IN, july2012_OUT)
SST_2012<- data.frame(SST_2012, direction_2012)
names(SST_2012) =c("SST", "Direction")

#graph for 2012
ggplot(SST_2012, aes(x=SST, fill=Direction)) + geom_density(alpha=0.25) + xlim(15,30)+labs(title = "SST 2012")


#SST_2012 <- c(SSTmay2012, SST_july_10_12, SST_july2) #list with SST values for 2012



#|2013| ----
#May: 21-28th, 29th, 31st (in) 

may1 <- nc_open("datos SST/21-28may2013.nc")
may2 <- nc_open("datos SST/29may2013.nc")
may3 <- nc_open("datos SST/31may2013.nc")

may2013_IN<- c(SST_fun(may1), SST_fun(may2), SST_fun(may3))

#June: 2nd (in)

june1 <- nc_open("datos SST/4june2013.nc")

IN_2013 <- c(may2013_IN, SST_fun(june1))

#June: 4, 6, 8, 14, 16, 18, 20-21, 23-24 (out)

june2 <- nc_open("datos SST/6june2013.nc")
june3 <- nc_open("datos SST/8june2013.nc")
june4 <- nc_open("datos SST/14june2013.nc")
june5 <- nc_open("datos SST/16june2013.nc")
june6 <- nc_open("datos SST/18june2013.nc")
june7 <- nc_open("datos SST/20&21june2013.nc")
june8 <- nc_open("datos SST/23&24june2013.nc")

SSTjune2013 <- c(SST_fun(june2), SST_fun(june3), SST_fun(june4), SST_fun(june5), SST_fun(june6), SST_fun(june7), SST_fun(june8))


#July: 5, 7, 9, 13, 17, 19, 21 (out)

july1 <- nc_open("datos SST/5july2013.nc")
july2 <- nc_open("datos SST/7july2013.nc")
july3 <- nc_open("datos SST/9july2013.nc")
july4 <- nc_open("datos SST/13july2013.nc")
july5 <- nc_open("datos SST/17july2013.nc")
july6 <- nc_open("datos SST/19july2013.nc")
july7 <- nc_open("datos SST/21july2013.nc")

SSTjuly2013 <- c(SST_fun(july1), SST_fun(july2), SST_fun(july3), SST_fun(july4), SST_fun(july5), SST_fun(july6), SST_fun(july7))

OUT_2013 <- c(SSTjune2013, SSTjuly2013)

#table with in/out data for 2013 
direction_2013 <- c(rep("in", length(IN_2013)), rep("out", length(OUT_2013)))
SST_2013 <- c(IN_2013, OUT_2013)
SST_2013<- data.frame(SST_2013, direction_2013)
names(SST_2013) =c("SST", "Direction")

#graph for 2013
ggplot(SST_2013, aes(x=SST, fill=Direction)) + geom_density(alpha=0.25) + xlim(15,30)+labs(title = "SST 2013")



#SST_2013<-c(SSTmay2013 ,SSTjune2013, SSTjuly2013) #list with all SST values for 2013

SST_IN <- c(may2011_IN, may2012_IN, IN_2013)
SST_OUT<- c(july2011_OUT, july2012_OUT, OUT_2013)

par(mfrow=c(2,1), mar=c(2,0.5,2,1), oma = c(1, 4, 1, 1))



#----






dir_in <- rep("IN", length(SST_IN))
dir_out <- rep("OUT", length((SST_OUT)))
IN_OUT <- c(dir_in, dir_out)

SST_values <- c(SST_IN, SST_OUT)

SST_inout <- data.frame(SST_values, IN_OUT)

#tabla con datos de SST, drección y año


####Distributions####


#histograms of SST for all years----
c1 <- rgb(216,238,192, max = 255, alpha = 120, names = "lt.green")
c2 <- rgb(255,100,100, max = 255, alpha = 100, names = "lt.red")

par(new=TRUE)

distribution_out <- hist(SST_OUT, col = c1, breaks = 100, xlim = c(13,27), xlab="", main ="SST distribution | May-july | 2011, 2012, 2013")
distribution_in <- hist(SST_IN, col = c2, breaks = 100, xlim = c(13,27), main = "", add=T)
legend("topright", c("IN", "OUT"), fill=c(c2, c1))
mtext("SST",side=1,col="black",line=2)  

#denisty plot with SST distribution In and Out for the three years with trakcing records

ggplot(SST_inout, aes(x=SST_values, fill=IN_OUT)) + geom_density(alpha=0.25) + 
  theme(plot.subtitle = element_text(size = 10), 
        axis.ticks = element_line(size = 1.2), 
        panel.grid.major = element_line(colour = "gray98"), 
        axis.title = element_text(size = 12, 
                                  face = "bold"), axis.text = element_text(size = 12), 
        plot.title = element_text(size = 15, 
                                  vjust = 0.25), legend.title = element_text(size = 11, 
                                                                             face = "bold"), panel.background = element_rect(fill = "gray97")) +labs(x = "SST", fill = "Direction", subtitle = "May-July of 2011,2012 and  2013")

#Yearly histograms SST distributions in and out

#2013

hist(OUT_2013, col = c1, breaks = 100, xlim = c(14,27), main ="SST distribution 2013", xlab="")
hist(IN_2013, col = c2, breaks = 100, xlim = c(14,27), main = "",  add=T)
legend("topright", c("IN", "OUT"), fill=c(c2, c1))
mtext("SST",side=1,col="black",line=2)  

#2012

hist(july2012_OUT, col = c1, breaks = 70, xlim = c(14,27), main ="SST distribution 2012", xlab="")
hist(may2012_IN, col = c2, breaks = 70, xlim = c(14,27), main = "",  add=T)
legend("topright", c("IN", "OUT"), fill=c(c2, c1))
mtext("SST",side=1,col="black",line=2)  


#2011

hist(may2011_IN, col = c2, breaks = 70, xlim = c(14,27), main ="SST distribution 2011", xlab="")
hist(july2011_OUT, col = c1, breaks = 100, xlim = c(14,27), main = "",  add=T)
legend("topright", c("IN", "OUT"), fill=c(c2, c1))
mtext("SST",side=1,col="black",line=2) 

### IN

par(mfrow=c(3,1), mar=c(2,0.5,2,1), oma = c(1, 4, 1, 1))

IN_2013<-data.frame(IN_2013)

ggplot(IN_2013, aes(x=IN_2013)) + geom_density(alpha=0.75, fill="pink") + xlim(13,27) + theme(axis.title = element_text(vjust = 0.25), 
    plot.title = element_text(vjust = 1)) +labs(title = "IN (2013)", x = "SST")

#2012
may2012_IN <- data.frame(may2012_IN)

ggplot(may2012_IN, aes(x=may2012_IN)) + geom_density(alpha=0.75, fill="pink") + xlim(13,27) + theme(axis.title = element_text(vjust = 0.25), 
                                                                                              plot.title = element_text(vjust = 1)) +labs(title = "IN (2012)", x = "SST")


hist(may2011_IN, col = "red", breaks = 100, xlim = c(13,27), ylim = c(0,1000), main = "SST distribution IN") #pico entorno 20-21
may2011_IN<-data.frame(may2011_IN)

#
ggplot(may2011_IN, aes(x=may2011_IN)) + geom_density(alpha=0.75, fill="pink") + xlim(13,27) + theme(axis.title = element_text(vjust = 0.25), 
                                                                                                    plot.title = element_text(vjust = 1)) +labs(title = "IN (2011)", x = "SST")



#
hist(may2013_IN, col = "red", breaks = 100, xlim = c(13,27), ylim = c(0,1000), main = "SST distribution IN")

hist(SST_fun(june1), col = "red", breaks = 100, xlim = c(13,27), ylim = c(0,1000), main = "SST distribution IN")
#

hist(IN_2013, col = "red", breaks = 100, xlim = c(13,27), ylim = c(0,1000), main = "SST distribution IN")

###OUT

july2011_OUT<- data.frame(july2011_OUT)
july2012_OUT <-data.frame(july2012_OUT)
OUT_2013 <- data.frame(OUT_2013)

ggplot(july2011_OUT, aes(x=july2011_OUT)) + geom_density(alpha=0.50, fill="sky blue") + xlim(13,27) + theme(axis.title = element_text(vjust = 0.25), 
   plot.title = element_text(vjust = 1)) +labs(title = "OUT (2011)", x = "SST")

ggplot(july2012_OUT, aes(x=july2012_OUT)) + geom_density(alpha=0.50, fill="sky blue") + xlim(13,27) + theme(axis.title = element_text(vjust = 0.25), 
   plot.title = element_text(vjust = 1)) +labs(title = "OUT (2012)", x = "SST")

ggplot(OUT_2013, aes(x=OUT_2013)) + geom_density(alpha=0.50, fill="sky blue") + xlim(13,27) + theme(axis.title = element_text(vjust = 0.25), 
   plot.title = element_text(vjust = 1)) +labs(title = "OUT (2011)", x = "SST")


##### Introducing TRACKING data and plotting distributions of SST compilated and from tracking data together ####

tracking_data <-read.csv("tagging/Tracking data.csv")


ggplot(SST_inout ,aes(x=SST_values)) + geom_density(fill= "red",alpha=0.25) + 
  geom_density(data= tracking_data, aes(x= tracking_data$Obs.SST), 
               fill= "sky blue",alpha=0.25)  +  xlim(10,30) 
 



##All Years (2011, 2012, 2013)

#IN

###Obs.SST
SST_in<-subset(SST_inout, IN_OUT=="IN")
tracking_in<- subset(tracking_data, inout=="in")

ggplot(SST_in ,aes(x=SST_values)) + geom_density(fill= "red",alpha=0.25) + 
  geom_density(data= tracking_in, aes(x= Obs.SST), fill= "sky blue",alpha=0.25) + 
  geom_vline(data = SST_in, aes(xintercept = mean(SST_values)), color= "red", alpha =0.75 ) +
  geom_vline(data= tracking_in, aes(xintercept = mean(Obs.SST)), color= "sky blue" ) +
   xlim(10,30) +labs(title = "Model (red) vs track (blue) SST distributions - Direction =\"IN\"") + labs(x = "SST") + 
   labs(subtitle = "Tracking SST values extracted from \"Obs.SST\" column")

### SST
trackingIN_SST_filtered <- filter(tracking_in, SST<40) #removing values >40 
mean(trackingIN_SST_filtered$SST)

summary(tracking_in$SST)

#histogram model vs track

hist(SST_IN, col = c1, breaks = 100, xlim = c(14,27), main ="SST distribution | Direction IN | 2011, 2012, 2013", xlab="")
hist(tracking_in$Obs.SST, col = c2, breaks = 100, xlim = c(14,27), main = "",  add=T)
legend("topright", c("IN", "OUT"), fill=c(c2, c1))
mtext("SST",side=1,col="black",line=2)  


#density plot
ggplot(SST_in ,aes(x=SST_values)) + geom_density(fill= "red",alpha=0.25) + 
  geom_density(data= tracking_in, aes(x= SST), fill= "sky blue",alpha=0.25) + 
  geom_vline(data = SST_in, aes(xintercept = mean(SST_values)), color= "red", alpha =0.75 ) +
  geom_vline(data= tracking_in, aes(xintercept = mean(trackingIN_SST_filtered$SST)), color= "sky blue" ) +
  xlim(10,30) +labs(title = "Model (red) vs track (blue) SST distributions - Direction =\"IN\"") +labs(x = "SST") + labs(subtitle = "Tracking SST values extracted from \"SST\" column")




#OUT

###???obs.SST
SST_out<-subset(SST_inout, IN_OUT=="OUT")
tracking_out<- subset(tracking_data, inout=="out")

ggplot(SST_out,aes(x=SST_values)) + geom_density(fill= "red",alpha=0.25) + 
  geom_density(data= tracking_out, aes(x= Obs.SST), fill= "sky blue",alpha=0.25) +  
  geom_vline(data = SST_out, aes(xintercept = mean(SST_values)), color= "red", alpha =0.75)+ 
  geom_vline(data= tracking_out, aes(xintercept = mean(Obs.SST)), color= "sky blue" ) +
   xlim(10,30) +labs(title = "Model (red) vs track (blue) SST distributions - Direction    =\"OUT\"") +labs(x = "SST") + labs(subtitle = "Tracking SST values extracted from \"Obs.SST\" column")

###SST

ggplot(SST_out,aes(x=SST_values)) + geom_density(fill= "red",alpha=0.25) + 
  geom_density(data= tracking_out, aes(x= SST), fill= "sky blue",alpha=0.25) +  
  geom_vline(data = SST_out, aes(xintercept = mean(SST_values)), color= "red", alpha =0.75)+ 
  geom_vline(data= tracking_out, aes(xintercept = mean(SST)), color= "sky blue" ) +
  xlim(10,30) +labs(title = "Model (red) vs track (blue) SST distributions - Direction    =\"OUT\"") + 
  labs(subtitle = "Tracking SST values extracted from \"SST\" column")



###Yearly----

##2011

#2011 IN
tracking_in_2011 <- subset(tracking_in, Year=="2011")
SST_in_2011 <- subset(SST_2011, SST_2011$Direction=="in")

ggplot(SST_in_2011,aes(x=SST)) + geom_density(fill= "red",alpha=0.25) + 
  geom_density(data= tracking_in_2011, aes(x= Obs.SST), 
               fill= "sky blue",alpha=0.25) +  xlim(10,30) +labs(title = "Model (red) vs track (blue) SST distributions - Direction =\"IN\"") + theme(plot.subtitle = element_text(face = "bold", 
    hjust = 0.5)) + labs(subtitle = 2011)  + 
  labs(subtitle = "Tracking SST values extracted from \"Obs.SST\" column")

tracking_in_2011$id


#2011 OUT

tracking_out_2011 <- subset(tracking_out, Year=="2011")
SST_out_2011 <- subset(SST_2011, SST_2011$Direction=="out")

ggplot(SST_out_2011,aes(x=SST)) + geom_density(fill= "red",alpha=0.25) + 
  geom_density(data= tracking_out_2011, aes(x= Obs.SST), 
               fill= "sky blue",alpha=0.25) +  xlim(10,30) +labs(title = "Model (red) vs track (blue) SST distributions - Direction =\"out\"") + theme(plot.subtitle = element_text(face = "bold", 
   hjust = 0.5)) +labs(subtitle = 2011)  + 
  labs(subtitle = "Tracking SST values extracted from \"Obs.SST\" column")

tracking_out_2011$id


##2012

# 2012 IN

tracking_in_2012 <- subset(tracking_in, Year=="2012")
SST_in_2012 <- subset(SST_2012, SST_2012$Direction=="in")

ggplot(SST_in_2012,aes(x=SST)) + geom_density(fill= "red",alpha=0.25) + 
  geom_density(data= tracking_in_2012, aes(x= Obs.SST), 
               fill= "sky blue",alpha=0.25) +  xlim(10,30) +labs(title = "Model (red) vs track (blue) SST distributions - Direction =\"in\"") + theme(plot.subtitle = element_text(face = "bold",                                                                                hjust = 0.5)) +labs(subtitle = 2012)

tracking_in_2012$id

#2012 OUT

tracking_out_2012 <- subset(tracking_out, Year=="2012")
SST_out_2012 <- subset(SST_2012, SST_2012$Direction=="out")

ggplot(SST_out_2012,aes(x=SST)) + geom_density(fill= "red",alpha=0.25) + 
  geom_density(data= tracking_out_2012, aes(x= Obs.SST), 
               fill= "sky blue",alpha=0.25) +  xlim(10,30) +labs(title = "Model (red) vs track (blue) SST distributions - Direction =\"out\"") + theme(plot.subtitle = element_text(face = "bold",                                                                                hjust = 0.5)) +labs(subtitle = 2012)


tracking_out_2012$id



##2013

#2013 IN

tracking_in_2013 <- subset(tracking_in, Year=="2013")
SST_in_2013 <- subset(SST_2013, SST_2013$Direction=="in")

ggplot(SST_in_2013,aes(x=SST)) + geom_density(fill= "red",alpha=0.25) + 
  geom_density(data= tracking_in_2013, aes(x= Obs.SST), 
               fill= "sky blue",alpha=0.25) +  xlim(10,30) +labs(title = "Model (red) vs track (blue) SST distributions - Direction =\"in\"") + theme(plot.subtitle = element_text(face = "bold",                                                                                hjust = 0.5)) +labs(subtitle = 2013)

#2013 OUT

tracking_out_2013 <- subset(tracking_out, Year=="2013")
SST_out_2013 <- subset(SST_2013, SST_2013$Direction=="out")

ggplot(SST_out_2013,aes(x=SST)) + geom_density(fill= "red",alpha=0.25) + 
  geom_density(data= tracking_out_2013, aes(x= Obs.SST), 
               fill= "sky blue",alpha=0.25) +  xlim(10,30) +labs(title = "Model (red) vs track (blue) SST distributions - Direction =\"out\"") + theme(plot.subtitle = element_text(face = "bold",                                                                                hjust = 0.5)) +labs(subtitle = 2013)