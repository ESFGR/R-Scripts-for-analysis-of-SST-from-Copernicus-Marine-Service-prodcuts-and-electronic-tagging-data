### Maps of tuna traps from Strait of Gibraltar, Northwestern Marocco and Southern Portugal

library(ggplot2)
library(rnaturalearth)
library(gdata)
library(raster)
library(scatterpie)
library (openxlsx)

worldMap <- ne_download(scale = "medium", returnclass = "sp")

# ------------------
# Define variables

mapBuffer <-0.2
quantileThreshold <- 0.9


resultsFolder <- "/Users/Usuario/Desktop/TFM/almadrabas/"
resultsName <- "Almadrabas mapa"
mainFolder <- "/Users/Usuario/Desktop/TFM/almadrabas/"
mainFile <- "/Users/Usuario/Desktop/TFM/almadrabas/Lista almadrabas - copia.xlsx"

# ------------------
# Read data: subset by coordinates of study area

mainData <- read.xlsx(mainFile)

mainData <- subset(mainData, Lon<-5 & Lon> -9)
mainData <- subset(mainData, Lat<37.5 & Lat> 35)

SampleCode <- as.character(mainData$Almadraba)
#worldMapCroped <- crop(worldMap,extent(min(mainData$Lon)-mapBuffer,max(mainData$Lon)+mapBuffer,min(mainData$Lat)-mapBuffer,max(mainData$Lat)+mapBuffer)) 

worldMapCroped <- crop(worldMap,extent(-9-mapBuffer,-5+mapBuffer,35-mapBuffer,37.5+mapBuffer)) 

# ------------------
# Generate map of region

mainGlobalMap <- ggplot() + geom_polygon(data = worldMap, aes(x = long, y = lat, group = group)) + coord_fixed() + theme(axis.ticks=element_blank()) + ylab("Latitude") + xlab("Longitude")

mainMap <- ggplot() + geom_polygon(data = worldMapCroped, aes(x = long, y = lat, group = group)) + coord_fixed() + theme(axis.ticks=element_blank()) + ylab("Latitude") + xlab("Longitude")

# ------------------
# Mapping almadrabas locations

AlmadrabasSites <- mainMap + geom_point(data = mainData, aes(x = Lon, y = Lat, color = factor(mainData$País)), size=1.5) + ggtitle("Sampled sites") + theme(legend.position="none") +labs(title = "Almadrabas locations")


pdf(file=paste0(resultsFolder,resultsName,"MainMapSites.pdf"),width=10)
AlmadrabasSites+labs(title = "Almadrabas locations")
dev.off()

#Adding tuna tracks locations
setwd("C:/Users/Usuario/Desktop/TFM/SST and Taggging")
trackData<-read.csv("Tracking data.csv")

trackData[c(trackData$Month=="5" & trackData$inout=="out"), 25]<- "west"

trackData <- subset(trackData, !trackData$inout=="west")



GPEsst <- read.csv2("C:/Users/Usuario/Desktop/marcas migratun/GPEsst study area.csv2")
#From GPE.csv = obs.SST

GPEtrack2010 <- subset(GPEsst, year=="2010")
GPEtrack2011 <- subset(GPEsst, year=="2011") #18 observaciones

trackData <- trackData[, c(2, 4, 5, 12, 13)]
GPEtrack2010 <- GPEtrack2010[, c(2, 16, 17, 5, 4)]
GPEtrack2011 <- GPEtrack2011[, c(2, 16, 17, 5, 4)]

names(GPEtrack2011) = c("id", "Year", "Month", "Long", "Lat")
names(GPEtrack2010) = c("id", "Year", "Month", "Long", "Lat")
names(trackData) = c("id", "Year", "Month", "Long", "Lat")

trackdataAll <- rbind(trackData, GPEtrack2010, GPEtrack2011)

TunaTrackSites <- mainMap + geom_point(data = trackdataAll, aes(x = Long, y = Lat), size=1.5 , color = trackdataAll$ID) + ggtitle("Sampled sites") + theme(legend.position="none") +labs(title = "tuna tracks locations")

#colours per year
mainMap + geom_point(data = mainData, aes(x = Lon, y = Lat, color="Almadrabas"), color= "red", size=1.5) + geom_point(data = trackdataAll, aes(x = Long, y = Lat, color = factor(trackdataAll$year)), size=1.5) + 
  labs(title = "Tuna Tracks & traps (red) locations") +labs(subtitle = "Years: 2010, 2011, 2012, 2013")

#colours per id
mainMap + geom_point(data = mainData, aes(x = Lon, y = Lat), size=1.5 , color = "red") +  geom_point(data = trackdataAll, aes(x = Long, y = Lat, color = factor(trackdataAll$ID)), size=1.5) + theme(legend.position="none") +
  labs(title = "Tuna Tracks & traps (red) locations") +labs(subtitle = "Years: 2010, 2011, 2012, 2013  |  colours = id")



#IN

track_in<-subset(trackdataAll, month="5")

#colours per year
mainMap + geom_point(data = mainData, aes(x = Lon, y = Lat), size=1.5 , color = "red") +  geom_point(data = track_in, aes(x = Long, y = Lat, color = factor(track_in$year)), size=1.5) + 
  labs(title = "Tuna Tracks & traps (red) locations")+labs(subtitle = "Direction: in  |   Years: 2010, 2011, 2012, 2013") +labs(colour = "Year")

#colours per id
mainMap + geom_point(data = mainData, aes(x = Lon, y = Lat), size=1.5 , color = "red") +  geom_point(data = track_in, aes(x = Long, y = Lat), size=1.5 , color = track_in$ID) + 
  labs(title = "Tuna Tracks & traps (red) locations")+labs(subtitle = "Direction: in  |   Years: 2010, 2011, 2012, 2013  |  colours = id")

#OUT
track_out1<- subset(trackdataAll, month== "7")
track_out2<- subset(trackdataAll, month== "6")

track_out <- rbind(track_out1, track_out2)

mainMap + geom_point(data = mainData, aes(x = Lon, y = Lat), size=1.5 , color = "red") +  geom_point(data = track_out, aes(x = Long, y = Lat), size=1.5 , color = track_out$year) + 
  labs(title = "Tuna Tracks & traps (red) locations")+labs(subtitle = "Direction: out  |   Years: 2010, 2011, 2012, 2013 ")


####Full map ----

load("tagging/dataset_tuna_for_R.RData")

as.Date(data$df_dates, 'GMT')
head(data)
str(data)
summary(data)
data$yday=yday(data$df_dates) #cambiar formato de la fecha 

t2=na.omit(data, cols=seq_along("Lon","Lat")) #eliminar filas sin datos de coordenadas
table(t2$id)

plot(t2$Lon,t2$Lat)
t2.1<-subset(t2, Year==c("2011","2012","2013"))

mainTracksMap  + geom_point(data = t2.1, aes(x = Long, y = Lat, color = factor(Year)), size=1.5) + 
  labs(title = "Tuna Tracks") +labs(colour = "Year", subtitle = NULL)



#creating map with the area of the full tracks

FullTracksMap <- crop(worldMap,extent(min(t2$Long)-mapBuffer,max(t2$Long)+mapBuffer,min(t2$Lat)-mapBuffer,max(t2$Lat)+mapBuffer)) 

mainTracksMap <- ggplot() + geom_polygon(data = FullTracksMap, aes(x = long, y = lat, group = group)) + coord_fixed() + theme(axis.ticks=element_blank()) + ylab("Latitude") + xlab("Longitude")

#Adding tracks locations
fulltrackslocations <- t2.1[c(1,3,4,11,12)]
fulltrackdataAll <- rbind(fulltrackslocations, GPEtrack2010, GPEtrack2011)

plot(fulltrackdataAll$Long, fulltrackdataAll$Lat)

#colours per year
mainTracksMap  + geom_point(data = fulltrackdataAll, aes(x = Long, y = Lat, color = factor(Year)), size=1) + 
  labs(title = "Tuna Tracks") +labs(subtitle = "Years: 2010, 2011, 2012, 2013") + theme(legend.key = element_rect(size = 1.5)) +labs(colour = "Year", subtitle = NULL)


#IN
fulltrack_in<-subset(fulltrackdataAll, Month=="5")

mainTracksMap  + geom_point(data = fulltrack_in, aes(x = Long, y = Lat, color = factor(Year)), size=1) + 
  labs(title = "Tuna Tracks  | Direction: 'IN'") +labs(subtitle = "Years: 2010, 2011, 2012, 2013") + theme(legend.key = element_rect(size = 1.5)) +labs(colour = "Year", subtitle = NULL)


#out

fulltrack_out<-subset(fulltrackdataAll, Month==c("6", "7"))

mainTracksMap  + geom_point(data = fulltrack_out, aes(x = Long, y = Lat, color = factor(Year)), size=1) + 
  labs(title = "Tuna Tracks  | Direction: 'OUT'") + theme(legend.key = element_rect(size = 1.5)) +labs(colour = "Year")



#yealy maps-----
#2011

trackData2011<- subset(trackdataAll, year=="2011")
IN_2011<- subset(trackData2011, inout=="in")

mainMap + geom_point(data = mainData, aes(x = Lon, y = Lat), size=1.5 , color = "red") +  geom_point(data = trackData2011, aes(x = Long, y = Lat), size=1.5 , color = trackData2011$id) + 
  theme(legend.position = c(0.95, 0.95),
        legend.justification = c("right", "top")) +
  labs(title = "Tuna Tracks & traps (red) locations 2011")

#in
mainMap + geom_point(data = mainData, aes(x = Lon, y = Lat), size=1.5 , color = "red") +  geom_point(data = IN_2011, aes(x = Long, y = Lat), size=1.5 , color = IN_2011$id) + 
  labs(title = "Tuna Tracks & traps (red) locations")+labs(subtitle = "Direction: in  | Year: 2011  |  Colours = individuals")








#2013

trackData2013<- subset(trackData, Year=="2013")
IN_2013<- subset(trackData2013, inout=="in")

mainMap + geom_point(data = mainData, aes(x = Lon, y = Lat), size=1.5 , color = "red") +  geom_point(data = trackData2013, aes(x = Long, y = Lat), size=1.5 , color = trackData2013$id) + 
  theme(legend.position = c(0.95, 0.95),
        legend.justification = c("right", "top")) +
  labs(title = "Tuna Tracks & traps (red) locations 2013")

#in
mainMap + geom_point(data = mainData, aes(x = Lon, y = Lat), size=1.5 , color = "red") +  geom_point(data = IN_2013, aes(x = Long, y = Lat), size=1.5 , color = IN_2013$id) + 
  labs(title = "Tuna Tracks & traps (red) locations")+labs(subtitle = "Direction: in  | Year: 2013  |  Colours = individuals")


