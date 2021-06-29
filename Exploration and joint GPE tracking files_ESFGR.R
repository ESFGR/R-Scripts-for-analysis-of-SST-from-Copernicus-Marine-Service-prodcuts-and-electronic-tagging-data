### Exploration and joint GPE files data (there are several folders from different individuals and the aim is to create one file including the data of interest from all ids)

#objective: table with --> | ID | Lon | Lat | Date | Hour | SST |
  
  #other interesting variables: "quality"  de lon lat en ID-Locations.cvs [ , 6]
  #"Depth"  ID-SST.csv [ , 9]

#source files names: 
 
  #ID/ID-Locations.cvs -->  "Latitude"([ , 7]), "Longitude"([ , 8]), 
  #ID/ID-SST.csv --> SST ([ , 10])


#dates and temperatures DO NOT COINCIDE in CSVs of LOCATIONS Y SST = problem to join
setwd("C:/Users/Usuario/Desktop/marcas migratun")

head(read.csv("61125/61125-Locations.csv")) # 13:00:00 08-Jun-2010
head(read.csv("61125/61125-SST.csv")) #08:44:01 22-Jun-2010

head(read.csv("61125/61125-Argos.csv"))
head(read.csv("61125/61125-Series.csv"))
locations1 <- read.csv("60520/60520-Locations.csv") # 22 june 2010 - 2 july 2010 --> givr eexact times and doesnt have coordinates for all dates when there is SST data


sst1<-read.csv("60520/60520-SST.csv") #02 June 2010 - 01 July 2010" --> o´clock times



# Table Argos locations---- 

resultsFolder <- "C:/Users/Usuario/Desktop/marcas migratun"
mainFolder <-"C:/Users/Usuario/Desktop/marcas migratun" #carpeta general

directories <- list.dirs(mainFolder,recursive=FALSE)
tablelocations <- data.frame()

for (dir in 1:length(directories)) {#loop para crear una tabla con todas las localizaciones de atunes marcados
  
  d <- directories[dir] # selecciona el directorio: ej--> "C:/Users/Usuario/Desktop/marcas migratun/60520"
  
  f <- gregexpr(pattern ='/',d) # extrae el nombre de la carpeta a partir del directorio = "60520"
  f <- as.numeric(unlist(f))
  fileName <- substr(d, f[length(f)]+1, nchar(d)) #establece el de la carpeta correspondiente a un individuo
  
  d2 <- (paste0(mainFolder,"/", fileName)) #crea el directorio para la carpeta del indiviuo

  
  
  if(!file.exists(fileName)) { next }
  
  #sst <- read.csv((paste0(d2,"/", fileName, "-SST.csv"))) #selecciona el csv con los datos de temepratura
  #sst <- sst[, c(1,5,10)] #1=ID, 5=date, 10= temperature ## Se seleccionan las columnas def interés
  
  locations <- read.csv((paste0(d2,"/", fileName, "-Locations.csv")))
  locations <- locations[, c(1,4, 7, 8)] #4= date, 7 y 8 = lat y long
  
  tablelocations <- rbind(tablelocations, locations)
  #sst_loc <- cbind(sst, locations) #junta las columnas en una matriz
  
  #tableTuna<- rbind(tableTuna, sst_loc) #añade la matriz a la tabla final
  
  
}  

write.csv(tablelocations, "All Tuna Locations.csv", row.names = F)

read.csv("All Tuna Locations.csv")
head(read.csv("All Tuna Locations.csv"))


####Mapping Argos locations-----

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
require(reshape)

#creating the map
range(t$Long)
range(t$Lat)

mapBuffer <-0.1
worldMap <- ne_download(scale = "medium", returnclass = "sp")

worldMapCroped <- crop(worldMap,extent(min(tablelocations$Longitude)-mapBuffer,max(tablelocations$Longitude)+mapBuffer,min(tablelocations$Latitude)-mapBuffer,max(tablelocations$Latitude)+mapBuffer)) 

mainMap <- ggplot() + geom_polygon(data = worldMapCroped, aes(x = long, y = lat, group = group)) + coord_fixed() + theme(axis.ticks=element_blank()) + ylab("Latitude") + xlab("Longitude")#mapa general

#map from study area only

##preparando mapa de la zona de estudio

mapBuffer <-0.1
worldMap <- ne_download(scale = "medium", returnclass = "sp")

MapCroped <- crop(worldMap,extent(-9,-5,34.5 ,37.5)) #coordenadas del área de estudio

StudyArea <- ggplot() + geom_polygon(data = MapCroped, aes(x = long, y = lat, group = group)) + coord_fixed() + theme(axis.ticks=element_blank()) + ylab("Latitude") + xlab("Longitude")#mapa del area de estudio


#Map with all argos locations

tracksmap <- mainMap + geom_point(data = tablelocations, aes(x = Longitude, y = Latitude,  color = DeployID )) + labs(title = "Tuna locations") + labs(colour = "Id")

pdf(file="C:/Users/Usuario/Desktop/TFM/SST and Taggging/tagging/Resulados marcas migratun/ Map with all tracks.pdf", width=10)
print(tracksmap)
dev.off()
#tracks in study area

studyareaLocations <- subset(tablelocations,Longitude<(-5.5) & Longitude>(-9) & Latitude>34.5 & Latitude<37.5) #seleccionar tracks dentro del área de estudio

studyareatracks <- StudyArea + geom_point(data = studyareaLocations, aes(x = Longitude, y = Latitude,  color = DeployID )) + labs(title = "Tuna locations") + labs(colour = "Id")

pdf(file="C:/Users/Usuario/Desktop/TFM/SST and Taggging/tagging/Resulados marcas migratun/ Tracks in study area.pdf", width=10)
print(studyareatracks)
dev.off()






# Table GPE3 locations----

#joining tables ----
head(read.csv("61125/61125-1-GPE3.csv", skip = 5))
head(read.csv("61127/61127-1-GPE3.csv", skip = 5)) #no  columns "DeployID" nor "Ptt" 
head(read.csv("61130/61130-1-GPE3.csv", skip = 5)) #no  columns "DeployID" nor "Ptt"
head(read.csv("72498/72498-1-GPE3.csv", skip = 5)) #no  columns "DeployID" nor "Ptt"
head(read.csv("72499/72499-1-GPE3.csv", skip = 5)) #no  columns "DeployID" nor "Ptt"
head(read.csv("86437/86437-1-GPE3.csv", skip = 5)) #no  columns "DeployID" nor "Ptt"
head(read.csv("86437B/86437B-1-GPE3.csv", skip = 5))
head(read.csv("86440/86440-1-GPE3.csv", skip = 5)) #no  columns "DeployID" nor "Ptt"
head(read.csv("86442/86442-1-GPE3.csv", skip = 5))


id61125<-read.csv("61125/61125-1-GPE3.csv", skip = 5)
id61125<-id61125[, 2:14] 
head(id61125)
id61125 <- plyr::rename(id61125, c("Ptt" = "DeployID"))

id86437B<-read.csv("86437B/86437B-1-GPE3.csv", skip = 5)
id86437B<-id86437B[, 2:14] 
head(id86437B)
id86437B <- plyr::rename(id86437B, c(Ptt = "DeployID"))

id86442<-read.csv("86442/86442-1-GPE3.csv", skip = 5)
id86442<-id86442[, 2:14] 
head(id86442)
id86442 <- plyr::rename(id86442, c(Ptt = "DeployID"))


id61127 <- read.csv("61127/61127-1-GPE3.csv", skip = 5)
DeployId <- data.frame("DeployID" = rep("61127", nrow(id61127)))
id61127 <- cbind(DeployId, id61127)

id61130 <- read.csv("61130/61130-1-GPE3.csv", skip = 5)
DeployId <- data.frame("DeployID" = rep("61130", nrow(id61130)))
id61130 <- cbind(DeployId, id61130)

id72498 <- read.csv("72498/72498-1-GPE3.csv", skip = 5)
DeployId <- data.frame("DeployID" = rep("72498", nrow(id72498)))
id72498 <- cbind(DeployId, id72498)

id72499 <- read.csv("72499/72499-1-GPE3.csv", skip = 5)
DeployId <- data.frame("DeployID" = rep("72499", nrow(id72499)))
id72499 <- cbind(DeployId, id72499)

id86437 <- read.csv("86437/86437-1-GPE3.csv", skip = 5)
DeployId <- data.frame("DeployID" = rep("86437", nrow(id86437)))
id86437 <- cbind(DeployId, id86437)

id86440 <- read.csv("86440/86440-1-GPE3.csv", skip = 5)
DeployId <- data.frame("DeployID" = rep("86440", nrow(id86440)))
id86440 <- cbind(DeployId, id86440)

AllGPEdata <- rbind(id61125, id61127, id61130, id72498, id72499, id86437, id86437B, id86440, id86442)

#creation of file with joint data from all ids
#write.csv(AllGPEdata, "GPE data from all individuals.csv", row.names = F)----

AllGPE <- read.csv("GPE data from all individuals.csv")
head(AllGPE)
table(AllGPE$DeployID)

#dates change

AllGPE$Date<-parse_date_time(AllGPE$Date, orders="dmy hms")

AllGPE$ddmmyy <- as.Date(AllGPE$Date)



tapply(AllGPE$DeployID, paste(AllGPE$DeployID, AllGPE$ddmmyy, sep = ""), length)

#study area 

GPEdata <- subset(AllGPE,Most.Likely.Longitude<(-5) & Most.Likely.Longitude>(-9) & Most.Likely.Latitude>35 & Most.Likely.Latitude<37.5) #select tracks in study area

hist(GPEdata$Observed.SST, breaks = 15 , xlim = c(14,27),  main ="", xlab="")

summary(GPEdata$Observed.SST) 

#### Remove repeated dates

SA.GPE.table<-GPEdata[!duplicated(GPEdata$ddmmyy), ] #tabla con solo un dato por día para unir con los datos del fichero SST
SA.GPE.table$DeployID<-as.character(SA.GPE.table$DeployID)

#### Tabla solo con las filas con valores de Obs.SST
GPEsst <-GPEdata[!is.na(GPEdata$Observed.SST),] #removing rows with NA in the Observed.SST
length(GPEsst$Observed.SST)



#Map with all GPE tracks----

#creating the map

mapBuffer <-0.1
worldMap <- ne_download(scale = "medium", returnclass = "sp")

worldMapCroped <- crop(worldMap,extent(min(AllGPEdata$Most.Likely.Longitude)-mapBuffer,max(AllGPEdata$Most.Likely.Longitude)+mapBuffer,min(AllGPEdata$Most.Likely.Latitude)-mapBuffer,max(AllGPEdata$Most.Likely.Latitude)+mapBuffer)) 

mainMap <- ggplot() + geom_polygon(data = worldMapCroped, aes(x = long, y = lat, group = group)) + coord_fixed() + theme(axis.ticks=element_blank()) + ylab("Latitude") + xlab("Longitude")#mapa general



GPEtracksmap <- mainMap + geom_point(data = AllGPEdata, aes(x = Most.Likely.Longitude, y = Most.Likely.Latitude,  color = factor(DeployID)), size = 0.1) + labs(title = "Tuna GPE tracks") + labs(colour = "Id")

#pdf(file="C:/Users/Usuario/Desktop/TFM/SST and Taggging/tagging/Resulados marcas migratun/ Map with all GPE tracks.pdf", width=10)
print(GPEtracksmap)
dev.off()


#tracks in study area----

#preparando mapa de la zona de estudio

mapBuffer <-0.1
worldMap <- ne_download(scale = "medium", returnclass = "sp")

MapCroped <- crop(worldMap,extent(-9,-5,34.5 ,37.5)) #coordenadas del área de estudio

StudyArea <- ggplot() + geom_polygon(data = MapCroped, aes(x = long, y = lat, group = group)) + coord_fixed() + theme(axis.ticks=element_blank()) + ylab("Latitude") + xlab("Longitude")#mapa del area de estudio

studyareaLocations <- subset(AllGPEdata, Most.Likely.Longitude<(-5) & Most.Likely.Longitude>(-9) & Most.Likely.Latitude>34.5 & Most.Likely.Latitude<37.5) #seleccionar tracks dentro del área de estudio

studyareaGPEtracks <- StudyArea + geom_point(data = studyareaLocations, aes(x = Most.Likely.Longitude, y = Most.Likely.Latitude,  color = factor(DeployID))) + labs(title = "Tuna GPE track in Study Area") + labs(colour = "Id")

#pdf(file="C:/Users/Usuario/Desktop/TFM/SST and Taggging/tagging/Resulados marcas migratun/ GPE Tracks in study area.pdf", width=10)
#print(studyareaGPEtracks)
#dev.off()



### SST Data ----

resultsFolder <- "C:/Users/Usuario/Desktop/marcas migratun"
mainFolder <-"C:/Users/Usuario/Desktop/marcas migratun" #carpeta general

head(read.csv("61125/61125-SST.csv")) # 1, 4, 5, 9, 10, 11

directories <- list.dirs(mainFolder,recursive=FALSE)
tablesst <- data.frame()

for (dir in 1:length(directories)) {#loop make one table with all locations of tagged individuals
  
  d <- directories[dir] # select directory: ej--> "C:/Users/Usuario/Desktop/marcas migratun/60520"
  
  f <- gregexpr(pattern ='/',d) # extract name of the file from the directory = "60520"
  f <- as.numeric(unlist(f))
  fileName <- substr(d, f[length(f)]+1, nchar(d)) # #names the file corresponding to ine individual
  
  d2 <- (paste0(mainFolder,"/", fileName)) #creates the directory to the folder with the individual
  
  
  
  if(!file.exists(fileName)) { next }
  
  
  
  sst <- read.csv((paste0(d2,"/", fileName, "-SST.csv")))
  sst <- sst[, c(1, 4, 5, 9, 10, 11)] 
  
  tablesst <- rbind(tablesst, sst)
 
  
  #tableTuna<- rbind(tableTuna, sst_loc) #añade la matriz a la tabla final
  
  
}  

tablesst
table(tablesst$DeployID)

#write.csv(tablesst, "All SST data.csv", row.names = F)

read.csv("All SST data.csv")
head(read.csv("All SST data.csv"))

SSTdata <- read.csv("All SST data.csv") #not possible to subset by study area using coordinates

#

SSTdata$Date<-parse_date_time(SSTdata$Date, orders="hms dmy")

SSTdata$ddmmyy <- as.Date(SSTdata$Date)

(tapply(SSTdata$DeployID, paste(SSTdata$DeployID, SSTdata$ddmmyy, sep = "-"), length) )#


SA.GPE.table2 <- SA.GPE.table[, c(1,3,4,14)]

SA.SST <- merge(SSTdata, SA.GPE.table2, by = c("DeployID", "ddmmyy"))
#SA.SST<-right_join(SSTdata, SA.GPE.table2, by=c("ddmmyy","DeployID"))

summary(SA.SST) ##por qué hay NAs en la columna de temeprature ????

length(na.omit(SA.SST))



write.csv(SA.SST, file="C:/Users/Usuario/Desktop/TFM/SST and Taggging/tagging/Resulados marcas migratun/BF Study Area SST .csv")

head(SSTdata)








