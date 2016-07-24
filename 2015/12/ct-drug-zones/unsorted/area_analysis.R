library(plyr)

daycares <- read.csv("shps/kendall/day_cares.csv", stringsAsFactors=FALSE)
public <- read.csv("shps/kendall/public_schools.csv", stringsAsFactors=FALSE)
private <- read.csv("shps/kendall/private_schools.csv", stringsAsFactors=FALSE)
housing <- read.csv("shps/kendall/pub_housing.csv", stringsAsFactors=FALSE)

datadc <- daycares[,c("NAME", "CITY", "LAT", "LON")]
dataps <- public[,c("NAME", "LCITY", "LAT", "LON")]
datapr <- private[,c("NAME", "LCITY", "LAT", "LON")]
dataph <- housing[,c("PROJECT_NA", "PLACE_NM2K", "Y", "X")]

colnames(dataph) <- c("NAME", "LCITY", "LAT", "LON")
colnames(datadc) <- c("NAME", "LCITY", "LAT", "LON")

datadc$TYPE <- "daycare"
dataps$TYPE <- "public school"
datapr$TYPE <- "private school"
dataph$TYPE <- "public housing"

data_all <- rbind(datadc, dataps)
data_all <- rbind(data_all, datapr)
data_all <- rbind(data_all, dataph)

write.csv(data_all, "data_all.csv")

#OLD DATA
datasc <- schools[,c("lat","lng")]

datahs <- housing[,c("LAT","LON")]
datahs$type <- "housing"

#lat lng for daycares
daycares$spot <- gsub("^.*?\\(","", daycares$Location)
daycares$spot <- gsub("\\)", "", daycares$spot)

list <- strsplit(daycares$spot, ", ")
df <- ldply(list)
colnames(df) <- c("lat", "lng")

daycares <- cbind(daycares, df)

#lat lng for schools

schools$spot <- gsub("^.*?\\(","", schools$Location.1)
schools$spot <- gsub("\\)", "", schools$spot)

list2 <- strsplit(schools$spot, ", ")
df2 <- ldply(list2)
colnames(df2) <- c("lat", "lng")

schools <- cbind(schools, df2)

datadc <- daycares[,c("lat","lng")]
datasc <- schools[,c("lat","lng")]



datadc$type <- "daycare"
datasc$type <- "school"
colnames(datahs) <- c("lat","lng", "type")
datastuff <- rbind(datadc, datasc)
datastuff <- rbind(datastuff, datahs)

# Let's calculate area square miles per town

#load rgdal package 
library(rgdal) 
## load your polygone shapefile 
dsn<-"townsmap" 
ogrListLayers(dsn) 
#list the layers in the above directory 
lol<-readOGR(dsn,"towns") 
#shapefile name is lol 
#check your data 
summary(lol) 
getClass("Polygon") 
ur.area<-sapply(slot(lol, "polygons"), function(x) sapply(slot(x, "Polygons"), slot, "area")) 
##check the areas 
str(ur.area)

area.map(dsn)

require(rgeos)
gArea(ur.area)

require(gtools)
require(ggplot2)
require(rgdal)
require(scales)
require(ggmap)
require(dplyr)
require(Cairo)
require(gpclib)
require(maptools)
require(reshape)
gpclibPermit()
gpclibPermitStatus()


towntracts <- readOGR(dsn="townsmap", layer="towns")
#towntracts <- fortify(towntracts, region="NAME10")
#gArea(towntracts)

# area.map

require(maps)
require(raster)
#area.map(towntracts, "NAME10")

# Blah, ok let's just look at schools

sch <- subset(schools, select=c("School.Name", "lat", "lng"))
colnames(sch) <- c("Name", "Lat", "Lng")

dc <- subset(daycares, select=c("Name", "lat", "lng"))
colnames(dc) <- c("Name", "Lat", "Lng")

sch_dc <- rbind(sch, dc)
sch_dc$Lat <- as.numeric(sch_dc$Lat)
sch_dc$Lng <- as.numeric(sch_dc$Lng)

plot(towntracts)

#coordinates(sch_dc) <- c("Lng", "Lat")
#projection(sch_dc) <- "+init=epsg:4326"

#plotted <- spTransform(sch_dc, CRS=CRS(projection(ct)))
plot(sch_dc, pch=20, col="steelblue", add=T)

, add=T

#library(rgeos)
#library(dismo)

#ct <- gmap("Connecticut, US")

#circles <- gBuffer(sch_dc, width=457.2)
#str(circles)
#plot(circles)


