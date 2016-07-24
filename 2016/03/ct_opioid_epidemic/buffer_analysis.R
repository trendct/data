# Geospatial analysis

## If you don't have the following packages installed, uncomment and run the line below
## install.packages("scales")
## install.packages("dplyr")
## install.packages("gtools")
## install.packages("ggplot2")
## install.packages("rgdal")
## install.packages("ggmap")
## install.packages("Cairo")
## install.packages("gpclib")
## install.packages("maptools")
## install.packages("reshape")
## install.packages("stringr")
## install.packages("tidyr")
## install.packages("devtools")
## install.packages("raster")
## install.packages("sp")
## install.packages("lubridate")
require(scales)
require(dplyr)
require(gtools)
require(ggplot2)
require(rgdal)
require(ggmap)
require(Cairo)
require(gpclib)
require(maptools)
require(reshape)
library(devtools)
library(stringr)
library(raster)
library(sp)
library(lubridate)
## install_github("trendct/ctnamecleaner")
library(ctnamecleaner)

## This location-specific data was taken from the state website. Names were stripped from the dataset before uploading.

test <- read.csv("data/raw_data_2014.csv", stringsAsFactors=FALSE)

coords <- cbind(test$X, test$Y)
sp <- SpatialPoints(coords)

proj4string(sp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4string(sp)

plot(towntracts_only)
plot(sp, col="red" , add=TRUE)

res <- over(sp, towntracts_only)
table(res$NAME10)

# points on a polygon for census tracts

censustracts <- readOGR(dsn="maps/census_tracts/wgs84", layer="tractct_37800_0000_2010_s100_census_1_shp_wgs84")
plot(censustracts)
plot(sp, col="red" , add=TRUE)
res <- over(sp, censustracts)

census_tracts <- res %>%
  group_by(NAME10) %>%
  summarise(Total=n())

table(res$NAME10)

censustracts <- fortify(censustracts, region="NAME10")
colnames(census_tracts) <- c("id", "total")
drugs_total_tracts <- left_join(censustracts, census_tracts)

# Towns overall (total)

dtm <- ggplot() +
  geom_polygon(data = drugs_total_tracts, aes(x=long, y=lat, group=group, fill=total), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Total overdose deaths by census tract in 2014", fill="")
dtm
