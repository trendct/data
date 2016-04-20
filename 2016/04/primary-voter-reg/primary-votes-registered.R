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
library(tidyr)


voters <- read.csv("data/ACTIVE_VOTER_BY_TOWN_PARTY_040716.csv", stringsAsFactors=FALSE)
voters$Affiliation <- gsub(" .*", "", voters$Affiliation)

voters <- spread(voters, Affiliation, Registered)
voters <- data.frame(voters)

trendmap(voters, headline="Total registered voters by town in Connecticut", subhead="As of April, 7, 2016.",
         src="Source: Office of the Secretary of the State", byline="Andrew Ba Tran/TrendCT.org", url_append="date", shape="towns", color="yellow-red")

voters2 <- ctpopulator(Town, voters)

voters2$dem <- round((voters2$Democratic / voters2$pop2013)*100,2)
voters2$rep <- round((voters2$Republican / voters2$pop2013)*100,2)
voters2$un <- round((voters2$Unaffiliated / voters2$pop2013)*100,2)

voters2 <- voters2[c("Town", "dem", "rep", "un")]
colnames(voters2) <- c("Town", "Democratic", "Republican", "Unaffiliated")

voters2 <- data.frame(voters2)
voters2$Town <- str_to_title(voters2$Town)

trendmap(voters2, headline="Registered voters by town in Connecticut", subhead="Per 100 residents. As of April, 7, 2016.",
         src="Source: Office of the Secretary of the State", byline="Andrew Ba Tran/TrendCT.org", url_append="date", shape="towns", color="yellow-red")


## ggmap

colnames(voters) <- c("id", "Democratic", "Republican", "Unaffiliated")
colnames(voters2) <- c("id", "Dem_per_capita", "Rep_per_capita", "Unaf_per_capita")
voters3 <- left_join(voters, voters2)

gpclibPermit()
gpclibPermitStatus()
towntracts <- readOGR(dsn="maps", layer="ctgeo")
towntracts <- fortify(towntracts, region="NAME10")

voters_map <- left_join(towntracts, voters3)

# Towns overall (total)

ggplot() +
  geom_polygon(data = voters_map, aes(x=long, y=lat, group=group, fill=Democratic), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Blues", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Total registered Democrats by town as of April 2016", fill="")

ggplot() +
  geom_polygon(data = voters_map, aes(x=long, y=lat, group=group, fill=Republican), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Total registered Republicans by town as of April 2016", fill="")


ggplot() +
  geom_polygon(data = voters_map, aes(x=long, y=lat, group=group, fill=Unaffiliated), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Greens", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Total registered Unaffiliated voters by town as of April 2016", fill="")

# Per capita


ggplot() +
  geom_polygon(data = voters_map, aes(x=long, y=lat, group=group, fill=Dem_per_capita), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Blues", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Registered Democrats per capita as of April 2016", fill="")

ggplot() +
  geom_polygon(data = voters_map, aes(x=long, y=lat, group=group, fill=Rep_per_capita), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Registered Republicans per capita as of April 2016", fill="")


ggplot() +
  geom_polygon(data = voters_map, aes(x=long, y=lat, group=group, fill=Unaf_per_capita), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Greens", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Registered Unaffiliated voters per capita as of April 2016", fill="")


