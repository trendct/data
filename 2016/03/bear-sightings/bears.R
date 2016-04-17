library(rvest)
library(dplyr)
library(RCurl)
library(scales)
require(rgdal)
require(ggmap)
require(Cairo)
require(gpclib)
require(maptools)
require(reshape)
library(stringr)
library(ggplot2)
library(tidyr)
# burl <- "http://www.depdata.ct.gov/wildlife/sighting/bearsight.asp"

#bear_table <- burl %>% read_html() %>%
#  html_nodes(xpath='/html/body/center/table/tbody/tr[2]/td/div/table/tbody/tr/td/div[3]/center/table') %>%
#  html_table()


burl <- "https://docs.google.com/spreadsheets/d/1iFb5ndUvQqc9adJLsbqPSkZeoU7Fr3Qem7st0qX_6pY/pub?output=csv"
gurl <- getURL(burl)
bear_data <- read.csv(textConnection(gurl))

gpclibPermit()
gpclibPermitStatus()
towntracts <- readOGR(dsn="maps", layer="ctgeo")
towntracts_only <- towntracts
towntracts <- fortify(towntracts, region="NAME10")

colnames(bear_data) <- c("id", "sightings")

bears_total_map <- left_join(towntracts, bear_data)


dtm <- ggplot() +
  geom_polygon(data = bears_total_map, aes(x=long, y=lat, group=group, fill=sightings), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Blues", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Bear sightings by town in Connecticut | 3/15 - 3/16", fill="")

dtm

library(ctnamecleaner)

bear_data_pop <- ctpopulator(id, bear_data)
bear_data_pop$percapita <- round((bear_data_pop$sightings/bear_data_pop$pop2013)*1000, 2)

bear_data_pop$id <- str_to_title(bear_data_pop$id)

bears_percapita_map <- left_join(towntracts, bear_data_pop)

#bears_percapita_map <- merge(towntracts, bear_data_pop, by="id", all.x=TRUE)

dtm2 <- ggplot() +
  geom_polygon(data = bears_percapita_map, aes(x=long, y=lat, group=group, fill=percapita), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Blues", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Bear sightings per 1,0000 residents in CT | 3/15 - 3/16", fill="")

dtm2


## Bear historical

bh <- read.csv("data/bear_history.csv")

bh$Year <- factor(bh$Year)

levels(bh$Month) 

bh$Month <- factor(bh$Month, levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), ordered=TRUE)

ggplot(data=bh, aes(x=Month, y=Sightings, colour=Year, group=Year)) +
  geom_line()

head(bear_data_pop)
bear_map <- bear_data_pop[c("id", "sightings", "percapita")]
colnames(bear_map) <- c("town", "sightings", "rate")

# These functions are specifically for creating dataviz for TrendCT.org
# It won't work unless you have our specific package

trendmap(bear_map, headline="Bear sightings in Connecticut", subhead="Total and per 1,000 residents",
         src="Department of Energy & Environmental Protection", byline="TrendCT.org", url_append="date", shape="towns", color="blues")

bh_t <- spread(bh, Year, Sightings)

trendchart(bh_t, headline = "Bear sightings over time", subhead = "", src = "Department of Energy & Environmental Protection",
           byline = "TrendCT.org", type = "spline", xTitle = "", yTitle = "",
           xSuffix = "", ySuffix = "", xPrefix = "", yPrefix = "", option = "")