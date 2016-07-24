library(RCurl)
library(dplyr)
library(trendct)
library(ctnamecleaner)
library(lubridate)
library(stringr)
library(ggplot2)

# Getting the CSV
the_csv <- getURL("https://data.ct.gov/api/views/rybz-nyjw/rows.csv?accessType=DOWNLOAD")
overdoses <- read.csv(textConnection(the_csv), stringsAsFactors=FALSE)

# Total overdoses
total <- nrow(overdoses)

# Average age of overdoser 
avg_age <- mean(overdoses$Age, na.rm=TRUE)

# Convert the date to the proper format
overdoses$Date <- mdy(overdoses$Date)

overdoses$Year <- year(overdoses$Date)
table(overdoses$Year)

overdoses$Heroin <- str_trim(overdoses$Heroin)

table(overdoses$Heroin)

# Location!

Southington, CT (41.600428, -72.878105)

overdoses$lat <- sub(".*\\(","", overdoses$DeathLoc)
overdoses$lat <- sub(",.*","", overdoses$lat)
overdoses$lng <- sub(".*\\(","", overdoses$DeathLoc)
overdoses$lng <- sub(".*\\ ","", overdoses$lng)
overdoses$lng <- sub("\\)","", overdoses$lng)

library(leaflet)

m <- leaflet(overdoses) %>% addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png') %>% setView(-72.878105, 41.600428, zoom=8) %>% 
  addCircles(~lng, ~lat, popup=overdoses$ImmediateCauseA, weight=3, radius=40,
             color="#ffa500", stroke=TRUE, fillOpacity=.8)
m

## OK, that's no good.

# Let's choropleth it

ct_only <- subset(overdoses, (Residence.State=="CT" | Residence.State==""))


outside_ct <- subset(overdoses, Residence.State!="CT")
choropleth_res <- data.frame(table(ct_only$Residence.City))

colnames(choropleth_res) <- c("Town", "Overdoses")

choropleth_death <- data.frame(table(ct_only$Death.City))
  
colnames(choropleth_death) <- c("Town", "Overdoses")

top10 <- head(choropleth_res[order(-choropleth_res$Overdoses),], 10)

rownames(top10) <- NULL
top10$Town <- as.character(top10$Town)
top10[6,1] <- "unspecified"

trendchart(top10, headline = "Towns with the most overdoses in Connecticut", subhead = "Between 2012 and Sept. 2015", src = "State Medical Examiner's Office",
           byline = "TrendCT.org", type = "column", xTitle = "", yTitle = "",
           xSuffix = "", ySuffix = "", xPrefix = "", yPrefix = "", option = "")

top10d <- head(choropleth_death[order(-choropleth_death$Overdoses),], 10)
top10d$Town <- as.character(top10$Town)

trendchart(top10d, headline = "Towns with the most overdoses in Connecticut city.death", subhead = "Between 2012 and Sept. 2015", src = "State Medical Examiner's Office",
           byline = "TrendCT.org", type = "column", xTitle = "", yTitle = "",
           xSuffix = "", ySuffix = "", xPrefix = "", yPrefix = "", option = "")

choropleth_death$Town <- str_trim(choropleth_death$Town)
ctnamecleaner(Town, choropleth_death, filename="analysis", case="Title")

choropleth_death <- subset(choropleth_death, Town!="")
choropleth_death <- subset(choropleth_death, Town!="North Grosvenordale")

choropleth_death <- read.csv("analysis.csv")

choropleth_death <- choropleth_death[!is.na(choropleth_death$real.town.name),]

choropleth_death <- data.frame(tapply(choropleth_death$Overdoses, choropleth_death$real.town.name, sum))

choropleth_death$Town <- rownames(choropleth_death)

rownames(choropleth_death) <- NULL

colnames(choropleth_death) <- c("Overdoses", "Town")
choropleth_death <- choropleth_death[c("Town", "Overdoses")]

choropleth_death[134,1] <- "Wethersfield"

trendmap(choropleth_death, headline="Accidental drug overdoes by town", subhead="Between 2012 and Sept. 2015",
         src="State Medical Examiner's Office", byline="TrendCT.org", url_append="date", shape="towns", color="yellow-red")


str(choropleth_death)
