# A look at lead screenings for children in Connecticut 

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
library(gridExtra)
gpclibPermit()
gpclibPermitStatus()
towntracts <- readOGR(dsn="maps", layer="ctgeo")
towntracts_only <- towntracts
towntracts <- fortify(towntracts, region="NAME10")

screenings_total <- read.csv("data/confirmed.town.total.csv", stringsAsFactors=FALSE)

# BLL5
bll5_total <- screenings_total[c("Town", "BLL5")]
colnames(bll5_total) <- c("id", "bll5")

bll5_total_map <- left_join(towntracts, bll5_total)

dtm5 <- ggplot() +
  geom_polygon(data = bll5_total_map, aes(x=long, y=lat, group=group, fill=bll5), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Blues", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Number of lead-poisoned children by town (BLL 5 and up)", fill="")



# BLL10
bll10_total <- screenings_total[c("Town", "BLL10")]
colnames(bll10_total) <- c("id", "bll10")

bll10_total_map <- left_join(towntracts, bll10_total)

dtm10 <- ggplot() +
  geom_polygon(data = bll10_total_map, aes(x=long, y=lat, group=group, fill=bll10), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Blues", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Number of lead-poisoned children by town (BLL 10 and up)", fill="")



# BLL15
bll15_total <- screenings_total[c("Town", "BLL15")]
colnames(bll15_total) <- c("id", "bll15")

bll15_total_map <- left_join(towntracts, bll15_total)

dtm15 <- ggplot() +
  geom_polygon(data = bll15_total_map, aes(x=long, y=lat, group=group, fill=bll15), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Blues", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Number of lead-poisoned children by town (BLL 15 and up)", fill="")




# BLL20
bll20_total <- screenings_total[c("Town", "BLL20")]
colnames(bll20_total) <- c("id", "bll20")

bll20_total_map <- left_join(towntracts, bll20_total)

dtm20 <- ggplot() +
  geom_polygon(data = bll20_total_map, aes(x=long, y=lat, group=group, fill=bll20), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Blues", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Number of lead-poisoned children by town (BLL 20 and up)", fill="")

grid.arrange(dtm5,dtm10,dtm15,dtm20, ncol=2)

##


screenings_percent <- read.csv("data/confirmed.town.percent.csv", stringsAsFactors=FALSE)

# BLL5
bll5_percent <- screenings_percent[c("Town", "BLL5")]
colnames(bll5_percent) <- c("id", "bll5")

bll5_percent_map <- left_join(towntracts, bll5_percent)

dtm5 <- ggplot() +
  geom_polygon(data = bll5_percent_map, aes(x=long, y=lat, group=group, fill=bll5), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Blues", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Percent of lead-poisoned children by town (BLL 5 and up)", fill="")



# BLL10
bll10_percent <- screenings_percent[c("Town", "BLL10")]
colnames(bll10_percent) <- c("id", "bll10")

bll10_percent_map <- left_join(towntracts, bll10_percent)

dtm10 <- ggplot() +
  geom_polygon(data = bll10_percent_map, aes(x=long, y=lat, group=group, fill=bll10), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Blues", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Percent of lead-poisoned children by town (BLL 10 and up)", fill="")



# BLL15
bll15_percent <- screenings_percent[c("Town", "BLL15")]
colnames(bll15_percent) <- c("id", "bll15")

bll15_percent_map <- left_join(towntracts, bll15_percent)

dtm15 <- ggplot() +
  geom_polygon(data = bll15_percent_map, aes(x=long, y=lat, group=group, fill=bll15), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Blues", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Percent of lead-poisoned children by town (BLL 15 and up)", fill="")




# BLL20
bll20_percent <- screenings_percent[c("Town", "BLL20")]
colnames(bll20_percent) <- c("id", "bll20")

bll20_percent_map <- left_join(towntracts, bll20_percent)

dtm20 <- ggplot() +
  geom_polygon(data = bll20_percent_map, aes(x=long, y=lat, group=group, fill=bll20), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Blues", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Percent of lead-poisoned children by town (BLL 20 and up)", fill="")

grid.arrange(dtm5,dtm10,dtm15,dtm20, ncol=2)


## Where children have been screened

screened <- read.csv("data/children.screened.csv", stringsAsFactors=FALSE)
screened_total <- screened[c("Town", "Children.Under.6.Screened")]

colnames(screened_total) <- c("id", "Children.Under.6.Screened")

screened_total_map <- left_join(towntracts, screened_total)
dtm_s <- ggplot() +
  geom_polygon(data = screened_total_map, aes(x=long, y=lat, group=group, fill=Children.Under.6.Screened), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Number of children screened for lead-poisoning by town", fill="")

screened_percent <- screened[c("Town", "Percent.9m.2yrs.Screened")]

colnames(screened_percent) <- c("id", "Percent.9m.2yrs.Screened")

screened_percent_map <- left_join(towntracts, screened_percent)
dtm_p <- ggplot() +
  geom_polygon(data = screened_percent_map, aes(x=long, y=lat, group=group, fill=Percent.9m.2yrs.Screened), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Percent of children screened for lead-poisoning by town", fill="")

grid.arrange(dtm_s,dtm_p, ncol=2)


