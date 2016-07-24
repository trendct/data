library(dplyr)
library(leaflet)
library(rgdal)

lawrence <- c("Lawrence + Memorial <br>Cancer Center", "41.370376", "-72.177799")
middle <- c("Middlesex Hospital<br>Cancer Center","41.541645", "-72.621195" )
yale <- c("Yale-New Haven Shoreline <br>Medical Center","41.291150", "-72.663660")
shoreline <- c("Shoreline Medical Center <br>(owned by Middlesex Hospital)", "41.290402", "-72.437508")


hlocations <- (rbind(lawrence, middle, yale, shoreline))

hlocations <- data.frame(hlocations)
hlocations$hospital <- rownames(hlocations)
colnames(hlocations) <- c("title", "latitude", "longitude", "hospital")
rownames(hlocations) <- NULL
hlocations <- hlocations[c("hospital", "title", "latitude", "longitude")]

hlocations$latitude <- as.numeric(as.character(hlocations$latitude))
hlocations$longitude <- as.numeric(as.character(hlocations$longitude))
hlocations$title <- as.character(hlocations$title)

towns <- readOGR(dsn="town_shapes", layer="towns")


towns.min <- subset(towns, NAME10=="Chester" | NAME10=="Clinton" | NAME10=="Colchester" |
                      NAME10=="Cromwell" | NAME10=="Deep River" | NAME10=="Durham" | NAME10=="East Haddam" | NAME10=="East Hampton" |
                      NAME10=="Essex" | NAME10=="Haddam" | NAME10=="Killingworth" | NAME10=="Madison" | NAME10=="Marlborough" | NAME10=="Middlefield" |
                      NAME10=="Middletown" |
                      NAME10=="Old Saybrook" |
                      NAME10=="Portland" |
                      NAME10=="Westbrook")


m <- leaflet(hlocations) %>% addTiles('http://{s}.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png', 
                                      attribution='&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') 
m %>% setView(-72.451815,41.31733, zoom = 9) %>%
  addPolygons(data=towns.min, popup=towns.min$NAME10,stroke=TRUE, weight=2, fillOpacity=.05, smoothFactor=.5, color="#f72a5f") %>%
  addCircles(lng=~longitude, lat=~latitude, popup=hlocations$title, weight = 2, radius=900, fillOpacity=1, 
             color="#ffffff", fillColor="#ffa500")


#

m <- leaflet(hlocations) %>% addTiles('http://{s}.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png', 
                                      attribution='&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') 
m %>% setView(-72.451815,41.551733, zoom = 9) %>%
  addProviderTiles("Stamen.TonerLabels") %>%
  # setView(-72.451815,41.451733, zoom = 10) %>%
  addPolygons(data=towns.min, popup=towns.min$NAME10,stroke=TRUE, weight=2, fillOpacity=.2, smoothFactor=.5, color="#f72a5f") %>%
  addCircles(lng=~longitude, lat=~latitude, popup=hlocations$title, weight = 2, radius=900, fillOpacity=1, 
             color="#ffffff", fillColor="#ffa500")
m


42.339686, -71.145058