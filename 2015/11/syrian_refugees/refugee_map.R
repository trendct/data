ref <- read.csv("refugees.csv", stringsAsFactors=FALSE)

library(leaflet)

# Be sure to first set the working directory in R to where the file is listed

m <- leaflet(ref) %>% addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png', 
                              attribution='Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') 
m %>% setView(-72.690940, 41.651426, zoom = 8)
m %>% addCircles(~y, ~x, popup=ref$Refugees, weight = 3, radius=ref$Refugees*10, 
                 color="#ffa500", stroke = TRUE, fillOpacity = 0.8)



m <- leaflet(ref) %>% addTiles('http://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png', 
                               attribution='Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') 
m %>% setView(-72.690940, 41.651426, zoom = 8)
m %>% addCircleMarkers(~y, ~x, popup=ref$Refugees, weight = 3, radius=ref$Refugees, 
                 color="#ffa500", stroke = TRUE, fillOpacity = 0.2)



?addCircles
