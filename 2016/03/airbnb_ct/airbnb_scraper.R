library(rvest)
library(dplyr)
library(leaflet)

rm(airbnb_df)

url_list <- read.csv("just_urls.csv", stringsAsFactors=FALSE)
url_list <- url_list[c("link")]
url_list <- unique(url_list)

for (i in 607:nrow(url_list)) {
    link <- url_list$link[i]
    
    ab <- html(link)
    
    all_meta_attrs <- unique(unlist(lapply(lapply(ab %>% html_nodes("meta"), html_attrs), names)))
    
    dat <- data.frame(lapply(all_meta_attrs, function(x) {
      ab %>% html_nodes("meta") %>% html_attr(x)
    }))
    
    if ((is.null(dat)) | (nrow(dat)==35)) {
      url <- link
      latitude <- 999
      longitude <- 999
      locality <- "nope"
      region <- "nope"
      city <- "nope"
      rating <- 9.9
      price <- 999
    } else {
    colnames(dat) <- all_meta_attrs
    
    url <- link
    
    filtered <- dat %>% 
      select(content, property, itemprop) %>%
      filter(property=="airbedandbreakfast:location:latitude" | 
               property=="airbedandbreakfast:location:longitude" | 
               property=="airbedandbreakfast:locality" | 
               property=="airbedandbreakfast:region"| 
               property=="airbedandbreakfast:city" | 
               property=="airbedandbreakfast:rating" |
               itemprop=="price")
    
    latitude <- filtered %>%
      filter(property=="airbedandbreakfast:location:latitude")
    latitude <- as.character(latitude$content)
    
    longitude<- filtered %>%
      filter(property=="airbedandbreakfast:location:longitude")
    longitude <- as.character(longitude$content)
    
    locality<- filtered %>%
      filter(property=="airbedandbreakfast:locality")
    locality<- as.character(locality$content)
    if (is.character(locality) && length(locality) == 0) {
      locality <- ""
    } else {
      locality <- locality
    }
    region<- filtered %>%
      filter(property=="airbedandbreakfast:region")
    region<- as.character(region$content)
    if (is.character(region) && length(region) == 0) {
      region <- ""
    } else {
      region <- region
    }
    city<- filtered %>%
      filter(property=="airbedandbreakfast:city")
    city<- as.character(city$content)
    if (is.character(city) && length(city) == 0) {
      city <- ""
    } else {
      city <- city
    }
    
    rating<- filtered %>%
      filter(property=="airbedandbreakfast:rating")
    rating<- as.character(rating$content)
    if (is.character(rating) && length(rating) == 0) {
      rating <- ""
    } else {
      rating <- rating
    }
    
    price<- filtered %>%
      filter(itemprop=="price")
    price<- as.numeric(as.character(price$content))
    if (length(price)==0) {
      price <- 999
    } else {
      prince <- price
    }
    }
    print(i)
    air_listing <- data.frame(link, latitude, longitude, locality, region, city, rating, price)
    if (exists("airbnb_df")) {
      airbnb_df <- rbind(airbnb_df, air_listing)
    } else {
    airbnb_df <- air_listing
    }
    
}

clean <- subset(airbnb_df, !(latitude==999))
leftovers <- subset(airbnb_df, latitude==999)
backup_clean <- clean

clean$latitude <- as.numeric(as.character(clean$latitude))
clean$longitude <- as.numeric(as.character(clean$longitude))
clean$rating <- as.numeric(as.character(clean$rating))
clean$price <- as.numeric(as.character(clean$price))

ct <- read.csv("ctlist.csv", stringsAsFactors=FALSE) # Brings in the file 'ctlist.csv'
# Be sure to first set the working directory in R to where the file is listed

library(leaflet)

library(rgdal)
campus <- readOGR(dsn="shapes", layer="ct_campuses")

m <- leaflet(clean) %>% addTiles('http://{s}.tile.openstreetmap.fr/hot/{z}/{x}/{y}.png',
                              attribution='&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>, Tiles courtesy of <a href="http://hot.openstreetmap.org/" target="_blank">Humanitarian OpenStreetMap Team</a>') 
m %>% setView(-72.690940, 41.651426, zoom = 8)

m %>% addPolygons(data=campus, stroke=TRUE, weight=1, fillOpacity=.8, smoothFactor=.5, color="#f72a5f") %>%
  addCircles(~longitude, ~latitude, popup=paste0("$",  clean$price, " per night <br /><a href='", clean$link, "' target='_blank'>link</a>"), weight = 3, radius=40, 
                 color="#0066cc", stroke = TRUE, fillOpacity = clean$price*.01)

library(ctnamecleaner)

?ctcorrelator


ctcorrelator(ctair_1, p=.9)

ctcorrelator(ctcorrelator )
  

ctair_1 <- ctair[c("town", "airbnbs")]
