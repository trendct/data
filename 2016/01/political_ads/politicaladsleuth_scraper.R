library(rvest)
library(XML)

ct_list <- "http://politicaladsleuth.com/political-files/stations/state/CT/"

wu <- html(ct_list)


list <- wu %>% 
  html_node("table.table.table-striped") %>%
  html_table()

colnames(list) <- c("tv.station", "location", "tv.market", "mandated")

list$tv.station <- sub(" .*","",list$tv.station)
list$tv.market <- sub("&amp;","and",list$tv.market)
list$tv.market.state <- sub(".* ","",list$tv.market)
list$tv.market <- sub(",.*","",list$tv.market)
list$tv.market.state <- sub("None","",list$tv.market.state)

list$tv.station.url <- paste0("http://politicaladsleuth.com/political-files/tv-station/", list$tv.station, "/")

## OK, LOOP TIME

for (i in 1:nrow(list)) { #pairs with line 93
  
  
  url <- list$tv.station.url[i]
  wu <- html(url)
  
  last <-  wu %>% 
    html_node('#btnLast > a') %>%
    html_attr("href") 
  last <- sub(".*=", "", last) %>%
    as.numeric()
  # last <- 1:last
  print(last)
  
  if (last > 1) { 
    
    for (x in 1:last) {
      url_paginated <- paste0(url, "?page=", x)
      wu_paginated <- html(url_paginated)
      station_list <- wu_paginated %>% 
        html_node("table.table.table-striped") %>%
        html_table()
      
      if (nrow(station_list) > 0) { # pairs with line ???
        
        wu2 <- htmlParse(url_paginated)
        
        urls <- wu2 %>% 
          html_node("table.table.table-striped") %>%
          html_nodes("a") %>%
          html_attr("href")
        urls <- data.frame(urls)
        urls <- subset(urls, !grepl("edit",urls))
        
        station_list <- cbind(station_list, urls)
        station_list$urls <- paste0("http://politicaladsleuth.com",station_list$urls )
        
        if (x == 1) { 
          station_list_master <- station_list
        } else { #pairs with line 59 and 63
          station_list_master <- rbind(station_list_master, station_list)
        } # pairs with line 61
      } # pairs with line 40
    }
  } else { #pairs with line 85 AND 46
    station_list <- wu %>% 
      html_node("table.table.table-striped") %>%
      html_table()
    wu2 <- htmlParse(url)
    
    urls <- wu2 %>% 
      html_node("table.table.table-striped") %>%
      html_nodes("a") %>%
      html_attr("href")
    urls <- data.frame(urls)
    
    station_list <- cbind(station_list, urls)
    if (nrow(station_list) > 0) {
      if (x == 1) { 
        station_list$urls <- paste0("http://politicaladsleuth.com",station_list$urls )
        station_list_master <- station_list
      } else { 
        station_list$urls <- paste0("http://politicaladsleuth.com",station_list$urls )
        station_list_master <- rbind(station_list_master, station_list)
      } 
    }
  } 
  
      if (i == 1) { 
        ads_master_list <- station_list_master
        print(i)
      } else { 
        checking <- nrow(station_list_master)
        if (checking > 0) {
        ads_master_list <- rbind(ads_master_list, station_list_master)
        print("growing!")
        } else {
          print("skipping")
        }
      } 


}

write.csv(ads_master_list, "ads_master_list.csv")
