library(dplyr)
library(ctnamecleaner)
library(trendct)
library(ggmap)
library(leaflet)
library(stringr)
library(lubridate)
ct <- read.csv("lead_ale_samples_ct.csv", stringsAsFactors=FALSE)

actions <- read.csv("actions_count.csv", stringsAsFactors=FALSE)

all <- actions
all$PWS.ID <- all$water.system.id
all <- all[!duplicated(all$PWS.ID),]

ct_all <- left_join(ct, all, by="PWS.ID")

ct_all_current <- ct_all[!is.na(ct_all$X),]
ct_all_current <- ctnamecleaner(city.s.served, ct_all_current)



names(ct_all_current)[names(ct_all_current) == 'real.town.name'] <- 'cities.served'
ct_all_current <- ctnamecleaner(city.s.served, ct_all_current)
ct_all_current$town <-  sub(",.*","",ct_all_current$address)
ct_all_current <- ctnamecleaner(town, ct_all_current)

ct_all_current$Sampling.Start.Date <- mdy(ct_all_current$Sampling.Start.Date)
ct_all_current$year.start <- year(ct_all_current$Sampling.Start.Date)

ct_all_current$Sampling.End.Date <- mdy(ct_all_current$Sampling.End.Date)
ct_all_current$year.end <- year(ct_all_current$Sampling.End.Date)

out_of_state <- ct_all_current[is.na(ct_all_current$real.town.name),]
ct_all_current <- ct_all_current[!is.na(ct_all_current$real.town.name),]

ct_all_current$name_address <- paste(ct_all_current$name, ct_all_current$address, sep=", ")
ct_all_current$ppb <- ct_all_current$Sample.Measure..mg.L.*1000

# This function geocodes a location (find latitude and longitude) using the Google Maps API
geo <- geocode(location = ct_all_current$name_address, output="latlon", source="google")

# add those coordinates to our dataset
ct_all_current$lon <- geo$lon
ct_all_current$lat <- geo$lat


ct_all_current_df <- ct_all_current %>%
  group_by(real.town.name) %>%
  summarise(Count=n())

ct_all_current_df <- data.frame(ct_all_current_df)
trendmap(ct_all_current_df, headline="Test map for R generator", subhead="This is a subhead",
         src="something", byline="TrendCT.org", url_append="date", shape="towns", color="yellow-red")


ct_all_current_unique <- ct_all_current[!duplicated(ct_all_current$PWS.Name),]

ct_all_current_u_df <- ct_all_current_unique %>%
  group_by(real.town.name) %>%
  summarise(Count=n(), Population=sum(Population.Served.Count))
ct_all_current_u_df <- data.frame(ct_all_current_u_df)

trendmap(ct_all_current_u_df, headline="Test map for R generator2", subhead="This is a subhead",
         src="something", byline="TrendCT.org", url_append="date", shape="towns", color="yellow-red")

ct_all_current_unique$name_address <- paste(ct_all_current_unique$name, ct_all_current_unique$address, sep=", ")


ct_all_current_for_map <- ct_all_current[!is.na(ct_all_current$lon),]

ct_all_current_for_map$name <- str_to_title(ct_all_current_for_map$name)
ct_all_current_for_map$Is.School.Or.Daycare.Ind <- gsub("Y", "School/Daycare", ct_all_current_for_map$Is.School.Or.Daycare.Ind)
ct_all_current_for_map$Is.School.Or.Daycare.Ind <- gsub("N", "Non-School", ct_all_current_for_map$Is.School.Or.Daycare.Ind)

pal <- colorFactor(c("navy", "red"), domain = c("Non-School", "School/Daycare"))


#Population served
pop <- ct_all_current_for_map %>%
  group_by(name, address, lat, lon, city.s.served, Is.School.Or.Daycare.Ind) %>%
  summarize(Pop=mean(Population.Served.Count), Year=max(year.end))

pop <- pop[order(-pop$Pop),] 

pop_popup <- paste0("<b>",pop$name, "</b><br/>",
                     pop$address, "<br />
                    Town served: ", pop$city.s.served, 
                     "<br />Population served: ", pop$Pop,
                     "<br />Latest year violation occurred: ", pop$Year)


m <- 
  
  leaflet(pop) %>% addTiles('http://{s}.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png', attribution='&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') %>% 
  setView(-72.690940, 41.651426, zoom = 8) %>% 
  addCircles(~lon, ~lat, weight = 1, popup=pop_popup, radius=pop$Pop*10, 
             color= ~pal(Is.School.Or.Daycare.Ind),, stroke = TRUE, fillOpacity = .2) %>%
  addLegend("bottomright", pal=pal, values=~Is.School.Or.Daycare.Ind, labels="Type'", title="Type")

m

#Biggest violations
most <- ct_all_current_for_map %>%
  group_by(name, address, lat, lon, year.end, city.s.served, Is.School.Or.Daycare.Ind, ppb) %>%
  select(name, address, lat, lon, year.end, city.s.served, Is.School.Or.Daycare.Ind, ppb) %>%
  
  most <- ct_all_current_for_map %>%
  group_by(name) %>%
  filter(ppb == max(ppb)) %>% 
  filter(1:n() == 1)
  
most <- most[order(-most$ppb),] 

most_popup <- paste0("<b>",most$name, "</b><br/>",
                      most$address, "<br />
                      Town served: ", most$city.s.served, 
                      "<br />Highest ppb detected: ",most$ppb,
                      "<br />Year: ", most$year.end)


m <- 
  
  leaflet(most) %>% addTiles('http://{s}.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png', attribution='&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') %>% 
  setView(-72.690940, 41.651426, zoom = 8) %>% 
  addCircles(~lon, ~lat, weight = 1, popup=most_popup, radius=most$ppb*10, 
           color= ~pal(Is.School.Or.Daycare.Ind),, stroke = TRUE, fillOpacity = .2) %>%
  addLegend("bottomright", pal=pal, values=~Is.School.Or.Daycare.Ind, labels="Type'", title="Type")


m

#Total violations

count <- ct_all_current_for_map %>%
  group_by(name, address, lat, lon, city.s.served, Is.School.Or.Daycare.Ind) %>%
  summarize(Count=n()) 

count <- count[order(-count$Count),] 

count_popup <- paste0("<b>",count$name, "</b><br/>",
                      count$address, "<br />
                      Town served: ", count$city.s.served, 
                      "<br />Lead violations: ",count$Count)

m <- 
  
  leaflet(count) %>% addTiles('http://{s}.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png', attribution='&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') %>% 
  setView(-72.690940, 41.651426, zoom = 8) %>% 
  addCircles(~lon, ~lat, weight = 1, popup=count_popup, radius=count$Count*1000, 
           color= ~pal(Is.School.Or.Daycare.Ind),, stroke = TRUE, fillOpacity = .2) %>%
  addLegend("bottomright", pal=pal, values=~Is.School.Or.Daycare.Ind, labels="Type'", title="Type")



##

ct_all_current_for_map$name_link <- paste0("<a href='", ct_all_current_for_map$link, "'>", ct_all_current_for_map$name, "</a>")

list <- ct_all_current_for_map %>%
  select(name_link, ppb, year.end, Population.Served.Count,   Is.School.Or.Daycare.Ind, address, city.s.served, primary.water.source.type, Owner.Type.Description)

list <- data.frame(list)

#fancytable(list, headline = "Elevated lead levels in drinking water systems", subhead = "Since 2010.", height = 400,
#           paging = "false", sourceline = "Source: U.S. EPA, Safe Drinking Water Information System", byline = "Andrew Ba Tran/TrendCT.org", col = 0,
#           desc_asc = "desc")

devtools::install_github("wesm/feather/R")
library(feather)
path <- "violations.feather"
write_feather(violations, path)
