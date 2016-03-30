library(dplyr)
library(ctnamecleaner)
library(trendct)
library(ggmap)
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

out_of_state <- ct_all_current[is.na(ct_all_current$real.town.name),]
ct_all_current <- ct_all_current[!is.na(ct_all_current$real.town.name),]

ct_all_current$name_address <- paste(ct_all_current$name, ct_all_current$address, sep=", ")


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
