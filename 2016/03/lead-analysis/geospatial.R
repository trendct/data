# A look at lead screenings for children in Connecticut 

#library(rvest)
#library(dplyr)
#library(RCurl)
#library(scales)
#require(rgdal)
#require(ggmap)
#require(Cairo)
#require(gpclib)
#require(maptools)
#require(reshape)
#library(stringr)
#library(ggplot2)
#library(tidyr)

library(dplyr)
library(scales)
require(rgdal)
require(ggmap)
require(Cairo)
require(maptools)
library(ggplot2)
#devtools::install_github("hrecht/censusapi")
library(censusapi)
source("keys.R")
library(rvest)
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

dtm5_total <- ggplot() +
  geom_polygon(data = bll5_total_map, aes(x=long, y=lat, group=group, fill=bll5), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Blues", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Number of lead-poisoned children by town (BLL 5 and up)", fill="")



# BLL10
bll10_total <- screenings_total[c("Town", "BLL10")]
colnames(bll10_total) <- c("id", "bll10")

bll10_total_map <- left_join(towntracts, bll10_total)

dtm10_total <- ggplot() +
  geom_polygon(data = bll10_total_map, aes(x=long, y=lat, group=group, fill=bll10), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Blues", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Number of lead-poisoned children by town (BLL 10 and up)", fill="")



# BLL15
bll15_total <- screenings_total[c("Town", "BLL15")]
colnames(bll15_total) <- c("id", "bll15")

bll15_total_map <- left_join(towntracts, bll15_total)

dtm15_total <- ggplot() +
  geom_polygon(data = bll15_total_map, aes(x=long, y=lat, group=group, fill=bll15), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Blues", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Number of lead-poisoned children by town (BLL 15 and up)", fill="")




# BLL20
bll20_total <- screenings_total[c("Town", "BLL20")]
colnames(bll20_total) <- c("id", "bll20")

bll20_total_map <- left_join(towntracts, bll20_total)

dtm20_total <- ggplot() +
  geom_polygon(data = bll20_total_map, aes(x=long, y=lat, group=group, fill=bll20), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Blues", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Number of lead-poisoned children by town (BLL 20 and up)", fill="")

grid.arrange(dtm5_total,dtm10_total,dtm15_total,dtm20_total, ncol=2)

##


screenings_percent <- read.csv("data/confirmed.town.percent.csv", stringsAsFactors=FALSE)

# BLL5
bll5_percent <- screenings_percent[c("Town", "BLL5")]
colnames(bll5_percent) <- c("id", "bll5")

bll5_percent_map <- left_join(towntracts, bll5_percent)

dtm5_percent <- ggplot() +
  geom_polygon(data = bll5_percent_map, aes(x=long, y=lat, group=group, fill=bll5), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Blues", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Percent of lead-poisoned children by town (BLL 5 and up)", fill="")



# BLL10
bll10_percent <- screenings_percent[c("Town", "BLL10")]
colnames(bll10_percent) <- c("id", "bll10")

bll10_percent_map <- left_join(towntracts, bll10_percent)

dtm10_percent <- ggplot() +
  geom_polygon(data = bll10_percent_map, aes(x=long, y=lat, group=group, fill=bll10), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Blues", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Percent of lead-poisoned children by town (BLL 10 and up)", fill="")



# BLL15
bll15_percent <- screenings_percent[c("Town", "BLL15")]
colnames(bll15_percent) <- c("id", "bll15")

bll15_percent_map <- left_join(towntracts, bll15_percent)

dtm15_percent <- ggplot() +
  geom_polygon(data = bll15_percent_map, aes(x=long, y=lat, group=group, fill=bll15), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Blues", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Percent of lead-poisoned children by town (BLL 15 and up)", fill="")




# BLL20
bll20_percent <- screenings_percent[c("Town", "BLL20")]
colnames(bll20_percent) <- c("id", "bll20")

bll20_percent_map <- left_join(towntracts, bll20_percent)

dtm20_percent <- ggplot() +
  geom_polygon(data = bll20_percent_map, aes(x=long, y=lat, group=group, fill=bll20), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Blues", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Percent of lead-poisoned children by town (BLL 20 and up)", fill="")

grid.arrange(dtm5_percent,dtm10_percent,dtm15_percent,dtm20_percent, ncol=2)


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



# Census stuff

housing_towns <- getCensus(name="acs5", 
                           vintage=2014,
                           key=censuskey, 
                           vars=c("NAME", "B25034_001E", "B25034_002E", "B25034_003E", 
                                  "B25034_004E","B25034_005E","B25034_006E","B25034_007E",
                                  "B25034_008E","B25034_009E","B25034_010E", "B17004_001E"
                           ), 
                           region="county subdivision:*", regionin="state:09")

housing_towns <- subset(housing_towns, county.subdivision!="00000")
housing_towns$id <- sub(" town.*","",housing_towns$NAME)


old <- housing_towns

old$age_39 <- old$B25034_010E * .68
old$age40_59 <- (old$B25034_009E + old$B25034_008E) * .43
old$age60_79 <- (old$B25034_007E + old$B25034_006E) * .08
old$age80_99 <- (old$B25034_005E + old$B25034_004E) * .03
old$age00_10 <- (old$B25034_003E + old$B25034_002E) * 0
old$total <-  old$B25034_001E

old$risk_sum <- old$age_39 + old$age40_59 + old$age60_79 + old$age80_99 + old$age80_99 + old$age00_10
old$risk <- old$risk_sum/old$total*100
#old <- subset(old, total>0)

poverty_towns <- getCensus(name="acs5", 
                     vintage=2014,
                     key=censuskey, 
                     vars=c("NAME", "B06012_001E", "B06012_002E"
                     ), 
                     region="county subdivision:*", regionin="state:09")
poverty_towns <- subset(poverty_towns, county.subdivision!="00000")
poverty_towns$id <- sub(" town.*","",housing_towns$NAME)

poverty_towns$percent_poverty <- poverty_towns$B06012_002E/poverty_towns$B06012_001E * 100
#poverty_towns <- subset(poverty_towns, B06012_001E>0)

# Z score
# (x-mean(x))/sd(x)

poverty_towns$z_poverty <- (poverty_towns$percent_poverty - mean(poverty_towns$percent_poverty))/sd(poverty_towns$percent_poverty)
old$z_housing <-  (old$risk - mean(old$risk))/sd(old$risk)


# Calcuating weighted figures
poverty_towns$weighted_poverty <- poverty_towns$z_poverty * .42
old$weighted_housing <- old$z_housing * .58

# join dataframes
#poverty_towns$tract_id <- paste0(poverty_towns$state, poverty_towns$county, poverty_towns$tract)
#old$tract_id <- paste0(old$state, old$county, old$tract)

old <- old[c("id", "risk", "z_housing", "weighted_housing")]
df <- left_join(poverty_towns, old)

df$riskscore_raw <- df$weighted_housing + df$weighted_poverty
df <- mutate(df, quantile = ntile(riskscore_raw, 10))

## mapping


gpclibPermit()
gpclibPermitStatus()
towntracts <- readOGR(dsn="maps", layer="ctgeo")
towntracts_only <- towntracts
towntracts <- fortify(towntracts, region="NAME10")

df_town_map <- left_join(towntracts, df)


dtm_df1 <- ggplot() +
  geom_polygon(data = df_town_map , aes(x=long, y=lat, group=group, fill=quantile), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Blues", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Estimated lead risk by town", fill="")

#comparing
library(trendct)
comparing <- df[c("id", "quantile", "riskscore_raw", "percent_poverty", "risk")]
trendmap(comparing, headline="Estimated lead risk by town", subhead="This is a subhead",
         src="something", byline="Andrew Ba Tran/TrendCT.org", url_append="date", shape="towns", color="yellow-red")

comparing2 <- left_join(comparing, bll5_total)
comparing2 <- left_join(comparing2, bll10_total)
comparing2 <- left_join(comparing2, bll15_total)
comparing2 <- left_join(comparing2, bll20_total)

colnames(bll5_percent) <- c("id", "bll5.percent")
comparing2 <- left_join(comparing2, bll5_percent)

colnames(bll10_percent) <- c("id", "bll10.percent")
comparing2 <- left_join(comparing2, bll10_percent)

colnames(bll15_percent) <- c("id", "bll15.percent")
comparing2 <- left_join(comparing2, bll15_percent)

colnames(bll20_percent) <- c("id", "bll20.percent")
comparing2 <- left_join(comparing2, bll20_percent)

#dtm_df1$quantile
#dtm_df1$riskscore_raw

cor(comparing2$quantile, comparing2$riskscore_raw)

cor(comparing2$quantile, comparing2$bll5)
cor(comparing2$quantile, comparing2$bll10)
cor(comparing2$quantile, comparing2$bll15)
cor(comparing2$quantile, comparing2$bll20)
cor(comparing2$quantile, comparing2$bll5.percent)
cor(comparing2$quantile, comparing2$bll10.percent)
cor(comparing2$quantile, comparing2$bll15.percent)
cor(comparing2$quantile, comparing2$bll20.percent)

cor(comparing2$riskscore_raw, comparing2$bll5)
cor(comparing2$riskscore_raw, comparing2$bll10)
cor(comparing2$riskscore_raw, comparing2$bll15)
cor(comparing2$riskscore_raw, comparing2$bll20)
cor(comparing2$riskscore_raw, comparing2$bll5.percent)
cor(comparing2$riskscore_raw, comparing2$bll10.percent)
cor(comparing2$riskscore_raw, comparing2$bll15.percent)
cor(comparing2$riskscore_raw, comparing2$bll20.percent)


# Elevated lead actions

ct <- read.csv("data/lead_ale_samples_ct.csv", stringsAsFactors=FALSE)

#details <- read.csv("data/details.csv", stringsAsFactors=FALSE)
summary <- read.csv("data/water_system_summary.csv", stringsAsFactors=FALSE)
#cities <- read.csv("data/cities.csv", stringsAsFactors=FALSE)
#colnames(cities) <- c("X", "Cities.Served", "Freq", "towns.served")
#summary <- left_join(summary, cities, by="Cities.Served")
ct <- left_join(ct, summary, by="PWS.ID")
#ct$town.address <-  sub("*.,","",ct$NAME)

ct$link <- paste0("https://oaspub.epa.gov/enviro/sdw_report_v3.first_table?pws_id=",
                           ct$PWS.ID, "&state=CT&source=",
                          ct$Primary.Source.Code, "&population=", 
                         ct$Population.Served.Count)

# scrape address

served <- ct %>%
  group_by(Cities.Served) %>%
  dplyr::summarise(Total=n())

served_single <- served[!grepl(",", served$Cities.Served),]
served_single <- served_single[!grepl("-", served_single$Cities.Served),]
colnames(served_single) <- c("id", "Total")
served_single$id <- str_to_title(served_single$id)
comparing3 <- left_join(comparing2, served_single)

cor(comparing3$riskscore_raw, comparing3$Total, use = "complete")

comparing3$Total[is.na(comparing3$Total)] <- 0
cor(comparing3$riskscore_raw, comparing3$Total)

# scraping adresses

ct$address.city

for (i in 1:nrow(ct)) {
  i_url <- ct$link[i] %>% read_html()
  ct$address.city[i] <- i_url %>% html_node("tr:nth-child(3) td") %>%
    html_text()
}

ct$town.city <- sub(",.*","",ct$address.city)
ct$state <- sub(".*,","",ct$address.city)
ct$state <- sub("  .*$","",ct$state)

ct_only <- subset(ct, state=="CT")

hq <- ct %>%
  group_by(town.city) %>%
  dplyr::summarise(Total=n())

library(ctnamecleaner)

hq2 <- ctnamecleaner(town.city, hq)
hq3 <- hq2[!is.na(hq2$real.town.name),]

hq4 <- hq3 %>%
  group_by(real.town.name) %>%
  dplyr::summarise(Total=sum(Total))

colnames(hq4) <- c("id", "town.hqs")
comparing4 <- left_join(comparing3, hq4)

cor(comparing4$riskscore_raw, comparing4$town.hqs, use = "complete")

comparing4$town.hqs[is.na(comparing4$town.hqs)] <- 0
cor(comparing4$riskscore_raw, comparing4$town.hqs)

## cleaner towns served instances

tserv <- read.csv("data/towns_served.csv", stringsAsFactors=FALSE)
tserv$Towns <- str_to_title(tserv$Towns)
colnames(tserv) <- c("id", "towns.served")
comparing5 <- left_join(comparing4, tserv)

cor(comparing5$riskscore_raw, comparing5$towns.served, use = "complete")

comparing5$towns.served[is.na(comparing5$towns.served)] <- 0
cor(comparing5$riskscore_raw, comparing5$towns.served)

cor(comparing5$bll20.percent, comparing5$towns.served)

cor(comparing5$bll20.percent, comparing5$town.hqs)


served_count_map <- left_join(towntracts, comparing5)



dtm_served <- ggplot() +
  geom_polygon(data = served_count_map, aes(x=long, y=lat, group=group, fill=towns.served), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Blues", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Towns served", fill="")

dtm_hqs <- ggplot() +
  geom_polygon(data = served_count_map, aes(x=long, y=lat, group=group, fill=town.hqs), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Blues", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="System address", fill="")

grid.arrange(dtm_served,dtm_hqs, ncol=2)


## PPBS?? HQ

ppbs_towns <- ct %>%
  group_by(town.city) %>%
  summarize(median.ppb=median(Sample.Measure..mg.L.)*1000, average.ppb=mean(Sample.Measure..mg.L.)*1000)

ppbs_towns_clean <- ctnamecleaner(town.city, ppbs_towns)
ppbs_towns_clean <- ppbs_towns_clean[!is.na(ppbs_towns_clean$real.town.name),]

ppbs_towns_clean <- ppbs_towns_clean %>%
  group_by(real.town.name) %>%
  summarize(median.ppb=median(median.ppb), average.ppb=mean(average.ppb))

colnames(ppbs_towns_clean) <- c("id", "median.ppb.hq", "average.ppb.hq")
comparing6 <- left_join(comparing5, ppbs_towns_clean)

ppb_map_hq <- left_join(towntracts, comparing6)


dtm_median <- ggplot() +
  geom_polygon(data = ppb_map_hq, aes(x=long, y=lat, group=group, fill=median.ppb.hq), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Blues", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Median PPB by town", fill="")


dtm_avg <- ggplot() +
  geom_polygon(data = ppb_map_hq, aes(x=long, y=lat, group=group, fill=average.ppb.hq), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Blues", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Average PPB by town", fill="")

grid.arrange(dtm_median,dtm_avg, ncol=2)

cor(comparing6$riskscore_raw, comparing6$median.ppb.hq, use = "complete")
cor(comparing6$riskscore_raw, comparing6$average.ppb.hq, use = "complete")

comparing6$median.ppb.hq[is.na(comparing6$median.ppb.hq)] <- 0
comparing6$average.ppb.hq[is.na(comparing6$average.ppb.hq)] <- 0

cor(comparing6$riskscore_raw, comparing6$median.ppb.hq)
cor(comparing6$riskscore_raw, comparing6$average.ppb.hq)


## PPBS?? served

ppbs_served <- ct %>%
  group_by(Cities.Served) %>%
  summarize(median.ppb=median(Sample.Measure..mg.L.)*1000, average.ppb=mean(Sample.Measure..mg.L.)*1000)

ppbs_served_clean <- ctnamecleaner(Cities.Served, ppbs_served)
ppbs_served_clean <- ppbs_served_clean[!is.na(ppbs_served_clean$real.town.name),]

ppbs_served_clean <- ppbs_served_clean %>%
  group_by(real.town.name) %>%
  summarize(median.ppb=median(median.ppb), average.ppb=mean(average.ppb))

colnames(ppbs_served_clean) <- c("id", "median.ppb.served", "average.ppb.served")
comparing7 <- left_join(comparing6, ppbs_served_clean)

ppb_map_hq <- left_join(towntracts, comparing7)


dtm_median_served <- ggplot() +
  geom_polygon(data = ppb_map_hq, aes(x=long, y=lat, group=group, fill=median.ppb.served), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Blues", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Median PPB by town served", fill="")


dtm_avg_served <- ggplot() +
  geom_polygon(data = ppb_map_hq, aes(x=long, y=lat, group=group, fill=average.ppb.served), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Blues", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Average PPB by town served", fill="")

grid.arrange(dtm_median_served,dtm_avg_served, ncol=2)

cor(comparing7$riskscore_raw, comparing7$median.ppb.served, use = "complete")
cor(comparing7$riskscore_raw, comparing7$average.ppb.served, use = "complete")

comparing7$median.ppb.served[is.na(comparing7$median.ppb.served)] <- 0
comparing7$average.ppb.served[is.na(comparing7$average.ppb.served)] <- 0

cor(comparing7$riskscore_raw, comparing7$median.ppb.served)
cor(comparing7$riskscore_raw, comparing7$average.ppb.served)

## urban or rural?

designation <- read.csv("data/urban_or_rural.csv", stringsAsFactors=FALSE)
town_des <- designation[c("NAME10", "Type", "perc_urban")]
colnames(town_des) <-c("id", "Type", "perc_urban")

comparing8 <- left_join(comparing7, town_des)

big_df <- comparing8
big_df_0 <- big_df
big_df_0[is.na(big_df_0)] <- 0

big_df_na <- big_df
big_df_na[big_df_na==0] <- NA

raw_scatter1_0 <- big_df_0[c("id", "riskscore_raw", "bll5.percent")]
raw_scatter1_0 <- raw_scatter1_0 %>%
  gather("Type", "Score", 2:3)

p <- ggplot(big_df_na, aes(riskscore_raw, bll5.percent))
p + geom_point(aes(colour = factor(Type)))
cor(big_df_na$riskscore_raw, big_df_na$bll5.percent, use="complete")

p <- ggplot(big_df_na, aes(riskscore_raw, bll5))
p + geom_point(aes(colour = Type)) + labs(title = "Number of children with lead detected in blood versus risk score by town type", x = "Risk score", y="Children with lead detected in blood stream")
cor(big_df_0$riskscore_raw, big_df_0$bll5)

p <- ggplot(big_df_0, aes(bll5.percent, quantile))
p + geom_point(aes(colour = factor(Type)))
cor(big_df_0$quantile, big_df_0$bll5.percent)



p <- ggplot(big_df_na, aes(quantile, bll5.percent))
p + geom_point(aes(colour = factor(Type)))

library(corrgram)
corrgram(big_df_na,order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie)


#box plot

p <- ggplot(big_df_na, aes(factor(Type), quantile))
p + geom_boxplot()

p <- ggplot(big_df_0, aes(factor(Type), bll5.percent))
p + geom_boxplot()

p <- ggplot(big_df_na, aes(factor(Type), median.ppb.served))
p + geom_boxplot()


# final maps

big_df_na_map <- left_join(towntracts, big_df_na, by="id")

map1 <- ggplot() +
  geom_polygon(data =big_df_na_map, aes(x=long, y=lat, group=group, fill=riskscore_raw), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Blues", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Lead risk score", fill="")

map2 <- ggplot() +
  geom_polygon(data =big_df_na_map, aes(x=long, y=lat, group=group, fill=bll5), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Blues", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Lead-poisoned children by town (BLL 5 and up)", fill="")

map3 <- ggplot() +
  geom_polygon(data =big_df_na_map, aes(x=long, y=lat, group=group, fill=towns.served), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Blues", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Median PPB detected in drinking water systems", fill="")

map4 <- ggplot() +
  geom_polygon(data =big_df_na_map, aes(x=long, y=lat, group=group, fill=median.ppb.served), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Blues", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Frequency of elevated action lead levels detected in drinking water", fill="")

grid.arrange(map2, map1, map3, map4, ncol=2)
