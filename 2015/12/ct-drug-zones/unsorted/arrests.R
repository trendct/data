require(lubridate)


#arrests <- read.csv("arrests.csv")
arrests <- read.csv("arrests_h.csv")

#Clean up races
#arrests$RaceOf <- paste(arrests$RACE, arrests$DEF_HISPANIC_IND, sep="")

#index <- c("Asian", "AsianY", "Black", "BlackY", "HispY", "Native American", 
           "Native AmericanY", "Not Identified", "White", "WhiteY")


#values <- c("Asian", "Hispanic", "Black", "Hispanic", "Hispanic", "Native American", 
            "Hispanic", "Not Identified", "White", "Hispanic")
#arrests$Race <- values[match(arrests$RaceOf, index)]


# Clean up the dates
arrests$Date <- dmy(arrests$ARREST.DATE)
arrests$Year <- year(arrests$Date)
arrests$Year[arrests$Year==2099] <- 1999

# Plot out year
plot(table(arrests$Year))

# Racial breakdown of arrests by year
race_year <- data.frame(table(arrests$Year,arrests$RACE))
colnames(race_year) <- c("Year", "Race", "Arrests")
ggplot(race_year, aes(Year, Arrests, group=Race, colour=Race)) +
  geom_path(alpha=0.5) +
  ggtitle("Race of those arrested since 1999") +
  theme_minimal()

# And by percent
ggplot(race_year, aes(Year, Arrests, group=Race, fill=Race)) + geom_area(position="fill")

# Which towns have the most arrests?
town_arrests <- data.frame(table(arrests$Town))
colnames(town_arrests) <- c("id", "Total.Arrests")
town_arrests <- town_arrests[order(-town_arrests$Total.Arrests),]
head(town_arrests, 10)

# Bring in town population
townpop <- read.csv("townsmap/townpopulation.csv")
colnames(townpop) <- c("id","Population")
townpop$id <- toupper(townpop$id)

# Have to make adjustments to neighborhood names to match town names

town_arrests$id <- gsub("BANTAM", "LITCHFIELD", town_arrests$id)
town_arrests$id <- gsub("BELLE HAVEN", "GREENWICH", town_arrests$id)
town_arrests$id <- gsub("BRANCHVILLE", "RIDGEFIELD", town_arrests$id)
town_arrests$id <- gsub("CANNONDALE", "WILTON", town_arrests$id)
town_arrests$id <- gsub("CENTERVILLE", "HAMDEN", town_arrests$id)
town_arrests$id <- gsub("CHESHIRE HEIGHTS", "CHESHIRE", town_arrests$id)
town_arrests$id <- gsub("DANIELSON", "WINDHAM", town_arrests$id)
town_arrests$id <- gsub("DOBSONVILLE", "VERNON", town_arrests$id)
town_arrests$id <- gsub("EAST SUFFIELD", "SUFFIELD", town_arrests$id)
town_arrests$id <- gsub("FITCHVILLE", "BOZRAH", town_arrests$id)
town_arrests$id <- gsub("GOSHEN CENTER", "GOSHEN", town_arrests$id)
town_arrests$id <- gsub("GROTON CITY OF", "GROTON", town_arrests$id)
town_arrests$id <- gsub("JEWETT CITY", "GRISWOLD", town_arrests$id)
town_arrests$id <- gsub("MOOSUP", "PLAINFIELD", town_arrests$id)
town_arrests$id <- gsub("NORTH COLEBROOK", "COLEBROOK", town_arrests$id)
town_arrests$id <- gsub("OAKVILLE", "WATERTOWN", town_arrests$id)
town_arrests$id <- gsub("POQUONOCK", "WINDSOR", town_arrests$id)
town_arrests$id <- gsub("PUTNAM HEIGHTS", "PUTNAM", town_arrests$id)
town_arrests$id <- gsub("ROCKVILLE", "VERNON", town_arrests$id)
town_arrests$id <- gsub("SOUTH CANAAN", "CANAAN", town_arrests$id)
town_arrests$id <- gsub("STORRS", "MANSFIELD", town_arrests$id)
town_arrests$id <- gsub("UNCASVILLE", "MONTVILLE", town_arrests$id)
town_arrests$id <- gsub("WAREHOUSE POINT", "WINDSORVILLE", town_arrests$id)
town_arrests$id <- gsub("WEST MORRIS", "LITCHFIELD", town_arrests$id)

town_arrests <- data.frame(tapply(town_arrests$Total.Arrests, town_arrests$id, sum))
town_arrests$id <- row.names(town_arrests)
colnames(town_arrests) <- c("Total.Arrests", "id")

towns_arrests_pop <- left_join(townpop, town_arrests)
towns_arrests_pop$Total.Arrests[is.na(towns_arrests_pop$Total.Arrests)] <-0
towns_arrests_pop$id <- stri_trans_general(towns_arrests_pop$id, id="Title")
towns_arrests_pop$Per10kResidents <- (towns_arrests_pop$Total.Arrests/towns_arrests_pop$Population)*10000
towns_arrests_pop$Per10kResidents <- round(towns_arrests_pop$Per10kResidents, digits=2)
towns_arrests_pop <- towns_arrests_pop[order(-towns_arrests_pop$Per10kResidents),]


head(towns_arrests_pop, 10)

# Wait, Ansonia? Really?

#ok, map it
require(gtools)
require(ggplot2)
require(rgdal)
require(scales)
require(ggmap)
require(dplyr)
require(Cairo)
require(gpclib)
require(maptools)
require(reshape)
gpclibPermit()
gpclibPermitStatus()

towntracts <- readOGR(dsn="townsmap", layer="towns")
towntracts <- fortify(towntracts, region="NAME10")

town_arr_Data <- left_join(towntracts, towns_arrests_pop)

p2 <- ggplot() +
  geom_polygon(data = town_arr_Data, aes(x=long, y=lat, group=group, 
                                    fill=Per10kResidents), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", palette = "Greens", breaks=pretty_breaks(n=5)) +
  theme_nothing(legend=TRUE) +
  labs(title="Arrests between 1999 and 2014 per 10,000 residents", fill="")
ggsave(p2, file="map4.png", width=8, height=4, type="cairo-png")

# OK, types of arrests by charge

charge_year <- data.frame(table(arrests$Year,arrests$ORIGINAL.STATUTE))
colnames(charge_year) <- c("Year", "Charge", "Arrests")
ggplot(charge_year, aes(Year, Arrests, group=Charge, colour=Charge)) +
  geom_path(alpha=0.5) +
  ggtitle("Types of charges since 1999") +
  theme_minimal()

# Percent of arrests by year
ggplot(charge_year, aes(Year, Arrests, group=Charge, fill=Charge)) + geom_area(position="fill")

# OK, all of the above but specifically at D

just_d <- subset(arrests, ORIGINAL.STATUTE=="21a-279(d)")
plot(table(just_d$Year))

race_year_d <- data.frame(table(just_d$Year,just_d$RACE))
colnames(race_year_d) <- c("Year", "Race", "Arrests")
ggplot(race_year_d, aes(Year, Arrests, group=Race, colour=Race)) +
  geom_path(alpha=0.5) +
  ggtitle("Race of those arrested for Possession since 1999") +
  theme_minimal()

# And by percent
ggplot(race_year_d, aes(Year, Arrests, group=Race, fill=Race)) + geom_area(position="fill")

# Which towns have the most arrests?
town_arrests_d <- data.frame(table(just_d$Town))
colnames(town_arrests_d) <- c("id", "Total.Arrests")
town_arrests_d <- town_arrests_d[order(-town_arrests_d$Total.Arrests),]
head(town_arrests_d, 10)


town_arrests_d$id <- gsub("BANTAM", "LITCHFIELD", town_arrests_d$id)
town_arrests_d$id <- gsub("BELLE HAVEN", "GREENWICH", town_arrests_d$id)
town_arrests_d$id <- gsub("BRANCHVILLE", "RIDGEFIELD", town_arrests_d$id)
town_arrests_d$id <- gsub("CANNONDALE", "WILTON", town_arrests_d$id)
town_arrests_d$id <- gsub("CENTERVILLE", "HAMDEN", town_arrests_d$id)
town_arrests_d$id <- gsub("CHESHIRE HEIGHTS", "CHESHIRE", town_arrests_d$id)
town_arrests_d$id <- gsub("DANIELSON", "WINDHAM", town_arrests_d$id)
town_arrests_d$id <- gsub("DOBSONVILLE", "VERNON", town_arrests_d$id)
town_arrests_d$id <- gsub("EAST SUFFIELD", "SUFFIELD", town_arrests_d$id)
town_arrests_d$id <- gsub("FITCHVILLE", "BOZRAH", town_arrests_d$id)
town_arrests_d$id <- gsub("GOSHEN CENTER", "GOSHEN", town_arrests_d$id)
town_arrests_d$id <- gsub("GROTON CITY OF", "GROTON", town_arrests_d$id)
town_arrests_d$id <- gsub("JEWETT CITY", "GRISWOLD", town_arrests_d$id)
town_arrests_d$id <- gsub("MOOSUP", "PLAINFIELD", town_arrests_d$id)
town_arrests_d$id <- gsub("NORTH COLEBROOK", "COLEBROOK", town_arrests_d$id)
town_arrests_d$id <- gsub("OAKVILLE", "WATERTOWN", town_arrests_d$id)
town_arrests_d$id <- gsub("POQUONOCK", "WINDSOR", town_arrests_d$id)
town_arrests_d$id <- gsub("PUTNAM HEIGHTS", "PUTNAM", town_arrests_d$id)
town_arrests_d$id <- gsub("ROCKVILLE", "VERNON", town_arrests_d$id)
town_arrests_d$id <- gsub("SOUTH CANAAN", "CANAAN", town_arrests_d$id)
town_arrests_d$id <- gsub("STORRS", "MANSFIELD", town_arrests_d$id)
town_arrests_d$id <- gsub("UNCASVILLE", "MONTVILLE", town_arrests_d$id)
town_arrests_d$id <- gsub("WAREHOUSE POINT", "WINDSORVILLE", town_arrests_d$id)
town_arrests_d$id <- gsub("WEST MORRIS", "LITCHFIELD", town_arrests_d$id)

town_arrests_d <- data.frame(tapply(town_arrests_d$Total.Arrests, town_arrests_d$id, sum))

town_arrests_d$id <- row.names(town_arrests_d)
colnames(town_arrests_d) <- c("Total.Arrests", "id")

towns_arrests_pop_d <- left_join(townpop, town_arrests_d)
towns_arrests_pop_d$Total.Arrests[is.na(towns_arrests_pop_d$Total.Arrests)] <-0
towns_arrests_pop_d$id <- stri_trans_general(towns_arrests_pop_d$id, id="Title")
towns_arrests_pop_d$Per10kResidents <- (towns_arrests_pop_d$Total.Arrests/towns_arrests_pop_d$Population)*10000
towns_arrests_pop_d$Per10kResidents <- round(towns_arrests_pop_d$Per10kResidents, digits=2)
towns_arrests_pop_d <- towns_arrests_pop_d[order(-towns_arrests_pop_d$Per10kResidents),]

head(towns_arrests_pop_d, 10)

town_arr_Data_d <- left_join(towntracts, towns_arrests_pop_d)

p2 <- ggplot() +
  geom_polygon(data = town_arr_Data_d, aes(x=long, y=lat, group=group, 
                                         fill=Per10kResidents), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", palette = "Greens", breaks=pretty_breaks(n=5)) +
  theme_nothing(legend=TRUE) +
  labs(title="Possession arrests between 1999 and 2014 per 10,000 residents", fill="")
ggsave(p2, file="map5.png", width=8, height=4, type="cairo-png")

# Ok, let's look specifically at New Haven, Hartford, Waterbury, Ansonia, Manchseter...


