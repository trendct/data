require(lubridate)
require(stringi)
require(gridExtra)

#raw <- read.csv("drugsentencing.csv")

raw <- read.csv("sentences.csv")

#Assigning race
raw$RaceOf <- paste(raw$Race, raw$Hispanic, sep=",")
index <- c("", "A", "B", "BY", "C", "CY", "HY")
values <- c("Unlisted", "Asian", "Black", "Hispanic", "Caucasian", "Hispanic", "Hispanic")
raw$RaceOfConvicted <- values[match(raw$RaceOf, index)]
raw$RaceOfConvicted <- gsub("Caucasian", "White", raw$RaceOfConvicted)

#Finding race stats overall
race_table <- data.frame(table(raw$RaceOfConvicted))
colnames(race_table) <- c("Race", "Convictions")
race_table$Percent <- (race_table$Convictions/sum(race_table$Convictions)*100)
race_table$Percent <- round(race_table$percent, digits=2)

#Cleaning up the dates
raw$Date <- mdy(raw$Sentenced)
raw$Year <- year(raw$Date)

#Arrests by year (Total)
year_arrests <- data.frame(table(raw$Year))
colnames(year_arrests) <- c("Year", "Total.Convictions")
year_arrests$Year <- as.numeric(year_arrests$Year)
plot(main="Total Convictions", year_arrests, type="o", col="blue")

#Arrests by year by race
year_convictions_race <- table(raw$Year, raw$RaceOfConvicted)
#write.csv(year_convictions_race, "racetally.csv")
year_convictions_race <- data.frame(year_convictions_race)
colnames(year_convictions_race) <- c("Year", "Race", "Convictions")
plot(year_convictions_race)



# Subset based on Finding results
convictions_year <- data.frame(table(raw$Year,raw$Final.Statute))
colnames(convictions_year) <- c("Year", "Statute", "Convictions")

ggplot(convictions_year, aes(Year, Convictions, group=Statute, fill=Statute)) + geom_area(position="fill")

# Subset based on arresting agency
juris_conv <- data.frame(table(raw$Police.Name))
colnames(juris_conv) <- c("Department", "Convictions.Total")
juris_conv <- juris_conv[order(-juris_conv$Convictions.Total),]

# subset out town police departments vs the rest
towns_only <- juris_conv[grepl("LOCAL POLICE", juris_conv$Department),]
not_towns <- juris_conv[grep("LOCAL POLICE", juris_conv$Department, invert=TRUE),]

csp_only <- juris_conv[grepl("CSP TROOP", juris_conv$Department),]
csp_only$Department <- as.character(csp_only$Department)
csp_only$Department <- gsub("CSP TROOP ", "", csp_only$Department)
colnames(csp_only) <- c("id", "Convictions")

towns_only$Department <- as.character(towns_only$Department)
towns_only$Department <- gsub("LOCAL POLICE ", "", towns_only$Department)
colnames(towns_only) <- c("id", "Convictions")

#bring in town populations
townpop <- read.csv("townsmap/townpopulation.csv")
colnames(townpop) <- c("id","Population")
townpop$id <- toupper(townpop$id)

towns_only <- left_join(townpop, towns_only)
towns_only$Convictions [is.na(towns_only$Convictions)] <-0
towns_only$id <- stri_trans_general(towns_only$id, id="Title")
towns_only$Rate <- (towns_only$Convictions/towns_only$Population)*10000
towns_only$Rate <- round(towns_only$Rate, digits=2)
towns_only <- towns_only[order(-towns_only$Rate),]

#OK, let's shapefile it up
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


#Connecticut State Police 
tract <- readOGR(dsn="CSPMap", layer="CSPJurisdictions")
tract <- fortify(tract, region="Troop")

plotData <- left_join(tract, csp_only)

p <- ggplot() +
  geom_polygon(data = plotData, aes(x=long, y=lat, group=group, 
                                    fill=Convictions), color = "black", size=0.25) +
  coord_map() +
  scale_fill_distiller(palette = "Greens",
                       breaks = pretty_breaks(n = 10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Number of convictions from Connecticut State Troopers", fill="")
ggsave(p, file="map1.png", width=8, height=4, type="cairo-png")


# Town police (total convictions)

towntracts <- readOGR(dsn="townsmap", layer="towns")
towntracts <- fortify(towntracts, region="NAME10")

townData <- left_join(towntracts, towns_only)


p2 <- ggplot() +
  geom_polygon(data = townData, aes(x=long, y=lat, group=group, 
                                    fill=Convictions), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", palette = "Greens", breaks=pretty_breaks(n=5)) +
  theme_nothing(legend=TRUE) +
  labs(title="Number of convictions from city police departments", fill="")
ggsave(p2, file="map2.png", width=8, height=4, type="cairo-png")

# Town police (convictions per 10,000 people)

towntracts <- readOGR(dsn="townsmap", layer="towns")
towntracts <- fortify(towntracts, region="NAME10")

townData <- left_join(towntracts, towns_only)

p2 <- ggplot() +
  geom_polygon(data = townData, aes(x=long, y=lat, group=group, 
                                    fill=Rate), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", palette = "Greens", breaks=pretty_breaks(n=5)) +
  theme_nothing(legend=TRUE) +
  labs(title="Convictions from city police departments per 10,000 residents", fill="")
ggsave(p2, file="map3.png", width=8, height=4, type="cairo-png")
---
  
# ok, let's look at specific convictions
  
table(raw$Final.Statute)

conv_c <- subset(raw, Final.Statute=="21a-267(c)")
conv_b <- subset(raw, Final.Statute=="21a-278a(b)")
conv_d <- subset(raw, Final.Statute=="21a-279(d)")

#doing everything above but for each subset now

c_conv <- data.frame(table(conv_c$Year))
colnames(c_conv) <- c("Year", "C.Convictions")
c_conv$Year <- as.numeric(as.character(c_conv$Year))
b_conv <- data.frame(table(conv_b$Year))
colnames(b_conv) <- c("Year", "B.Convictions")
b_conv$Year <- as.numeric(as.character(b_conv$Year))
d_conv <- data.frame(table(conv_d$Year))
colnames(d_conv) <- c("Year", "D.Convictions")
d_conv$Year <- as.numeric(as.character(d_conv$Year))

conv_all <- left_join(c_conv, b_conv)
conv_all <- left_join(conv_all, d_conv)

meltedJoinsByYear <- melt(conv_all, id="Year")
colnames(meltedJoinsByYear) <- c("Year", "Final.Statute", "Convictions")

ggplot(meltedJoinsByYear, aes(x=Year, y=Convictions, colour=Final.Statute)) +
  geom_line() +
  ylab(label="Convictions") +
  xlab(label="Year") +
  scale_colour_manual(values=c("blue","red", "green"))


#Convictions by race (timeline charts)

c_convictions_race <- table(conv_c$Year, conv_c$RaceOfConvicted)
c_convictions_race <- data.frame(c_convictions_race)
colnames(c_convictions_race) <- c("Year", "Race", "Convictions")
ggplot(c_convictions_race, aes(Year, Convictions, group=Race, colour=Race)) +
  geom_path(alpha=0.5) +
  ggtitle("Race of those convicted for Delivering by year") +
  theme_minimal()


b_convictions_race <- table(conv_b$Year, conv_b$RaceOfConvicted)
b_convictions_race <- data.frame(b_convictions_race)
colnames(b_convictions_race) <- c("Year", "Race", "Convictions")
ggplot(b_convictions_race, aes(Year, Convictions, group=Race, colour=Race)) +
  geom_path(alpha=0.5) +
  ggtitle("Race of those convicted for Selling by year") +
  theme_minimal()

d_convictions_race <- table(conv_d$Year, conv_d$RaceOfConvicted)
d_convictions_race <- data.frame(d_convictions_race)
colnames(d_convictions_race) <- c("Year", "Race", "Convictions")
ggplot(d_convictions_race, aes(Year, Convictions, group=Race, colour=Race)) +
  geom_path(alpha=0.5) +
ggtitle("Race of those convicted for Possession by year") +
  theme_minimal()

year_convictions_race <- table(raw$Year, raw$RaceOfConvicted)
#write.csv(year_convictions_race, "racetally.csv")
year_convictions_race <- data.frame(year_convictions_race)
colnames(year_convictions_race) <- c("Year", "Race", "Convictions")
plot(year_convictions_race)



FindingGuilty <- subset(raw, Finding=="Guilty")
FindingDismiss <- subset(raw, Finding=="Dismiss")
FindingNolle <- subset(raw, Finding=="Nolle")

table(FindingGuilty$Race)
table(FindingDismiss$Race)
table(FindingNolle$Race)

tapply(raw$Effective.Sentence, raw$Race, mean)
tapply(raw$Effective.Sentence, raw$Race, median)

raw0 <- subset(raw, Effective.Sentence!=0)
tapply(raw0$Effective.Sentence, raw0$Race, mean)
tapply(raw0$Effective.Sentence, raw0$Race, median)

Asians <- subset(raw, Race=="Asian")


mean(testvec[testvec != 0]) 

table(raw$Offense.Town, raw$Race)
table(FindingGuilty$Offense.Town, FindingGuilty$Race)


table(FindingGuilty$Offense.Town, FindingGuilty$Race)
table(FindingDismiss$Offense.Town, FindingDismiss$Race)
table(FindingNolle$Offense.Town, FindingNolle$Race)


#dates

raw$Finding.Date.Date <- dmy(raw$Finding.Date)
raw$D.O.B.Date <- dmy(raw$D.O.B.)

foo <- function(x, year=1968){
  m <- year(x) %% 100
  year(x) <- ifelse(m > year %% 100, 1900+m, 2000+m)
  x
}

raw$Interval <- interval(raw$Finding.Date.Date, raw$D.O.B.Date)


raw$default.inteval <- interval(raw$D.O.B.Date, raw$default)

raw$age <- raw$Interval/eyears(1)

raw$age.all <- abs(raw$age.all)

raw$age.all <- raw$default.inteval/eyears(1)


tapply(raw$age.all, raw$Race, mean,na.rm=T)

tapply(raw$age.all, raw$Race, median,na.rm=T)

tapply(FindingGuilty$age.all, FindingGuilty$Race, median,na.rm=T)
tapply(FindingGuilty$age.all, FindingGuilty$Race, mean,na.rm=T)

table(raw$Original.Statute)

statuteA <- subset(raw, Original.Statute=="21a-277(a)" | 
                     Original.Statute=="21a-277(a)+" | 
                     Original.Statute=="")
statuteC <- subset(raw, )

#ok new arrests

y2011 <- read.csv("arrests2011.csv")

BigTowns <- subset(y2011, TX_OFFENSE_TOWN_DESC=="HARTFORD" | 
                     TX_OFFENSE_TOWN_DESC=="BRIDGEPORT" | 
                     TX_OFFENSE_TOWN_DESC=="NEW HAVEN" | 
                     TX_OFFENSE_TOWN_DESC=="WEST HAVEN" | 
                     TX_OFFENSE_TOWN_DESC=="WATERBURY" | 
                     TX_OFFENSE_TOWN_DESC=="NORWALK" | 
                     TX_OFFENSE_TOWN_DESC=="ANSONIA" | 
                     TX_OFFENSE_TOWN_DESC=="STAMFORD" | 
                     TX_OFFENSE_TOWN_DESC=="NEW BRITAIN")

y2011 <- subset(y2011, CD_STATUTE_ID_VALUE!="21a-267(c)")

y2011b <- subset(y2011, CD_STATUTE_ID_VALUE=="21a-278a(b)")
y2011c <- subset(y2011, CD_STATUTE_ID_VALUE=="21a-267(c)")
y2011d <- subset(y2011, CD_STATUTE_ID_VALUE=="21a-279(d)")

write.csv(table(y2011$RACE), "race2011.csv")
write.csv(table(y2011$RACE2), "race2011-2.csv")

write.csv(table(y2011b$RACE2), "race2011b-2.csv")
write.csv(table(y2011c$RACE2), "race2011c-2.csv")
write.csv(table(y2011d$RACE2), "race2011d-2.csv")

write.csv(table(BigTowns$TX_OFFENSE_TOWN_DESC, BigTowns$RACE2), "BigRaceTowns.csv")
BigTownsb <- subset(BigTowns, CD_STATUTE_ID_VALUE=="21a-278a(b)")
BigTownsc <- subset(BigTowns, CD_STATUTE_ID_VALUE=="21a-267(c)")
BigTownsd <- subset(BigTowns, CD_STATUTE_ID_VALUE=="21a-279(d)")

write.csv(table(BigTownsb$TX_OFFENSE_TOWN_DESC, BigTownsb$RACE2), "BT2011b-2.csv")
write.csv(table(BigTownsc$TX_OFFENSE_TOWN_DESC, BigTownsc$RACE2), "BT2011c-2.csv")
write.csv(table(BigTownsd$TX_OFFENSE_TOWN_DESC, BigTownsd$RACE2), "BT2011d-2.csv")

# ok, circling back to 2008...

big2 <- subset(raw, Original.Statute=="21a-278(b)" |
                 Original.Statute=="21a-278(b)*" | 
                 Original.Statute=="21a-278(b)*+" |                  
                 Original.Statute=="21a-278(b)+" | 
                 Original.Statute=="21a-279(d)"
                 )
write.csv(big2, "big2.csv")
big2 <- read.csv("big2.csv")
write.csv(table(big2$Race), "race2008.csv")
write.csv(table(big2$Original.Statute, big2$Race), "BigRaceTowns2008.csv")

big2b <- subset(big2, Original.Statute=="21a-278(b)")
big2d <- subset(big2, Original.Statute=="21a-279(d)")

write.csv(table(big2b$Offense.Town, big2b$Race), "bigb.csv")

write.csv(table(big2d$Offense.Town, big2d$Race), "bigd.csv")


## BLAH
raw$Date <- mdy(raw$Sentenced)
raw$Year <- year(raw$Date)

raw$Police.Name <- as.character(raw$Police.Name)
towns_subset <- raw[grepl("LOCAL POLICE", raw$Police.Name),]
towns_subset$Police.Name <- gsub("LOCAL POLICE ", "", towns_subset$Police.Name)

urban_convictions <- subset(towns_subset, (Police.Name=="BRIDGEPORT" | Police.Name=="HARTFORD" | Police.Name=="NEW HAVEN" |
                               Police.Name=="NEW BRITAIN" | Police.Name=="WEST HAVEN" | Police.Name=="NEW LONDON" |
                               Police.Name=="WATERBURY" | Police.Name=="NORWALK" | Police.Name=="WATERBURY" |
                               Police.Name=="NORWALK" | Police.Name=="ANSONIA" | Police.Name=="STAMFORD"))

suburban_convictions <- subset(towns_subset, !(Police.Name=="BRIDGEPORT" | Police.Name=="HARTFORD" | Police.Name=="NEW HAVEN" |
                                    Police.Name=="NEW BRITAIN" | Police.Name=="WEST HAVEN" | Police.Name=="NEW LONDON" |
                                    Police.Name=="WATERBURY" | Police.Name=="NORWALK" | Police.Name=="WATERBURY" |
                                    Police.Name=="NORWALK" | Police.Name=="ANSONIA" | Police.Name=="STAMFORD"))

urban_conv_statute <- data.frame(table(urban_convictions$Year, urban_convictions$Final.Statute))
colnames(urban_conv_statute) <- c("Year", "Statute", "Convictions")

ucs1 <- ggplot(urban_conv_statute, aes(Year, Convictions, group=Statute, colour=Statute)) +
  geom_path(alpha=0.5) +
  ggtitle("Total") +
  theme(legend.position="top")

ucs2 <- ggplot(urban_conv_statute, aes(Year, Convictions, group=Statute, fill=Statute)) + geom_area(position="fill") + 
  ggtitle("Percent")  +
  theme(legend.position="top")

grid.arrange(ucs1, ucs2, ncol=2, main="Types of convictions in Urban towns by year")

suburban_conv_statute <- data.frame(table(suburban_convictions$Year, suburban_convictions$Final.Statute))
colnames(suburban_conv_statute) <- c("Year", "Statute", "Convictions")

scs1 <- ggplot(suburban_conv_statute, aes(Year, Convictions, group=Statute, colour=Statute)) +
  geom_path(alpha=0.5) +
  ggtitle("Total") +
  theme(legend.position="top")

scs2 <- ggplot(suburban_conv_statute, aes(Year, Convictions, group=Statute, fill=Statute)) + geom_area(position="fill") + 
  ggtitle("Percent")  +
  theme(legend.position="top")

grid.arrange(scs1, scs2, ncol=2, main="Types of convictions in Suburban towns by year")


urban_conv_race <- data.frame(table(urban_convictions$Year, urban_convictions$RaceOfConvicted))
colnames(urban_conv_race) <- c("Year", "Race", "Convictions")

ucr1 <- ggplot(urban_conv_race, aes(Year, Convictions, group=Race, colour=Race)) +
  geom_path(alpha=0.5) +
  ggtitle("Total") +
  theme(legend.position="top")

ucr2 <- ggplot(urban_conv_race, aes(Year, Convictions, group=Race, fill=Race)) + geom_area(position="fill") + 
  ggtitle("Percent")  +
  theme(legend.position="top")

grid.arrange(ucr1, ucr2, ncol=2, main="Race of those convicted in Urban towns by year")

suburban_conv_race <- data.frame(table(suburban_convictions$Year, suburban_convictions$RaceOfConvicted))
colnames(suburban_conv_race) <- c("Year", "Race", "Convictions")

scr1 <- ggplot(suburban_conv_race, aes(Year, Convictions, group=Race, colour=Race)) +
  geom_path(alpha=0.5) +
  ggtitle("Total") +
  theme(legend.position="top")

scr2 <- ggplot(suburban_conv_race, aes(Year, Convictions, group=Race, fill=Race)) + geom_area(position="fill") + 
  ggtitle("Percent")  +
  theme(legend.position="top")

grid.arrange(scr1, scr2, ncol=2, main="Race of those convicted in Suburban towns by year")