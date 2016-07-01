library(readxl)
library(openxlsx)
stuns <- read_excel("2015 Reported Taser Data.xlsx", sheet=1)

stuns[1,] <- ifelse(is.na(stuns[1,]), colnames(stuns), stuns[1,])
colnames(stuns) <- stuns[1,]

stuns <- stuns[-1,]

colnames(stuns) <- make.names(colnames(stuns))

# another stream but for python data analysis going on right now:
# https://www.livecoding.tv/jakekara/

library(dplyr)

# Cleaning up the data

colnames(stuns) <- c("Law.Enforcement.Agency",
                     "Number.of.Incident.Reports",
                     "Incident.Case.Number",
                     "Date.of.Incident",
                     "Time.of.Incident",
                     "Sex",
                     "Race",
                     "Hispanic",
                     "Height",
                     "Weight",
                     "Age",
                     "Deployment.Type.I",
                     "Deployment.Type.II",
                     "Displays.or.Arc",
                     "Drive.Stun.Application",
                     "Activation.After.Probe.Contact",
                     "Number.of.Deployments",
                     "Warning.to.Subject",
                     "Subject.Injured",
                     "Subject.Not.Injured",
                     "Subject.Bruises",
                     "Subject.Abrasions",
                     "Subject.Breathing.Difficulty",
                     "Subject.Probe.Puncture.Only",
                     "Subject.Lost.Consciousness",
                     "Subject.Death",
                     "Subject.Other",
                     "Officer.Injured",
                     "Officer.Not.Injured",
                     "Officer.Bruises",
                     "Officer.Abrasions",
                     "Officer.Breathing.Difficulty",
                     "Officer.Probe.Puncture.Only",
                     "Officer.Lost.Consciousness",
                     "Officer.Death",
                     "Officer.Other",
                     "Location.Environment",
                     "Officer.s.Arrival",
                     "If.Other..Explain",
                     "Crime.in.Progress",
                     "Domestic.Disturbance",
                     "Disturbance..Other.",
                     "Traffic.Stop",
                     "Emotionally.Disturbed.Person",
                     "Suspicious.Person",
                     "Executing.Warrant",
                     "Under.Influence",
                     "Activity.Other",
                     "Non.aggressive",
                     "Previous.Hostility",
                     "Possibly.Intoxicated",
                     "Emotionally.Disturbed",
                     "Aggressive..Verbal.",
                     "Aggressive..Physical.",
                     "Armed.with",
                     "Officer.Perception.Other",
                     "Threat..Hostile",
                     "Dead.Weight..Non.Compliant",
                     "Fighting.Stance..Combative",
                     "Threaten.Use.of.Weapon",
                     "Fleeing",
                     "Unarmed.Assault",
                     "Armed.with.Firearm",
                     "Armed.with.Edged.Weapon",
                     "Armed.with.Blunt.Instrument",
                     "Armed.with.Other",
                     "Failed.to.Follow.Directions",
                     "Suicidal",
                     "Resistance.Other")

stuns$race_ethnicity <- ifelse(stuns$Hispanic==1, "Hispanic", stuns$Race)

# Total stun incidents by state
# Stun incidents by race in the state

by_state <- stuns %>%
  group_by(race_ethnicity) %>%
  summarise(total=n()) %>%
  mutate(percent=round(total/sum(total)*100,2))
  
library(knitr)
kable(by_state)
  
# Total stun incidents per department
# Stun incidents per department by race

library(tidyr)
library(DT)

by_dept_total <- stuns %>%
  group_by(Law.Enforcement.Agency, race_ethnicity) %>%
  summarise(total=n()) %>%
  spread(race_ethnicity, total)

datatable(by_dept_total)

by_dept_percent <- stuns %>%
  group_by(Law.Enforcement.Agency, race_ethnicity) %>%
  summarise(total=n()) %>%
  mutate(percent=round(total/sum(total)*100,2)) %>%
  select(Law.Enforcement.Agency, race_ethnicity, percent) %>%
  spread(race_ethnicity, percent)

datatable(by_dept_percent)


# Time of stun incidents

stuns$Time.of.Incident <- convertToDateTime(as.numeric(stuns$Time.of.Incident), origin = "2016-07-04")

library(lubridate)

stuns$hour <- hour(stuns$Time.of.Incident)

library(ggplot2)

ggplot(stuns, aes(hour)) + geom_histogram(binwidth=1)

# Time of stun incidents by race

ggplot(stuns, aes(hour, fill=race_ethnicity)) + geom_histogram(binwidth=1)

# Month

stuns$Date.of.Incident <- as.POSIXct(as.numeric(stuns$Date.of.Incident) * (60*60*24)
                                     , origin="1899-12-30"
                                     , tz="GMT")

stuns$month <- month(stuns$Date.of.Incident, label=TRUE)

ggplot(stuns, aes(month)) + geom_bar()


ggplot(stuns, aes(month)) + geom_bar() + facet_grid(race_ethnicity ~.)

# Day of the week

stuns$day <- wday(stuns$Date.of.Incident, label=TRUE)

ggplot(stuns, aes(day)) + geom_bar()

ggplot(stuns, aes(day)) + geom_bar() + facet_grid(race_ethnicity ~.)

# Deaths

death <- subset(stuns, Subject.Death==1)

## Mapping

require(scales)
require(dplyr)
require(gtools)
require(ggplot2)
require(rgdal)
require(ggmap)
require(Cairo)
require(gpclib)
require(maptools)
require(reshape)
library(devtools)
library(stringr)
library(raster)
library(sp)


by_dept_total[is.na(by_dept_total)] <-0 
by_dept_total$total <- by_dept_total$Asian + by_dept_total$Black + by_dept_total$Hispanic + by_dept_total$Unknown + by_dept_total$White
  
gpclibPermit()
gpclibPermitStatus()
townborders <- readOGR(dsn="maps", layer="ctgeo")
townborders_only <- townborders
townborders<- fortify(townborders, region="NAME10")

names(by_dept_total)[names(by_dept_total) == 'Law.Enforcement.Agency'] <- 'id'


total_map <- left_join(townborders, by_dept_total)

tm_ct <- ggplot() +
  geom_polygon(data = total_map, aes(x=long, y=lat, group=group, fill=total), color = "black", size=0.2) +
  geom_polygon(data = total_map, aes(x=long, y=lat, group=group, fill=total), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Where people are tased (total)", fill="")

print(tm_ct)

# percent map

# White
names(by_dept_percent)[names(by_dept_percent) == 'Law.Enforcement.Agency'] <- 'id'


percent_map <- left_join(townborders, by_dept_percent)

tm_ct <- ggplot() +
  geom_polygon(data = percent_map, aes(x=long, y=lat, group=group, fill=White), color = "black", size=0.2) +
  geom_polygon(data = percent_map, aes(x=long, y=lat, group=group, fill=White), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Percent of white suspects tased", fill="")

print(tm_ct)


# Black

tm_ct <- ggplot() +
  geom_polygon(data = percent_map, aes(x=long, y=lat, group=group, fill=Black), color = "black", size=0.2) +
  geom_polygon(data = percent_map, aes(x=long, y=lat, group=group, fill=Black), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Percent of Black suspects tased", fill="")

print(tm_ct)


# Hispanic
tm_ct <- ggplot() +
  geom_polygon(data = percent_map, aes(x=long, y=lat, group=group, fill=Hispanic), color = "black", size=0.2) +
  geom_polygon(data = percent_map, aes(x=long, y=lat, group=group, fill=Hispanic), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n=10)) +
  theme_nothing(legend=TRUE) +
  labs(title="Percent of Hispanic suspects tased", fill="")

print(tm_ct)


# breakdown between men and women

gender <- stuns %>%
  group_by(Sex) %>%
  summarise(Count=n()) %>%
  mutate(Percent=round(Count/sum(Count)*100,2))

kable(gender)

# Median age

median(as.numeric(stuns$Age), na.rm=T)

stuns$Age <- as.numeric(stuns$Age)

# Age distribution
ggplot(stuns, aes(Age)) + geom_histogram(binwidth=1)
ggplot(stuns, aes(Age)) + geom_histogram(binwidth=10)

ggplot(stuns, aes(Age)) + geom_histogram(binwidth=5) + facet_grid(race_ethnicity ~ .)

# Where does this happen?

locations <- stuns %>%
  group_by(Location.Environment) %>%
  summarise(count=n()) %>%
  mutate(percent=round(count/sum(count)*100,2))

kable(locations)

ggplot(stuns, aes(Location.Environment)) + facet_grid(race_ethnicity ~.) + geom_bar() + coord_flip()

# What is the subject holding

stuns$Armed.with <- str_to_title(stuns$Armed.with)
weapon <- stuns %>%
  group_by(Armed.with) %>%
  summarise(count=n()) %>%
  arrange(-count)

weapon <- subset(weapon, !is.na(Armed.with))
kable(weapon)

# How often tased

stuns$Number.of.Deployments <- as.numeric(stuns$Number.of.Deployments)

deployed <- stuns %>%
  group_by(race_ethnicity) %>%
  summarise(mean(Number.of.Deployments, na.rm=T))

# Doesn't say much, let's check out another factor

stuns$deployment <- ifelse(stuns$Number.of.Deployments>1, "multiple", "single")
stuns$deployment <- ifelse(stuns$Number.of.Deployments==0, "none", stuns$deployment)

deployed <- stuns %>%
  group_by(race_ethnicity, deployment) %>%
  summarise(count=n()) %>%
  mutate(percent=round(count/sum(count)*100,2)) 

deployed <- deployed[c("race_ethnicity", "deployment", "percent")]

deployed <- subset(deployed, !is.na(deployment))

deployed <- deployed %>%
  spread(deployment, percent)

kable(deployed)
