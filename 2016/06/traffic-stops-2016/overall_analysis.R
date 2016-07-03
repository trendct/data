# This script was early on in the process and just does a quick time summary of the traffic stop data

library(lubridate)
library(dplyr)
library(stringr)
library(tidyr)
library(ggalt)
library(ggplot2)

# Most traffic stops

stops <- read.csv("data/stops_fixed.csv")
stops$sunset <- NULL
stops$solarnoon <- NULL
stops$sunrise <- NULL
stops$RealTime2 <- as.character(stops$RealTime2)
# Adust for race


stops$race.ethnicity <- paste0(stops$Subject.Ethnicity.Code, stops$Subject.Race.Code)

stops$race.ethnicity <- gsub("HA", "Hispanic", stops$race.ethnicity)
stops$race.ethnicity <- gsub("HB", "Hispanic", stops$race.ethnicity)
stops$race.ethnicity <- gsub("HI", "Hispanic", stops$race.ethnicity)
stops$race.ethnicity <- gsub("HW", "Hispanic", stops$race.ethnicity)
stops$race.ethnicity <- gsub("MA", "Middle-Eastern", stops$race.ethnicity)
stops$race.ethnicity <- gsub("MB", "Middle-Eastern", stops$race.ethnicity)
stops$race.ethnicity <- gsub("MI", "Middle-Eastern", stops$race.ethnicity)
stops$race.ethnicity <- gsub("MW", "Middle-Eastern", stops$race.ethnicity)
stops$race.ethnicity <- gsub("NA", "Asian", stops$race.ethnicity)
stops$race.ethnicity <- gsub("NB", "Black", stops$race.ethnicity)
stops$race.ethnicity <- gsub("NI", "Indian", stops$race.ethnicity)
stops$race.ethnicity <- gsub("NW", "White", stops$race.ethnicity)


# Chart 1: Aggregate Traffic Stops by Month of the Year

stops$month <- month(stops$ForRealTime, label=TRUE, abbr = TRUE)
stops_months_race <- stops %>%
  group_by(month, race.ethnicity) %>%
  summarise(stops=n())

p <- ggplot(stops_months_race, aes(x=month, y=stops))
p <- p + geom_bar(stat = "identity") 
p <- p +  facet_wrap(~ race.ethnicity) 
p <- p + theme_minimal(base_family="Arial Narrow")
p <- p + theme(plot.title=element_text(face="bold"))
p <- p + labs(x="Month", y="Traffic stops", 
              title="Aggregate traffic stops by month of the year",
              subtitle="Between October 30, 2013 and September 1, 2014",
              caption="Traffic Stop Data Report, CCSU Institute for Municipal and Regional Policy")
p

# Chart 2: Aggregate Traffic Stops by week day

stops$Day.of.Week = factor(stops$Day.of.Week,levels(stops$Day.of.Week)[c(4,2,6,7,5,1,3)])

stops_days_race <- stops %>%
  group_by(Day.of.Week, race.ethnicity) %>%
  summarise(stops=n())

p <- ggplot(stops_days_race, aes(x=Day.of.Week, y=stops))
p <- p + geom_bar(stat = "identity") 
p <- p +  facet_wrap(~ race.ethnicity) 
p <- p + theme_minimal(base_family="Arial Narrow")
p <- p + theme(plot.title=element_text(face="bold"))
p <- p + labs(x="Day", y="Traffic stops", 
              title="Aggregate traffic stops by day of the week",
              subtitle="Between October 30, 2013 and September 1, 2014",
              caption="Traffic Stop Data Report, CCSU Institute for Municipal and Regional Policy")
p

# Chart 3: Aggregate Traffic Stops by hour of day

stops$hour_day <- hour(ymd_hms(stops$RealTime2))

stops_hours_race <- stops %>%
  group_by(hour_day, race.ethnicity) %>%
  summarise(stops=n())

p <- ggplot(stops_hours_race, aes(x=hour_day, y=stops))
p <- p + geom_bar(stat = "identity") 
p <- p +  facet_wrap(~ race.ethnicity) 
p <- p + theme_minimal(base_family="Arial Narrow")
p <- p + theme(plot.title=element_text(face="bold"))
p <- p + labs(x="Month", y="Traffic stops", 
              title="Aggregate traffic stops by hour of the day",
              subtitle="Between October 30, 2013 and September 1, 2014",
              caption="Traffic Stop Data Report, CCSU Institute for Municipal and Regional Policy")
p