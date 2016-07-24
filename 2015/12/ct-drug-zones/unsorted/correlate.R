library(dplyr)
library(stringr)
library(plotly)
library(DT)
require(ggplot2)

percent <- read.csv("percentzone.csv", stringsAsFactors=FALSE)

datatable(percent)

arrests <- read.csv("arrests_h.csv")
town_arrests <- data.frame(table(arrests$TOWN))
colnames(town_arrests) <- c("TOWN", "Total.Arrests")
town_arrests$TOWN <- str_to_title(town_arrests$TOWN)
head(town_arrests, 10)

arrestscor <- left_join(percent, town_arrests)
cor(arrestscor$PERCENT, arrestscor$Total.Arrests)

conv_new <- read.csv("convictionsnew.csv")
arrests_new <- read.csv("arrests_h.csv")

conv_new$Sentenced <- as.character(conv_new$Sentenced)
conv_new$Date <- mdy(conv_new$Sentenced)
conv_new$Year <- year(conv_new$Date)
conv_new$Year[conv_new$Year==2099] <- 1999

arrests_new$ARREST.DATE <- as.character(arrests_new$ARREST.DATE)
arrests_new$Date <- mdy(arrests_new$ARREST.DATE)
arrests_new$Year <- year(arrests_new$Date)
arrests_new$Year[arrests_new$Year==2099] <- 1999


arrests_new$RaceOf <- paste(arrests_new$RACE, arrests_new$DEF_HISPANIC_IND, sep="")

index <- c("Asian", "AsianY", "Black", "BlackY", "HispY", "Native American", 
           "Native AmericanY", "Not Identified", "White", "WhiteY")


values <- c("Asian", "Hispanic", "Black", "Hispanic", "Hispanic", "Native American", 
            "Hispanic", "Not Identified", "White", "Hispanic")
arrests_new$Def_Race <- values[match(arrests_new$RaceOf, index)]

race_year_new <- data.frame(table(arrests_new$Year,arrests_new$Def_Race))
