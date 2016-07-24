# This will create a PNG of calendar deaths for every town

## If you don't have the following packages installed, uncomment and run the line below
## install.packages("plyr")
## install.packages("reshape2")
## install.packages("devtools")
## install.packages("lubridate")
## install.packages("dplyr")
## install.packages("stringr")
## install.packages("zoo")
## install.packages("googleVis")

library(plyr)
library(reshape2)
library(RSocrata)
library(devtools)
library(lubridate)
library(dplyr)
library(stringr)
library(zoo)
library(googleVis)


## install_github("trendct/ctnamecleaner")
library(ctnamecleaner)

# Importing the data from the data.ct.gov portal
# https://data.ct.gov/Health-and-Human-Services/Accidental-Drug-Related-Deaths-January-2012-Sept-2/rybz-nyjw

raw <- read.socrata("https://data.ct.gov/resource/rybz-nyjw.csv")
raw <- unique(raw)

# raw <- read.csv("data/clean_no_duplicates.csv")


raw$Date <- ymd(raw$Date)

raw$Year <- year(raw$Date)
raw$Month <- month(raw$Date)
raw$Monthf <- month(raw$Date, label=TRUE)
raw$Weekday <- wday(raw$Date)
raw$Weekdayf <- wday(raw$Date, label=TRUE)


raw$yearmonth<-as.yearmon(raw$Date)
raw$yearmonthf<-factor(raw$yearmonth)
raw$week <- as.numeric(format(raw$Date,"%W"))
# and now for each monthblock we normalize the week to start at 1 

dat<-ddply(raw,.(yearmonthf),transform,monthweek=1+week-min(week))

# Checking the distribution
hist(raw$Date, breaks="days")

# Fixing town names

raw <- ctnamecleaner(Residence.City, raw, case="Title")

names(raw)[names(raw) == 'real.town.name'] <- 'Real.Residence'
raw <- ctnamecleaner(Death.City, raw, case="Title")
names(raw)[names(raw) == 'real.town.name'] <- 'Real.Death'


Years <- raw %>%
  group_by(Year, Date) %>% 
  dplyr::summarize(Count=n())

date_analysis <- raw %>%
  group_by(Date, Real.Death) %>% 
  dplyr::summarize(Count=n())
  
# Initial attempt to visualize
g <- ggplot(date_analysis,aes(x=Date, y=Count))
g <- g + geom_bar(stat="identity") 
g <- g + facet_wrap(~Real.Death, ncol=3)
g

## Well, that didn't work out well. It needs to be much taller to accomodate the number of towns

## Another try
date_analysis$Year <- year(date_analysis$Date)
date_analysis$Month <- month(date_analysis$Date)
date_analysis$Monthf <- month(date_analysis$Date, label=TRUE)
date_analysis$Weekday <- wday(date_analysis$Date)
date_analysis$Weekdayf <- wday(date_analysis$Date, label=TRUE)


date_analysis$yearmonth<-as.yearmon(date_analysis$Date)
date_analysis$yearmonthf<-factor(date_analysis$yearmonth)
date_analysis$week <- as.numeric(format(date_analysis$Date,"%W"))
# and now for each monthblock we normalize the week to start at 1 
date_analysis <-ddply(date_analysis,.(yearmonthf),transform,monthweek=1+week-min(week))

date_analysis$weekdayf<-factor(date_analysis$Weekday,levels=rev(1:7),labels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")),ordered=TRUE)


y2015 <- subset(date_analysis, Year==2015)
y2014 <- subset(date_analysis, Year==2014)
y2013 <- subset(date_analysis, Year==2013)
y2012 <- subset(date_analysis, Year==2012)
y2011 <- subset(date_analysis, Year==2011)

## Using ggplot to do a heatmap calendar of overall trends

P <- ggplot(date_analysis, aes(monthweek, weekdayf, fill = Count)) +
  geom_tile(colour = "white") + facet_grid(Year~Monthf) + scale_fill_gradient(low="yellow", high="red") 
  # opts(title = "Time-Series Calendar Heatmap") + xlab("Week of Month") + ylab("")
P
## Interesting. It looks like there might have been a bad batch in June where there are a cluster of deaths

## Ok, trying again but this time with Google Vis Calendar
plot( 
  gvisCalendar(data=date_analysis, datevar="Date", numvar="Count",
               options=list(
                 title="Calendar heat map of drug overdoses in CT",
                 calendar="{cellSize:10,
                 yearLabel:{fontSize:20, color:'#444444'},
                 focusedCellColor:{stroke:'red'}}",
                 width=900, height=720),
               chartid="Calendar")
)
# Nice and is more interactive


## Let's look at each towns, though
## This loop will generate a calendar heat map for each municipality in the "towns" folder

source("calendarHeat.R")


setwd("towns")

town_names <- data.frame(table(date_analysis$Real.Death))

for (i in 1:nrow(town_names)) {
  named <- town_names$Var1[i]
  subsetted <- subset(date_analysis, Real.Death==named)
  head_name <- paste("drug overdose deaths in", named)
  fil3name <- paste0('heatmaps/',named, ".png")
  png(filename=fil3name)
  calendarHeat(subsetted$Date, subsetted$Count, 
             varname=head_name)
  dev.off()

}

setwd("..")
