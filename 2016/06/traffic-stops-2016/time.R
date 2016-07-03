# This script is an exploratory analysis on Veil of Darkness data in Hamden

library(maptools)

# Calculate sunrise

portsmouth <- matrix(c(-70.762553, 43.071755), nrow=1)
for_date <- as.POSIXct("2014-12-25", tz="America/New_York")
test <- sunriset(portsmouth, for_date, direction="sunrise", POSIXct.out=TRUE)

# Calculate sunset

ephemeris <- function(lat, lon, date, span=1, tz="EST") {
  # convert to the format we need
  lon.lat <- matrix(c(lon, lat), nrow=1)
  
  # make our sequence - using noon gets us around daylight saving time issues
  day <- as.POSIXct(date, tz=tz)
  sequence <- seq(from=day, length.out=span , by="days")
  
  # get our data
  sunrise <- sunriset(lon.lat, sequence, direction="sunrise", POSIXct.out=TRUE)
  sunset <- sunriset(lon.lat, sequence, direction="sunset", POSIXct.out=TRUE)
  solar_noon <- solarnoon(lon.lat, sequence, POSIXct.out=TRUE)
  
  # build a data frame from the vectors
  data.frame(date.3=as.Date(sunrise$time),
             sunrise=sunrise$time,
             solarnoon=solar_noon$time,
             sunset=sunset$time,
             day_length=as.numeric(sunset$time-sunrise$time))
  
}

sunsetrise <- ephemeris(41.93608, -72.79147, "2013-10-01", 730, tz="America/New_York")

library(lubridate)
library(dplyr)
hamden$date.join <- as.character(hamden$date.3)
colnames(sunsetrise) <- c("date.join", "sunrise", "noon", "sunset", "day_length")
sunsetrise$date.join <- as.character(sunsetrise$date.join)

hamden_time <- merge(hamden, sunsetrise, by="date.join")
hamden_time$ForRealTime <- paste(hamden_time$date.join, hamden_time$RealTime)

hamden_time$ForRealTime <- ymd_hm(hamden_time$ForRealTime, tz="America/New_York")
check_tzones(hamden_time$ForRealTime)

hamden_time$light_dark <- ""
hamden_time$light_dark <- ifelse(((hamden_time$ForRealTime >  hamden_time$sunrise) & (hamden_time$ForRealTime <  hamden_time$sunset)), "light", "dark")
hamden_time$ethnicity <- ifelse(((hamden_time$SubjectRaceCode ==  "W") & (hamden_time$SubjectEthnicityCode =="N")), "White", "Minority")

table(hamden_time$ethnicity)
table(hamden_time$SubjectRaceCode)
table(hamden_time$SubjectEthnicityCode)

hamden_time$hours_since <-  hamden_time$sunset %--%  hamden_time$ForRealTime
hamden_time$hours_since <- hamden_time$hours_since / dhours(1)

hamden_time_sub <- subset(hamden_time, hours_since < 4 & hours_since > -4)

library(ggplot2)


p <- ggplot(hamden_time_sub, aes(RealTime2, hours_since))
p + geom_point(aes(colour=ethnicity)) +
 geom_hline(yintercept = 0)

table(hamden_time$ethnicity, hamden_time$light_dark)
