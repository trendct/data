# This script creates exploratory charts looking at Veil of Darkness data
# for all towns that had location data available

# Uncomment the row below to look at data from 2013 - 2014 (but you might need to rename some columns in this script)
# Using data from October 1, 2013 - September 30, 2014
stops <- read.csv("https://data.ct.gov/api/views/g7s9-f7az/rows.csv?accessType=DOWNLOAD")

#library(openxlsx)
library(readxl)
#stops <- read.xlsx("data/RP-2014-2015-Data-3-9-16.xlsx", sheet=1)
#stops <- read_excel("ignore/RP-2014-2015-Data-3-9-16.xlsx", sheet=1)

stops_backup <- stops
# stops <- stops_backup

library(lubridate)
library(dplyr)
library(maptools)
library(stringr)
library(tidyr)
library(ggalt)

# THIS IS FOR 2013-2014 DATA
#stops$date.join <- gsub(" .*", "", stops$Intervention.Date)
# stops$date.join <- mdy(stops$date.join)
# stops$date.join <- as.character(stops$date.join)
# 
# ## Gotta deal with am pm in some
# stops$Intervention.Time <- as.character(stops$Intervention.Time)
# stops$Intervention.Time <- str_to_upper(stops$Intervention.Time)
# 
# stops$Intervention.Time2 <- stops$Intervention.Time
# 
# stops_military <- subset(stops, (!grepl(" AM", Intervention.Time2) & !grepl(" PM", Intervention.Time2)))
# 
# stops_m <-  subset(stops, (grepl(" AM", Intervention.Time2) | grepl(" PM", Intervention.Time2)))
# 
# for (i in 1:nrow(stops_m)) {
#   if (grepl("PM", stops_m$Intervention.Time2[i])) {
#     breaker <- gsub(" PM", "", stops_m$Intervention.Time2[i])
#     break_a <- as.numeric(gsub(":.*", "", breaker))+12
#     if (break_a==24) { break_a <- 12 }
#     break_b <- gsub(".*:", "", breaker)
#     print(paste0("if", i))
#     stops_m$Intervention.Time[i] <- paste0(break_a, ":", break_b)
#     } else if (grepl("AM", stops_m$Intervention.Time2[i])) {
#     breaker <- gsub(" AM", "", stops_m$Intervention.Time2[i])
#     break_a <- as.numeric(gsub(":.*", "", breaker))
#     
#     if (break_a==12) { 
#       break_a <- "00"
#       break_b <- gsub(".*:", "", breaker)
#       stops_m$Intervention.Time[i] <- paste0(break_a, ":", break_b)
#       } else {
#     stops_m$Intervention.Time[i] <- breaker
#     print(paste0("elseif",i))
#       }
#     } else {
#     stops_m$Intervention.Time[i] <- stops_m$Intervention.Time2[i]
#     print(paste0("else",i))
#     } 
# }
# 
# stops <- rbind(stops_military, stops_m)
# 
# 
# 
# stops$ForRealTime <- paste(stops$date.join, stops$Intervention.Time)

stops$ForRealTime <- ymd_hms(stops$InterventionDateTime, tz="America/New_York")

stops$date.join <- as.character(stops$InterventionDate)
stops$date.join <- gsub(" .*", "", stops$date.join)

ephemeris <- function(lat, lon, date, span=1, tz="America/New_York") {
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
  data.frame(date.join=as.Date(sunrise$time),
             sunrise=sunrise$time,
             solarnoon=solar_noon$time,
             sunset=sunset$time,
             day_length=as.numeric(sunset$time-sunrise$time))
  
}

sunsetrise <- ephemeris(41.93608, -72.79147, "2014-10-01", 730, tz="America/New_York")



sunsetrise$date.join <- as.character(sunsetrise$date.join)

stops <- merge(stops, sunsetrise, by="date.join")
check_tzones(stops$sunset)

#write.csv(stops, "stops_fixed.csv")

stops$light_dark <- ""
stops$light_dark <- ifelse(((stops$ForRealTime >  stops$sunrise) & (stops$ForRealTime <  stops$sunset)), "light", "dark")
stops$ethnicity <- ifelse(((stops$SubjectRaceCode ==  "W") & (stops$SubjectEthnicityCode =="N")), "White", "Minority")
#stops$ethnicity <- ifelse(((stops$Subject.Race.Code ==  "W") & (stops$Subject.Ethnicity.Code =="N")), "White", "Minority")


stops$hours_since <-  stops$sunset %--%  stops$ForRealTime
stops$hours_since <- stops$hours_since / dhours(1)
stops$RealTime2 <- strptime(as.character(stops$InterventionTime), "%H:%M")

stops_sub <- subset(stops, hours_since < 4 & hours_since > -4)

stops_sub <- subset(stops_sub, RealTime2 < strptime("2016-05-08 21:15", "%Y-%m-%d %H:%M") & RealTime2 > strptime("2016-05-08 17:15", "%Y-%m-%d %H:%M"))

stops_sub <- subset(stops_sub, ForRealTime > solarnoon)

# write.csv(stops_sub, "stops_sub.csv")
write.csv(stops_sub, "data/stops_sub2.csv")

library(ggplot2)

stops_sub2 <- subset(stops_sub, Department.Name=="Bloomfield")
#stops_sub2 <- subset(stops, Department.Name=="Bloomfield")

stops_sub3 <- stops_sub2
stops_sub3$RealTime2 <- NULL
stops_sub3$sunrise <- NULL
stops_sub3$solarnoon <- NULL
stops_sub3$sunset <- NULL



light_dark_df1 <- stops_sub3 %>%
  group_by(ethnicity, light_dark) %>%
  summarise(count=n()) %>%
  spread(light_dark, count) %>%
  mutate(dark_percent=(round(dark/(dark+light)*100,2)),light_percent=(round(light/(dark+light)*100,2)))


light_dark_df2 <- stops_sub3 %>%
  group_by(ethnicity, light_dark) %>%
  summarise(count=n()) %>%
  spread(ethnicity, count) %>%
  mutate(minority_percent=(round(Minority/(Minority+White)*100,2)),white_percent=(round(White/(Minority+White)*100,2)))

dark_percent <- light_dark_df2[1,4]

light_percent <- light_dark_df2[2,4]

department_name <- "Bloomfield"

p <- ggplot()
p <- p + geom_point(data=stops_sub2, aes(x=RealTime2, y=hours_since, colour=ethnicity))
p <- p + geom_rect(aes(xmin = as.POSIXct(strptime("2016-05-08 17:15", format = "%Y-%m-%d %H:%M")), xmax = as.POSIXct(strptime("2016-05-08 21:15", format = "%Y-%m-%d %H:%M")), ymin = 0, ymax = 4), alpha = 0.1, fill = "grey") 
p <- p + geom_hline(yintercept = 0)
p <- p + theme_minimal(base_family="Arial Narrow")
p <- p + theme(plot.title=element_text(face="bold"))
p <- p + labs(x="Clock time", y="Hours since darkness", 
              title=paste0(department_name, " traffic stops around sunset"),
              subtitle=paste0("During daylight hours, ", light_percent, "% of stops involved minority drivers; after dark, this figure changed to ", dark_percent, "%. \nThe large diagonal gap is a result of the shift from Eastern Daylight Time to Eastern Standard Time."),
              caption="Traffic Stop Data Report, CCSU Institute for Municipal and Regional Policy")
p


## Towns

town_list <- c("Bloomfield", 
               "New Milford", 
               "Norwalk", 
               "West Hartford", 
               "Wethersfield", 
               "Stratford",
               "Meriden",
               "New Britain",
               "Newington",
               "Trumbull",
               "Waterbury")


for (i in 1:length(town_list)) {
  
  
  
  stops_sub2 <- subset(stops_sub, Department.Name==town_list[i])
  #stops_sub2 <- subset(stops, Department.Name=="Bloomfield")
  
  stops_sub3 <- stops_sub2
  stops_sub3$RealTime2 <- NULL
  stops_sub3$sunrise <- NULL
  stops_sub3$solarnoon <- NULL
  stops_sub3$sunset <- NULL
  
  light_dark_df1 <- stops_sub3 %>%
    group_by(ethnicity, light_dark) %>%
    summarise(count=n()) %>%
    spread(light_dark, count) %>%
    mutate(dark_percent=(round(dark/(dark+light)*100,2)),light_percent=(round(light/(dark+light)*100,2)))
  
  light_dark_df2 <- stops_sub3 %>%
    group_by(ethnicity, light_dark) %>%
    summarise(count=n()) %>%
    spread(ethnicity, count) %>%
    mutate(minority_percent=(round(Minority/(Minority+White)*100,2)),white_percent=(round(White/(Minority+White)*100,2)))
  
  dark_percent <- light_dark_df2[1,4]
  
  light_percent <- light_dark_df2[2,4]
  
  department_name <- town_list[i]
  
  p <- ggplot()
  p <- p + geom_point(data=stops_sub2, aes(x=RealTime2, y=hours_since, colour=ethnicity))
  p <- p + geom_rect(aes(xmin = as.POSIXct(strptime("2016-05-08 17:15", format = "%Y-%m-%d %H:%M")), xmax = as.POSIXct(strptime("2016-05-08 21:15", format = "%Y-%m-%d %H:%M")), ymin = 0, ymax = 4), alpha = 0.1, fill = "grey") 
  p <- p + geom_hline(yintercept = 0)
  p <- p + theme_minimal(base_family="Arial Narrow")
  p <- p + theme(plot.title=element_text(face="bold"))
  p <- p + labs(x="Clock time", y="Hours since darkness", 
                title=paste0(department_name, " traffic stops around sunset"),
                subtitle=paste0("During daylight hours, ", light_percent, "% of stops involved minority drivers; after dark, this figure changed to ", dark_percent, "%. \nThe large diagonal gap is a result of the shift from Eastern Daylight Time to Eastern Standard Time."),
                caption="Traffic Stop Data Report, CCSU Institute for Municipal and Regional Policy")
  print(p)
  
}

light_dark_df3 <- stops_sub3 %>%
  group_by(ethnicity, light_dark) %>%
  summarise(count=n())

ggplot(light_dark_df3, aes(x=ethnicity, y=count, fill=light_dark)) +
  geom_bar(stat="identity", position=position_dodge(), colour="black") +
  coord_flip() + 
  labs(x="Traffic stops", y="Ethnicity", 
       title=paste0(department_name, " traffic stops: Daylight versus night"),
       subtitle=paste0("During daylight hours, ", light_percent, "% of stops involved minority drivers; after dark, this figure changed to ", dark_percent, "%."),
       caption="Traffic Stop Data Report, CCSU Institute for Municipal and Regional Policy")


## OK, what are the worst towns with the biggest disparity at night vs day
stops_sub_no_pos <- stops_sub
stops_sub_no_pos$RealTime2 <- NULL
stops_sub_no_pos$sunrise <- NULL
stops_sub_no_pos$solarnoon <- NULL
stops_sub_no_pos$sunset <- NULL 

light_dark_all_towns <- stops_sub_no_pos %>%
  group_by(Department.Name, ethnicity, light_dark) %>%
  summarise(count=n()) %>%
  spread(ethnicity, count) %>%
  mutate(minority_percent=(round(Minority/(Minority+White)*100,2)),white_percent=(round(White/(Minority+White)*100,2)))

night_diff <- light_dark_all_towns %>%
  select(Department.Name, light_dark, minority_percent) %>%
  spread(light_dark, minority_percent) %>%
  summarise(diff=dark-light)

# Maybe should break this out by individual race, not just white vs minorities