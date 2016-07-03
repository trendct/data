# This script creates exploratory charts looking at Veil of Darkness data
# for all towns that had location data available

# Adds columns determining if a stop happened in sunlight or darkness
# Adds time of sunrise, sunset, dusk, dawn
# Adds other date-specific info to help chart later on

# Uncomment the row below to look at data from 2013 - 2014 (but you might need to rename some columns in this script)
# Using data from October 1, 2013 - September 30, 2014
# stops <- read.csv("https://data.ct.gov/api/views/g7s9-f7az/rows.csv?accessType=DOWNLOAD")

library(readxl)

stops <- read_excel("data/RP-2014-2015-Data-3-9-16.xlsx", sheet=1)

#names(stops)[names(stops) == 'Department Name'] <- 'DepartmentName'



stops_backup <- stops
# stops <- stops_backup
colnames(stops) <- make.names(colnames(stops))

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
  dawn <- crepuscule(lon.lat, sequence, solarDep=6, direction="dawn", POSIXct.out=TRUE)
  dusk <- crepuscule(lon.lat, sequence, solarDep=6, direction="dusk", POSIXct.out=TRUE)
  
  solar_noon <- solarnoon(lon.lat, sequence, POSIXct.out=TRUE)
  
  # build a data frame from the vectors
  data.frame(date.join=as.Date(sunrise$time),
             sunrise=sunrise$time,
             solarnoon=solar_noon$time,
             sunset=sunset$time,
             dawn=dawn$time,
             dusk=dusk$time,
             day_length=as.numeric(sunset$time-sunrise$time))
  
}

sunsetrise <- ephemeris(41.93608, -72.79147, "2014-10-01", 365, tz="America/New_York")

# Easternmost is Sterling, CT
sunsetrise_e <- ephemeris(41.699616, -71.805483, "2014-10-01", 365, tz="America/New_York")

# Westernmost is Stamford, CT
sunsetrise_w <- ephemeris(41.137039, -73.617785, "2014-10-01", 365, tz="America/New_York")

# Earliest dawn east
sunsetrise_e$real.time <- as.character(sunsetrise_e$dawn)
sunsetrise_e$real.time <- gsub(".* ", "", sunsetrise_e$real.time)
min_dawn_e <- min(sunsetrise_e$real.time)

# Earliest sunset east
sunsetrise_e$real.time <- as.character(sunsetrise_e$sunset)
sunsetrise_e$real.time <- gsub(".* ", "", sunsetrise_e$real.time)
# min_sunset_e <- min(sunsetrise_e$real.time)

# The code to find the min is above but it's not exact in this case. It's not the earliest time-- it's the second-earliest time.
arranged <- sunsetrise_e$real.time[order(sunsetrise_e$real.time)]
min_sunset_e <- arranged[2]

# latest sunrise west
sunsetrise_w$real.time <- as.character(sunsetrise_w$sunrise)
sunsetrise_w$real.time <- gsub(".* ", "", sunsetrise_w$real.time)
max_dawn_w <- max(sunsetrise_w$real.time)

# latest sunset west
sunsetrise_w$real.time <- as.character(sunsetrise_w$dusk)
sunsetrise_w$real.time <- gsub(".* ", "", sunsetrise_w$real.time)
max_sunset_w <- max(sunsetrise_w$real.time)

# Join stops with the new sunrise/sunset data
sunsetrise$date.join <- as.character(sunsetrise$date.join)

#stops <- merge(stops, sunsetrise, by="date.join")
stops <- left_join(stops, sunsetrise, by="date.join")
check_tzones(stops$sunset)

# add colummns to determine inter-twilight periods

stops$min_dawn <- paste(stops$date.join, min_dawn_e)
stops$max_dawn <- paste(stops$date.join, max_dawn_w)
stops$min_dusk <- paste(stops$date.join, min_sunset_e)
stops$max_dusk <- paste(stops$date.join, max_sunset_w)

# convert strings to times

stops$min_dawn <- ymd_hms(stops$min_dawn, tz="America/New_York")
stops$max_dawn <- ymd_hms(stops$max_dawn, tz="America/New_York")
stops$min_dusk <- ymd_hms(stops$min_dusk, tz="America/New_York")
stops$max_dusk <- ymd_hms(stops$max_dusk, tz="America/New_York")


#write.csv(stops, "stops_fixed.csv")

stops$twi_dawn <- ifelse(
        (((stops$ForRealTime >  stops$min_dawn) & (stops$ForRealTime <  stops$dawn)) |
        ((stops$ForRealTime >  stops$sunrise) & (stops$ForRealTime <  stops$max_dawn)))
        , "Yes", "No")

stops$twi_dusk <- ifelse(
        (((stops$ForRealTime >  stops$min_dusk) & (stops$ForRealTime <  stops$sunset)) |
        ((stops$ForRealTime >  stops$dusk) & (stops$ForRealTime <  stops$max_dusk)))
         , "Yes", "No")

stops$twi_combined <- ifelse(((stops$twi_dawn == "Yes") | (stops$twi_dusk == "Yes")), "Yes", "No")

stops$light_dark <- ifelse(((stops$ForRealTime < stops$dawn) | (stops$ForRealTime > stops$dusk)), "light", "dark")
stops$light_dark <- ifelse(((stops$ForRealTime < stops$sunrise) & (stops$ForRealTime > stops$dawn)), "neither", stops$light_dark)
stops$light_dark <- ifelse(((stops$ForRealTime < stops$dusk) & (stops$ForRealTime > stops$sunset)), "neither", stops$light_dark)

stops$ethnicity_wm <- ifelse(((stops$SubjectRaceCode ==  "W") & (stops$SubjectEthnicityCode =="N")), "White", "Minority")

stops$hours_since <-  stops$sunset %--%  stops$ForRealTime
stops$hours_since <- stops$hours_since / dhours(1)
stops$hours_since_rise <-  stops$sunrise %--%  stops$ForRealTime
stops$hours_since_rise <- stops$hours_since_rise / dhours(1)
stops$RealTime2 <- as.character(stops$InterventionTime) 
stops$RealTime2 <- gsub(".* ", "", stops$RealTime2) 

stops$RealTime2 <- strptime(paste("1899-12-30", stops$RealTime2),  "%Y-%m-%d %H:%M:%S")

# write.csv(stops, "data/stops_sub2.csv")


for_test <- stops
for_test$RealTime2 <- NULL

test_sub <- for_test %>%
  group_by(Department.Name, ethnicity_wm, twi_combined) %>%
  summarise(total=n()) %>%
  spread(ethnicity_wm, total) %>%
  filter(twi_combined=="Yes")

stops_sub <- subset(stops, hours_since < 4 & hours_since > -4)

stops_sub <- subset(stops_sub, RealTime2 < strptime("1899-12-30 21:15", "%Y-%m-%d %H:%M") & RealTime2 > strptime("1899-12-30 17:15", "%Y-%m-%d %H:%M"))

stops_sub <- subset(stops_sub, ForRealTime > solarnoon)

write.csv(stops_sub, "stops_sub2.csv")

library(ggplot2)

stops_sub2 <- subset(stops_sub, DepartmentName=="Bloomfield")
#stops_sub2 <- subset(stops, Department.Name=="Bloomfield")

stops_sub3 <- stops_sub2
stops_sub3$RealTime2 <- NULL
stops_sub3$sunrise <- NULL
stops_sub3$solarnoon <- NULL
stops_sub3$sunset <- NULL



light_dark_df1 <- stops_sub3 %>%
  group_by(ethnicity_wm, light_dark) %>%
  summarise(count=n()) %>%
  spread(light_dark, count) %>%
  mutate(dark_percent=(round(dark/(dark+light)*100,2)),light_percent=(round(light/(dark+light)*100,2)))


light_dark_df2 <- stops_sub3 %>%
  group_by(ethnicity_wm, light_dark) %>%
  summarise(count=n()) %>%
  spread(ethnicity_wm, count) %>%
  mutate(minority_percent=(round(Minority/(Minority+White)*100,2)),white_percent=(round(White/(Minority+White)*100,2)))

dark_percent <- light_dark_df2[1,4]

light_percent <- light_dark_df2[2,4]

department_name <- "Bloomfield"

p <- ggplot()
p <- p + geom_point(data=stops_sub2, aes(x=RealTime2, y=hours_since, colour=ethnicity_wm))
p <- p + geom_rect(aes(xmin = as.POSIXct(strptime("1899-12-30 17:15", format = "%Y-%m-%d %H:%M")), xmax = as.POSIXct(strptime("1899-12-30 21:15", format = "%Y-%m-%d %H:%M")), ymin = 0, ymax = 4), alpha = 0.1, fill = "grey") 
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
    group_by(ethnicity_wm, light_dark) %>%
    summarise(count=n()) %>%
    spread(light_dark, count) %>%
    mutate(dark_percent=(round(dark/(dark+light)*100,2)),light_percent=(round(light/(dark+light)*100,2)))
  
  light_dark_df2 <- stops_sub3 %>%
    group_by(ethnicity_wm, light_dark) %>%
    summarise(count=n()) %>%
    spread(ethnicity_wm, count) %>%
    mutate(minority_percent=(round(Minority/(Minority+White)*100,2)),white_percent=(round(White/(Minority+White)*100,2)))
  
  dark_percent <- light_dark_df2[1,4]
  
  light_percent <- light_dark_df2[2,4]
  
  department_name <- town_list[i]
  
  p <- ggplot()
  p <- p + geom_point(data=stops_sub2, aes(x=RealTime2, y=hours_since, colour=ethnicity_wm))
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
  group_by(ethnicity_wm, light_dark) %>%
  summarise(count=n())

ggplot(light_dark_df3, aes(x=ethnicity_wm, y=count, fill=light_dark)) +
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
  group_by(Department.Name, ethnicity_wm, light_dark) %>%
  summarise(count=n()) %>%
  spread(ethnicity_wm, count) %>%
  mutate(minority_percent=(round(Minority/(Minority+White)*100,2)),white_percent=(round(White/(Minority+White)*100,2)))

night_diff <- light_dark_all_towns %>%
  select(Department.Name, light_dark, minority_percent) %>%
  spread(light_dark, minority_percent) %>%
  summarise(diff=dark-light)

# Maybe should break this out by individual race, not just white vs minorities