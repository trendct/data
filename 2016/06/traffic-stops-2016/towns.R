# This script creates a department-level summary analysis 
# The generated dataframe is used for stories and the database at http://trafficstops.trendct.org/

library(readxl)
library(data.table)

# The data is too large to host on github but can be found at http://ctrp3.ctdata.org/rawdata/
stops <- read_excel("ignore/RP-2014-2015-Data-3-9-16.xlsx", sheet=1)
names(stops)[names(stops) == 'Department Name'] <- 'DepartmentName'

## Creating a backup
stops_backup <- stops
# stops <- stops_backup

library(lubridate)
library(dplyr)
library(maptools)
library(stringr)
library(tidyr)
library(ggalt)
library(data.table)

# Adjusting data for race

stops$ethnicity <- ifelse(((stops$SubjectRaceCode ==  "W") & (stops$SubjectEthnicityCode =="N")), "White", "Minority")
stops$RE <- paste0(stops$SubjectRaceCode, stops$SubjectEthnicityCode)
stops$RE <- gsub("AH", "hispanic", stops$RE)
stops$RE <- gsub("AM", "middle-eastern", stops$RE)
stops$RE <- gsub("AN", "Asian", stops$RE)
stops$RE <- gsub("BH", "Black", stops$RE)
stops$RE <- gsub("BM", "Black", stops$RE)
stops$RE <- gsub("BN", "Black", stops$RE)
stops$RE <- gsub("IH", "Indian", stops$RE)
stops$RE <- gsub("IM", "middle-eastern", stops$RE)
stops$RE <- gsub("IN", "Indian", stops$RE)
stops$RE <- gsub("WH", "hispanic", stops$RE)
stops$RE <- gsub("WM", "middle-eastern", stops$RE)
stops$RE <- gsub("WN", "White", stops$RE)

# Adjusting for state police troops
# This changes the name of a troop department name based on the Organization Identification ID

stops$DepartmentName <- ifelse(stops$OrganizationIdentificationID=="CTCSP0000", "State Police: Headquarters", stops$DepartmentName)
stops$DepartmentName <- ifelse(stops$OrganizationIdentificationID=="CTCSP0023", "State Police: Headquarters", stops$DepartmentName)
stops$DepartmentName <- ifelse(stops$OrganizationIdentificationID=="CTCSP0029", "State Police: Headquarters", stops$DepartmentName)
stops$DepartmentName <- ifelse(stops$OrganizationIdentificationID=="CTCSP1900", "State Police: Headquarters", stops$DepartmentName)
stops$DepartmentName <- ifelse(stops$OrganizationIdentificationID=="CTCSP2900", "State Police: Headquarters", stops$DepartmentName)
stops$DepartmentName <- ifelse(stops$OrganizationIdentificationID=="CTCSP3800", "State Police: Headquarters", stops$DepartmentName)
stops$DepartmentName <- ifelse(stops$OrganizationIdentificationID=="CTCSP0200", "State Police: Troop A", stops$DepartmentName)
stops$DepartmentName <- ifelse(stops$OrganizationIdentificationID=="CTCSP0300", "State Police: Troop B", stops$DepartmentName)
stops$DepartmentName <- ifelse(stops$OrganizationIdentificationID=="CTCSP0400", "State Police: Troop C", stops$DepartmentName)
stops$DepartmentName <- ifelse(stops$OrganizationIdentificationID=="CTCSP0500", "State Police: Troop D", stops$DepartmentName)
stops$DepartmentName <- ifelse(stops$OrganizationIdentificationID=="CTCSP0600", "State Police: Troop E", stops$DepartmentName)
stops$DepartmentName <- ifelse(stops$OrganizationIdentificationID=="CTCSP0700", "State Police: Troop F", stops$DepartmentName)
stops$DepartmentName <- ifelse(stops$OrganizationIdentificationID=="CTCSP0800", "State Police: Troop G", stops$DepartmentName)
stops$DepartmentName <- ifelse(stops$OrganizationIdentificationID=="CTCSP0900", "State Police: Troop H", stops$DepartmentName)
stops$DepartmentName <- ifelse(stops$OrganizationIdentificationID=="CTCSP1000", "State Police: Troop I", stops$DepartmentName)
stops$DepartmentName <- ifelse(stops$OrganizationIdentificationID=="CTCSP1100", "State Police: Troop J", stops$DepartmentName)
stops$DepartmentName <- ifelse(stops$OrganizationIdentificationID=="CTCSP1200", "State Police: Troop K", stops$DepartmentName)

# Converts date of stop to format recognized by R

stops$ForRealTime <- ymd_hms(stops$InterventionDateTime, tz="America/New_York")

# Creates an additional column with just the date to join with later
stops$date.join <- as.character(stops$InterventionDate)
stops$date.join <- gsub(" .*", "", stops$date.join)

# This is a function that determines the exact time of sunset, sunrise, dusk, and dawn
# based on location and date

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

# Testing out the function. Creates dataframe for 365 days.
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
# Why? It's just a quirk in the code. 
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


#write.csv(stops, "data/stops_fixed.csv")

# Figuring out if the traffic stopped happened within twilight dawn
stops$twi_dawn <- ifelse(
  (((stops$ForRealTime >  stops$min_dawn) & (stops$ForRealTime <  stops$dawn)) |
     ((stops$ForRealTime >  stops$sunrise) & (stops$ForRealTime <  stops$max_dawn)))
  , "Yes", "No")

# Figuring out if the traffic stopped happened within twilight dusk

stops$twi_dusk <- ifelse(
  (((stops$ForRealTime >  stops$min_dusk) & (stops$ForRealTime <  stops$sunset)) |
     ((stops$ForRealTime >  stops$dusk) & (stops$ForRealTime <  stops$max_dusk)))
  , "Yes", "No")

# Figuring out if the traffic stopped happened within twilight dusk or twilight dawn
stops$twi_combined <- ifelse(((stops$twi_dawn == "Yes") | (stops$twi_dusk == "Yes")), "Yes", "No")

# Adding a column to that labels if a stop happened in the dark or light or neither (civil twilight)
stops$light_dark <- ifelse(((stops$ForRealTime < stops$dawn) | (stops$ForRealTime > stops$dusk)), "light", "dark")
stops$light_dark <- ifelse(((stops$ForRealTime < stops$sunrise) & (stops$ForRealTime > stops$dawn)), "neither", stops$light_dark)
stops$light_dark <- ifelse(((stops$ForRealTime < stops$dusk) & (stops$ForRealTime > stops$sunset)), "neither", stops$light_dark)

# Adding a column to distinguish between Whites and Minorities
stops$ethnicity_wm <- ifelse(((stops$SubjectRaceCode ==  "W") & (stops$SubjectEthnicityCode =="N")), "White", "Minority")

# Calculating time between stop and sunset

stops$hours_since <-  stops$sunset %--%  stops$ForRealTime
stops$hours_since <- stops$hours_since / dhours(1)
stops$RealTime2 <- as.character(stops$InterventionTime) 
stops$RealTime2 <- gsub(".* ", "", stops$RealTime2) 

# Collapsing all the stops onto a single day for charting purposes

stops$RealTime2 <- strptime(paste("1899-12-30", stops$RealTime2),  "%Y-%m-%d %H:%M:%S")

stops$RealTime2 <- as.character(stops$RealTime2)

# Determining the percent of white and minority drivers stopped in the darkness versus sunlight

town_light_dark_summary <- stops %>%
  group_by(DepartmentName, light_dark, ethnicity_wm) %>%
  summarise(total=n()) %>%
  filter(light_dark=='dark' | light_dark=='light') %>%
  spread(ethnicity_wm, total) %>%
  mutate(minority_p=round(Minority/(Minority+White)*100,2), white_p=100-minority_p) %>%
  select(DepartmentName, light_dark, minority_p) %>%
  spread(light_dark, minority_p) 

  colnames(town_light_dark_summary) <- c("DepartmentName", "minority_dark", "minority_light")
  town_light_dark_summary$white_dark <- 100- town_light_dark_summary$minority_dark
  town_light_dark_summary$white_light <- 100- town_light_dark_summary$minority_light
  town_light_dark_summary <- town_light_dark_summary[c("DepartmentName", "minority_dark", "white_dark", "minority_light", "white_light")]
  
  
state_light_dark_summary <- stops %>%
    group_by(light_dark, ethnicity_wm) %>%
    summarise(total=n()) %>%
    filter(light_dark=='dark' | light_dark=='light') %>%
    spread(ethnicity_wm, total) %>%
    mutate(minority_p=round(Minority/(Minority+White)*100,2), white_p=100-minority_p) %>%
    select(light_dark, minority_p) %>%
    spread(light_dark, minority_p) 
  
  colnames(state_light_dark_summary) <- c("minority_dark", "minority_light")
  state_light_dark_summary$white_dark <- 100- state_light_dark_summary$minority_dark
  state_light_dark_summary$white_light <- 100- state_light_dark_summary$minority_light
  state_light_dark_summary <- state_light_dark_summary[c("minority_dark", "white_dark", "minority_light", "white_light")]
  
  state_light_dark_summary$DepartmentName <- "Connecticut average"
  state_light_dark_summary <- state_light_dark_summary[c("DepartmentName", "minority_dark", "white_dark", "minority_light", "white_light")]
  
  
# Racial summary, percent breakdown of stops by race
  
town_race_summary <- stops %>%
  group_by(DepartmentName, RE) %>%
  summarise(total=n()) %>%
  spread(RE, total) %>%
  group_by(DepartmentName) %>%
  mutate(total=sum(Asian, Black, hispanic, Indian, `middle-eastern`, White, na.rm=TRUE), min=total-White) %>%
  mutate(asian_p=round(Asian/total*100,2), black_p=round(Black/total*100,2), hispanic_p=round(hispanic/total*100,2), 
         indian_p=round(Indian/total*100,2), mid_e_p=round(`middle-eastern`/total*100,2), white_p=round(White/total*100,2), min_p=round(min/total*100,2))
  
town_disposition_summary <- stops %>%
  group_by(DepartmentName, InterventionDispositionCode) %>%
  summarise(total=n()) %>%
  spread(InterventionDispositionCode, total) %>%
  group_by(DepartmentName) %>%
  mutate(total=sum(I, M, N, U, V, W, na.rm=TRUE)) %>%
  mutate(infraction_ticket_p=round(I/total*100,2), misdemeanor_summons_p=round(M/total*100,2),
         no_disposition_p=round(N/total*100,2), uniform_arrest_p=round(U/total*100,2),
         verbal_warning_p=round(V/total*100,2), written_warning_p=round(W/total*100,2))
town_disposition_summary$total <- NULL
names(town_disposition_summary)[names(town_disposition_summary) == 'I'] <- 'infraction_ticket'
names(town_disposition_summary)[names(town_disposition_summary) == 'M'] <- 'misdemeanor_summons'
names(town_disposition_summary)[names(town_disposition_summary) == 'N'] <- 'no_disposition'
names(town_disposition_summary)[names(town_disposition_summary) == 'U'] <- 'uniform_arrest'
names(town_disposition_summary)[names(town_disposition_summary) == 'V'] <- 'verbal_warning'
names(town_disposition_summary)[names(town_disposition_summary) == 'W'] <- 'written_warning'

## State racial summary

state_race_summary <- stops %>%
  group_by(RE) %>%
  summarise(total=n()) %>%
  spread(RE, total) %>%
  mutate(total=sum(Asian, Black, hispanic, Indian, `middle-eastern`, White, na.rm=TRUE), min=total-White) %>%
  mutate(asian_p=round(Asian/total*100,2), black_p=round(Black/total*100,2), hispanic_p=round(hispanic/total*100,2), 
         indian_p=round(Indian/total*100,2), mid_e_p=round(`middle-eastern`/total*100,2), white_p=round(White/total*100,2), min_p=round(min/total*100,2))

state_race_summary <- state_race_summary[c("Asian", "Black", "hispanic", "Indian", "middle-eastern", "White", "total", "min", "asian_p", "black_p", "hispanic_p", "indian_p", "mid_e_p", "white_p", "min_p")]
state_mega <- cbind(state_light_dark_summary, state_race_summary)

state_disposition_summary <- stops %>%
  group_by(InterventionDispositionCode) %>%
  summarise(total=n()) %>%
  spread(InterventionDispositionCode, total) %>%
  mutate(total=sum(I, M, N, U, V, W, na.rm=TRUE)) %>%
  mutate(infraction_ticket_p=round(I/total*100,2), misdemeanor_summons_p=round(M/total*100,2),
         no_disposition_p=round(N/total*100,2), uniform_arrest_p=round(U/total*100,2),
         verbal_warning_p=round(V/total*100,2), written_warning_p=round(W/total*100,2))
state_disposition_summary$total <- NULL
names(state_disposition_summary)[names(state_disposition_summary) == 'I'] <- 'infraction_ticket'
names(state_disposition_summary)[names(state_disposition_summary) == 'M'] <- 'misdemeanor_summons'
names(state_disposition_summary)[names(state_disposition_summary) == 'N'] <- 'no_disposition'
names(state_disposition_summary)[names(state_disposition_summary) == 'U'] <- 'uniform_arrest'
names(state_disposition_summary)[names(state_disposition_summary) == 'V'] <- 'verbal_warning'
names(state_disposition_summary)[names(state_disposition_summary) == 'W'] <- 'written_warning'

state_mega <- cbind(state_mega, state_disposition_summary)
names(state_mega)[names(state_mega) == 'White'] <- 'White.x'
names(state_mega)[names(state_mega) == 'middle-eastern'] <- 'middle.eastern'

# gender summary

town_gender_summary <- stops %>%
  group_by(DepartmentName, SubjectSexCode) %>%
  summarise(total=n()) %>%
  spread(SubjectSexCode, total) %>%
  group_by(DepartmentName) %>%
  mutate(total=sum(F, M, na.rm=TRUE)) %>%
  mutate(male_p=round(M/total*100,2), female_p=round(F/total*100,2))
town_gender_summary$total <- NULL

state_gender_summary <- stops %>%
  group_by(SubjectSexCode) %>%
  summarise(total=n()) %>%
  spread(SubjectSexCode, total) %>%
  mutate(total=sum(F, M, na.rm=TRUE)) %>%
  mutate(male_p=round(M/total*100,2), female_p=round(F/total*100,2))
state_gender_summary$total <- NULL

state_mega <- cbind(state_mega, state_gender_summary)


# Age group adjustment

stops$agegroup <- ""
stops <- data.table(stops)

stops[SubjectAge < 22, agegroup := "y16_21"]
stops[SubjectAge >= 22 & SubjectAge < 28, agegroup := "y22_27"]
stops[SubjectAge >= 28 & SubjectAge < 32, agegroup := "y28_31"]
stops[SubjectAge >= 32 & SubjectAge < 38, agegroup := "y32_37"]
stops[SubjectAge >= 38 & SubjectAge < 42, agegroup := "y38_41"]
stops[SubjectAge >= 42 & SubjectAge < 48, agegroup := "y42_47"]
stops[SubjectAge >= 48 & SubjectAge < 52, agegroup := "y48_51"]
stops[SubjectAge >= 52 & SubjectAge < 58, agegroup := "y52_57"]
stops[SubjectAge >= 58 & SubjectAge < 62, agegroup := "y58_61"]
stops[SubjectAge >= 62 & SubjectAge < 68, agegroup := "y62_67"]
stops[SubjectAge >= 68 & SubjectAge < 72, agegroup := "y68_71"]
stops[SubjectAge >= 72 & SubjectAge < 78, agegroup := "y72_77"]
stops[SubjectAge >= 78 & SubjectAge < 82, agegroup := "y78_81"]
stops[SubjectAge >= 82, agegroup := "y82_"]

# Age summary
town_age_summary <- stops %>%
  group_by(DepartmentName) %>%
  summarise(average_age=round(mean(SubjectAge, na.rm=TRUE),0), median_age=round(median(SubjectAge, na.rm=TRUE),0))

state_age_summary <- stops %>%
  summarise(average_age=round(mean(SubjectAge, na.rm=TRUE),0), median_age=round(median(SubjectAge, na.rm=TRUE),0))

state_mega <- cbind(state_mega, state_age_summary)


# Age group summary
town_age_group_summary <- stops %>%
  group_by(DepartmentName, agegroup) %>%
  summarise(total=n()) %>%
  spread(agegroup, total) 

town_age_group_summary[,2] <- NULL

town_age_group_summary <- town_age_group_summary %>%
  group_by(DepartmentName) %>%
  mutate(total = sum(y16_21, y22_27, y28_31, y32_37, y38_41, y42_47, y48_51,
                     y52_57, y58_61, y62_67, y68_71, y72_77, y78_81, y82_, na.rm=TRUE)) %>%
  mutate(y16_21_p=round(y16_21/total*100,2), y22_27_p=round(y22_27/total*100,2), y28_31_p=round(y28_31/total*100,2), 
         y32_37_p=round(y32_37/total*100,2), y38_41_p=round(y38_41/total*100,2), y42_47_p=round(y42_47/total*100,2),
         y48_51_p=round(y48_51/total*100,2), y52_57_p=round(y52_57/total*100,2), y58_61_p=round(y58_61/total*100,2),
         y62_67_p=round(y62_67/total*100,2), y68_71_p=round(y68_71/total*100,2), y72_77_p=round(y72_77/total*100,2),
         y78_81_p=round(y78_81/total*100,2), y82_p=round(y82_/total*100,2))
town_age_group_summary$total <- NULL


state_age_group_summary <- stops %>%
  group_by(agegroup) %>%
  summarise(total=n()) 

state_age_group_summary$agegroup[15] <- "total"
state_age_group_summary$total[15] <- sum(state_age_group_summary$total)

state_age_group_summary <- state_age_group_summary %>%
  spread(agegroup, total) %>%
  mutate(y16_21_p=round(y16_21/total*100,2), y22_27_p=round(y22_27/total*100,2), y28_31_p=round(y28_31/total*100,2), 
         y32_37_p=round(y32_37/total*100,2), y38_41_p=round(y38_41/total*100,2), y42_47_p=round(y42_47/total*100,2),
         y48_51_p=round(y48_51/total*100,2), y52_57_p=round(y52_57/total*100,2), y58_61_p=round(y58_61/total*100,2),
         y62_67_p=round(y62_67/total*100,2), y68_71_p=round(y68_71/total*100,2), y72_77_p=round(y72_77/total*100,2),
         y78_81_p=round(y78_81/total*100,2), y82_p=round(y82_/total*100,2))
state_age_group_summary$total <- NULL

state_mega <- cbind(state_mega, state_age_group_summary)

# Statute code summary
town_statute_code_summary <- stops %>%
  group_by(DepartmentName, StatuteReason) %>%
  summarise(total=n())

town_statute_code_summary$StatuteReason <- gsub(" ", "", town_statute_code_summary$StatuteReason)

town_statute_code_summary <- town_statute_code_summary %>%
  group_by(DepartmentName, StatuteReason) %>%
  summarise(total=sum(total)) %>%
  spread(StatuteReason, total) %>%
  group_by(DepartmentName) %>%
  mutate(total=sum(CellPhone, DefectiveLights, DisplayofPlates, EquipmentViolation, MovingViolation,
                   Other, Registration, Seatbelt, SpeedRelated, StopSign, StopSign, SuspendedLicense,
                   TrafficControlSignal, WindowTint, na.rm=TRUE)) %>%
  mutate(CellPhone_p=round(CellPhone/total*100,2), DefectiveLights_p=round(DefectiveLights/total*100,2), DisplayofPlates_p=round(DisplayofPlates/total*100,2), 
         EquipmentViolation_p=round(EquipmentViolation/total*100,2), MovingViolation_p=round(MovingViolation/total*100,2), Other_p=round(Other/total*100,2),
         Registration_p=round(Registration/total*100,2), Seatbelt_p=round(Seatbelt/total*100,2), SpeedRelated_p=round(SpeedRelated/total*100,2),
         StopSign_p=round(StopSign/total*100,2), SuspendedLicense_p=round(SuspendedLicense/total*100,2), TrafficControlSignal_p=round(TrafficControlSignal/total*100,2),
         WindowTint_p=round(WindowTint/total*100,2))
town_statute_code_summary$total <- NULL

state_statute_code_summary <- stops %>%
  group_by(StatuteReason) %>%
  summarise(total=n()) 


temp_df <- t(data.frame(c(StatuteReason="total", total=sum(state_statute_code_summary$total))))
rownames(temp_df) <- NULL
state_statute_code_summary <- data.frame(state_statute_code_summary)

state_statute_code_summary <- rbind(state_statute_code_summary,temp_df)

state_statute_code_summary$StatuteReason <- gsub(" ", "", state_statute_code_summary$StatuteReason)
state_statute_code_summary$total <- as.numeric(state_statute_code_summary$total)

state_statute_code_summary <- state_statute_code_summary %>%
  group_by(StatuteReason) %>%
  summarise(total=sum(total)) %>%
  spread(StatuteReason, total) 


state_statute_code_summary <- state_statute_code_summary %>%
  mutate(total=sum(CellPhone, DefectiveLights, DisplayofPlates, EquipmentViolation, MovingViolation,
                   Other, Registration, Seatbelt, SpeedRelated, StopSign, StopSign, SuspendedLicense,
                   TrafficControlSignal, WindowTint, na.rm=TRUE)) %>%
  mutate(CellPhone_p=round(CellPhone/total*100,2), DefectiveLights_p=round(DefectiveLights/total*100,2), DisplayofPlates_p=round(DisplayofPlates/total*100,2), 
         EquipmentViolation_p=round(EquipmentViolation/total*100,2), MovingViolation_p=round(MovingViolation/total*100,2), Other_p=round(Other/total*100,2),
         Registration_p=round(Registration/total*100,2), Seatbelt_p=round(Seatbelt/total*100,2), SpeedRelated_p=round(SpeedRelated/total*100,2),
         StopSign_p=round(StopSign/total*100,2), SuspendedLicense_p=round(SuspendedLicense/total*100,2), TrafficControlSignal_p=round(TrafficControlSignal/total*100,2),
         WindowTint_p=round(WindowTint/total*100,2))
state_statute_code_summary$total <- NULL

state_mega <- cbind(state_mega, state_statute_code_summary)

# Month of the year summary

## Putting the months in order
stops$Month <- as.factor(stops$Month)
stops$Month = factor(stops$Month,levels(stops$Month)[c(11, 10, 3, 5, 4, 8, 1, 9, 7, 6, 2, 12 )])

stops_months_summary <- stops %>%
  group_by(DepartmentName, Month) %>%
  summarise(stops=n()) %>%
  spread(Month, stops) 

state_stops_months_summary <- stops %>%
  group_by(Month) %>%
  summarise(stops=n()) %>%
  spread(Month, stops) 

state_mega <- cbind(state_mega, state_stops_months_summary)


# Day of the week summary

## Pulling out the day

stops$day <- lubridate::wday(stops$InterventionDate, label=TRUE)

stops_day_summary <- stops %>%
  group_by(DepartmentName, day) %>%
  summarise(stops=n()) %>%
  spread(day, stops) 

state_stops_day_summary <- stops %>%
  group_by(day) %>%
  summarise(stops=n()) %>%
  spread(day, stops) 

state_mega <- cbind(state_mega, state_stops_day_summary)


# Hour of the day summary

## Pulling out the hour

stops$hour <- lubridate::hour(stops$InterventionDateTime)


stops_hour_summary <- stops %>%
  group_by(DepartmentName, hour) %>%
  summarise(stops=n()) %>%
  spread(hour, stops) 


state_stops_hour_summary <- stops %>%
  group_by(hour) %>%
  summarise(stops=n()) %>%
  spread(hour, stops) 

# Cleaning up the column names a bit so it's compatible with binding to a future dataframe

state_mega <- cbind(state_mega, state_stops_hour_summary)
names(state_mega)[names(state_mega) == '0'] <- 'X0'
names(state_mega)[names(state_mega) == '1'] <- 'X1'
names(state_mega)[names(state_mega) == '2'] <- 'X2'
names(state_mega)[names(state_mega) == '3'] <- 'X3'
names(state_mega)[names(state_mega) == '4'] <- 'X4'
names(state_mega)[names(state_mega) == '5'] <- 'X5'
names(state_mega)[names(state_mega) == '6'] <- 'X6'
names(state_mega)[names(state_mega) == '7'] <- 'X7'
names(state_mega)[names(state_mega) == '8'] <- 'X8'
names(state_mega)[names(state_mega) == '9'] <- 'X9'
names(state_mega)[names(state_mega) == '10'] <- 'X10'
names(state_mega)[names(state_mega) == '11'] <- 'X11'
names(state_mega)[names(state_mega) == '12'] <- 'X12'
names(state_mega)[names(state_mega) == '13'] <- 'X13'
names(state_mega)[names(state_mega) == '14'] <- 'X14'
names(state_mega)[names(state_mega) == '15'] <- 'X15'
names(state_mega)[names(state_mega) == '16'] <- 'X16'
names(state_mega)[names(state_mega) == '17'] <- 'X17'
names(state_mega)[names(state_mega) == '18'] <- 'X18'
names(state_mega)[names(state_mega) == '19'] <- 'X19'
names(state_mega)[names(state_mega) == '20'] <- 'X20'
names(state_mega)[names(state_mega) == '21'] <- 'X21'
names(state_mega)[names(state_mega) == '22'] <- 'X22'
names(state_mega)[names(state_mega) == '23'] <- 'X23'
names(state_mega)[names(state_mega) == '24'] <- 'X24'




# how many town residents stopped

stops$TownRecidentIndicator <- as.character(stops$TownRecidentIndicator)
stops$TownRecidentIndicator <- gsub("0", "not.resident", stops$TownRecidentIndicator)
stops$TownRecidentIndicator <- gsub("1", "resident", stops$TownRecidentIndicator)

town_residents_stops <- stops %>%
  group_by(DepartmentName, TownRecidentIndicator) %>%
  summarise(total=n()) %>%
  spread(TownRecidentIndicator, total) %>%
  group_by(DepartmentName) %>%
  mutate(total=sum(resident, not.resident, na.rm=TRUE)) %>%
  mutate(resident_p=round(resident/total*100,2), not.resident_p=round(not.resident/total*100,2))

town_residents_stops[,4] <- NULL
town_residents_stops$total <- NULL



state_residents_stops <- stops %>%
  group_by(TownRecidentIndicator) %>%
  summarise(total=n()) 

state_residents_stops$TownRecidentIndicator[3] <- "total"
state_residents_stops$total[3] <- sum(state_residents_stops$total)

state_residents_stops <- state_residents_stops %>%
  spread(TownRecidentIndicator, total) %>%
  mutate(total=sum(resident, not.resident, na.rm=TRUE)) %>%
  mutate(resident_p=round(resident/total*100,2), not.resident_p=round(not.resident/total*100,2))

state_residents_stops[,3] <- NULL
state_mega <- cbind(state_mega, state_residents_stops)

## by race

town_residents_stops_race_b <- stops %>%
  group_by(DepartmentName, TownRecidentIndicator, RE) %>%
  summarise(total=n()) %>%
  filter(RE=="Black") %>%
  spread(TownRecidentIndicator, total) %>%
  group_by(DepartmentName, RE) %>%
  mutate(total=sum(not.resident, resident, na.rm=TRUE)) %>%
  mutate(not.resident_b_p=round(not.resident/total*100,2),resident_b_p=round(resident/total*100,2)) %>%
  select(DepartmentName, not.resident_b_p, resident_b_p)

state_residents_stops_race_b <- stops %>%
  group_by(TownRecidentIndicator, RE) %>%
  summarise(total=n()) %>%
  filter(RE=="Black") %>%
  spread(TownRecidentIndicator, total) %>%
  group_by(RE) %>%
  mutate(total=sum(not.resident, resident, na.rm=TRUE)) %>%
  mutate(not.resident_b_p=round(not.resident/total*100,2),resident_b_p=round(resident/total*100,2)) %>%
  select(not.resident_b_p, resident_b_p)

state_mega <- cbind(state_mega, state_residents_stops_race_b)

town_residents_stops_race_h <- stops %>%
  group_by(DepartmentName, TownRecidentIndicator, RE) %>%
  summarise(total=n()) %>%
  filter(RE=="hispanic") %>%
  spread(TownRecidentIndicator, total) %>%
  group_by(DepartmentName, RE) %>%
  mutate(total=sum(not.resident, resident, na.rm=TRUE)) %>%
  mutate(not.resident_h_p=round(not.resident/total*100,2),resident_h_p=round(resident/total*100,2)) %>%
  select(DepartmentName, not.resident_h_p, resident_h_p)

state_residents_stops_race_h <- stops %>%
  group_by(TownRecidentIndicator, RE) %>%
  summarise(total=n()) %>%
  filter(RE=="hispanic") %>%
  spread(TownRecidentIndicator, total) %>%
  group_by(RE) %>%
  mutate(total=sum(not.resident, resident, na.rm=TRUE)) %>%
  mutate(not.resident_h_p=round(not.resident/total*100,2),resident_h_p=round(resident/total*100,2)) %>%
  select(not.resident_h_p, resident_h_p)

state_mega <- cbind(state_mega, state_residents_stops_race_h)


town_residents_stops_race_m <- stops %>%
  group_by(DepartmentName, TownRecidentIndicator, RE) %>%
  summarise(total=n()) %>%
  spread(RE, total) %>%
  group_by(DepartmentName, TownRecidentIndicator) %>%
  mutate(total=sum(Asian,Black,hispanic,Indian,`middle-eastern`, White, na.rm=TRUE))  %>%
  mutate(minorities=total-White) %>%
  select(DepartmentName, TownRecidentIndicator, minorities) %>%
  group_by(DepartmentName) %>%
  mutate(total=sum(minorities), percent=round(minorities/total*100,2)) %>%
  select(DepartmentName, TownRecidentIndicator, percent) %>%
  spread(TownRecidentIndicator, percent)
town_residents_stops_race_m[,4] <- NULL

colnames(town_residents_stops_race_m) <- c("DepartmentName", "not.resident_m_p", "resident_m_p")

state_residents_stops_race_m <- stops %>%
  group_by(TownRecidentIndicator, RE) %>%
  summarise(total=n()) %>%
  spread(RE, total) %>%
  group_by(TownRecidentIndicator) %>%
  mutate(total=sum(Asian,Black,hispanic,Indian,`middle-eastern`, White, na.rm=TRUE))  %>%
  mutate(minorities=total-White) 

state_residents_stops_race_m <- state_residents_stops_race_m[-3,]

state_residents_stops_race_m <- state_residents_stops_race_m %>%
  select(TownRecidentIndicator, minorities) %>%
  mutate(total=sum(minorities), percent=round(minorities/total*100,2)) %>%
  select(TownRecidentIndicator, percent) %>%
  spread(TownRecidentIndicator, percent)

colnames(state_residents_stops_race_m) <- c("not.resident_m_p", "resident_m_p")
state_mega <- cbind(state_mega, state_residents_stops_race_m)
  


# VEHICLESEARCHEDINDICATOR


stops$VehicleSearchedIndicator <- as.character(stops$VehicleSearchedIndicator)
stops$VehicleSearchedIndicator <- gsub("0", "not.searched", stops$VehicleSearchedIndicator)
stops$VehicleSearchedIndicator <- gsub("1", "searched", stops$VehicleSearchedIndicator)

town_searches_stops <- stops %>%
  group_by(DepartmentName, VehicleSearchedIndicator) %>%
  summarise(total=n()) %>%
  spread(VehicleSearchedIndicator , total) %>%
  group_by(DepartmentName) %>%
  mutate(total=sum(searched, not.searched, na.rm=TRUE)) %>%
  mutate(searched_p=round(searched/total*100,2), not.searched_p=round(not.searched/total*100,2))
town_searches_stops$total <- NULL


state_searches_stops <- stops %>%
  group_by(VehicleSearchedIndicator) %>%
  summarise(total=n()) 

temp_df <- t(data.frame(c(VehicleSearchedIndicator="total", total=sum(state_searches_stops$total))))
rownames(temp_df) <- NULL
state_searches_stops <- data.frame(state_searches_stops)
state_searches_stops <- rbind(state_searches_stops,temp_df)

state_searches_stops$total <- as.numeric(state_searches_stops$total)

state_searches_stops <- state_searches_stops %>%
  spread(VehicleSearchedIndicator, total) %>%
  mutate(searched_p=round(searched/total*100,2), not.searched_p=round(not.searched/total*100,2))
state_searches_stops$total <- NULL

state_mega <- cbind(state_mega, state_searches_stops)


# VEHICLESEARCHEDINDICATOR by race 

town_searches_race_stops <- stops %>%
  group_by(DepartmentName, ethnicity, VehicleSearchedIndicator) %>%
  summarise(total=n()) %>%
  spread(VehicleSearchedIndicator, total) %>%
  group_by(DepartmentName) %>%
  mutate(total=sum(searched, not.searched, na.rm=TRUE)) %>%
  select(DepartmentName, ethnicity, searched, total) %>%
  mutate(searched_p=round(searched/total*100,2)) %>%
  select(DepartmentName, ethnicity, searched_p) %>%
  spread(ethnicity, searched_p)
town_searches_race_stops$total <- NULL

state_searches_race_stops <- stops %>%
  group_by(ethnicity, VehicleSearchedIndicator) %>%
  summarise(total=n()) %>%
  spread(VehicleSearchedIndicator, total) %>%
  mutate(total=sum(searched, not.searched, na.rm=TRUE)) %>%
  select(ethnicity, searched, total) %>%
  mutate(searched_p=round(searched/total*100,2)) %>%
  select(ethnicity, searched_p) %>%
  spread(ethnicity, searched_p)
state_searches_race_stops$total <- NULL

state_mega <- cbind(state_mega, state_searches_race_stops)
names(state_mega)[names(state_mega) == 'White'] <- 'White.y'
names(state_mega)[names(state_mega) == 'Minority'] <- 'Minority.x'


# INTERVENTIONTECHNIQUECODE BY RACE??

stops$InterventionTechniqueCode <- as.character(stops$InterventionTechniqueCode)
stops$InterventionTechniqueCode <- gsub("G", "general.enforcement", stops$InterventionTechniqueCode)
stops$InterventionTechniqueCode <- gsub("B", "blind.enforcement", stops$InterventionTechniqueCode)
stops$InterventionTechniqueCode <- gsub("S", "spot.check", stops$InterventionTechniqueCode)

town_intervention_stops <- stops %>%
  group_by(DepartmentName, InterventionTechniqueCode) %>%
  summarise(total=n()) %>%
  spread(InterventionTechniqueCode, total) %>%
  group_by(DepartmentName) %>%
  mutate(total=sum(general.enforcement, blind.enforcement, spot.check, na.rm=TRUE)) %>%
  mutate(general.enforcement_p=round(general.enforcement/total*100,2), blind.enforcement_p=round(blind.enforcement/total*100,2), spot.check_p=round(spot.check/total*100,2))
town_intervention_stops$total <- NULL


state_intervention_stops <- stops %>%
  group_by(InterventionTechniqueCode) %>%
  summarise(total=n()) %>%
  spread(InterventionTechniqueCode, total) %>%
    mutate(total=sum(general.enforcement, blind.enforcement, spot.check, na.rm=TRUE)) %>%
  mutate(general.enforcement_p=round(general.enforcement/total*100,2), blind.enforcement_p=round(blind.enforcement/total*100,2), spot.check_p=round(spot.check/total*100,2))
state_intervention_stops$total <- NULL

state_mega <- cbind(state_mega, state_intervention_stops)

# Spot checks
town_spot.checks_race_stops <- stops %>%
  group_by(DepartmentName, ethnicity, InterventionTechniqueCode) %>%
  summarise(total=n()) %>%
  spread(InterventionTechniqueCode, total) %>%
  group_by(DepartmentName) %>%
  mutate(total=sum(general.enforcement, blind.enforcement, spot.check, na.rm=TRUE)) %>%
  select(DepartmentName, ethnicity, spot.check, total) %>%
  mutate(spot.check_p=round(spot.check/total*100,2)) %>%
  select(DepartmentName, ethnicity, spot.check_p) %>%
  spread(ethnicity, spot.check_p)

state_spot.checks_race_stops <- stops %>%
  group_by(ethnicity, InterventionTechniqueCode) %>%
  summarise(total=n()) %>%
  spread(InterventionTechniqueCode, total) %>%
  mutate(total=sum(general.enforcement, blind.enforcement, spot.check, na.rm=TRUE)) %>%
  select(ethnicity, spot.check, total) %>%
  mutate(spot.check_p=round(spot.check/total*100,2)) %>%
  select(ethnicity, spot.check_p) %>%
  spread(ethnicity, spot.check_p)

state_mega <- cbind(state_mega, state_spot.checks_race_stops)
names(state_mega)[names(state_mega) == 'Minority'] <- 'Minority.y'
names(state_mega)[names(state_mega) == 'White'] <- 'White.x.1'

# Blind enforcement

town_blind.enforcement_race_stops <- stops %>%
  group_by(DepartmentName, ethnicity, InterventionTechniqueCode) %>%
  summarise(total=n()) %>%
  spread(InterventionTechniqueCode, total) %>%
  group_by(DepartmentName) %>%
  mutate(total=sum(general.enforcement, blind.enforcement, spot.check, na.rm=TRUE)) %>%
  select(DepartmentName, ethnicity, blind.enforcement, total) %>%
  mutate(blind.enforcement_p=round(blind.enforcement/total*100,2)) %>%
  select(DepartmentName, ethnicity, blind.enforcement_p) %>%
  spread(ethnicity, blind.enforcement_p)


state_blind.enforcement_race_stops <- stops %>%
  group_by(ethnicity, InterventionTechniqueCode) %>%
  summarise(total=n()) %>%
  spread(InterventionTechniqueCode, total) %>%
  mutate(total=sum(general.enforcement, blind.enforcement, spot.check, na.rm=TRUE)) %>%
  select(ethnicity, blind.enforcement, total) %>%
  mutate(blind.enforcement_p=round(blind.enforcement/total*100,2)) %>%
  select(ethnicity, blind.enforcement_p) %>%
  spread(ethnicity, blind.enforcement_p)

state_mega <- cbind(state_mega, state_blind.enforcement_race_stops)
names(state_mega)[names(state_mega) == 'White'] <- 'White.y.1'

# minorities out of residential stops

town_residents_stops_race_m_r <- stops %>%
  group_by(DepartmentName, TownRecidentIndicator, RE) %>%
  summarise(total=n()) %>%
  spread(RE, total) %>%
  group_by(DepartmentName, TownRecidentIndicator) %>%
  mutate(total=sum(Asian,Black,hispanic,Indian,`middle-eastern`, White, na.rm=TRUE))  %>%
  mutate(minorities=total-White) %>%
  filter(TownRecidentIndicator=="resident") %>%
  select(DepartmentName, TownRecidentIndicator, total, minorities) %>%
  mutate(resident_m_p_r=round(minorities/total*100,2)) %>%
  select(resident_m_p_r)

state_residents_stops_race_m_r <- stops %>%
  group_by(TownRecidentIndicator, RE) %>%
  summarise(total=n()) %>%
  spread(RE, total) 

state_residents_stops_race_m_r <- state_residents_stops_race_m_r[-3,]

state_residents_stops_race_m_r<- state_residents_stops_race_m_r %>%
  group_by(TownRecidentIndicator) %>%
  mutate(total=sum(Asian,Black,hispanic,Indian,`middle-eastern`, White, na.rm=TRUE))  %>%
  mutate(minorities=total-White) %>%
  filter(TownRecidentIndicator=="resident") %>%
  select(TownRecidentIndicator, total, minorities) %>%
  mutate(resident_m_p_r=round(minorities/total*100,2)) %>%
  select(resident_m_p_r)
state_mega <- cbind(state_mega, state_residents_stops_race_m_r)


# blacks out of residential stops

town_residents_stops_race_b_r <- stops %>%
  group_by(DepartmentName, TownRecidentIndicator, RE) %>%
  summarise(total=n()) %>%
  filter(TownRecidentIndicator=="resident") %>%
  spread(RE, total) %>%
  group_by(DepartmentName) %>%
  mutate(total=sum(Asian, Black, hispanic, Indian, `middle-eastern`, White, na.rm=TRUE)) %>%
  mutate(resident_b_p_r=round(Black/total*100,2)) %>%
  select(resident_b_p_r)

state_residents_stops_race_b_r <- stops %>%
  group_by(TownRecidentIndicator, RE) %>%
  summarise(total=n()) %>%
  filter(TownRecidentIndicator=="resident") %>%
  spread(RE, total) %>%
#  group_by(TownRecidentIndicator) %>%
  mutate(total=sum(Asian, Black, hispanic, Indian, `middle-eastern`, White, na.rm=TRUE)) %>%
  mutate(resident_b_p_r=round(Black/total*100,2)) %>%
  select(resident_b_p_r)
state_mega <- cbind(state_mega, state_residents_stops_race_b_r)

# hispanics out of residential stops

town_residents_stops_race_h_r <- stops %>%
  group_by(DepartmentName, TownRecidentIndicator, RE) %>%
  summarise(total=n()) %>%
  filter(TownRecidentIndicator=="resident") %>%
  spread(RE, total) %>%
  group_by(DepartmentName) %>%
  mutate(total=sum(Asian, Black, hispanic, Indian, `middle-eastern`, White, na.rm=TRUE)) %>%
  mutate(resident_h_p_r=round(hispanic/total*100,2)) %>%
  select(resident_h_p_r)

state_residents_stops_race_h_r <- stops %>%
  group_by(TownRecidentIndicator, RE) %>%
  summarise(total=n()) %>%
  filter(TownRecidentIndicator=="resident") %>%
  spread(RE, total) %>%
#  group_by(TownRecidentIndicator) %>%
  mutate(total=sum(Asian, Black, hispanic, Indian, `middle-eastern`, White, na.rm=TRUE)) %>%
  mutate(resident_h_p_r=round(hispanic/total*100,2)) %>%
  select(resident_h_p_r)
state_mega <- cbind(state_mega, state_residents_stops_race_h_r)

# Combining all the previous individual data frames into one mega data frame

mega_town_df <- left_join(town_light_dark_summary, town_race_summary, by="DepartmentName")
mega_town_df <- left_join(mega_town_df, town_disposition_summary, by="DepartmentName")
mega_town_df <- left_join(mega_town_df, town_gender_summary, by="DepartmentName")
mega_town_df <- left_join(mega_town_df, town_age_summary, by="DepartmentName")
mega_town_df <- left_join(mega_town_df, town_age_group_summary, by="DepartmentName")
mega_town_df <- left_join(mega_town_df, town_statute_code_summary, by="DepartmentName")
mega_town_df <- left_join(mega_town_df, stops_months_summary, by="DepartmentName")
mega_town_df <- left_join(mega_town_df, stops_day_summary, by="DepartmentName")
mega_town_df <- left_join(mega_town_df, stops_hour_summary, by="DepartmentName")
mega_town_df <- left_join(mega_town_df, town_residents_stops, by="DepartmentName")
mega_town_df <- left_join(mega_town_df, town_residents_stops_race_b, by="DepartmentName")
mega_town_df <- left_join(mega_town_df, town_residents_stops_race_h, by="DepartmentName")
mega_town_df <- left_join(mega_town_df, town_residents_stops_race_m, by="DepartmentName")

mega_town_df <- left_join(mega_town_df, town_searches_stops, by="DepartmentName")
mega_town_df <- left_join(mega_town_df, town_searches_race_stops, by="DepartmentName")
mega_town_df <- left_join(mega_town_df, town_intervention_stops, by="DepartmentName")
mega_town_df <- left_join(mega_town_df, town_spot.checks_race_stops, by="DepartmentName")
mega_town_df <- left_join(mega_town_df, town_blind.enforcement_race_stops, by="DepartmentName")

mega_town_df <- cbind(mega_town_df, town_residents_stops_race_m_r)
mega_town_df <- cbind(mega_town_df, town_residents_stops_race_b_r)
mega_town_df <- cbind(mega_town_df, town_residents_stops_race_h_r)

mega_town_df <- data.frame(mega_town_df)

# Binding the department-level dataframe with the state-level dataframe that was created concurrently
mega_town_df <- rbind(mega_town_df, state_mega)

# Bringing in FBI UCR data
fbi <-read.csv("data/fbi-department-data.csv", stringsAsFactors=FALSE)
fbi2 <- data.table(fbi)
mega_town_df <- dplyr::left_join(mega_town_df,fbi2, by="DepartmentName")

# Bringing in and joining driving-age population data
over15 <- read.csv("data/race_over15.csv", stringsAsFactors=FALSE)
mega_town_df <- dplyr::left_join(mega_town_df,over15, by="DepartmentName")

# Figuring out the difference between the state average and department average for pullovers
mega_town_df$b_t_s_diff <- mega_town_df$black_p - state_mega$black_p[1]
mega_town_df$h_t_s_diff <- mega_town_df$hispanic_p - state_mega$hispanic_p[1]
mega_town_df$minorities_p <- 100-mega_town_df$white_p

# Isolating the Connecticut average
state_mega2 <- subset(mega_town_df, DepartmentName=="Connecticut average")

# Figuring out the difference between the state average and department average for pullovers (minorities)
mega_town_df$m_t_s_diff <- mega_town_df$minorities_p - state_mega2$minorities_p[1]

# Calculating the percent makeup of those over 15 (driving age) by town and state
mega_town_df$white_over_15_p <- round(mega_town_df$white_over_15/mega_town_df$population*100,2)

mega_town_df$black_over_15_p <- round(mega_town_df$black_over_15/mega_town_df$population*100,2)
mega_town_df$hispanic_over_15_p <- round(mega_town_df$hispanic_over_15/mega_town_df$population*100,2)
mega_town_df$minorities_over_15_p <- round(mega_town_df$minorities_over_15/mega_town_df$population*100,2)

state_mega2 <- subset(mega_town_df, DepartmentName=="Connecticut average")

# Calculating the difference between the state average and town average for racial makeup
mega_town_df$b_t_s_pop_diff <- round(mega_town_df$black_over_15_p - state_mega2$black_over_15_p[1],2)
mega_town_df$h_t_s_pop_diff <- round(mega_town_df$hispanic_over_15_p - state_mega2$hispanic_over_15_p[1],2)
mega_town_df$m_t_s_pop_diff <- round(mega_town_df$minorities_over_15_p - state_mega2$minorities_over_15_p[1],2)

# Calculating the distance between the stops and population state/town differences
mega_town_df$b_distance <- round(mega_town_df$b_t_s_diff - mega_town_df$b_t_s_pop_diff,2)
mega_town_df$h_distance <- round(mega_town_df$h_t_s_diff - mega_town_df$h_t_s_pop_diff,2)
mega_town_df$m_distance <- round(mega_town_df$m_t_s_diff - mega_town_df$m_t_s_pop_diff,2)

# Calculating total residents in a town by race
mega_town_df$b_res <- round(mega_town_df$Black_total/mega_town_df$population*100,2)
mega_town_df$h_res <- round(mega_town_df$Hispanic_total/mega_town_df$population*100,2)
mega_town_df$m_res <- round((mega_town_df$population-mega_town_df$White_total)/mega_town_df$population*100,2)

# Calculating resident difference and ratio (minorities)
mega_town_df$res_diff_m <- round(mega_town_df$resident_m_p_r - mega_town_df$minorities_over_15_p, 2)
#mega_town_df$res_ratio_m <- round(mega_town_df$resident_m_p_r/mega_town_df$m_res, 2)
mega_town_df$res_ratio_m <- round(mega_town_df$resident_m_p_r/mega_town_df$minorities_over_15_p, 2)

# Calculating resident difference and ratio (black)
mega_town_df$res_diff_b <- round(mega_town_df$resident_b_p_r - mega_town_df$black_over_15_p, 2)
#mega_town_df$res_ratio_b <- round(mega_town_df$resident_b_p_r/mega_town_df$b_res, 2)
mega_town_df$res_ratio_b <- round(mega_town_df$resident_b_p_r/mega_town_df$black_over_15_p, 2)

# Calculating resident difference and ratio (Hispanic)
mega_town_df$res_diff_h <- round(mega_town_df$resident_h_p_r - mega_town_df$hispanic_over_15_p, 2)
#mega_town_df$res_ratio_h <- round(mega_town_df$resident_h_p_r/mega_town_df$h_res, 2)
mega_town_df$res_ratio_h <- round(mega_town_df$resident_h_p_r/mega_town_df$hispanic_over_15_p, 2)

# Bringing in and joining estimated driving population as calculated by CCSU
edp <- read.csv("data/edp_from_report.csv", stringsAsFactors=FALSE)
mega_town_df <- left_join(mega_town_df, edp, by="DepartmentName")

# Calculating difference between minority stops and minority population
mega_town_df$edp_m_diff <- round(mega_town_df$edp_m_s - mega_town_df$edp_m, 2)
mega_town_df$edp_b_diff <- round(mega_town_df$edp_b_s - mega_town_df$edp_b, 2)
mega_town_df$edp_h_diff <- round(mega_town_df$edp_h_s - mega_town_df$edp_h, 2)

# Calculating the ratio between minority stops and minority population
mega_town_df$edp_m_ratio <- round(mega_town_df$edp_m_s/mega_town_df$edp_m, 2)
mega_town_df$edp_b_ratio <- round(mega_town_df$edp_b_s/mega_town_df$edp_b, 2)
mega_town_df$edp_h_ratio <- round(mega_town_df$edp_h_s/mega_town_df$edp_h, 2)

state_mega_safe <- subset(mega_town_df, DepartmentName=="Connecticut average")

# creating some new columns so they'll line up with the department data frame
mega_town_df$searched<- NULL
mega_town_df$searched_p<- NULL
mega_town_df$hispanic_p<- NULL
mega_town_df$h_t_s_diff<- NULL
mega_town_df$hispanic_16<- NULL
#mega_town_df$hispanic_over_15_p<- NULL
mega_town_df$h_distance <- NULL
mega_town_df$h_t_s_pop_diff<- NULL
mega_town_df$resident_h_p_r<- NULL
mega_town_df$minorities_p<- NULL
mega_town_df$m_t_s_diff<- NULL
mega_town_df$minorities_16<- NULL
#mega_town_df$minorities_over_15_p<- NULL
mega_town_df$m_distance <- NULL
mega_town_df$m_t_s_pop_diff<- NULL
mega_town_df$resident_m_p_r<- NULL
mega_town_df$black_p<- NULL
mega_town_df$b_t_s_diff<- NULL
mega_town_df$black_16<- NULL
#mega_town_df$black_over_15_p<- NULL
mega_town_df$b_distance <- NULL
mega_town_df$b_t_s_pop_diff<- NULL
mega_town_df$resident_b_p_r<- NULL
mega_town_df$edp_b_t<- NULL
mega_town_df$edp_b_p<- NULL
mega_town_df$edp_b<- NULL
mega_town_df$edp_b_diff<- NULL
mega_town_df$edp_b_ratio<- NULL
mega_town_df$edp_h_t<- NULL
mega_town_df$edp_h_p<- NULL
mega_town_df$edp_h<- NULL
mega_town_df$edp_h_diff<- NULL
mega_town_df$edp_h_ratio<- NULL
mega_town_df$edp_m_t<- NULL
mega_town_df$edp_m_p<- NULL
mega_town_df$edp_m<- NULL
mega_town_df$edp_m_diff<- NULL
mega_town_df$edp_m_ratio<- NULL
mega_town_df$residents_total<- NULL
mega_town_df$b_res<- NULL
mega_town_df$b_res_total<- NULL
mega_town_df$b_res_stops<- NULL
mega_town_df$res_diff_b<- NULL
mega_town_df$res_ratio_b<- NULL
mega_town_df$h_res<- NULL
mega_town_df$h_res_total<- NULL
mega_town_df$h_res_stops<- NULL
mega_town_df$res_diff_h<- NULL
mega_town_df$res_ratio_h<- NULL
mega_town_df$m_res<- NULL
mega_town_df$m_res_total<- NULL
mega_town_df$m_res_stops<- NULL
mega_town_df$res_diff_m<- NULL
mega_town_df$res_ratio_m<- NULL
mega_town_df$m_coeff<- NULL
mega_town_df$b_coeff<- NULL
mega_town_df$h_coeff<- NULL
mega_town_df$b_h_coeff<- NULL
mega_town_df$m_coeff_mv<- NULL
mega_town_df$b_coeff_mv<- NULL
mega_town_df$h_coeff_mv<- NULL
mega_town_df$b_h_coeff_mv<- NULL
mega_town_df$m_kpt_diff<- NULL
mega_town_df$b_kpt_diff<- NULL
mega_town_df$h_kpt_diff<- NULL
mega_town_df$b_h_kpt_diff<- NULL
mega_town_df$m_kpt_pv<- NULL
mega_town_df$b_kpt_pv<- NULL
mega_town_df$h_kpt_pv<- NULL
mega_town_df$b_h_kpt_pv<- NULL
mega_town_df$m_kpt_ess<- NULL
mega_town_df$b_kpt_ess<- NULL
mega_town_df$h_kpt_ess<- NULL
mega_town_df$b_h_kpt_ess<- NULL
mega_town_df$m_synth<- NULL
mega_town_df$b_synth<- NULL
mega_town_df$h_synth<- NULL
mega_town_df$b_h_synth<- NULL

mega_town_df <- left_join(mega_town_df,ccsu_data)

where_state <- nrow(mega_town_df)

# This will replace certain columns from the original dataframe with the columns from the CCSU dataframe

for (i in 2:ncol(state_mega_safe)) {

  c_name <- colnames(mega_town_df[i])
  
  for (x in 2:ncol(state_mega_safe)) {
    if (colnames(state_mega_safe[x])==c_name) {
      mega_town_df[where_state, i] <- state_mega_safe[1,x]
    }
    
  }
}
  
#state_mega_safe 

write.csv(mega_town_df, "data/mega_town_df11.csv")

# Next step, run officers.R
