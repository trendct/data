# This script creates an officer-level summary analysis that is appended to the department-level summary analysis
# The generated dataframe is used for stories and the database at http://trafficstops.trendct.org/
# BEFORE running this script, run towns.R

library(readxl)
library(data.table)
library(lubridate)
library(dplyr)
library(maptools)
library(stringr)
library(tidyr)
library(ggalt)
library(data.table)

# The data is too large to host on github but can be found at http://ctrp3.ctdata.org/rawdata/
stops <- read_excel("ignore/RP-2014-2015-Data-3-9-16.xlsx", sheet=1)

## Adjusting the names column
names(stops)[names(stops) == 'Department Name'] <- 'DepartmentName'

## Creating a backup
stops_backup <- stops
# stops <- stops_backup

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

# Figuring out the total number of stops by officer

officer_departments <- stops %>%
  select(DepartmentName, ReportingOfficerIdentificationID) %>%
  unique()

# This adds an extra level of identification information in case officers have the same ID number but are in different departments

officer_departments$ReportingOfficerIdentificationID <- paste0(officer_departments$DepartmentName, "--", officer_departments$ReportingOfficerIdentificationID)
stops$ReportingOfficerIdentificationID <- paste0(stops$DepartmentName, "--", stops$ReportingOfficerIdentificationID)

# Adjust for time to Eastern Standard Time

stops$ForRealTime <- ymd_hms(stops$InterventionDateTime, tz="America/New_York")

stops$date.join <- as.character(stops$InterventionDate)
stops$date.join <- gsub(" .*", "", stops$date.join)

# This function gathers the time of sunrise, noon, sunset, dawn, and dusk based on location and date

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

# Testing out the function
sunsetrise <- ephemeris(41.93608, -72.79147, "2014-10-01", 365, tz="America/New_York")

# Creating a data frame of sunrise, noon, sunset, dawn, and dusk based on locations indicated by researchers at CCSu

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


# write.csv(stops, "data/stops_fixed.csv")

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

officer_light_dark_summary <- stops %>%
  group_by(ReportingOfficerIdentificationID, light_dark, ethnicity_wm) %>%
  summarise(total=n()) %>%
  filter(light_dark=='dark' | light_dark=='light') %>%
  spread(ethnicity_wm, total) %>%
  mutate(minority_p=round(Minority/(Minority+White)*100,2), white_p=100-minority_p) %>%
  select(ReportingOfficerIdentificationID, light_dark, minority_p) %>%
  spread(light_dark, minority_p) 

colnames(officer_light_dark_summary) <- c("ReportingOfficerIdentificationID", "minority_dark", "minority_light")
officer_light_dark_summary$white_dark <- 100- officer_light_dark_summary$minority_dark
officer_light_dark_summary$white_light <- 100- officer_light_dark_summary$minority_light

officer_light_dark_summary <- officer_light_dark_summary %>%
  select(ReportingOfficerIdentificationID, minority_dark, white_dark, minority_light, white_light)

# Racial summary, percent breakdown of stops by race

officer_race_summary <- stops %>%
  group_by(ReportingOfficerIdentificationID, RE) %>%
  summarise(total=n()) %>%
  spread(RE, total) %>%
  group_by(ReportingOfficerIdentificationID) %>%
  mutate(total=sum(Asian, Black, hispanic, Indian, `middle-eastern`, White, na.rm=TRUE), min=total-White) %>%
  mutate(asian_p=round(Asian/total*100,2), black_p=round(Black/total*100,2), hispanic_p=round(hispanic/total*100,2), 
         indian_p=round(Indian/total*100,2), mid_e_p=round(`middle-eastern`/total*100,2), white_p=round(White/total*100,2), min_p=round(min/total*100,2))

# Summary of stop results

officer_disposition_summary <- stops %>%
  group_by(ReportingOfficerIdentificationID, InterventionDispositionCode) %>%
  summarise(total=n()) %>%
  spread(InterventionDispositionCode, total) %>%
  group_by(ReportingOfficerIdentificationID) %>%
  mutate(total=sum(I, M, N, U, V, W, na.rm=TRUE)) %>%
  mutate(infraction_ticket_p=round(I/total*100,2), misdemeanor_summons_p=round(M/total*100,2),
         no_disposition_p=round(N/total*100,2), uniform_arrest_p=round(U/total*100,2),
         verbal_warning_p=round(V/total*100,2), written_warning_p=round(W/total*100,2))
officer_disposition_summary$total <- NULL
names(officer_disposition_summary)[names(officer_disposition_summary) == 'I'] <- 'infraction_ticket'
names(officer_disposition_summary)[names(officer_disposition_summary) == 'M'] <- 'misdemeanor_summons'
names(officer_disposition_summary)[names(officer_disposition_summary) == 'N'] <- 'no_disposition'
names(officer_disposition_summary)[names(officer_disposition_summary) == 'U'] <- 'uniform_arrest'
names(officer_disposition_summary)[names(officer_disposition_summary) == 'V'] <- 'verbal_warning'
names(officer_disposition_summary)[names(officer_disposition_summary) == 'W'] <- 'written_warning'

# gender summary

officer_gender_summary <- stops %>%
  group_by(ReportingOfficerIdentificationID, SubjectSexCode) %>%
  summarise(total=n()) %>%
  spread(SubjectSexCode, total) %>%
  group_by(ReportingOfficerIdentificationID) %>%
  mutate(total=sum(F, M, na.rm=TRUE)) %>%
  mutate(male_p=round(M/total*100,2), female_p=round(F/total*100,2))
officer_gender_summary$total <- NULL

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
officer_age_summary <- stops %>%
  group_by(ReportingOfficerIdentificationID) %>%
  summarise(average_age=round(mean(SubjectAge, na.rm=TRUE),0), median_age=round(median(SubjectAge, na.rm=TRUE),0))

# Age group summary
officer_age_group_summary <- stops %>%
  group_by(ReportingOfficerIdentificationID, agegroup) %>%
  summarise(total=n()) %>%
  spread(agegroup, total) 

officer_age_group_summary[,2] <- NULL

officer_age_group_summary <- officer_age_group_summary %>%
  group_by(ReportingOfficerIdentificationID) %>%
  mutate(total = sum(y16_21, y22_27, y28_31, y32_37, y38_41, y42_47, y48_51,
                     y52_57, y58_61, y62_67, y68_71, y72_77, y78_81, y82_, na.rm=TRUE)) %>%
  mutate(y16_21_p=round(y16_21/total*100,2), y22_27_p=round(y22_27/total*100,2), y28_31_p=round(y28_31/total*100,2), 
         y32_37_p=round(y32_37/total*100,2), y38_41_p=round(y38_41/total*100,2), y42_47_p=round(y42_47/total*100,2),
         y48_51_p=round(y48_51/total*100,2), y52_57_p=round(y52_57/total*100,2), y58_61_p=round(y58_61/total*100,2),
         y62_67_p=round(y62_67/total*100,2), y68_71_p=round(y68_71/total*100,2), y72_77_p=round(y72_77/total*100,2),
         y78_81_p=round(y78_81/total*100,2), y82_p=round(y82_/total*100,2))
officer_age_group_summary$total <- NULL

# Statute code summary
officer_statute_code_summary <- stops %>%
  group_by(ReportingOfficerIdentificationID, StatuteReason) %>%
  summarise(total=n())

officer_statute_code_summary$StatuteReason <- gsub(" ", "", officer_statute_code_summary$StatuteReason)

officer_statute_code_summary <- officer_statute_code_summary %>%
  group_by(ReportingOfficerIdentificationID, StatuteReason) %>%
  summarise(total=sum(total)) %>%
  spread(StatuteReason, total) %>%
  group_by(ReportingOfficerIdentificationID) %>%
  mutate(total=sum(CellPhone, DefectiveLights, DisplayofPlates, EquipmentViolation, MovingViolation,
                   Other, Registration, Seatbelt, SpeedRelated, StopSign, StopSign, SuspendedLicense,
                   TrafficControlSignal, WindowTint, na.rm=TRUE)) %>%
  mutate(CellPhone_p=round(CellPhone/total*100,2), DefectiveLights_p=round(DefectiveLights/total*100,2), DisplayofPlates_p=round(DisplayofPlates/total*100,2), 
         EquipmentViolation_p=round(EquipmentViolation/total*100,2), MovingViolation_p=round(MovingViolation/total*100,2), Other_p=round(Other/total*100,2),
         Registration_p=round(Registration/total*100,2), Seatbelt_p=round(Seatbelt/total*100,2), SpeedRelated_p=round(SpeedRelated/total*100,2),
         StopSign_p=round(StopSign/total*100,2), SuspendedLicense_p=round(SuspendedLicense/total*100,2), TrafficControlSignal_p=round(TrafficControlSignal/total*100,2),
         WindowTint_p=round(WindowTint/total*100,2))
officer_statute_code_summary$total <- NULL

# Month of the year summary

## Putting the months in order
stops$Month <- as.factor(stops$Month)
stops$Month = factor(stops$Month,levels(stops$Month)[c(11, 10, 3, 5, 4, 8, 1, 9, 7, 6, 2, 12 )])

stops_months_summary <- stops %>%
  group_by(ReportingOfficerIdentificationID, Month) %>%
  summarise(stops=n()) %>%
  spread(Month, stops) 

# Day of the week summary

## Pulling out the day

stops$day <- lubridate::wday(stops$InterventionDate, label=TRUE)

stops_day_summary <- stops %>%
  group_by(ReportingOfficerIdentificationID, day) %>%
  summarise(stops=n()) %>%
  spread(day, stops) 

# Hour of the day summary

## Pulling out the hour

stops$hour <- lubridate::hour(stops$InterventionDateTime)

stops_hour_summary <- stops %>%
  group_by(ReportingOfficerIdentificationID, hour) %>%
  summarise(stops=n()) %>%
  spread(hour, stops) 

# how many town residents stopped

stops$TownRecidentIndicator <- as.character(stops$TownRecidentIndicator)
stops$TownRecidentIndicator <- gsub("0", "not.resident", stops$TownRecidentIndicator)
stops$TownRecidentIndicator <- gsub("1", "resident", stops$TownRecidentIndicator)

officer_residents_stops <- stops %>%
  group_by(ReportingOfficerIdentificationID, TownRecidentIndicator) %>%
  summarise(total=n()) %>%
  spread(TownRecidentIndicator, total) %>%
  group_by(ReportingOfficerIdentificationID) %>%
  mutate(total=sum(resident, not.resident, na.rm=TRUE)) %>%
  mutate(resident_p=round(resident/total*100,2), not.resident_p=round(not.resident/total*100,2))

officer_residents_stops[,4] <- NULL
officer_residents_stops$total <- NULL

## by race (black)

officer_residents_stops_race_b <- stops %>%
  group_by(ReportingOfficerIdentificationID, TownRecidentIndicator, RE) %>%
  summarise(total=n()) %>%
  filter(RE=="Black") %>%
  spread(TownRecidentIndicator, total) %>%
  group_by(ReportingOfficerIdentificationID, RE) %>%
  mutate(total=sum(not.resident, resident, na.rm=TRUE)) %>%
  mutate(not.resident_b_p=round(not.resident/total*100,2),resident_b_p=round(resident/total*100,2)) %>%
  select(ReportingOfficerIdentificationID, not.resident_b_p, resident_b_p)

## by race (hispanic)

officer_residents_stops_race_h <- stops %>%
  group_by(ReportingOfficerIdentificationID, TownRecidentIndicator, RE) %>%
  summarise(total=n()) %>%
  filter(RE=="hispanic") %>%
  spread(TownRecidentIndicator, total) %>%
  group_by(ReportingOfficerIdentificationID, RE) %>%
  mutate(total=sum(not.resident, resident, na.rm=TRUE)) %>%
  mutate(not.resident_h_p=round(not.resident/total*100,2),resident_h_p=round(resident/total*100,2)) %>%
  select(ReportingOfficerIdentificationID, not.resident_h_p, resident_h_p)

## by race (minorities)

officer_residents_stops_race_m <- stops %>%
  group_by(ReportingOfficerIdentificationID, DepartmentName, TownRecidentIndicator, RE) %>%
  summarise(total=n()) %>%
  spread(RE, total) %>%
  group_by(ReportingOfficerIdentificationID, TownRecidentIndicator) %>%
  mutate(total=sum(Asian,Black,hispanic,Indian,`middle-eastern`, White, na.rm=TRUE))  %>%
  mutate(minorities=total-White) %>%
  select(ReportingOfficerIdentificationID, TownRecidentIndicator, minorities) %>%
  group_by(ReportingOfficerIdentificationID) %>%
  mutate(total=sum(minorities), percent=round(minorities/total*100,2)) %>%
  select(ReportingOfficerIdentificationID, TownRecidentIndicator, percent) %>%
  spread(TownRecidentIndicator, percent)
officer_residents_stops_race_m[,4] <- NULL

colnames(officer_residents_stops_race_m) <- c("ReportingOfficerIdentificationID", "not.resident_m_p", "resident_m_p")

# VEHICLESEARCHEDINDICATOR


stops$VehicleSearchedIndicator <- as.character(stops$VehicleSearchedIndicator)
stops$VehicleSearchedIndicator <- gsub("0", "not.searched", stops$VehicleSearchedIndicator)
stops$VehicleSearchedIndicator <- gsub("1", "searched", stops$VehicleSearchedIndicator)

officer_searches_stops <- stops %>%
  group_by(ReportingOfficerIdentificationID, VehicleSearchedIndicator) %>%
  summarise(total=n()) %>%
  spread(VehicleSearchedIndicator , total) %>%
  group_by(ReportingOfficerIdentificationID) %>%
  mutate(total=sum(searched, not.searched, na.rm=TRUE)) %>%
  mutate(searched_p=round(searched/total*100,2), not.searched_p=round(not.searched/total*100,2))
officer_searches_stops$total <- NULL

# VEHICLESEARCHEDINDICATOR by race 

officer_searches_race_stops <- stops %>%
  group_by(ReportingOfficerIdentificationID, ethnicity, VehicleSearchedIndicator) %>%
  summarise(total=n()) %>%
  spread(VehicleSearchedIndicator, total) %>%
  group_by(ReportingOfficerIdentificationID) %>%
  mutate(total=sum(searched, not.searched, na.rm=TRUE)) %>%
  select(ReportingOfficerIdentificationID, ethnicity, searched, total) %>%
  mutate(searched_p=round(searched/total*100,2)) %>%
  select(ReportingOfficerIdentificationID, ethnicity, searched_p) %>%
  spread(ethnicity, searched_p)
officer_searches_race_stops$total <- NULL

# INTERVENTIONTECHNIQUECODE BY RACE??

stops$InterventionTechniqueCode <- as.character(stops$InterventionTechniqueCode)
stops$InterventionTechniqueCode <- gsub("G", "general.enforcement", stops$InterventionTechniqueCode)
stops$InterventionTechniqueCode <- gsub("B", "blind.enforcement", stops$InterventionTechniqueCode)
stops$InterventionTechniqueCode <- gsub("S", "spot.check", stops$InterventionTechniqueCode)

officer_intervention_stops <- stops %>%
  group_by(ReportingOfficerIdentificationID, InterventionTechniqueCode) %>%
  summarise(total=n()) %>%
  spread(InterventionTechniqueCode, total) %>%
  group_by(ReportingOfficerIdentificationID) %>%
  mutate(total=sum(general.enforcement, blind.enforcement, spot.check, na.rm=TRUE)) %>%
  mutate(general.enforcement_p=round(general.enforcement/total*100,2), blind.enforcement_p=round(blind.enforcement/total*100,2), spot.check_p=round(spot.check/total*100,2))
officer_intervention_stops$total <- NULL

# Spot checks by race
officer_spot.checks_race_stops <- stops %>%
  group_by(ReportingOfficerIdentificationID, ethnicity, InterventionTechniqueCode) %>%
  summarise(total=n()) %>%
  spread(InterventionTechniqueCode, total) %>%
  group_by(ReportingOfficerIdentificationID) %>%
  mutate(total=sum(general.enforcement, blind.enforcement, spot.check, na.rm=TRUE)) %>%
  select(ReportingOfficerIdentificationID, ethnicity, spot.check, total) %>%
  mutate(spot.check_p=round(spot.check/total*100,2)) %>%
  select(ReportingOfficerIdentificationID, ethnicity, spot.check_p) %>%
  spread(ethnicity, spot.check_p)

# Blind enforcement by race
officer_blind.enforcement_race_stops <- stops %>%
  group_by(ReportingOfficerIdentificationID, ethnicity, InterventionTechniqueCode) %>%
  summarise(total=n()) %>%
  spread(InterventionTechniqueCode, total) %>%
  group_by(ReportingOfficerIdentificationID) %>%
  mutate(total=sum(general.enforcement, blind.enforcement, spot.check, na.rm=TRUE)) %>%
  select(ReportingOfficerIdentificationID, ethnicity, blind.enforcement, total) %>%
  mutate(blind.enforcement_p=round(blind.enforcement/total*100,2)) %>%
  select(ReportingOfficerIdentificationID, ethnicity, blind.enforcement_p) %>%
  spread(ethnicity, blind.enforcement_p)


names(state_mega)[names(state_mega) == 'DepartmentName'] <- 'ReportingOfficerIdentificationID'

names(officer_race_summary)[names(officer_race_summary) == 'middle-eastern'] <- 'middle.eastern'

# minorities out of residential stops

officer_residents_stops_race_m_r <- stops %>%
  group_by(ReportingOfficerIdentificationID, DepartmentName, TownRecidentIndicator, RE) %>%
  summarise(total=n()) %>%
  spread(RE, total) %>%
  group_by(ReportingOfficerIdentificationID, TownRecidentIndicator) %>%
  mutate(total=sum(Asian,Black,hispanic,Indian,`middle-eastern`, White, na.rm=TRUE))  %>%
  mutate(minorities=total-White) %>%
  filter(TownRecidentIndicator=="resident") %>%
  select(ReportingOfficerIdentificationID, TownRecidentIndicator, total, minorities) %>%
  mutate(resident_m_p_r=round(minorities/total*100,2)) %>%
  select(ReportingOfficerIdentificationID, resident_m_p_r)

officer_residents_stops_race_m_r <- left_join(officer_departments, officer_residents_stops_race_m_r)
officer_residents_stops_race_m_r$DepartmentName <-NULL
officer_residents_stops_race_m_r$ReportingOfficerIdentificationID <-NULL

# blacks out of residential stops

officer_residents_stops_race_b_r <- stops %>%
  group_by(ReportingOfficerIdentificationID, DepartmentName, TownRecidentIndicator, RE) %>%
  summarise(total=n()) %>%
  filter(TownRecidentIndicator=="resident") %>%
  spread(RE, total) %>%
  group_by(ReportingOfficerIdentificationID) %>%
  mutate(total=sum(Asian, Black, hispanic, Indian, `middle-eastern`, White, na.rm=TRUE)) %>%
  mutate(resident_b_p_r=round(Black/total*100,2)) %>%
  select(ReportingOfficerIdentificationID,resident_b_p_r)

officer_residents_stops_race_b_r <- left_join(officer_departments, officer_residents_stops_race_b_r)
officer_residents_stops_race_b_r$DepartmentName <-NULL
officer_residents_stops_race_b_r$ReportingOfficerIdentificationID <-NULL

# hispanics out of residential stops
officer_residents_stops_race_h_r <- stops %>%
  group_by(ReportingOfficerIdentificationID, DepartmentName, TownRecidentIndicator, RE) %>%
  summarise(total=n()) %>%
  filter(TownRecidentIndicator=="resident") %>%
  spread(RE, total) %>%
  group_by(ReportingOfficerIdentificationID) %>%
  mutate(total=sum(Asian, Black, hispanic, Indian, `middle-eastern`, White, na.rm=TRUE)) %>%
  mutate(resident_h_p_r=round(hispanic/total*100,2)) %>%
  select(ReportingOfficerIdentificationID,resident_h_p_r)

# Joining all the sub dataframes to create one big data frame

officer_residents_stops_race_h_r <- left_join(officer_departments, officer_residents_stops_race_h_r)
officer_residents_stops_race_h_r$DepartmentName <-NULL
officer_residents_stops_race_h_r$ReportingOfficerIdentificationID <-NULL

mega_officer_df <- left_join(officer_departments, officer_light_dark_summary, by="ReportingOfficerIdentificationID")
mega_officer_df <- left_join(mega_officer_df, officer_race_summary, by="ReportingOfficerIdentificationID")
mega_officer_df <- left_join(mega_officer_df, officer_disposition_summary, by="ReportingOfficerIdentificationID")
mega_officer_df <- left_join(mega_officer_df, officer_gender_summary, by="ReportingOfficerIdentificationID")
mega_officer_df <- left_join(mega_officer_df, officer_age_summary, by="ReportingOfficerIdentificationID")
mega_officer_df <- left_join(mega_officer_df, officer_age_group_summary, by="ReportingOfficerIdentificationID")
mega_officer_df <- left_join(mega_officer_df, officer_statute_code_summary, by="ReportingOfficerIdentificationID")
mega_officer_df <- left_join(mega_officer_df, stops_months_summary, by="ReportingOfficerIdentificationID")
mega_officer_df <- left_join(mega_officer_df, stops_day_summary, by="ReportingOfficerIdentificationID")
mega_officer_df <- left_join(mega_officer_df, stops_hour_summary, by="ReportingOfficerIdentificationID")
mega_officer_df <- left_join(mega_officer_df, officer_residents_stops, by="ReportingOfficerIdentificationID")
mega_officer_df <- left_join(mega_officer_df, officer_residents_stops_race_b, by="ReportingOfficerIdentificationID")
mega_officer_df <- left_join(mega_officer_df, officer_residents_stops_race_h, by="ReportingOfficerIdentificationID")
mega_officer_df <- left_join(mega_officer_df, officer_residents_stops_race_m, by="ReportingOfficerIdentificationID")

mega_officer_df <- left_join(mega_officer_df, officer_searches_stops, by="ReportingOfficerIdentificationID")
mega_officer_df <- left_join(mega_officer_df, officer_searches_race_stops, by="ReportingOfficerIdentificationID")
mega_officer_df <- left_join(mega_officer_df, officer_intervention_stops, by="ReportingOfficerIdentificationID")
mega_officer_df <- left_join(mega_officer_df, officer_spot.checks_race_stops, by="ReportingOfficerIdentificationID")
mega_officer_df <- left_join(mega_officer_df, officer_blind.enforcement_race_stops, by="ReportingOfficerIdentificationID")
mega_officer_df <- cbind(mega_officer_df, officer_residents_stops_race_m_r)
mega_officer_df <- cbind(mega_officer_df, officer_residents_stops_race_b_r)
mega_officer_df <- cbind(mega_officer_df, officer_residents_stops_race_h_r)

mega_officer_df <- data.frame(mega_officer_df)

# Cleaning up some column names so it's easier to bind to the departments dataframe

names(mega_officer_df)[names(mega_officer_df) == '0'] <- 'X0'
names(mega_officer_df)[names(mega_officer_df) == '1'] <- 'X1'
names(mega_officer_df)[names(mega_officer_df) == '2'] <- 'X2'
names(mega_officer_df)[names(mega_officer_df) == '3'] <- 'X3'
names(mega_officer_df)[names(mega_officer_df) == '4'] <- 'X4'
names(mega_officer_df)[names(mega_officer_df) == '5'] <- 'X5'
names(mega_officer_df)[names(mega_officer_df) == '6'] <- 'X6'
names(mega_officer_df)[names(mega_officer_df) == '7'] <- 'X7'
names(mega_officer_df)[names(mega_officer_df) == '8'] <- 'X8'
names(mega_officer_df)[names(mega_officer_df) == '9'] <- 'X9'
names(mega_officer_df)[names(mega_officer_df) == '10'] <- 'X10'
names(mega_officer_df)[names(mega_officer_df) == '11'] <- 'X11'
names(mega_officer_df)[names(mega_officer_df) == '12'] <- 'X12'
names(mega_officer_df)[names(mega_officer_df) == '13'] <- 'X13'
names(mega_officer_df)[names(mega_officer_df) == '14'] <- 'X14'
names(mega_officer_df)[names(mega_officer_df) == '15'] <- 'X15'
names(mega_officer_df)[names(mega_officer_df) == '16'] <- 'X16'
names(mega_officer_df)[names(mega_officer_df) == '17'] <- 'X17'
names(mega_officer_df)[names(mega_officer_df) == '18'] <- 'X18'
names(mega_officer_df)[names(mega_officer_df) == '19'] <- 'X19'
names(mega_officer_df)[names(mega_officer_df) == '20'] <- 'X20'
names(mega_officer_df)[names(mega_officer_df) == '21'] <- 'X21'
names(mega_officer_df)[names(mega_officer_df) == '22'] <- 'X22'
names(mega_officer_df)[names(mega_officer_df) == '23'] <- 'X23'
names(mega_officer_df)[names(mega_officer_df) == '24'] <- 'X24'
names(mega_officer_df)[names(mega_officer_df) == 'White.x'] <- 'White.x.1'

colnames(mega_officer_df)[12] <- "White.x"

# Binding the officer dataframe to the state and departments data frame
mega_officer_df <- rbind(mega_officer_df, state_mega)

c_a <- data.frame(t(c(DepartmentName="ReportingOfficerIdentificationID", ReportingOfficerIdentificationID="ReportingOfficerIdentificationID")))

officer_departments <- rbind(officer_departments, c_a)

mega_officer_df <- left_join(mega_officer_df, officer_departments, by="ReportingOfficerIdentificationID")

mega_officer_df$DepartmentName.y <- NULL
names(mega_officer_df)[names(mega_officer_df) == 'DepartmentName.x'] <- 'DepartmentName'

# Bringing in FBI UCR data
fbi <-read.csv("data/fbi-department-data.csv", stringsAsFactors=FALSE)
fbi2 <- data.table(fbi)
mega_officer_df <- dplyr::left_join(mega_officer_df,fbi2, by="DepartmentName")

# Bringing in and joining driving-age population data
over15 <- read.csv("data/race_over15.csv", stringsAsFactors=FALSE)
mega_officer_df <- dplyr::left_join(mega_officer_df,over15, by="DepartmentName")

# Figuring out the difference between the state average and department average for pullovers
mega_officer_df$b_t_s_diff <- mega_officer_df$black_p - state_mega$black_p[1]
mega_officer_df$h_t_s_diff <- mega_officer_df$hispanic_p - state_mega$hispanic_p[1]
mega_officer_df$minorities_p <- 100-mega_officer_df$white_p

# Isolating the Connecticut average
state_mega2 <- subset(mega_officer_df, ReportingOfficerIdentificationID=="Connecticut average")


# Figuring out the difference between the state average and department average for pullovers (minorities)
mega_officer_df$m_t_s_diff <- mega_officer_df$minorities_p - state_mega2$minorities_p[1]

# Calculating the percent makeup of those over 15 (driving age) by town and state
mega_officer_df$white_over_15_p <- round(mega_officer_df$white_over_15/mega_officer_df$population*100,2)
mega_officer_df$black_over_15_p <- round(mega_officer_df$black_over_15/mega_officer_df$population*100,2)
mega_officer_df$hispanic_over_15_p <- round(mega_officer_df$hispanic_over_15/mega_officer_df$population*100,2)
mega_officer_df$minorities_over_15_p <- round(mega_officer_df$minorities_over_15/mega_officer_df$population*100,2)

state_mega2 <- subset(mega_officer_df, ReportingOfficerIdentificationID=="Connecticut average")

# Calculating the difference between the state average and town average for racial makeup
mega_officer_df$b_t_s_pop_diff <- round(mega_officer_df$black_over_15_p - state_mega2$black_over_15_p[1],2)
mega_officer_df$h_t_s_pop_diff <- round(mega_officer_df$hispanic_over_15_p - state_mega2$hispanic_over_15_p[1],2)
mega_officer_df$m_t_s_pop_diff <- round(mega_officer_df$minorities_over_15_p - state_mega2$minorities_over_15_p[1],2)

# Calculating the distance between the stops and population state/town differences
mega_officer_df$b_distance <- round(mega_officer_df$b_t_s_diff - mega_officer_df$b_t_s_pop_diff,2)
mega_officer_df$h_distance <- round(mega_officer_df$h_t_s_diff - mega_officer_df$h_t_s_pop_diff,2)
mega_officer_df$m_distance <- round(mega_officer_df$m_t_s_diff - mega_officer_df$m_t_s_pop_diff,2)

# Calculating total residents in a town by race
mega_officer_df$b_res <- round(mega_officer_df$Black_total/mega_officer_df$population*100,2)
mega_officer_df$h_res <- round(mega_officer_df$Hispanic_total/mega_officer_df$population*100,2)
mega_officer_df$m_res <- round((mega_officer_df$population-mega_officer_df$White_total)/mega_officer_df$population*100,2)

# Calculating resident difference and ratio (minorities)
mega_officer_df$res_diff_m <- round(mega_officer_df$resident_m_p_r - mega_officer_df$minorities_over_15_p, 2)
#mega_officer_df$res_ratio_m <- round(mega_officer_df$resident_m_p_r/mega_officer_df$m_res, 2)
mega_officer_df$res_ratio_m <- round(mega_officer_df$resident_m_p_r/mega_officer_df$minorities_over_15_p, 2)

# Calculating resident difference and ratio (black)
mega_officer_df$res_diff_b <- round(mega_officer_df$resident_b_p_r - mega_officer_df$black_over_15_p, 2)
#mega_officer_df$res_ratio_b <- round(mega_officer_df$resident_b_p_r/mega_officer_df$b_res, 2)
mega_officer_df$res_ratio_b <- round(mega_officer_df$resident_b_p_r/mega_officer_df$black_over_15_p, 2)

# Calculating resident difference and ratio (Hispanic)
mega_officer_df$res_diff_h <- round(mega_officer_df$resident_h_p_r - mega_officer_df$hispanic_over_15_p, 2)
#mega_officer_df$res_ratio_h <- round(mega_officer_df$resident_h_p_r/mega_officer_df$h_res, 2)
mega_officer_df$res_ratio_h <- round(mega_officer_df$resident_h_p_r/mega_officer_df$hispanic_over_15_p, 2)

# Bringing in and joining estimated driving population as calculated by CCSU
edp <- read.csv("data/edp_from_report.csv", stringsAsFactors=FALSE)
mega_officer_df <- left_join(mega_officer_df, edp, by="DepartmentName")

# Calculating difference between minority stops and minority population
mega_officer_df$edp_m_diff <- round(mega_officer_df$minorities_p - mega_officer_df$edp_m, 2)
mega_officer_df$edp_b_diff <- round(mega_officer_df$black_p - mega_officer_df$edp_b, 2)
mega_officer_df$edp_h_diff <- round(mega_officer_df$hispanic_p - mega_officer_df$edp_h, 2)

# Calculating the ratio between minority stops and minority population
mega_officer_df$edp_m_ratio <- round(mega_officer_df$minorities_p/mega_officer_df$edp_m, 2)
mega_officer_df$edp_b_ratio <- round(mega_officer_df$black_p/mega_officer_df$edp_b, 2)
mega_officer_df$edp_h_ratio <- round(mega_officer_df$hispanic_p/mega_officer_df$edp_h, 2)

# Cleaning up more column names
col_idx1 <- grep("DepartmentName", names(mega_officer_df))
mega_officer_df <- mega_officer_df[, c(col_idx1, (1:ncol(mega_officer_df))[-col_idx1])]

# Bringing in the town department summaries to join

dep_mf <- read.csv("data/mega_town_df11.csv")
names(dep_mf)[names(dep_mf) == 'X'] <- 'ReportingOfficerIdentificationID'
dep_mf$ReportingOfficerIdentificationID <- "NA"

col_idx2 <- grep("DepartmentName", names(dep_mf))
dep_mf <- dep_mf[, c(col_idx2, (1:ncol(dep_mf))[-col_idx2])]

# creating some new columns so they'll line up with the department data frame

mega_officer_df$hispanic_16<- 0
mega_officer_df$minorities_16<- 0
mega_officer_df$black_16<- 0
mega_officer_df$edp_b_t<- 0
mega_officer_df$edp_b_p<- 0
mega_officer_df$edp_h_t<- 0
mega_officer_df$edp_h_p<- 0
mega_officer_df$edp_m_t<- 0
mega_officer_df$edp_m_p<- 0
mega_officer_df$residents_total<- 0
mega_officer_df$b_res_total<- 0
mega_officer_df$b_res_stops<- 0
mega_officer_df$h_res_total<- 0
mega_officer_df$h_res_stops<- 0
mega_officer_df$m_res_total<- 0
mega_officer_df$m_res_stops<- 0
mega_officer_df$m_coeff<- ""
mega_officer_df$b_coeff<- ""
mega_officer_df$h_coeff<- ""
mega_officer_df$b_h_coeff<- ""
mega_officer_df$m_coeff_mv<- ""
mega_officer_df$b_coeff_mv<- ""
mega_officer_df$h_coeff_mv<- ""
mega_officer_df$b_h_coeff_mv<- ""
mega_officer_df$m_kpt_diff<- ""
mega_officer_df$b_kpt_diff<- ""
mega_officer_df$h_kpt_diff<- ""
mega_officer_df$b_h_kpt_diff<- ""
mega_officer_df$m_kpt_pv<- ""
mega_officer_df$b_kpt_pv<- ""
mega_officer_df$h_kpt_pv<- ""
mega_officer_df$b_h_kpt_pv<- ""
mega_officer_df$m_kpt_ess<- ""
mega_officer_df$b_kpt_ess<- ""
mega_officer_df$h_kpt_ess<- ""
mega_officer_df$b_h_kpt_ess<- ""
mega_officer_df$m_synth<- ""
mega_officer_df$b_synth<- ""
mega_officer_df$h_synth<- ""
mega_officer_df$b_h_synth<- ""


mega_officer_df$DepartmentName <- ifelse(mega_officer_df$ReportingOfficerIdentificationID=="Connecticut average", "Connecticut average", mega_officer_df$DepartmentName)

dep_mf <- dep_mf %>%
  select(DepartmentName,ReportingOfficerIdentificationID, minority_dark, white_dark, minority_light, white_light, Asian,Black,hispanic,Indian,middle.eastern,White.x,total,min,asian_p,black_p,hispanic_p,indian_p,mid_e_p,white_p,min_p,infraction_ticket,misdemeanor_summons,no_disposition,uniform_arrest,verbal_warning,written_warning,infraction_ticket_p,misdemeanor_summons_p,no_disposition_p,uniform_arrest_p,verbal_warning_p,written_warning_p,F,M,male_p,female_p,average_age,median_age,y16_21,y22_27,y28_31,y32_37,y38_41,y42_47,y48_51,y52_57,y58_61,y62_67,y68_71,y72_77,y78_81,y82_,y16_21_p,y22_27_p,y28_31_p,y32_37_p,y38_41_p,y42_47_p,y48_51_p,y52_57_p,y58_61_p,y62_67_p,y68_71_p,y72_77_p,y78_81_p,y82_p,CellPhone,DefectiveLights,DisplayofPlates,EquipmentViolation,MovingViolation,Other,Registration,Seatbelt,SpeedRelated,StopSign,SuspendedLicense,TrafficControlSignal,WindowTint,CellPhone_p,DefectiveLights_p,DisplayofPlates_p,EquipmentViolation_p,MovingViolation_p,Other_p,Registration_p,Seatbelt_p,SpeedRelated_p,StopSign_p,SuspendedLicense_p,TrafficControlSignal_p,WindowTint_p,August,July,December,February,January,May,October,June,April,March,November,September,Sun,Mon,Tues,Wed,Thurs,Fri,Sat,X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,not.resident,resident,resident_p,not.resident_p,not.resident_b_p,resident_b_p,not.resident_h_p,resident_h_p,not.resident_m_p,resident_m_p,not.searched,searched,searched_p,not.searched_p,Minority.x,White.y,blind.enforcement,general.enforcement,spot.check,general.enforcement_p,blind.enforcement_p,spot.check_p,Minority.y,White.x.1,Minority,White.y.1,resident_m_p_r,resident_b_p_r,resident_h_p_r,fbi_name,population,total_leo,total_officers,total_civilians,violent_crime,murder_manslaughter,rape,robbery,aggravated_robbery,property_crime,burglary,larceny_theft,motor_vehicle_theft,arson,leo_per_capita,officers_per_capita,civ_per_capita,violent_crime_pc,murder_manslaughter_pc,rape_pc,robbery_pc,aggravated_robbery_pc,property_crime_pc,burglary_pc,larceny_theft_pc,motor_vehicle_theft_pc,arson_pc,Black_total,black_over_15_men,black_over_15_women,black_over_15,Hispanic_total,hispanic_over_15_men,hispanic_over_15_women,hispanic_over_15,White_total,white_over_15_men,white_over_15_women,white_over_15,ct_over_15_men,ct_over_15_women,ct_over_15,minorities_over_15,b_t_s_diff,h_t_s_diff,minorities_p,m_t_s_diff,white_over_15_p,black_over_15_p,hispanic_over_15_p,minorities_over_15_p,b_t_s_pop_diff,h_t_s_pop_diff,m_t_s_pop_diff,b_distance,h_distance,m_distance,b_res,h_res,m_res,res_diff_m,res_ratio_m,res_diff_b,res_ratio_b,res_diff_h,res_ratio_h,edp_m,edp_b,edp_h,edp_m_s,edp_b_s,edp_h_s,edp_m_diff,edp_b_diff,edp_h_diff,edp_m_ratio,edp_b_ratio,edp_h_ratio,hispanic_16,minorities_16,black_16,edp_b_t,edp_b_p,edp_h_t,edp_h_p,edp_m_t,edp_m_p,residents_total,b_res_total,b_res_stops,h_res_total,h_res_stops,m_res_total,m_res_stops,m_coeff,b_coeff,h_coeff,b_h_coeff,m_coeff_mv,b_coeff_mv,h_coeff_mv,b_h_coeff_mv,m_kpt_diff,b_kpt_diff,h_kpt_diff,b_h_kpt_diff,m_kpt_pv,b_kpt_pv,h_kpt_pv,b_h_kpt_pv,m_kpt_ess,b_kpt_ess,h_kpt_ess,b_h_kpt_ess,m_synth,b_synth,h_synth,b_h_synth)

dep_mf$DepartmentName <- as.character(dep_mf$DepartmentName)
mega_officer_df$hispanic_16 <- ifelse(mega_officer_df$DepartmentName==dep_mf$DepartmentName, dep_mf$hispanic_16, mega_officer_df$hispanic_16)
mega_officer_df$black_16 <- ifelse(mega_officer_df$DepartmentName== dep_mf$DepartmentName, dep_mf$black_16, mega_officer_df$black_16 )
mega_officer_df$minorities_16 <- ifelse(mega_officer_df$DepartmentName== dep_mf$DepartmentName, dep_mf$minorities_16, mega_officer_df$hispanic_16 )

mega_df <- rbind(mega_officer_df, dep_mf)
mega_df_backup <- mega_df


mega_df$b_t_d_diff <- 0
mega_df$h_t_d_diff <- 0
mega_df$m_t_d_diff <- 0
mega_df$b_t_d_pop_diff <- 0
mega_df$h_t_d_pop_diff <- 0
mega_df$m_t_d_pop_diff <- 0

# This will replace certain columns from the original dataframe with the columns from the CCSU dataframe

for (i in 1:nrow(mega_df)) {
  what_dept <- mega_df$DepartmentName[i]
  state_mega <- subset(dep_mf, DepartmentName==what_dept)
  mega_df$b_t_d_diff[i] <- mega_df$black_p[i] - dep_mf$black_p[1]
  mega_df$h_t_d_diff[i] <- mega_df$hispanic_p[i] - dep_mf$hispanic_p[1]
  mega_df$m_t_d_diff[i] <- mega_officer_df$minorities_p[i] - dep_mf$minorities_p[1]
  mega_df$b_t_d_pop_diff[i] <- round(mega_df$black_16[i] - dep_mf$black_16[1],2)
  mega_df$h_t_d_pop_diff[i] <- round(mega_df$hispanic_16[i] - dep_mf$hispanic_16[1],2)
  mega_df$m_t_d_pop_diff[i] <- round(mega_df$minorities_16[i] - dep_mf$minorities_16[1],2)
  if (mega_df$ReportingOfficerIdentificationID[i]=="Connecticut average") {
    mega_df$DepartmentName[i]=="Connecticut average"
  }
  print(i)
}


mega_df$b_d_distance <- round(mega_df$b_t_d_diff - mega_df$b_t_d_pop_diff,2)
mega_df$h_d_distance <- round(mega_df$h_t_d_diff - mega_df$h_t_d_pop_diff,2)
mega_df$m_d_distance <- round(mega_df$m_t_d_diff - mega_df$m_t_d_pop_diff,2)

# Calculating the disparity points based on state average distance, resident population/tickets, and estimated driving population/tickets
scores <- mega_df %>%
  filter(ReportingOfficerIdentificationID=="NA") %>%
  select(DepartmentName, m_distance, b_distance, h_distance, m_t_s_diff, b_t_s_diff, 
         h_t_s_diff, m_t_s_pop_diff, b_t_s_pop_diff, h_t_s_pop_diff,
         edp_m_diff, edp_b_diff, edp_h_diff, edp_m_ratio, edp_b_ratio, edp_h_ratio, 
         res_diff_m, res_diff_b, res_diff_h, res_ratio_m, res_ratio_b, res_ratio_h)

scores$m_ratio <- round(scores$m_t_s_diff/scores$m_t_s_pop_diff, 2)
scores$b_ratio <- round(scores$b_t_s_diff/scores$b_t_s_pop_diff, 2)
scores$h_ratio <- round(scores$h_t_s_diff/scores$h_t_s_pop_diff, 2)

# Bringing in the official points as calculated by CCSU
dept_points <- read.csv("data/dept_points.csv")

# Joining in the new points system
scores <- scores %>%
  select(DepartmentName, points) 

mega_df <- left_join(mega_df, dept_points)

# Writing out the final dataframe
write.csv(mega_df, "data/mega_df.csv")

