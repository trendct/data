# This script brings in data from an Excel sheet that includes latitude and longitude data for traffic stops from 8 departments
# It fixes latitude and longitude inconsistencies and combines them into one big data frame
# The story from the data can be found http://trafficstops.trendct.org/story/case-studies-which-neighborhoods-police-focus-traffic-enforcement/

library(openxlsx)

## The data provided by CCSU Institute for Municipal and Regional Policy came in a spreadsheet format
## They hand-curated most of the location data themselves to varying levels of success
## Read more in our story http://trafficstops.trendct.org/story/case-studies-which-neighborhoods-police-focus-traffic-enforcement/


groton <- read.xlsx("data/2013-2014-Department-Location-Data.xlsx",
                    sheet=1)
hamden <- read.xlsx("data/2013-2014-Department-Location-Data.xlsx",
                    sheet=2)
manchester <- read.xlsx("data/2013-2014-Department-Location-Data.xlsx",
                        sheet=3)
new_britain <- read.xlsx("data/2013-2014-Department-Location-Data.xlsx",
                         sheet=4)
stratford <- read.xlsx("data/2013-2014-Department-Location-Data.xlsx",
                       sheet=5)
waterbury <- read.xlsx("data/2013-2014-Department-Location-Data.xlsx",
                       sheet=6)
wethersfield <- read.xlsx("data/2013-2014-Department-Location-Data.xlsx",
                          sheet=7)
state_police <- read.xlsx("data/2013-2014-Department-Location-Data.xlsx",
                          sheet=8)

# Preparing the location data 
# Some of the lat/long is in degree/minute/seconds

library(sp)

groton$InterventionLocationLatitude <- paste0(groton$DegreeY,"d",groton$MinY, "'", groton$SecondY,"\"N")
groton$InterventionLocationLatitude <- as.numeric(char2dms(groton$InterventionLocationLatitude ))
groton$InterventionLocationLongitude <- paste0(groton$DegreeX,"d",groton$MinX, "'", groton$SecondX,"\"W")
groton$InterventionLocationLongitude <- as.numeric(char2dms(groton$InterventionLocationLongitude ))

hamden$InterventionLocationLatitude <- paste0(hamden$DegreeY,"d",hamden$MinY, "'", hamden$SecondY,"\"N")
hamden$InterventionLocationLatitude <- as.numeric(char2dms(hamden$InterventionLocationLatitude ))
hamden$InterventionLocationLongitude <- paste0(hamden$DegreeX,"d",hamden$MinX, "'", hamden$SecondX,"\"W")
hamden$InterventionLocationLongitude <- as.numeric(char2dms(hamden$InterventionLocationLongitude ))

manchester$InterventionLocationLatitude <- paste0(manchester$DegreeY,"d",manchester$MinY, "'", manchester$SecondY,"\"N")
manchester$InterventionLocationLatitude <- as.numeric(char2dms(manchester$InterventionLocationLatitude ))
manchester$InterventionLocationLongitude <- paste0(manchester$DegreeX,"d",manchester$MinX, "'", manchester$SecondX,"\"W")
manchester$InterventionLocationLongitude <- as.numeric(char2dms(manchester$InterventionLocationLongitude ))

state_police$InterventionLocationLatitude <- paste0(state_police$DegreeY,"d",state_police$MinY, "'", state_police$SecondY,"\"N")
state_police$InterventionLocationLatitude <- as.numeric(char2dms(state_police$InterventionLocationLatitude ))
state_police$InterventionLocationLongitude <- paste0(state_police$DegreeX,"d",state_police$MinX, "'", state_police$SecondX,"\"W")
state_police$InterventionLocationLongitude <- as.numeric(char2dms(state_police$InterventionLocationLongitude ))

names(stratford)[names(stratford) == 'X'] <- 'InterventionLocationLongitude'
names(stratford)[names(stratford) == 'Y'] <- 'InterventionLocationLatitude'
names(stratford)[names(stratford) == 'Time'] <- 'InterventionTime'

names(new_britain)[names(new_britain) == 'Column3'] <- 'Month'


waterbury$InterventionLocationLatitude <- paste0(waterbury$DegreeY,"d",waterbury$MinY, "'", waterbury$SecondY,"\"N")
waterbury$InterventionLocationLatitude <- as.numeric(char2dms(waterbury$InterventionLocationLatitude ))
waterbury$InterventionLocationLongitude <- paste0(waterbury$DegreeX,"d",waterbury$MinX, "'", waterbury$SecondX,"\"W")
waterbury$InterventionLocationLongitude <- as.numeric(char2dms(waterbury$InterventionLocationLongitude ))

wethersfield$InterventionLocationLatitude <- gsub("NULL", "0", wethersfield$InterventionLocationLatitude)
wethersfield$InterventionLocationLongitude <- gsub("NULL", "0", wethersfield$InterventionLocationLongitude)
wethersfield$InterventionLocationLatitude <- as.numeric(wethersfield$InterventionLocationLatitude)
wethersfield$InterventionLocationLongitude <- as.numeric(wethersfield$InterventionLocationLongitude)
names(wethersfield)[names(wethersfield) == 'Column3'] <- 'Month'

groton$town <- "Groton"
hamden$town <- "Hamden"
manchester$town <- "Manchester"
new_britain$town <- "New Britain"
state_police$town <- "State Police"
stratford$town <- "Stratford"
waterbury$town <- "Waterbury"
wethersfield$town <- "Wethersfield"

mega <- rbind(groton, hamden, manchester, new_britain, state_police, stratford, waterbury, wethersfield)
mega$InterventionLocationLatitude <- as.numeric(mega$InterventionLocationLatitude)
mega$InterventionLocationLongitude <- as.numeric(mega$InterventionLocationLongitude)

write.csv('data/mega.csv')