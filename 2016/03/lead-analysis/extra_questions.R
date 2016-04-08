#Question 1
# When were schools tested? Where?

library(RSocrata)

schools <- read.socrata("https://data.ct.gov/resource/3z9z-e275.csv")

daycares <- read.socrata("https://data.ct.gov/resource/4jc3-c5bs.csv")

ls <- read.csv("lead_samples_school_checks.csv")
schools$PWS.Name <- str_to_upper(schools$School.Name)
ls$PWS.Name <- as.character(ls$PWS.Name)
ls_joined <- left_join(ls, schools, by="PWS.Name")

#Queston 2
# Is there a correlation with lead testing and where children were found with high levels of lead?
# Answered.
