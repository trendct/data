## If you don't have the following packages installed, uncomment and run the line below
## install.packages("dplyr")
## install.packages("tidyr")

# This takes the data and converts it into a D3 structure

library(dplyr)
library(tidyr)

source("prepping_data.R")

town_count_year <- raw_ct %>%
  group_by(RealResidenceCity, Year) %>%
  summarise(Overdoses=n()) %>%
  spread(Year, Overdoses)

town_count_year[,2] [is.na(town_count_year[,2])] <- 0
town_count_year[,3] [is.na(town_count_year[,3])] <- 0
town_count_year[,4] [is.na(town_count_year[,4])] <- 0
town_count_year[,5] [is.na(town_count_year[,5])] <- 0

town_count_year_cumu <- town_count_year

town_count_year_cumu[,3] <- town_count_year_cumu[,2] + town_count_year_cumu[,3]
town_count_year_cumu[,4] <- town_count_year_cumu[,3] + town_count_year_cumu[,4]
town_count_year_cumu[,5] <- town_count_year_cumu[,4] + town_count_year_cumu[,5]

town_count_year_cumu_d3 <- town_count_year_cumu %>%
  gather("Year", "Overdoses", 2:5)

start_json <- 
"{
  \"data\": ["

value_array <- ""

town_d3_array <- for (i in 2:ncol(town_count_year_cumu)) {

values <- "{
    \"values\": {
    "
value_array <- paste0(value_array, values)

  for(x in 1:nrow(town_count_year_cumu)) {
    value_array <- paste0(value_array, "
                          \"", town_count_year_cumu$RealResidenceCity[x], "\": ", town_count_year_cumu[x,i], ",")
  }
  
  value_array <- substr(value_array, 1, nchar(value_array)-1)
  value_array <- paste0(value_array, "
                        },
                        \"key\": \"", colnames(town_count_year_cumu[,i]), "\"
          },")
}


value_array <- substr(value_array, 1, nchar(value_array)-1)

value_array <- paste0(start_json, value_array, "]
                      }")

write(value_array, "towns_d3.json")
