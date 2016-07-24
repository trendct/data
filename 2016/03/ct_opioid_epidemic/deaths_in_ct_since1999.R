## If you don't have the following packages installed, uncomment and run the line below
## install.packages("dplyr")
## install.packages("tidyr")
## install.packages("stringr")
## install.packages("ggplot2")

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

# Data I handcleaned from http://www.ct.gov/dph/cwp/view.asp?a=3132&q=394598
# Working on a script to bring all the data into one dataframe

deaths <- read.csv("data/deaths_in_ct_since_1999.csv")

deaths$BROAD <- str_trim(deaths$BROAD)

annually_all <- deaths %>% 
  filter(CATEGORY=="All" & BROAD=="All Races/Ethnicities") %>%
  select(TYPE, YEAR, Total)

ggplot(data=annually_all, aes(x=YEAR, y=Total, group=TYPE, color=TYPE)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  ggtitle("Accidental deaths since 1999 in CT")
