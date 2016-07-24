# Looking at CT vs US overdose death rates by year

## If you don't have the following packages installed, uncomment and run the line below
## install.packages("RSocrata")
## install.packages("tidyr")
## install.packages("ggplot2")
library(RSocrata)
library(tidyr)
library(ggplot2)


cdc <- read.socrata("https://data.cdc.gov/resource/jx6g-fdh6.csv")

ct_cdc <- subset(cdc, State=="Connecticut")

ct_cdc_chart <- ct_cdc[c("Year", "Age.adjusted.Rate", "US.Age.adjusted.Rate")]

ct_cdc_chart <- ct_cdc_chart %>%
  gather("Type", "Rate", 2:3)

# library(trendct)

#Drug and Opioid Overdose Deaths

ggplot(data=ct_cdc_chart, aes(x=Year, y=Rate, group=Type, color=Type)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  ggtitle("Drug and opioid overdose deaths: CT and US rates")
