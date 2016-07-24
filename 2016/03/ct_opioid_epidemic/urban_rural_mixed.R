
## If you don't have the following packages installed, uncomment and run the line below
#devtools::install_github("ropensci/plotly")
## install.packages("dplyr")
## install.packages("ggplot2")

library(dplyr)
library(ggplot2)
library(plotly)
## What determines if a town is Urban or Rural or Mixed?
## The U.S. Census Bureau defines urban and rural at the block level. 
## Therefore, geographic entities, including cities, towns and census tracts,
## may be urban (located wholly within an urban area), 
## rural (located entirely outside an urban area), 
## or contain both urban and rural territory (only the densely settled portion of the city is within an urban area). 
## https://ask.census.gov/faq.php?id=5000&faqId=6403

## Bringing in the Urban Area Relationship files for each county in Connecticut
## You can get your state's here ## https://ask.census.gov/faq.php?id=5000&faqId=6403

uace <- read.csv("data/uace1.csv", stringsAsFactors=FALSE)
uace2 <- read.csv("data/uace2.csv", stringsAsFactors=FALSE)
uace <- rbind(uace, uace2)

uace3 <- read.csv("data/uace3.csv", stringsAsFactors=FALSE)
uace <- rbind(uace, uace3)

uace4 <- read.csv("data/uace4.csv", stringsAsFactors=FALSE)
uace <- rbind(uace, uace4)
uace5 <- read.csv("data/uace5.csv", stringsAsFactors=FALSE)
uace <- rbind(uace, uace5)
uace6 <- read.csv("data/uace6.csv", stringsAsFactors=FALSE)
uace <- rbind(uace, uace6)
uace7 <- read.csv("data/uace7.csv", stringsAsFactors=FALSE)
uace <- rbind(uace, uace7)
uace8 <- read.csv("data/uace8.csv", stringsAsFactors=FALSE)
uace <- rbind(uace, uace8)

nrow(uace) + nrow(uace2) + nrow(uace3)+ nrow(uace4)+ nrow(uace5)+ nrow(uace6)+ nrow(uace7)+ nrow(uace8)

## Bringing in a list of town (county subdivision) names from the Census
## In order to match town names to census blocks/tracts

towns_list <- read.csv("data/urban_suburban.csv")

towns_list <- towns_list[c("COUSUBFP10", "COUSUBNS10", "GEOID10", "NAME10")]
uace <- uace[c("COUSUBFP10", "UACE10")]

towns_list_uace <- left_join(uace, towns_list)

# Determining if a town is Urban or Rural or Mixed (Suburban)

town_count_uace <- towns_list_uace %>%
  group_by(NAME10) %>%
  summarise(Count=n())

town_count <- left_join(towns_list, town_count_uace)

town_count_uace2 <- towns_list_uace %>%
  group_by(NAME10) %>%
  summarise(Blank=sum(is.na(UACE10)))

town_count <- left_join(town_count, town_count_uace2)

town_count <- subset(town_count, NAME10!="County subdivisions not defined")
town_count$Type <- "blank"

town_count$perc_urban <- round((town_count$Count - town_count$Blank)/town_count$Count*100,2)

## Just to stop briefly to talk about what's going on

head(town_count)

## The Count column is the number of census tracts there are in a town
## The Blank column is the number of census tracts in the town that is Rural
## The perc_urban is the how much a municipality is urban


## The loop below fills in the Type column: Urban, Rural, Mixed?

for (i in 1:nrow(town_count)) {
  # Set the threshold for what percent is enough
  # for a town to be considered urban. Census says 100.
  if (town_count$perc_urban[i]>=100) {
    town_count$Type[i] <- "Urban"
  } else if (town_count$Blank[i]==town_count$Count[i]) {
    town_count$Type[i] <- "Rural"
  } else {
    town_count$Type[i] <- "Mixed"
  }
}

