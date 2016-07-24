## If you don't have the following packages installed, uncomment and run the line below
## install.packages("RSocrata")
## install.packages("tidyr")
## install.packages("stringr")
## install.packages("ggplot2")
## install.packages("tidyr)
## install.packages("devtools")

# Preparing the data

library(RSocrata)
library(ctnamecleaner)
library(devtools)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(lubridate)
## install_github("trendct/ctnamecleaner")
library(ctnamecleaner)

# Importing the data from the data.ct.gov portal
# https://data.ct.gov/Health-and-Human-Services/Accidental-Drug-Related-Deaths-January-2012-Sept-2/rybz-nyjw

raw <- read.socrata("https://data.ct.gov/resource/rybz-nyjw.csv")
raw <- read.csv("values2014.csv", stringsAsFactors=FALSE)
raw <- unique(raw)

# stripped out 14 repeat rows of data

# Take out any non-CT residents
raw$Date <- mdy(raw$Date)
raw$Year <- year(raw$Date)
raw$Residence.City <- as.character(raw$Residence.City)

raw <- ctnamecleaner(Residence.City, raw, case="Title")

names(raw)[names(raw) == 'real.town.name'] <- 'Real.Residence'
raw <- ctnamecleaner(Death.City, raw, case="Title")
names(raw)[names(raw) == 'real.town.name'] <- 'Real.Death'

#raw_ct <- subset(raw, !is.na(Real.Residence))
raw_ct <- subset(raw, !is.na(Real.Death))

raw_not_ct <- subset(raw, is.na(Real.Residence))

# export not ct data and determine municipality by hand

# All drugs
# 
# drugs_total <- raw_ct %>%
#   group_by(Real.Residence) %>%
#   dplyr::summarize(Total=n())

drugs_total <- raw_ct %>%
  group_by(Real.Death) %>%
  dplyr::summarize(Total=n())

colnames(drugs_total) <- c("id", "Total")

drugs_total <- ctpopulator(id, drugs_total)
drugs_total$percapita <- round((drugs_total$Total/drugs_total$pop2013)*10000,2)
drugs_total$id <- str_to_title(drugs_total$id)


# fentanyl only
new_fentanyl <- raw_ct %>%
  mutate(cause=str_to_lower(raw_ct$ImmediateCauseA)) %>%
  filter(grepl("fent", cause) | Fentanyl=="Y" )
  
# new_fentanyl2$Year <- year(new_fentanyl2$Date)
new_fentanyl_summary <- new_fentanyl %>%
  group_by(Year) %>%
  summarise(Fentanyl=n())

# heroin only

new_heroin <- raw_ct %>%
  mutate(cause=str_to_lower(raw_ct$ImmediateCauseA)) %>%
  filter(grepl("heroin", cause) | Heroin=="Y" )

# new_heroin$Year <- year(new_heroin2$Date)
new_heroin_summary <- new_heroin %>%
  group_by(Year) %>%
  summarise(Heroin=n())

# prescription opioids

new_presc <- raw_ct %>%
  mutate(cause=str_to_lower(raw_ct$ImmediateCauseA)) %>%
  filter(grepl("oxy", cause) | Oxycodone=="Y" | Oxymorphone=="Y" |
           grepl("hydro", cause) | Hydrocodone=="Y" |
           grepl("tramad", cause) | Tramad=="Y" |
           grepl("morphine", cause) | Morphine..not.heroin.=="Y" )


# prescription opioids

new_presc <- raw_ct %>%
  mutate(cause=str_to_lower(raw_ct$ImmediateCauseA)) %>%
  filter(grepl("oxy", cause) | Oxycodone=="Y" | Oxymorphone=="Y" |
           grepl("hydro", cause) | Hydrocodone=="Y" |
           grepl("tramad", cause) | Tramad=="Y" |
           grepl("morphine", cause) | Morphine..not.heroin.=="Y" )

new_presc_summary <- new_presc %>%
  group_by(Year) %>%
  summarise(Prescription.Opioids=n())

## Joined

new_summary <- left_join(new_heroin_summary, new_fentanyl_summary)
new_summary <- left_join(new_summary, new_presc_summary)

# Replacing the figures with new summary from Office of the Chief Medical Examiner

# new_summary$Heroin[4] <- 415
# new_summary$Fentanyl[4] <- 186

backup_summary <- subset(new_summary, Year!=2015)


new_summary <- new_summary %>%
  gather("Type", "Deaths", 2:4)

ggplot(new_summary, aes(Year, Deaths, fill=Type)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_brewer(palette="Set1") +
  theme_minimal() +
  ggtitle("Drug overdose deaths over time. *Prescription opioid figures are as of September 2015 and not the full year.")

## By year
# new_presc <- read.csv("new_prescription_opioids.csv", stringsAsFactors=FALSE)
# new_heroin <- read.csv("new_heroin.csv", stringsAsFactors=FALSE)
# new_fentanyl <- read.csv("new_fentanyl.csv", stringsAsFactors=FALSE)

table(new_presc$Year)
table(new_heroin$Year)
table(new_fentanyl$Year)

yp2012 <- new_presc %>%
  filter(Year==2012)

pdrugs_total_2012 <- yp2012 %>%
  group_by(Real.Residence) %>%
  dplyr::summarize(Total=n())

colnames(pdrugs_total_2012) <- c("id", "Total")
pdrugs_total_2012 <- ctpopulator(id, pdrugs_total_2012)
pdrugs_total_2012$percapita <- round((pdrugs_total_2012$Total/pdrugs_total_2012$pop2013)*10000,2)
pdrugs_total_2012$id <- str_to_title(pdrugs_total_2012$id)


yp2013 <- new_presc %>%
  filter(Year==2013)

pdrugs_total_2013 <- yp2013 %>%
  group_by(Real.Residence) %>%
  dplyr::summarize(Total=n())

colnames(pdrugs_total_2013) <- c("id", "Total")
pdrugs_total_2013 <- ctpopulator(id, pdrugs_total_2013)
pdrugs_total_2013$percapita <- round((pdrugs_total_2013$Total/pdrugs_total_2013$pop2013)*10000,2)
pdrugs_total_2013$id <- str_to_title(pdrugs_total_2013$id)

yp2014 <- new_presc %>%
  filter(Year==2014)

pdrugs_total_2014 <- yp2014 %>%
  group_by(Real.Residence) %>%
  dplyr::summarize(Total=n())

colnames(pdrugs_total_2014) <- c("id", "Total")
pdrugs_total_2014 <- ctpopulator(id, pdrugs_total_2014)
pdrugs_total_2014$percapita <- round((pdrugs_total_2014$Total/pdrugs_total_2014$pop2013)*10000,2)
pdrugs_total_2014$id <- str_to_title(pdrugs_total_2014$id)

yp2015 <- new_presc %>%
  filter(Year==2015)

pdrugs_total_2015 <- yp2015 %>%
  group_by(Real.Residence) %>%
  dplyr::summarize(Total=n())

colnames(pdrugs_total_2015) <- c("id", "Total")
pdrugs_total_2015 <- ctpopulator(id, pdrugs_total_2015)
pdrugs_total_2015$percapita <- round((pdrugs_total_2015$Total/pdrugs_total_2015$pop2013)*10000,2)
pdrugs_total_2015$id <- str_to_title(pdrugs_total_2015$id)

## heroin


hp2012 <- new_heroin %>%
  filter(Year==2012)

hdrugs_total_2012 <- hp2012 %>%
  group_by(Real.Residence) %>%
  dplyr::summarize(Total=n())

colnames(hdrugs_total_2012) <- c("id", "Total")
hdrugs_total_2012 <- ctpopulator(id, hdrugs_total_2012)
hdrugs_total_2012$percapita <- round((hdrugs_total_2012$Total/hdrugs_total_2012$pop2013)*10000,2)
hdrugs_total_2012$id <- str_to_title(hdrugs_total_2012$id)


hp2013 <- new_heroin %>%
  filter(Year==2013)

hdrugs_total_2013 <- hp2013 %>%
  group_by(Real.Residence) %>%
  dplyr::summarize(Total=n())

colnames(hdrugs_total_2013) <- c("id", "Total")
hdrugs_total_2013 <- ctpopulator(id, hdrugs_total_2013)
hdrugs_total_2013$percapita <- round((hdrugs_total_2013$Total/hdrugs_total_2013$pop2013)*10000,2)
hdrugs_total_2013$id <- str_to_title(hdrugs_total_2013$id)

hp2014 <- new_heroin %>%
  filter(Year==2014)

hdrugs_total_2014 <- hp2014 %>%
  group_by(Real.Residence) %>%
  dplyr::summarize(Total=n())

colnames(hdrugs_total_2014) <- c("id", "Total")
hdrugs_total_2014 <- ctpopulator(id, hdrugs_total_2014)
hdrugs_total_2014$percapita <- round((hdrugs_total_2014$Total/hdrugs_total_2014$pop2013)*10000,2)
hdrugs_total_2014$id <- str_to_title(hdrugs_total_2014$id)

hp2015 <- new_heroin %>%
  filter(Year==2015)

hdrugs_total_2015 <- hp2015 %>%
  group_by(Real.Residence) %>%
  dplyr::summarize(Total=n())

colnames(hdrugs_total_2015) <- c("id", "Total")
hdrugs_total_2015 <- ctpopulator(id, hdrugs_total_2015)
hdrugs_total_2015$percapita <- round((hdrugs_total_2015$Total/hdrugs_total_2015$pop2013)*10000,2)
hdrugs_total_2015$id <- str_to_title(hdrugs_total_2015$id)


## fentanyl


fp2012 <- new_fentanyl %>%
  filter(Year==2012)

fdrugs_total_2012 <- fp2012 %>%
  group_by(Real.Residence) %>%
  dplyr::summarize(Total=n())

colnames(fdrugs_total_2012) <- c("id", "Total")
fdrugs_total_2012 <- ctpopulator(id, fdrugs_total_2012)
fdrugs_total_2012$percapita <- round((fdrugs_total_2012$Total/fdrugs_total_2012$pop2013)*10000,2)
fdrugs_total_2012$id <- str_to_title(fdrugs_total_2012$id)


fp2013 <- new_fentanyl %>%
  filter(Year==2013)

fdrugs_total_2013 <- fp2013 %>%
  group_by(Real.Residence) %>%
  dplyr::summarize(Total=n())

colnames(fdrugs_total_2013) <- c("id", "Total")
fdrugs_total_2013 <- ctpopulator(id, fdrugs_total_2013)
fdrugs_total_2013$percapita <- round((fdrugs_total_2013$Total/fdrugs_total_2013$pop2013)*10000,2)
fdrugs_total_2013$id <- str_to_title(fdrugs_total_2013$id)

fp2014 <- new_fentanyl %>%
  filter(Year==2014)

fdrugs_total_2014 <- fp2014 %>%
  group_by(Real.Residence) %>%
  dplyr::summarize(Total=n())

colnames(fdrugs_total_2014) <- c("id", "Total")
fdrugs_total_2014 <- ctpopulator(id, fdrugs_total_2014)
fdrugs_total_2014$percapita <- round((fdrugs_total_2014$Total/fdrugs_total_2014$pop2013)*10000,2)
fdrugs_total_2014$id <- str_to_title(fdrugs_total_2014$id)

fp2015 <- new_fentanyl %>%
  filter(Year==2015)

fdrugs_total_2015 <- fp2015 %>%
  group_by(Real.Residence) %>%
  dplyr::summarize(Total=n())

colnames(fdrugs_total_2015) <- c("id", "Total")
fdrugs_total_2015 <- ctpopulator(id, fdrugs_total_2015)
fdrugs_total_2015$percapita <- round((fdrugs_total_2015$Total/fdrugs_total_2015$pop2013)*10000,2)
fdrugs_total_2015$id <- str_to_title(fdrugs_total_2015$id)

## URBAN?
