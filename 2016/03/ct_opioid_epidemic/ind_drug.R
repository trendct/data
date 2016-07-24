# By Drug Type analysis
# exploratory data analysis using data from the CTDataCollaborative.
# http://ctdata.org/visualization/accidental-drug-related-deaths-by-drug-type

## If you don't have the following packages installed, uncomment and run the line below
## install.packages("ggplot2")


ind <- read.csv("http://data.ctdata.org/dataset/925c6ee5-daa1-45a4-b71a-5e9dc965024c/resource/26522dd7-6860-47be-8cdc-2b0911dfd877/download/accidentaldrugrelateddeathsbyindividualdrugsdetected.csv")
ind$RE <- paste(ind$Race, ind$Ethnicity)

library(ggplot2)


ind_adjusted <- ind
for (i in 1:ncol(ind)) {
  ind_adjusted <- subset(ind_adjusted, ind_adjusted[,i]!="Total")
}

ct_ind <- subset(ind_adjusted, Town=="Connecticut")

ind_adjusted <- subset(ind_adjusted, Town!="Connecticut")



# charting


ggplot(data=ct_ind, aes(x=Age,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Drug.Type) +
  ggtitle("Overall overdose deaths by age group and drug type") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=ct_ind, aes(x=Year,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Age) +
  ggtitle("Overall overdose deaths by age group and Year") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=ind_adjusted, aes(x=Age,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Overall overdose deaths by age group and Town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=ind_adjusted, aes(x=Drug.Type,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Overall overdose deaths by town and drug type") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=ind_adjusted, aes(x=Drug.Type,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~RE) +
  ggtitle("Overall overdose deaths by drug type and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=ind_adjusted, aes(x=RE,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town, ncol=6) +
  ggtitle("Overall overdose deaths by town and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 

# Break it out by age group?

age1 <- subset(ind_adjusted, Age=="Under 21 years")
age2 <- subset(ind_adjusted, Age=="21 to 45 years")
age3 <- subset(ind_adjusted, Age=="46 to 60 years")
age4 <- subset(ind_adjusted, Age=="61 years and over")

# Under 21 years

ggplot(data=age1, aes(x=Gender,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~RE, ncol=6) +
  ggtitle("Under 21 years: Overall overdose deaths by gender and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=age1, aes(x=Year,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Drug.Type, ncol=6) +
  ggtitle("Under 21 years: Overall overdose deaths by year and drug type") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=age1, aes(x=Year,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Gender, ncol=6) +
  ggtitle("Under 21 years: Overall overdose deaths by year and gender") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=age1, aes(x=RE,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Gender, ncol=6) +
  ggtitle("Under 21 years: Overall overdose deaths by race/ethnicity and gender") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=age1, aes(x=RE,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Drug.Type, ncol=6) +
  ggtitle("Under 21 years: Overall overdose deaths by race/ethnicity and drug type") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=age1, aes(x=RE,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town, ncol=6) +
  ggtitle("Under 21 years: Overall overdose deaths by race/ethnicity and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=age1, aes(x=Year,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town, ncol=6) +
  ggtitle("Under 21 years: Overall overdose deaths by year and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=age1, aes(x=Gender,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town, ncol=6) +
  ggtitle("Under 21 years: Overall overdose deaths by gender and Town") +
  theme(axis.text.x = element_text(angle=90)) 


# 21 to 45 years

ggplot(data=age2, aes(x=Gender,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~RE, ncol=6) +
  ggtitle("21 to 45 years: Overall overdose deaths by gender and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=age2, aes(x=Year,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Drug.Type, ncol=6) +
  ggtitle("21 to 45 years: Overall overdose deaths by year and drug type") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=age2, aes(x=Year,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Gender, ncol=6) +
  ggtitle("21 to 45 years: Overall overdose deaths by year and gender") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=age2, aes(x=RE,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Gender, ncol=6) +
  ggtitle("21 to 45 years: Overall overdose deaths by race/ethnicity and gender") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=age2, aes(x=RE,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Drug.Type, ncol=6) +
  ggtitle("21 to 45 years: Overall overdose deaths by race/ethnicity and drug type") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=age2, aes(x=RE,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town, ncol=6) +
  ggtitle("21 to 45 years: Overall overdose deaths by race/ethnicity and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=age2, aes(x=Year,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town, ncol=6) +
  ggtitle("21 to 45 years: Overall overdose deaths by year and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=age2, aes(x=Gender,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town, ncol=6) +
  ggtitle("21 to 45 years: Overall overdose deaths by gender and Town") +
  theme(axis.text.x = element_text(angle=90)) 

# 46 to 60 years

ggplot(data=age3, aes(x=Gender,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~RE, ncol=6) +
  ggtitle("46 to 60 years: Overall overdose deaths by gender and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=age3, aes(x=Year,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Drug.Type, ncol=6) +
  ggtitle("46 to 60 years: Overall overdose deaths by year and drug type") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=age3, aes(x=Year,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Gender, ncol=6) +
  ggtitle("46 to 60 years: Overall overdose deaths by year and gender") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=age3, aes(x=RE,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Gender, ncol=6) +
  ggtitle("46 to 60 years: Overall overdose deaths by race/ethnicity and gender") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=age3, aes(x=RE,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Drug.Type, ncol=6) +
  ggtitle("46 to 60 years: Overall overdose deaths by race/ethnicity and drug type") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=age3, aes(x=RE,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town, ncol=6) +
  ggtitle("46 to 60 years: Overall overdose deaths by race/ethnicity and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=age3, aes(x=Year,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town, ncol=6) +
  ggtitle("46 to 60 years: Overall overdose deaths by year and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=age3, aes(x=Gender,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town, ncol=6) +
  ggtitle("46 to 60 years: Overall overdose deaths by gender and Town") +
  theme(axis.text.x = element_text(angle=90)) 

# 61 years and over

ggplot(data=age4, aes(x=Gender,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~RE, ncol=6) +
  ggtitle("61 years and over: Overall overdose deaths by gender and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=age4, aes(x=Year,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Drug.Type, ncol=6) +
  ggtitle("61 years and over: Overall overdose deaths by year and drug type") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=age4, aes(x=Year,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Gender, ncol=6) +
  ggtitle("61 years and over: Overall overdose deaths by year and gender") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=age4, aes(x=RE,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Gender, ncol=6) +
  ggtitle("61 years and over: Overall overdose deaths by race/ethnicity and gender") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=age4, aes(x=RE,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Drug.Type, ncol=6) +
  ggtitle("61 years and over: Overall overdose deaths by race/ethnicity and drug type") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=age4, aes(x=RE,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town, ncol=6) +
  ggtitle("61 years and over: Overall overdose deaths by race/ethnicity and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=age4, aes(x=Year,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town, ncol=6) +
  ggtitle("61 years and over: Overall overdose deaths by year and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=age4, aes(x=Gender,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town, ncol=6) +
  ggtitle("61 years and over: Overall overdose deaths by gender and Town") +
  theme(axis.text.x = element_text(angle=90)) 

# Break it out by drug type

anho <- subset(ind_adjusted, Drug.Type=="Any Non-Heroin Opioid")
ano <- subset(ind_adjusted, Drug.Type=="Any Non-Opioid")
onho <- subset(ind_adjusted, Drug.Type=="Only Non-Heroin Opioids")
ono <- subset(ind_adjusted, Drug.Type=="Only Non-Opioids")
oo <- subset(ind_adjusted, Drug.Type=="Only Opioids")

amph <- subset(ind_adjusted, Drug.Type=="Amphetamine")
benz <- subset(ind_adjusted, Drug.Type=="Benzodiazepine")
coca <- subset(ind_adjusted, Drug.Type=="Cocaine")
etha <- subset(ind_adjusted, Drug.Type=="Ethanol")
fent <- subset(ind_adjusted, Drug.Type=="Fentanyl")
hero <- subset(ind_adjusted, Drug.Type=="Heroin")
hydr <- subset(ind_adjusted, Drug.Type=="Hydrocodone")
meth <- subset(ind_adjusted, Drug.Type=="Methadone")
othe <- subset(ind_adjusted, Drug.Type=="Other")
oxyc <- subset(ind_adjusted, Drug.Type=="Oxycodone")
oxym <- subset(ind_adjusted, Drug.Type=="Oxymorphone")
tram <- subset(ind_adjusted, Drug.Type=="Tramadol")

# Amphetamine

ggplot(data=amph, aes(x=Age,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~RE) +
  ggtitle("Amphetamine overdose deaths by age group and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=amph, aes(x=Year,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Age) +
  ggtitle("Amphetamine overdose deaths by year and age group") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=amph, aes(x=Gender,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~RE) +
  ggtitle("Amphetamine overdose deaths by age gender and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=amph, aes(x=Year,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Amphetamine overdose deaths by year and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=amph, aes(x=RE,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Amphetamine overdose deaths by race/ethnicity and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=amph, aes(x=Gender,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Amphetamine overdose deaths by gender and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=amph, aes(x=RE,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town, ncol=6) +
  ggtitle("Amphetamine overdose deaths by town and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 

# Benzodiazepine

ggplot(data=benz, aes(x=Age,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~RE) +
  ggtitle("Benzodiazepine overdose deaths by age group and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=benz, aes(x=Year,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Age) +
  ggtitle("Benzodiazepine overdose deaths by year and age group") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=benz, aes(x=Gender,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~RE) +
  ggtitle("Benzodiazepine overdose deaths by age gender and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=benz, aes(x=Year,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Benzodiazepine overdose deaths by year and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=benz, aes(x=RE,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Benzodiazepine overdose deaths by race/ethnicity and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=benz, aes(x=Gender,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Benzodiazepine overdose deaths by gender and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=benz, aes(x=RE,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town, ncol=6) +
  ggtitle("Benzodiazepine overdose deaths by town and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 

# Cocaine

ggplot(data=coca, aes(x=Age,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~RE) +
  ggtitle("Cocaine overdose deaths by age group and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=coca, aes(x=Year,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Age) +
  ggtitle("Cocaine overdose deaths by year and age group") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=coca, aes(x=Gender,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~RE) +
  ggtitle("Cocaine overdose deaths by age gender and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=coca, aes(x=Year,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Cocaine overdose deaths by year and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=coca, aes(x=RE,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Cocaine overdose deaths by race/ethnicity and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=coca, aes(x=Gender,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Cocaine overdose deaths by gender and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=coca, aes(x=RE,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town, ncol=6) +
  ggtitle("Cocaine overdose deaths by town and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 

# Ethanol

ggplot(data=etha, aes(x=Age,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~RE) +
  ggtitle("Ethanol overdose deaths by age group and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=etha, aes(x=Year,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Age) +
  ggtitle("Ethanol overdose deaths by year and age group") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=etha, aes(x=Gender,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~RE) +
  ggtitle("Ethanol overdose deaths by age gender and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=etha, aes(x=Year,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Ethanol overdose deaths by year and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=etha, aes(x=RE,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Ethanol overdose deaths by race/ethnicity and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=etha, aes(x=Gender,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Ethanol overdose deaths by gender and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=etha, aes(x=RE,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town, ncol=6) +
  ggtitle("Ethanol overdose deaths by town and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 

# Fentanyl

ggplot(data=fent, aes(x=Age,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~RE) +
  ggtitle("Fentanyl overdose deaths by age group and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=fent, aes(x=Year,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Age) +
  ggtitle("Fentanyl overdose deaths by year and age group") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=fent, aes(x=Gender,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~RE) +
  ggtitle("Fentanyl overdose deaths by age gender and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=fent, aes(x=Year,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Fentanyl overdose deaths by year and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=fent, aes(x=RE,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Fentanyl overdose deaths by race/ethnicity and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=fent, aes(x=Gender,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Fentanyl overdose deaths by gender and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=fent, aes(x=RE,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town, ncol=6) +
  ggtitle("Fentanyl overdose deaths by town and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 

# Heroin

ggplot(data=hero, aes(x=Age,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~RE) +
  ggtitle("Heroin overdose deaths by age group and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=hero, aes(x=Year,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Age) +
  ggtitle("Heroin overdose deaths by year and age group") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=hero, aes(x=Gender,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~RE) +
  ggtitle("Heroin overdose deaths by age gender and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=hero, aes(x=Year,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Heroin overdose deaths by year and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=hero, aes(x=RE,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Heroin overdose deaths by race/ethnicity and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=hero, aes(x=Gender,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Heroin overdose deaths by gender and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=hero, aes(x=RE,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town, ncol=6) +
  ggtitle("Heroin overdose deaths by town and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 

# Hydrocodone

ggplot(data=hydr, aes(x=Age,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~RE) +
  ggtitle("Hydrocodone overdose deaths by age group and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=hydr, aes(x=Year,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Age) +
  ggtitle("Hydrocodone overdose deaths by year and age group") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=hydr, aes(x=Gender,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~RE) +
  ggtitle("Hydrocodone overdose deaths by age gender and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=hydr, aes(x=Year,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Hydrocodone overdose deaths by year and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=hydr, aes(x=RE,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Hydrocodone overdose deaths by race/ethnicity and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=hydr, aes(x=Gender,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Hydrocodone overdose deaths by gender and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=hydr, aes(x=RE,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town, ncol=6) +
  ggtitle("Hydrocodone overdose deaths by town and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 


# Methadone

ggplot(data=meth, aes(x=Age,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~RE) +
  ggtitle("Methadone overdose deaths by age group and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=meth, aes(x=Year,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Age) +
  ggtitle("Methadone overdose deaths by year and age group") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=meth, aes(x=Gender,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~RE) +
  ggtitle("Methadone overdose deaths by age gender and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=meth, aes(x=Year,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Methadone overdose deaths by year and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=meth, aes(x=RE,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Methadone overdose deaths by race/ethnicity and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=meth, aes(x=Gender,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Methadone overdose deaths by gender and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=meth, aes(x=RE,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town, ncol=6) +
  ggtitle("Methadone overdose deaths by town and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 


# Other

ggplot(data=othe, aes(x=Age,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~RE) +
  ggtitle("Other overdose deaths by age group and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=othe, aes(x=Year,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Age) +
  ggtitle("Other overdose deaths by year and age group") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=othe, aes(x=Gender,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~RE) +
  ggtitle("Other overdose deaths by age gender and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=othe, aes(x=Year,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Other overdose deaths by year and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=othe, aes(x=RE,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Other overdose deaths by race/ethnicity and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=othe, aes(x=Gender,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Other overdose deaths by gender and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=othe, aes(x=RE,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town, ncol=6) +
  ggtitle("Other overdose deaths by town and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 


# Oxycodone

ggplot(data=othe, aes(x=Age,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~RE) +
  ggtitle("Oxycodone overdose deaths by age group and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=othe, aes(x=Year,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Age) +
  ggtitle("Oxycodone overdose deaths by year and age group") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=othe, aes(x=Gender,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~RE) +
  ggtitle("Oxycodone overdose deaths by age gender and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=othe, aes(x=Year,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Oxycodone overdose deaths by year and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=othe, aes(x=RE,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Oxycodone overdose deaths by race/ethnicity and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=othe, aes(x=Gender,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Oxycodone overdose deaths by gender and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=othe, aes(x=RE,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town, ncol=6) +
  ggtitle("Oxycodone overdose deaths by town and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90))


# Oxymorphone

ggplot(data=oxym, aes(x=Age,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~RE) +
  ggtitle("Oxymorphone overdose deaths by age group and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=oxym, aes(x=Year,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Age) +
  ggtitle("Oxymorphone overdose deaths by year and age group") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=oxym, aes(x=Gender,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~RE) +
  ggtitle("Oxymorphone overdose deaths by age gender and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=oxym, aes(x=Year,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Oxymorphone overdose deaths by year and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=oxym, aes(x=RE,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Oxymorphone overdose deaths by race/ethnicity and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=oxym, aes(x=Gender,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Oxymorphone overdose deaths by gender and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=oxym, aes(x=RE,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town, ncol=6) +
  ggtitle("Oxymorphone overdose deaths by town and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 


# Tramadol

ggplot(data=tram, aes(x=Age,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~RE) +
  ggtitle("Tramadol overdose deaths by age group and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=tram, aes(x=Year,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Age) +
  ggtitle("Tramadol overdose deaths by year and age group") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=tram, aes(x=Gender,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~RE) +
  ggtitle("Tramadol overdose deaths by age gender and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=tram, aes(x=Year,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Tramadol overdose deaths by year and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=tram, aes(x=RE,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Tramadol overdose deaths by race/ethnicity and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=tram, aes(x=Gender,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Tramadol overdose deaths by gender and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=tram, aes(x=RE,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town, ncol=6) +
  ggtitle("Tramadol overdose deaths by town and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 