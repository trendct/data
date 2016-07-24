# By Drug Type analysis
# exploratory data analysis using data from the CTDataCollaborative.
# http://ctdata.org/visualization/accidental-drug-related-deaths-by-drug-type

## If you don't have the following packages installed, uncomment and run the line below
## install.packages("ggplot2")

library(ggplot2)

acc <- read.csv("http://data.ctdata.org/dataset/a4ad31a4-4d6c-41c5-9e32-72bdd1c4fcd0/resource/882710c3-a9ec-4e36-9138-33dcc3ee35ba/download/accidentaldrugrelateddeathsbydrugtype.csv")

acc$RE <- paste(acc$Race, acc$Ethnicity)
levels(acc$Age) <- c("Under 21 years", "21 to 45 years", "46 to 60 years", "61 years and over", "Total")
# Take out the totals

acc_adjusted <- acc
for (i in 1:ncol(acc)) {
  acc_adjusted <- subset(acc_adjusted, acc_adjusted[,i]!="Total")
}

ct_acc <- subset(acc_adjusted, Town=="Connecticut")

acc_adjusted <- subset(acc_adjusted, Town!="Connecticut")

acc_adjusted_total <- acc
acc_adjusted_total <- subset(acc_adjusted_total, RE=="Total Total")
acc_adjusted_total <- subset(acc_adjusted_total, Gender=="Total")
acc_adjusted_total <- subset(acc_adjusted_total, Race=="Total")

acc_adjusted_total <- subset(acc_adjusted_total, Ethnicity=="Total")
acc_adjusted_total <- subset(acc_adjusted_total, Drug.Type=="Total")
table(acc_adjusted_total$Age)
  


# charting

ggplot(data=ct_acc, aes(x=Age,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Drug.Type) +
  ggtitle("Overall overdose deaths by age group and drug type") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=ct_acc, aes(x=Year,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Age) +
  ggtitle("Overall overdose deaths by age group and Year") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=acc_adjusted, aes(x=Age,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Overall overdose deaths by age group and Town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=acc_adjusted, aes(x=Drug.Type,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Overall overdose deaths by town and drug type") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=acc_adjusted, aes(x=Drug.Type,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~RE) +
  ggtitle("Overall overdose deaths by drug type and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=acc_adjusted, aes(x=RE,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town, ncol=6) +
  ggtitle("Overall overdose deaths by town and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 

# Break it out by drug type

anho <- subset(acc_adjusted, Drug.Type=="Any Non-Heroin Opioid")
ano <- subset(acc_adjusted, Drug.Type=="Any Non-Opioid")
onho <- subset(acc_adjusted, Drug.Type=="Only Non-Heroin Opioids")
ono <- subset(acc_adjusted, Drug.Type=="Only Non-Opioids")
oo <- subset(acc_adjusted, Drug.Type=="Only Opioids")

# Any non-heroin opioid

ggplot(data=anho, aes(x=Age,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~RE) +
  ggtitle("Any Non-Heroin Opioid overdose deaths by age group and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=anho, aes(x=Year,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Age) +
  ggtitle("Any Non-Heroin Opioid overdose deaths by year and age group") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=anho, aes(x=Gender,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~RE) +
  ggtitle("Any Non-Heroin Opioid overdose deaths by age gender and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=anho, aes(x=Year,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Any Non-Heroin Opioid overdose deaths by year and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=anho, aes(x=RE,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Any Non-Heroin Opioid overdose deaths by race/ethnicity and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=anho, aes(x=Gender,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Any Non-Heroin Opioid overdose deaths by gender and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=anho, aes(x=RE,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town, ncol=6) +
  ggtitle("Any Non-Heroin Opioid overdose deaths by town and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 

# Any Non-Opioid

ggplot(data=ano, aes(x=Age,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~RE) +
  ggtitle("Any Non-Opioid overdose deaths by age group and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=ano, aes(x=Year,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Age) +
  ggtitle("Any Non-Opioid overdose deaths by year and age group") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=ano, aes(x=Gender,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~RE) +
  ggtitle("Any Non-Opioid overdose deaths by age gender and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=ano, aes(x=Year,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Any Non-Opioid overdose deaths by year and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=ano, aes(x=RE,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Any Non-Opioid overdose deaths by race/ethnicity and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=ano, aes(x=Gender,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Any Non-Opioid overdose deaths by gender and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=ano, aes(x=RE,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town, ncol=6) +
  ggtitle("Any Non-Opioid overdose deaths by town and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 

# Only Non-Heroin Opioids

ggplot(data=onho, aes(x=Age,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~RE) +
  ggtitle("Only Non-Heroin Opioids overdose deaths by age group and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=onho, aes(x=Year,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Age) +
  ggtitle("Only Non-Heroin Opioids overdose deaths by year and age group") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=onho, aes(x=Gender,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~RE) +
  ggtitle("Only Non-Heroin Opioids overdose deaths by age gender and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=onho, aes(x=Year,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Only Non-Heroin Opioids overdose deaths by year and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=onho, aes(x=RE,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Only Non-Heroin Opioids overdose deaths by race/ethnicity and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=onho, aes(x=Gender,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Only Non-Heroin Opioids overdose deaths by gender and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=onho, aes(x=RE,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town, ncol=6) +
  ggtitle("Only Non-Heroin Opioids overdose deaths by town and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 

# Only Non-Opioids

ggplot(data=ono, aes(x=Age,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~RE) +
  ggtitle("Only Non-Opioids overdose deaths by age group and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=ono, aes(x=Year,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Age) +
  ggtitle("Only Non-Opioids overdose deaths by year and age group") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=ono, aes(x=Gender,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~RE) +
  ggtitle("Only Non-Opioids overdose deaths by age gender and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=ono, aes(x=Year,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Only Non-Opioids overdose deaths by year and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=ono, aes(x=RE,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Only Non-Opioids overdose deaths by race/ethnicity and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=ono, aes(x=Gender,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Only Non-Opioids overdose deaths by gender and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=ono, aes(x=RE,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town, ncol=6) +
  ggtitle("Only Non-Opioids overdose deaths by town and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 

# Only Opioids

ggplot(data=oo, aes(x=Age,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~RE) +
  ggtitle("Only Opioids overdose deaths by age group and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=oo, aes(x=Year,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Age) +
  ggtitle("Only Opioids overdose deaths by year and age group") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=oo, aes(x=Gender,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~RE) +
  ggtitle("Only Opioids overdose deaths by age gender and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=oo, aes(x=Year,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Only Opioids overdose deaths by year and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=oo, aes(x=RE,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Only Opioids overdose deaths by race/ethnicity and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=oo, aes(x=Gender,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town) +
  ggtitle("Only Opioids overdose deaths by gender and town") +
  theme(axis.text.x = element_text(angle=90)) 

ggplot(data=oo, aes(x=RE,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Town, ncol=6) +
  ggtitle("Only Opioids overdose deaths by town and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 

# Break it out by age group?

age1 <- subset(acc_adjusted, Age=="Under 21 years")
age2 <- subset(acc_adjusted, Age=="21 to 45 years")
age3 <- subset(acc_adjusted, Age=="46 to 60 years")
age4 <- subset(acc_adjusted, Age=="61 years and over")

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