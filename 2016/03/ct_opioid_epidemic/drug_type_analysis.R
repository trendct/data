library(dplyr)
library(tidyr)

acc <- read.csv("http://data.ctdata.org/dataset/a4ad31a4-4d6c-41c5-9e32-72bdd1c4fcd0/resource/882710c3-a9ec-4e36-9138-33dcc3ee35ba/download/accidentaldrugrelateddeathsbydrugtype.csv")

acc$RE <- paste(acc$Race, acc$Ethnicity)

acc$Age = factor(acc$Age,levels(acc$Age)[c(5,1,2,3,4)])


acc_adjusted <- acc

for (i in 1:ncol(acc_adjusted)) {
  acc_adjusted <- subset(acc_adjusted, acc_adjusted[,i]!="Total")
}

# acc_adjusted <- acc
ct_acc <- subset(acc_adjusted, Town=="Connecticut")

acc_adjusted <- subset(acc_adjusted, Town!="Connecticut")

ggplot(data=ct_acc, aes(x=Age,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Drug.Type) +
  ggtitle("Overall overdose deaths by age group and drug type") +
  theme(axis.text.x = element_text(angle=90)) 





age_drug_type <- ct_acc %>%
  group_by(Age, Drug.Type) %>%
  summarise(Total=sum(Value)) 

  write.table(age_drug_type, file='age_type.tsv', quote=FALSE, sep='\t', col.names = NA)

  

age_drug_type <- data.frame(age_drug_type)
age_drug_type2 <- t(age_drug_type)
age_drug_type2 <- data.frame(age_drug_type2)
colnames(age_drug_type2) <- age_drug_type2[1,]
write.csv(age_drug_type2, "age_druge_type2.csv")

# This is a custom function from TrendCT.org to make maps 
# trendchart(age_drug_type, headline = "Overall overdose deaths by age group and drug type", subhead = "Between 2012 and September 2015.", src = "<a href='http://ctdata.org/visualization/accidental-drug-related-deaths-by-drug-type'>CT Data Collaborative</a>, Chief Medical Examiners Office",
#            byline = "Andrew Ba Tran/TrendCT.org", type = "bar", xTitle = "", yTitle = "",
#            xSuffix = "", ySuffix = "", xPrefix = "", yPrefix = "", option = "")
# 
# trendchart(age_drug_type2, headline = "Overall overdose deaths by age group and drug type", subhead = "", src = "<a href='http://ctdata.org/visualization/accidental-drug-related-deaths-by-drug-type'>CT Data Collaborative</a>, Chief Medical Examiners Office",
#            byline = "TrendCT.org", type = "bar", xTitle = "", yTitle = "",
#            xSuffix = "", ySuffix = "", xPrefix = "", yPrefix = "", option = "")


ggplot(data=acc_adjusted, aes(x=Drug.Type,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~RE) +
  ggtitle("Overall overdose deaths by drug type and race/ethnicity") +
  theme(axis.text.x = element_text(angle=90)) 

race_drug_type <- ct_acc %>%
  group_by(RE, Drug.Type) %>%
  summarise(Total=sum(Value)) %>%
  spread(RE, Total)
race_drug_type <- data.frame(race_drug_type)

trendchart(race_drug_type, headline = "Overall overdose deaths by drug type and race-ethnicity", subhead = "Between 2012 and September 2015.", src = "<a href='http://ctdata.org/visualization/accidental-drug-related-deaths-by-drug-type'>CT Data Collaborative</a>, Chief Medical Examiners Office",
           byline = "Andrew Ba Tran/TrendCT.org", type = "bar", xTitle = "", yTitle = "",
           xSuffix = "", ySuffix = "", xPrefix = "", yPrefix = "", option = "")

#

ind <- read.csv("accidentaldrugrelateddeathsbyindividualdrugsdetected.csv")
ind$RE <- paste(ind$Race, ind$Ethnicity)
ind$Age = factor(ind$Age,levels(ind$Age)[c(5,1,2,3,4)])



ind_adjusted <- ind
for (i in 1:ncol(ind)) {
  ind_adjusted <- subset(ind_adjusted, ind_adjusted[,i]!="Total")
}

ct_ind <- subset(ind_adjusted, Town=="Connecticut")

ind_adjusted <- subset(ind_adjusted, Town!="Connecticut")

library(ggplot2)


# charting

library(ggplot2)

ggplot(data=ct_ind, aes(x=Age,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Drug.Type) +
  ggtitle("Overall overdose deaths by age group and drug type") +
  theme(axis.text.x = element_text(angle=90)) 


race_drug <- ct_ind %>%
  group_by(RE, Drug.Type) %>%
  summarise(Total=sum(Value)) 
write.table(race_drug, file='race_drug.tsv', quote=FALSE, sep='\t', col.names = NA)


age_drug <- ct_ind %>%
  group_by(Age, Drug.Type) %>%
  summarise(Total=sum(Value)) 
write.table(age_drug, file='age_drug.tsv', quote=FALSE, sep='\t', col.names = NA)

