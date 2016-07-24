## If you don't have the following packages installed, uncomment and run the line below
## install.packages("ggplot2")

library(ggplot2)

ind <- read.csv("data/accidentaldrugrelateddeathsbyindividualdrugsdetected.csv")
acc <- read.csv("data/accidentaldrugrelateddeathsbydrugtype.csv")

# Take out the totals

acc_adjusted <- acc
for (i in 1:ncol(acc)) {
  acc_adjusted <- subset(acc_adjusted, acc_adjusted[,i]!="Total")
}

ind_adjusted <- ind
for (i in 1:ncol(ind)) {
  ind_adjusted <- subset(ind_adjusted, ind_adjusted[,i]!="Total")
}

ct_ind <- subset(ind_adjusted, Town=="Connecticut")
ct_acc <- subset(acc_adjusted, Town=="Connecticut")

ind_adjusted <- subset(ind_adjusted, Town!="Connecticut")
acc_adjusted <- subset(acc_adjusted, Town!="Connecticut")

# charting

ggplot(data=ct_ind, aes(x=Age,y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~Drug.Type) +
  ggtitle("Small Multiples Drug Type and Age") +
  theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=20, hjust=0, color="#555555")) +
  theme(axis.text.x = element_text(angle=90)) 
