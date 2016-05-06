library(dplyr)
library(RCurl)
# devtools::install_github("hadley/readxl")
library(readxl)
library(ggplot2)

## This file contains district level covariates (socioeconomic, demographic, school level data). There is one observation per district.
#url <- "https://stacks.stanford.edu/file/druid:db586ns4974/district%20covariates.xlsx"
#x <- getURLContent(url)
#loc.download <- paste0(getwd(), "/data/", "district-covariates.xlsx")
#download.file(url, loc.download, mode="wb")
#grades <- read_excel("data/district-covariates.xlsx", sheet = 1)

# district level means in grade equivalent units. There is one observations per district; values are averaged across years, grades and subjects.
url <- "https://stacks.stanford.edu/file/druid:db586ns4974/district%20means%20grade%20equivalent%20std%20(gs)%20(pooled%20year,%20grade%20and%20sub).xlsx"
loc.download <- paste0(getwd(), "/data/", "district-means-grade-equivalent-std-gs-pooled-year-grade-and-sub.xlsx")
download.file(url, loc.download, mode="wb")
grades <- read_excel("data/district-means-grade-equivalent-std-gs-pooled-year-grade-and-sub.xlsx", sheet = 1)
colnames(grades) <- make.names(colnames(grades))

## Subsetting dataframe to just Connecticut schools
# Feel free to switch the CT abbreviation for whatever school you're interested in

ct_grades <- subset(grades, location.state=="CT")
us_grades <- subset(grades, location.state!="CT")
ct_grades_sub <- ct_grades[c("education.agency.name", "average.test.score..math.ela.pooled..in.grade.equiv")]
colnames(ct_grades_sub) <- c("district", "average.grade")

# District level covariates (socioeconomic, demographic, school level data). There are multiple observations per district; one for each year and grade.
# This is a 500 mb file so expect to sit a round a while

url <- "https://stacks.stanford.edu/file/druid:db586ns4974/district%20covariates%20by%20year%20and%20grade%20(long%20file).csv"
loc.download <- paste0(getwd(), "/data/", "district-covariates-by-year-and-grade-long-file.csv")
download.file(url, loc.download, mode="wb")
soc <- read.csv("data/district-covariates-by-year-and-grade-long-file.csv")

ct_soc <- subset(soc, stateabb=="CT")
ct_soc_6th <- subset(ct_soc, grade==6)
ct_soc_6th_2014 <- subset(ct_soc_6th, year==2014)

ct_6th_income <- ct_soc_6th_2014[c("leaname", "inc50all", "totenrl")]
colnames(ct_6th_income) <- c("district", "median.income", "students.in.grade")

ct_scatter <- left_join(ct_6th_income, ct_grades_sub)
ct_scatter <- subset(ct_scatter, !is.na(average.grade))
ct_scatter <- subset(ct_scatter, !is.na(median.income))

ct_scatter <- ct_scatter[c("district", "median.income", "average.grade", "students.in.grade")]
ct_scatter$average.grade <- round(ct_scatter$average.grade, 1)
ct_scatter$median.income <- round(ct_scatter$median.income, 0)


## This is to transform the dataframe into a json structure that highcharts.js can turn into a bubble chart

top <- "var data=["
bot <- "];"
array_row <- ""
for (i in 1:nrow(ct_scatter)) {
  
  array_row <- paste0(array_row, "{
                      \"name\": \"", ct_scatter[i,1], "\",
                      \"data\": [
                      [ ", ct_scatter[i,2], ", ", ct_scatter[i,3], ", ", ct_scatter[i,4], "]
                      ]
},")
  
  }

array_row <- substr(array_row, 1, nchar(array_row)-1)
array <- paste0(top, array_row, bot)

write(array, "array.txt")

# Visualizing the results

p <- ggplot(ct_scatter, aes(median.income, average.grade))
p + geom_point(aes(size=students.in.grade), alpha=.5) + 
  theme_bw() +
  xlab("Parents' socioeconomic status") + ylab("Grades above or below average") +
  ggtitle("Educational attainment in CT school districts")


# Data tables version


# district level means in grade equivalent units. There are multiple observations per district; one for each year, grade and subject.
url <- "https://stacks.stanford.edu/file/druid:db586ns4974/district%20means%20grade%20equivalent%20std%20(gs)%20(separate%20sheets%20year%20and%20grade).xlsx"
loc.download <- paste0(getwd(), "/data/", "district-means-grade-equivalent-std-gs-separate-sheets-year-and-grade.xlsx")
download.file(url, loc.download, mode="wb")
grades <- read_excel("data/district-means-grade-equivalent-std-gs-separate-sheets-year-and-grade.xlsx", sheet = 1)
colnames(grades) <- make.names(colnames(grades))
ct_grades2 <- subset(grades, location.state=="CT")
ct_grades2 <- ct_grades2[c("education.agency.name", "Estimated.District.Mean.in.ela..grade.equivalent.std..gs.", "Estimated.District.Mean.in.math..grade.equivalent.std..gs.")]
colnames(ct_grades2) <- c("district", "math", "ela")

ct_scatter2 <- left_join(ct_scatter, ct_grades2)
ct_scatter2 <- ct_scatter2[c("district", "average.grade", "math", "ela", "median.income", "students.in.grade")]


## Overall Connecticut versus US

us_soc <- subset(soc, stateabb!="CT")
us_soc_6th <- subset(us_soc, grade==6)
us_soc_6th_2014 <- subset(us_soc_6th, year==2014)

us_6th_income <- us_soc_6th_2014[c("leaname", "inc50all", "totenrl")]

us_grades_sub <- us_grades[c("education.agency.name", "average.test.score..math.ela.pooled..in.grade.equiv")]

colnames(us_6th_income) <- c("district", "median.income", "students.in.grade")
colnames(us_grades_sub) <- c("district", "average.grade")

us_scatter <- left_join(us_6th_income, us_grades_sub)
us_scatter <- subset(us_scatter, !is.na(average.grade))
us_scatter <- subset(us_scatter, !is.na(median.income))

us_scatter <- us_scatter[c("district", "median.income", "average.grade", "students.in.grade")]
us_scatter$average.grade <- round(us_scatter$average.grade, 1)
us_scatter$median.income <- round(us_scatter$median.income, 0)

## This is to transform the dataframe into a json structure that highcharts.js can turn into a bubble chart
top <- "var data=["
bot <- "];"
array_row <- ""
for (i in 1:nrow(us_scatter)) {
  
  array_row <- paste0(array_row, "{
                      \"name\": \"", us_scatter[i,1], "\",
                      \"color\": \"#fc9272\",
                      \"data\": [
                      [ ", us_scatter[i,2], ", ", us_scatter[i,3], ", ", us_scatter[i,4], "]
                      ]
},")
  
  }

array_row <- substr(array_row, 1, nchar(array_row)-1)
array <- paste0(top, array_row, bot)

write(array, "array_us.txt")

# Charting the results

ct_scatter$where <- "CT"
us_scatter$where <- "US"
all_scatter <- rbind(us_scatter, ct_scatter)

ggplot(all_scatter,
       aes(x = median.income, y = average.grade)) + scale_x_log10() +
  geom_point(aes(size = students.in.grade), pch = 21, show.legend = TRUE, alpha=.8) +
  scale_size_continuous(range=c(1,40)) +
  aes(fill = where) + theme_bw() +
  xlab("Parents' socioeconomic status") + ylab("Grades above or below average") +
  ggtitle("Educational attainment in U.S. school districts")



