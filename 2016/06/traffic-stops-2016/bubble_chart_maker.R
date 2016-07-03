# This script converts department-level summaries into a json array for a highcharts connected-scatterplot visualization
# http://projects.ctmirror.org/content/trend/2016/06/bubble_test2.html
# The chart was ultimately not used and we went with a different unconnected scatter plot.
# http://trafficstops.trendct.org/story/digging-deeper-into-racial-disparities-in-ct-traffic-stops/

library(dplyr)
library(tidyr)
library(ggplot2)

## Bringing in the data

explore <- read.csv("data/mega_df.csv")

## Excluding the blanks

dept <- subset(explore, is.na(ReportingOfficerIdentificationID))

## One dataframe for state average
state <- subset(dept, DepartmentName=="Connecticut average")

## Wone dataframe for departments
dept <- subset(dept, DepartmentName!="Connecticut average")

## Getting rid of some odd outliers
dept <- subset(dept, white_over_15_p < 100)

## Creating a new dataframe that compares population to stops by race
stops_race1 <- dept %>%
  select(DepartmentName, white_p, black_p, hispanic_p, minorities_p, white_over_15_p, black_over_15_p, hispanic_over_15_p, minorities_over_15_p) %>%
  gather("Ethnicity", "Percent", 2:9)

stops_race2 <- state %>%
  select(DepartmentName, white_p, black_p, hispanic_p, minorities_p, white_over_15_p, black_over_15_p, hispanic_over_15_p, minorities_over_15_p) %>%
  gather("Ethnicity", "Percent", 2:9)

stops_race2$DepartmentName <- gsub("Connecticut average", "Connecticut", stops_race2$DepartmentName)

stops_race <- rbind(stops_race1, stops_race2)

stops_race$Ethnicity <- gsub("black_over_15_p", "Black driving population", stops_race$Ethnicity)
stops_race$Ethnicity <- gsub("black_p", "Black traffic stops", stops_race$Ethnicity)
stops_race$Ethnicity <- gsub("hispanic_p", "Hispanic traffic stops", stops_race$Ethnicity)
stops_race$Ethnicity <- gsub("hispanic_over_15_p", "Hispanic driving population", stops_race$Ethnicity)
stops_race$Ethnicity <- gsub("white_p", "White traffic stops", stops_race$Ethnicity)
stops_race$Ethnicity <- gsub("white_over_15_p", "White driving population", stops_race$Ethnicity)
stops_race$Ethnicity <- gsub("minorities_p", "Non-White traffic stops", stops_race$Ethnicity)
stops_race$Ethnicity <- gsub("minorities_over_15_p", "Non-White driving population", stops_race$Ethnicity)

stops_race$type <- gsub("White ", "", stops_race$Ethnicity)
stops_race$type <- gsub("Black ", "", stops_race$Ethnicity)
stops_race$type <- gsub("Hispanic ", "", stops_race$type)
stops_race$type <- gsub("Non-White ", "", stops_race$type)



stops_race$type <- gsub(" ", "_", stops_race$type)
stops_race$type <- gsub("White_", "", stops_race$type)

stops_race$Ethnicity<- gsub(" traffic stops", "", stops_race$Ethnicity)
stops_race$Ethnicity<- gsub(" driving population", "", stops_race$Ethnicity)
stops_race$Ethnicity<- gsub("Non-White", "Minority", stops_race$Ethnicity)


stops_race <- stops_race %>%
  spread(type, Percent)

## creating an array of colors for charting
five_colors <- c("#8dd3c7", "#ffffb3", "#bebada", "#fb8072", "#80b1d3")


stops_race <- subset(stops_race, !is.na(driving_population))

dept_names <- data.frame(table(stops_race$DepartmentName))
dept_names <-subset(dept_names, Freq>0)

## A loop that will create the javascript array I can plug into my highcharts chart html page

for (i in 1:nrow(dept_names)) {

  dept_n <- dept_names$Var1[i]
  freq <- dept_names$Freq[i]
  
  the_text <- paste0("{
  name: '", dept_n, "',
  data: [")
  
  new_df <- subset(stops_race, DepartmentName==dept_n)
  
  for (x in 1:freq) {
    d_text <- paste0("{
                     ethnicity: '", new_df[x,2], "', 
                     x: ", new_df[x,3], ",
                     y: ", new_df[x,4], ",
                     z: 1, 
                     marker: {
                        fillColor: '", five_colors[x], "'
                     }
                },")
    print(d_text)
    if (x==1) {
      d_text_all <- d_text
    } else {
      d_text_all<- paste(d_text_all, d_text)
    }
  }
  
  d_text_all <- substr(d_text_all, 1, nchar(d_text_all)-1)
  
  if (i == 1) {
    the_text_big <- paste0(the_text, d_text_all, "]
                     },")
  } else
    the_text_big <- paste0(the_text_big, the_text, d_text_all, "]
                           },")

  
}

the_text_big <- substr(the_text_big, 1, nchar(the_text_big)-1)

write(the_text_big, "text.txt")

########## Version 2 for legend
