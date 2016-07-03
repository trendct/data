# Writes three javascript arrays that created scatter plots used in 
# http://trafficstops.trendct.org/story/digging-deeper-into-racial-disparities-in-ct-traffic-stops/


explore <- read.csv("data/mega_df.csv")

dept <- subset(explore, is.na(ReportingOfficerIdentificationID))
state <- subset(dept, DepartmentName=="Connecticut average")
dept <- subset(dept, DepartmentName!="Connecticut average")


stops_race1 <- dept %>%
  select(DepartmentName, black_p, hispanic_p, minorities_p, black_16, hispanic_16, minorities_16, b_t_s_diff, h_t_s_diff, m_t_s_diff, b_t_s_pop_diff, h_t_s_pop_diff, m_t_s_pop_diff, b_distance, h_distance, m_distance, resident_b_p_r, resident_h_p_r, resident_m_p_r) %>%
  gather("Ethnicity", "Percent", 2:19)


stops_race2 <- state %>%
  select(DepartmentName, black_p, hispanic_p, minorities_p, black_16, hispanic_16, minorities_16, b_t_s_diff, h_t_s_diff, m_t_s_diff, b_t_s_pop_diff, h_t_s_pop_diff, m_t_s_pop_diff, b_distance, h_distance, m_distance, resident_b_p_r, resident_h_p_r, resident_m_p_r) %>%
  gather("Ethnicity", "Percent", 2:19)

stops_race2$DepartmentName <- gsub("Connecticut average", "Connecticut", stops_race2$DepartmentName)

stops_race <- rbind(stops_race1, stops_race2)

stops_race$Ethnicity <- gsub("black_16", "Black driving population", stops_race$Ethnicity)
stops_race$Ethnicity <- gsub("black_p", "Black traffic stops", stops_race$Ethnicity)
stops_race$Ethnicity <- gsub("hispanic_p", "Hispanic traffic stops", stops_race$Ethnicity)
stops_race$Ethnicity <- gsub("hispanic_16", "Hispanic driving population", stops_race$Ethnicity)
#stops_race$Ethnicity <- gsub("white_p", "White traffic stops", stops_race$Ethnicity)
#stops_race$Ethnicity <- gsub("white_16", "White driving population", stops_race$Ethnicity)
stops_race$Ethnicity <- gsub("minorities_p", "Non-White traffic stops", stops_race$Ethnicity)
stops_race$Ethnicity <- gsub("minorities_16", "Non-White driving population", stops_race$Ethnicity)

stops_race$Ethnicity <- gsub("b_t_s_diff", "Black stops difference between town and state average", stops_race$Ethnicity)
stops_race$Ethnicity <- gsub("h_t_s_diff", "Hispanic stops difference between town and state average", stops_race$Ethnicity)
stops_race$Ethnicity <- gsub("m_t_s_diff", "Minority stops difference between town and state average", stops_race$Ethnicity)

stops_race$Ethnicity <- gsub("b_t_s_pop_diff", "Black population difference between town and state average", stops_race$Ethnicity)
stops_race$Ethnicity <- gsub("h_t_s_pop_diff", "Hispanic population difference between town and state average", stops_race$Ethnicity)
stops_race$Ethnicity <- gsub("m_t_s_pop_diff", "Minority population difference between town and state average", stops_race$Ethnicity)

stops_race$Ethnicity <- gsub("b_distance", "Black distance between stops and population", stops_race$Ethnicity)
stops_race$Ethnicity <- gsub("h_distance", "Hispanic distance between stops and population", stops_race$Ethnicity)
stops_race$Ethnicity <- gsub("m_distance", "Minority distance between stops and population", stops_race$Ethnicity)

stops_race$Ethnicity <- gsub("resident_b_p_r", "Black non-resident stops", stops_race$Ethnicity)
stops_race$Ethnicity <- gsub("resident_h_p_r", "Hispanic non-resident stops", stops_race$Ethnicity)
stops_race$Ethnicity <- gsub("resident_m_p_r", "Minority non-resident stops", stops_race$Ethnicity)

stops_race$type <- stops_race$Ethnicity 
stops_race$type <- gsub("Black ", "", stops_race$type)
stops_race$type <- gsub("Hispanic ", "", stops_race$type)
stops_race$type <- gsub("Non-White ", "", stops_race$type)
stops_race$type <- gsub("Minority ", "", stops_race$type)

stops_race$type <- gsub(" ", "_", stops_race$type)
stops_race$type <- gsub("-", "_", stops_race$type)


#stops_race$type <- gsub("White_", "", stops_race$type)

stops_race$Ethnicity<- gsub(" traffic stops", "", stops_race$Ethnicity)
stops_race$Ethnicity<- gsub(" driving population", "", stops_race$Ethnicity)
stops_race$Ethnicity<- gsub(" stops difference between town and state average", "", stops_race$Ethnicity)
stops_race$Ethnicity<- gsub(" population difference between town and state average", "", stops_race$Ethnicity)
stops_race$Ethnicity<- gsub(" distance between stops and population", "", stops_race$Ethnicity)
stops_race$Ethnicity<- gsub(" non-resident stops", "", stops_race$Ethnicity)

stops_race$Ethnicity<- gsub("Non-White", "Minority", stops_race$Ethnicity)
stops_race$Percent <- stops_race$Percent/100

stops_race <- stops_race %>%
  spread(type, Percent)



stops_race$type <- "blank"

stops_race$DepartmentName <- as.character(stops_race$DepartmentName)

for (i in 1:nrow(stops_race)) {
  
  if (stops_race$DepartmentName[i]=="Connecticut") {
    stops_race$type[i] <-"Connecticut"
  } else {
    stops_race$type[i] <-"Department"
  }
}


stops_race_sub <- subset(stops_race, stops_difference_between_town_and_state_average > .10)

# officers_sub <- officers %>%
#   select(ReportingOfficerIdentificationID, minorities_p)
# officers_sub$minorities_p <- round(officers_sub$minorities_p/100,2)
# officers_sub <- subset(officers_sub, !is.na(minorities_p))
# 
# officers_sub$ReportingOfficerIdentificationID <- gsub(".*--", "", officers_sub$ReportingOfficerIdentificationID)

officers_m1 <- stops_race_sub %>%
  filter(Ethnicity=="Minority") %>%
  #  filter(DepartmentName!="Connecticut") %>%
  arrange(-distance_between_stops_and_population)

officers_m1$DepartmentName <- factor(officers_m1$DepartmentName, levels = officers_m1$DepartmentName[order(officers_m1$distance_between_stops_and_population)])


officers_s1 <- stops_race %>%
  filter(DepartmentName=="Connecticut") %>%
  filter(Ethnicity=="Minority")

stops2 <- subset(stops, DepartmentName=="New Haven")

town_residents_stops_race_m <- stops %>%
  group_by(DepartmentName, TownRecidentIndicator, RE) %>%
  summarise(total=n()) %>%
  spread(RE, total) %>%
  group_by(DepartmentName, TownRecidentIndicator) %>%
  mutate(total=sum(Asian,Black,hispanic,Indian,`middle-eastern`, White, na.rm=TRUE))  %>%
  mutate(minorities=total-White) %>%
  select(DepartmentName, TownRecidentIndicator, minorities) %>%
  mutate(total=sum(minorities), percent=round(minorities/total*100,2)) %>%
  select(DepartmentName, TownRecidentIndicator, percent) %>%
  spread(TownRecidentIndicator, percent)

colnames(town_residents_stops_race_m2) <- c("DepartmentName", "not.resident_m_p", "resident_m_p")


town_residents_stops_race_m_r <- stops %>%
  group_by(DepartmentName, TownRecidentIndicator, RE) %>%
  summarise(total=n()) %>%
  spread(RE, total) %>%
  group_by(DepartmentName, TownRecidentIndicator) %>%
  mutate(total=sum(Asian,Black,hispanic,Indian,`middle-eastern`, White, na.rm=TRUE))  %>%
  mutate(minorities=total-White) %>%
  filter(TownRecidentIndicator=="resident") %>%
  select(DepartmentName, TownRecidentIndicator, total, minorities) %>%
  mutate(resident_m_p_r=round(minorities/total*100,2)) %>%
  select(DepartmentName, resident_m_p_r)

#colnames(town_residents_stops_race_m_r) <- c("DepartmentName", "not.resident_m_p_r", "resident_m_p_r")
#minorities not resident 2636, minorities resident 5671, total minorities pulled over: 8307 
#minorities residents: 5671, total pulled over:  12818
#minorities residents: 5671, total residents pulled over:  7039 *** WE HAVE A WINNER
# not 31.73       res 68.27


town_residents_stops_race_b_r <- stops %>%
  group_by(DepartmentName, TownRecidentIndicator, RE) %>%
  summarise(total=n()) %>%
  filter(TownRecidentIndicator=="resident") %>%
  spread(RE, total) %>%
#  group_by(DepartmentName, RE) %>%
  mutate(total=sum(Asian, Black, hispanic, Indian, `middle-eastern`, White, na.rm=TRUE)) %>%
  mutate(resident_b_p_r=round(Black/total*100,2)) %>%
  select(DepartmentName, resident_b_p_r)

#####


five_colors <- c("#8dd3c7", "#ffffb3", "#bebada", "#fb8072", "#80b1d3")

stops_race_bub <- subset(stops_race, !is.na(population_difference_between_town_and_state_average))
stops_race_bub <- subset(stops_race_bub, !is.na(stops_difference_between_town_and_state_average))

stops_race_bub$distance_between_stops_and_population <- round(stops_race_bub$distance_between_stops_and_population*100,2)
stops_race_bub$stops_difference_between_town_and_state_average <- round(stops_race_bub$stops_difference_between_town_and_state_average*100,2)
stops_race_bub$population_difference_between_town_and_state_average <- round(stops_race_bub$population_difference_between_town_and_state_average*100,2)

# I want bubbles grouped by Ethnicity
dept_names <- data.frame(table(stops_race$Ethnicity))
dept_names <-subset(dept_names, Freq>0)

## MAYBE TAKE OUT THE D3 COLOR

for (i in 1:nrow(dept_names)) {
  
  dept_n <- dept_names$Var1[i]
  freq <- dept_names$Freq[i]
  
  the_text <- paste0("{
  name: '", dept_n, "',
  data: [")
  
  #Looking at ethnicity here, not department names
  new_df <- subset(stops_race_bub, Ethnicity==dept_n)
  
  for (x in 1:nrow(new_df)) {
    d_text <- paste0("{
                     department: '", new_df[x,1], "', 
                     x: ", new_df[x,6], ",
                     y: ", new_df[x,7], ",
                     z: ", new_df[x,3], ", 
                     marker: {
                        fillColor: '", five_colors[i], "'
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

write(the_text_big, "text_bub1.txt")




### bubble 2


stops_race1 <- dept %>%
  select(DepartmentName, edp_b_p, edp_b, edp_h_p, edp_h, edp_m_p, edp_m, edp_b_diff, edp_h_diff, edp_m_diff, edp_b_ratio, edp_h_ratio, edp_m_ratio) %>%
  gather("Ethnicity", "Percent", 2:13)


#-----------

stops_race <- stops_race1

stops_race$type<- ifelse(grepl("_p", stops_race$Ethnicity), "Traffic stops", "Estimated Driving Population")
stops_race$type<- ifelse(grepl("_ratio", stops_race$Ethnicity), "Ratio", stops_race$type)
stops_race$type<- ifelse(grepl("_diff", stops_race$Ethnicity), "Difference", stops_race$type)


stops_race$Ethnicity <- ifelse(grepl("_b", stops_race$Ethnicity), "Black", stops_race$Ethnicity)
stops_race$Ethnicity <- ifelse(grepl("_h", stops_race$Ethnicity), "Hispanic", stops_race$Ethnicity)
stops_race$Ethnicity <- ifelse(grepl("_m", stops_race$Ethnicity), "Minority", stops_race$Ethnicity)

stops_race <- stops_race %>%
  spread(type, Percent)
stops_race$type <- "blank"

stops_race$DepartmentName <- as.character(stops_race$DepartmentName)

stops_race <- subset(stops_race, !is.na(Difference))


five_colors <- c("#8dd3c7", "#ffffb3", "#bebada", "#fb8072", "#80b1d3")

stops_race_bub <- stops_race 

# stops_race_bub$Difference <- round(stops_race_bub$Difference/100,2)
# stops_race_bub$`Estimated Driving Population` <- round(stops_race_bub$`Estimated Driving Population`/100,2)
# stops_race_bub$`Traffic stops` <- round(stops_race_bub$`Traffic stops`/100,2)

# I want bubbles grouped by Ethnicity
dept_names <- data.frame(table(stops_race_bub$Ethnicity))
dept_names <-subset(dept_names, Freq>0)

## MAYBE TAKE OUT THE D3 COLOR

for (i in 1:nrow(dept_names)) {
  
  dept_n <- dept_names$Var1[i]
  freq <- dept_names$Freq[i]
  
  the_text <- paste0("{
                     name: '", dept_n, "',
                     data: [")
  
  #Looking at ethnicity here, not department names
  new_df <- subset(stops_race_bub, Ethnicity==dept_n)
  
  for (x in 1:nrow(new_df)) {
    d_text <- paste0("{
                     department: '", new_df[x,1], "', 
                     x: ", new_df[x,4], ",
                     y: ", new_df[x,6], ",
                     z: ", new_df[x,3], ", 
                     marker: {
                     fillColor: '", five_colors[i], "'
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

write(the_text_big, "text_bub2.txt")

## residents


stops_race1 <- dept %>%
  select(DepartmentName, m_res, m_res_stops, res_diff_m, b_res, b_res_stops, res_diff_b, h_res, h_res_stops, res_diff_h) %>%
  gather("Ethnicity", "Percent", 2:10)


#-----------

stops_race <- stops_race1

stops_race$type<- ifelse(grepl("_res", stops_race$Ethnicity), "Resident population", stops_race$Ethnicity)
stops_race$type<- ifelse(grepl("_res_stops", stops_race$Ethnicity), "Resident stops", stops_race$type)
stops_race$type<- ifelse(grepl("_diff", stops_race$Ethnicity), "Difference", stops_race$type)


stops_race$Ethnicity <- ifelse(grepl("b_", stops_race$Ethnicity), "Black", stops_race$Ethnicity)
stops_race$Ethnicity <- ifelse(grepl("h_", stops_race$Ethnicity), "Hispanic", stops_race$Ethnicity)
stops_race$Ethnicity <- ifelse(grepl("m_", stops_race$Ethnicity), "Minority", stops_race$Ethnicity)
stops_race$Ethnicity <- ifelse(grepl("_b", stops_race$Ethnicity), "Black", stops_race$Ethnicity)
stops_race$Ethnicity <- ifelse(grepl("_h", stops_race$Ethnicity), "Hispanic", stops_race$Ethnicity)
stops_race$Ethnicity <- ifelse(grepl("_m", stops_race$Ethnicity), "Minority", stops_race$Ethnicity)


stops_race <- stops_race %>%
  spread(type, Percent)
stops_race$type <- "blank"

stops_race$DepartmentName <- as.character(stops_race$DepartmentName)

stops_race <- subset(stops_race, !is.na(Difference))



five_colors <- c("#8dd3c7", "#ffffb3", "#bebada", "#fb8072", "#80b1d3")

stops_race_bub <- stops_race 

# stops_race_bub$Difference <- round(stops_race_bub$Difference/100,2)
# stops_race_bub$`Estimated Driving Population` <- round(stops_race_bub$`Estimated Driving Population`/100,2)
# stops_race_bub$`Traffic stops` <- round(stops_race_bub$`Traffic stops`/100,2)

# I want bubbles grouped by Ethnicity
dept_names <- data.frame(table(stops_race_bub$Ethnicity))
dept_names <-subset(dept_names, Freq>0)

## MAYBE TAKE OUT THE D3 COLOR

for (i in 1:nrow(dept_names)) {
  
  dept_n <- dept_names$Var1[i]
  freq <- dept_names$Freq[i]
  
  the_text <- paste0("{
                     name: '", dept_n, "',
                     data: [")
  
  #Looking at ethnicity here, not department names
  new_df <- subset(stops_race_bub, Ethnicity==dept_n)
  
  for (x in 1:nrow(new_df)) {
    d_text <- paste0("{
                     department: '", new_df[x,1], "', 
                     x: ", new_df[x,4], ",
                     y: ", new_df[x,5], ",
                     z: ", new_df[x,3], ", 
                     marker: {
                     fillColor: '", five_colors[i], "'
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

write(the_text_big, "text_bub3.txt")