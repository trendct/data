# This script generates a data array used in Highcharts
# Don't remember if this was actually used in the final product. It was definitely exploratory

#stops_sub <- read.csv("stops_sub.csv")
stops_sub <- read.csv("data/stops_sub2.csv")

stops_sub$Department.Name <- as.character(stops_sub$Department.Name)
stops_sub$Department.Name <- ifelse(stops_sub$OrganizationIdentificationID=="CTCSP0000", "State Police: Headquarters", stops_sub$Department.Name)
stops_sub$Department.Name <- ifelse(stops_sub$OrganizationIdentificationID=="CTCSP0023", "State Police: Headquarters", stops_sub$Department.Name)
stops_sub$Department.Name <- ifelse(stops_sub$OrganizationIdentificationID=="CTCSP0029", "State Police: Headquarters", stops_sub$Department.Name)
stops_sub$Department.Name <- ifelse(stops_sub$OrganizationIdentificationID=="CTCSP1900", "State Police: Headquarters", stops_sub$Department.Name)
stops_sub$Department.Name <- ifelse(stops_sub$OrganizationIdentificationID=="CTCSP2900", "State Police: Headquarters", stops_sub$Department.Name)
stops_sub$Department.Name <- ifelse(stops_sub$OrganizationIdentificationID=="CTCSP3800", "State Police: Headquarters", stops_sub$Department.Name)
stops_sub$Department.Name <- ifelse(stops_sub$OrganizationIdentificationID=="CTCSP0200", "State Police: Troop A", stops_sub$Department.Name)
stops_sub$Department.Name <- ifelse(stops_sub$OrganizationIdentificationID=="CTCSP0300", "State Police: Troop B", stops_sub$Department.Name)
stops_sub$Department.Name <- ifelse(stops_sub$OrganizationIdentificationID=="CTCSP0400", "State Police: Troop C", stops_sub$Department.Name)
stops_sub$Department.Name <- ifelse(stops_sub$OrganizationIdentificationID=="CTCSP0500", "State Police: Troop D", stops_sub$Department.Name)
stops_sub$Department.Name <- ifelse(stops_sub$OrganizationIdentificationID=="CTCSP0600", "State Police: Troop E", stops_sub$Department.Name)
stops_sub$Department.Name <- ifelse(stops_sub$OrganizationIdentificationID=="CTCSP0700", "State Police: Troop F", stops_sub$Department.Name)
stops_sub$Department.Name <- ifelse(stops_sub$OrganizationIdentificationID=="CTCSP0800", "State Police: Troop G", stops_sub$Department.Name)
stops_sub$Department.Name <- ifelse(stops_sub$OrganizationIdentificationID=="CTCSP0900", "State Police: Troop H", stops_sub$Department.Name)
stops_sub$Department.Name <- ifelse(stops_sub$OrganizationIdentificationID=="CTCSP1000", "State Police: Troop I", stops_sub$Department.Name)
stops_sub$Department.Name <- ifelse(stops_sub$OrganizationIdentificationID=="CTCSP1100", "State Police: Troop J", stops_sub$Department.Name)
stops_sub$Department.Name <- ifelse(stops_sub$OrganizationIdentificationID=="CTCSP1200", "State Police: Troop K", stops_sub$Department.Name)

stops_sub$hour_only <- gsub(":.*", "", stops_sub$InterventionTime)
stops_sub$hour_only <- gsub(".* ", "", stops_sub$hour_only)

stops_sub$minute_only <- as.character(stops_sub$InterventionTime)
stops_sub$minute_only <- substr(stops_sub$minute_only, 3, nchar(stops_sub$minute_only)-3)
stops_sub$minute_only <- gsub(".*\\:", "", stops_sub$minute_only)
stops_sub$ampm <- ifelse(as.numeric(stops_sub$hour_only)>12, "p.m.", "a.m.")
stops_sub$hour_only <- ifelse(as.numeric(stops_sub$hour_only)>12, as.numeric(stops_sub$hour_only)-12,  as.numeric(stops_sub$hour_only))
stops_sub$official_time <- paste0(stops_sub$hour_only, ":", stops_sub$minute_only, " ", stops_sub$ampm)
stops_sub$date_chart <- gsub(" .*", "", stops_sub$InterventionDate)

#stops_sub_backup <- stops_sub
#stops_sub <- stops_sub_backup

stops_sub <- subset(stops_sub, light_dark!="neither")

#stops_sub <- read.csv("stops_fixed.csv")
town_list <- c("Bloomfield", 
               "New Milford", 
               "Norwalk", 
               "West Hartford", 
               "Wethersfield", 
               "Stratford",
               "Meriden",
               "New Britain",
               "Newington",
               "Trumbull",
               "Waterbury",
               "State Police: Troop H")

#stops_sub$as_integer <- as.integer(stops_sub$ForRealTime)
#stops_sub$as_integer <- stops_sub$as_integer * 1000

#stops_race <- stops_sub[c("DepartmentName", )]

#Intervention.Time
#Vehicle.Searched.Indicator (29)
#Town.Resident.Indicator (17)
stops_sub$VehicleSearchedIndicator <- as.character(stops_sub$VehicleSearchedIndicator)
stops_sub$VehicleSearchedIndicator <- gsub("1", "Yes", stops_sub$VehicleSearchedIndicator) 
stops_sub$VehicleSearchedIndicator <- gsub("0", "No", stops_sub$VehicleSearchedIndicator) 

stops_sub$TownRecidentIndicator <- as.character(stops_sub$TownRecidentIndicator)
stops_sub$TownRecidentIndicator <- gsub("1", "Yes", stops_sub$TownRecidentIndicator) 
stops_sub$TownRecidentIndicator <- gsub("0", "No", stops_sub$TownRecidentIndicator) 

five_colors <- c( "#bebada", "#fb8072", "#80b1d3", "#8dd3c7", "#ffffb3")

dept_names <- data.frame(table(stops_sub$ethnicity_wm))
dept_names <- dept_names[order(-dept_names$Freq),]

# as.numeric(as.POSIXct("1899-12-30 17:15:00"))
# as.numeric(as.POSIXct("1899-12-30 21:15:00"))

for (y in 1:length(town_list)) {
  
  
  stops_sub2 <- subset(stops_sub, (hours_since < 4 & hours_since > -4))
  stops_sub2$RealTime2 <- ymd_hms(stops_sub2$RealTime2, tz="America/New_York")
  #stops_sub2 <- subset(stops_sub2, RealTime2 < strptime("1899-12-30 21:15:00 EST", "%Y-%m-%d %H:%M:%S") & RealTime2 > strptime("1899-12-30 17:15:00 EST", "%Y-%m-%d %H:%M:%S"))
  stops_sub2 <- subset(stops_sub2, Department.Name==town_list[y])
  
  stops_sub2$as_integer <-  as.numeric(stops_sub2$RealTime2)
  stops_sub2$as_integer <- as.character(stops_sub2$as_integer * 1000)
  
  
  
  for (i in 1:nrow(dept_names)) {
    
    dept_n <- dept_names$Var1[i]
    freq <- dept_names$Freq[i]
    
    the_text <- paste0("{
                       name: '", dept_n, "',
                       color: '", five_colors[i], "',

                       data: [")
    
    #Looking at ethnicity here, not department names
    new_df <- subset(stops_sub2, ethnicity_wm==dept_n)
    
    for (x in 1:nrow(new_df)) {
      d_text <- paste0("{
                       department: '", new_df[x,67], "', 
                       x: ", new_df[x,69], ",
                       y: ", new_df[x,61], ",
                       z: '", new_df[x,29], "',
                       v: '", new_df[x,17], "', 
                       u: '", new_df[x,68], "'
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
  dept_name_fn <- gsub(" ", "", town_list[y])
  dept_name_fn <- gsub("\\:", "", dept_name_fn)
  
  file_name <- paste0(dept_name_fn,".txt")
  write(the_text_big, file_name)
  
  #stops_sub2 <- subset(stops, Department.Name=="Bloomfield")
  
  
}


---
  
  
  

stops_race_bub <- stops_race 

# stops_race_bub$Difference <- round(stops_race_bub$Difference/100,2)
# stops_race_bub$`Estimated Driving Population` <- round(stops_race_bub$`Estimated Driving Population`/100,2)
# stops_race_bub$`Traffic stops` <- round(stops_race_bub$`Traffic stops`/100,2)

# I want bubbles grouped by Ethnicity


## MAYBE TAKE OUT THE D3 COLOR
