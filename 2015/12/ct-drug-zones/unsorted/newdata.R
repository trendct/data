
require(lubridate)
require(ggplot2)
require(dplyr)
require(lattice)
require(tidyr)
conv_new <- read.csv("convictionsnew.csv")
arrests_new <- read.csv("arrests_h.csv")

arrests_new$ARREST.DATE <- as.character(arrests_new$ARREST.DATE)
arrests_new$Date <- mdy(arrests_new$ARREST.DATE)
arrests_new$Year <- year(arrests_new$Date)
arrests_new$Year[arrests_new$Year==2099] <- 1999


arrests_new$RaceOf <- paste(arrests_new$RACE, arrests_new$DEF_HISPANIC_IND, sep="")

index <- c("Asian", "AsianY", "Black", "BlackY", "HispY", "Native American", 
           "Native AmericanY", "Not Identified", "White", "WhiteY")


values <- c("Asian", "Hispanic", "Black", "Hispanic", "Hispanic", "Native American", 
            "Hispanic", "Not Identified", "White", "Hispanic")
arrests_new$Def_Race <- values[match(arrests_new$RaceOf, index)]


# Race for all charges
race_year_new <- data.frame(table(arrests_new$Year,arrests_new$Def_Race))

colnames(race_year_new) <- c("Year", "Race", "Arrests")
race_year_new$Race <- as.character(race_year_new$Race)
race_year_new$Race <- gsub("^$", "Not Listed", race_year_new$Race)

ggplot(race_year_new, aes(Year, Arrests, group=Race, colour=Race)) +
  geom_path(alpha=0.5) +
  ggtitle("Race of those arrested since 1999") +
  theme_minimal()

# And by percent
ggplot(race_year_new, aes(Year, Arrests, group=Race, fill=Race)) + geom_area(position="fill") +
  ggtitle("Race of those arrested since 1999 by percent")

# Types of charges over time
charges_year_new <- data.frame(table(arrests_new$Year,arrests_new$ORIGINAL_STATUTE))
colnames(charges_year_new) <- c("Year", "Charge", "Arrests")

ggplot(charges_year_new, aes(Year, Arrests, group=Charge, colour=Charge)) +
  geom_path(alpha=0.5) +
  ggtitle("Types of Possession charges since 1999") +
  theme_minimal()

# And by percent
ggplot(charges_year_new, aes(Year, Arrests, group=Charge, fill=Charge)) + geom_area(position="fill") +
  ggtitle("Possession charges since 1999 by percent")


# Comparing to convictions now

conv_new$Sentenced <- as.character(conv_new$Sentenced)
conv_new$Date <- mdy(conv_new$Sentenced)
conv_new$Year <- year(conv_new$Date)
conv_new$Year[conv_new$Year==2099] <- 1999

# Race for all convictions

conv_new$RaceOf <- paste(conv_new$Race, conv_new$Hispanic, sep="")
index <- c("", "A", "B", "BY", "C", "CY", "HY", "O", "OY")
values <- c("Unlisted", "Asian", "Black", "Hispanic", "White", "Hispanic", "Hispanic", "Other", "Hispanic")
conv_new$RaceOfConvicted <- values[match(conv_new$RaceOf, index)]

conv_year_new <- data.frame(table(conv_new$Year,conv_new$RaceOfConvicted))

colnames(conv_year_new) <- c("Year", "Race", "Convictions")
conv_year_new$Race <- as.character(conv_year_new$Race)

ggplot(conv_year_new, aes(Year, Convictions, group=Race, colour=Race)) +
  geom_path(alpha=0.5) +
  ggtitle("Race of those convicted since 1999") +
  theme_minimal()

# And by percent
ggplot(conv_year_new, aes(Year, Convictions, group=Race, fill=Race)) + geom_area(position="fill") +
  ggtitle("Race of those convicted since 1999 by percent")


# Now going into type of convictions

# Types of charges over time

conv_year_new <- data.frame(table(conv_new$Year,conv_new$Final.Statute))
colnames(conv_year_new) <- c("Year", "Type", "Convictions")

ggplot(conv_year_new, aes(Year, Convictions, group=Type, colour=Type)) +
  geom_path(alpha=0.5) +
  ggtitle("Types of Possession convictions since 1999") +
  theme_minimal()

# And by percent
ggplot(conv_year_new, aes(Year, Convictions, group=Type, fill=Type)) + geom_area(position="fill") +
  ggtitle("Possession convictions since 1999 by percent")

# charges as percent of all arrests

arrests.charges <- data.frame(table(arrests_new$ORIGINAL_STATUTE))
colnames(arrests.charges) <- c("Statute", "Arrest.Charge")
arrests.charges$Arrest.Charge.Percent <- round((arrests.charges$Arrest.Charge/sum(arrests.charges$Arrest.Charge)*100), digits=2)
arrests.charges <- subset(arrests.charges, (Statute=="21a-279(a)" | Statute=="21a-279(b)" | Statute=="21a-279(c)" | Statute=="21a-279(d)"))

# charges as percent of all convictions 

convictions.charges <- data.frame(table(conv_new$Final.Statute))
colnames(convictions.charges) <- c("Statute", "Conviction.Charge")
convictions.charges$Conviction.Charge.Percent <- round((convictions.charges$Conviction.Charge/sum(convictions.charges$Conviction.Charge)*100), digits=2)
convictions.charges <- subset(convictions.charges, (Statute=="21a-279(a)" | Statute=="21a-279(b)" | Statute=="21a-279(c)" | Statute=="21a-279(d)"))
                          
charges_compare <- left_join(arrests.charges,convictions.charges)

charges_comp <- charges_compare
charges_comp$Arrest.Charge <- NULL
charges_comp$Conviction.Charge <- NULL

compared <- gather(charges_comp, "Type", "Percent", 2:3)

ggplot(data=compared, aes(x=Type, y=Percent, fill=Statute)) + geom_bar(stat="identity") + ggtitle("Percent of subcharges for 21a-279: Arrests vs. Convictions ")

compared$Percent

# Divide up towns

conv_new$Police.Name <- as.character(conv_new$Police.Name)
towns_subset_c <- conv_new[grepl("LOCAL POLICE", conv_new$Police.Name),]
towns_subset_c$Police.Name <- gsub("LOCAL POLICE ", "", towns_subset_c$Police.Name)

# HEY! WHAT ABOUT STATE POLICE/ PROBATION DEPARTMENTS??

urban_convictions_d <- subset(towns_subset_c, (Police.Name=="BRIDGEPORT" | Police.Name=="HARTFORD" | Police.Name=="NEW HAVEN" |
                                             Police.Name=="NEW BRITAIN" | Police.Name=="WEST HAVEN" | Police.Name=="NEW LONDON" |
                                             Police.Name=="WATERBURY" | Police.Name=="NORWALK" | Police.Name=="WATERBURY" |
                                             Police.Name=="NORWALK" | Police.Name=="ANSONIA" | Police.Name=="STAMFORD"))

suburban_convictions_d <- subset(towns_subset_c, !(Police.Name=="BRIDGEPORT" | Police.Name=="HARTFORD" | Police.Name=="NEW HAVEN" |
                                                 Police.Name=="NEW BRITAIN" | Police.Name=="WEST HAVEN" | Police.Name=="NEW LONDON" |
                                                 Police.Name=="WATERBURY" | Police.Name=="NORWALK" | Police.Name=="WATERBURY" |
                                                 Police.Name=="NORWALK" | Police.Name=="ANSONIA" | Police.Name=="STAMFORD"))

# Let's fix the town names in the arrests dataset 

arrests_new$TOWN <- as.character(arrests_new$TOWN)

arrests_new$TOWN <- gsub("BANTAM", "LITCHFIELD", arrests_new$TOWN)
arrests_new$TOWN <- gsub("BELLE HAVEN", "GREENWICH", arrests_new$TOWN)
arrests_new$TOWN <- gsub("BRANCHVILLE", "RIDGEFIELD", arrests_new$TOWN)
arrests_new$TOWN <- gsub("CANNONDALE", "WILTON", arrests_new$TOWN)
arrests_new$TOWN <- gsub("CENTERVILLE", "HAMDEN", arrests_new$TOWN)
arrests_new$TOWN <- gsub("CHESHIRE HEIGHTS", "CHESHIRE", arrests_new$TOWN)
arrests_new$TOWN <- gsub("DANIELSON", "WINDHAM", arrests_new$TOWN)
arrests_new$TOWN <- gsub("DOBSONVILLE", "VERNON", arrests_new$TOWN)
arrests_new$TOWN <- gsub("EAST SUFFIELD", "SUFFIELD", arrests_new$TOWN)
arrests_new$TOWN <- gsub("FITCHVILLE", "BOZRAH", arrests_new$TOWN)
arrests_new$TOWN <- gsub("GOSHEN CENTER", "GOSHEN", arrests_new$TOWN)
arrests_new$TOWN <- gsub("GROTON CITY OF", "GROTON", arrests_new$TOWN)
arrests_new$TOWN <- gsub("JEWETT CITY", "GRISWOLD", arrests_new$TOWN)
arrests_new$TOWN <- gsub("MOOSUP", "PLAINFIELD", arrests_new$TOWN)
arrests_new$TOWN <- gsub("NORTH COLEBROOK", "COLEBROOK", arrests_new$TOWN)
arrests_new$TOWN <- gsub("OAKVILLE", "WATERTOWN", arrests_new$TOWN)
arrests_new$TOWN <- gsub("POQUONOCK", "WINDSOR", arrests_new$TOWN)
arrests_new$TOWN <- gsub("PUTNAM HEIGHTS", "PUTNAM", arrests_new$TOWN)
arrests_new$TOWN <- gsub("ROCKVILLE", "VERNON", arrests_new$TOWN)
arrests_new$TOWN <- gsub("SOUTH CANAAN", "CANAAN", arrests_new$TOWN)
arrests_new$TOWN <- gsub("STORRS", "MANSFIELD", arrests_new$TOWN)
arrests_new$TOWN <- gsub("UNCASVILLE", "MONTVILLE", arrests_new$TOWN)
arrests_new$TOWN <- gsub("WAREHOUSE POINT", "WINDSORVILLE", arrests_new$TOWN)
arrests_new$TOWN <- gsub("WEST MORRIS", "LITCHFIELD", arrests_new$TOWN)


#HEY, THIS IS WHERE I'M INSERTING THE BUFFER ZONE STATS
# REFERING TO correlate.R commands
percent$TOWN <- str_to_upper(percent$TOWN)

# ARRESTS TOTAL
arr_total <- data.frame(table(arrests_new$TOWN))
colnames(arr_total) <- c("TOWN", "TOTAL.ARRESTS")
arr_total$TOWN <- as.character(arr_total$TOWN)

# CONVICTIONS TOTAL
conv_total <- data.frame(table(towns_subset_c$Police.Name))
colnames(conv_total) <- c("TOWN", "TOTAL.CONVICTIONS")
conv_total$TOWN <- as.character(conv_total$TOWN)

# ARRESTS FOR SPECIFIC SUBCHARGES

arr_charges <- data.frame(table(arrests_new$TOWN, arrests_new$ORIGINAL_STATUTE))
colnames(arr_charges) <- c("TOWN", "Charge", "Freq")
arr_charges <- spread(arr_charges, Charge, Freq)
arr_charges$TOWN <- as.character(arr_charges$TOWN)

# JOIN ALL TO MAKE ONE ARRESTS DATAFRAME

arr_all <- left_join(percent, arr_total) 
arr_all <- left_join(arr_all, arr_charges)

# CORRELATION TIME
arr_all$cor279a <- (cor(arr_all$PERCENT, arr_all[,7]))
arr_all$cor279b <- (cor(arr_all$PERCENT, arr_all[,10]))
arr_all$cor279c <- (cor(arr_all$PERCENT, arr_all[,12]))
arr_all$cor279d <- (cor(arr_all$PERCENT, arr_all[,14]))

# OK, as a whole, ther's only a .61 correlation between 279d and buffer zone percent

# CONVICTIONS FOR SPECIFIC SUBCHARGES
# COME BACK TO THIS IN A SECOND

arrestscorr <- left_join(percent, arrests_new)

#Divide up arrests between urban and suburbs

#urban_arrests_d <- subset(arrests_new, (TOWN=="BRIDGEPORT" | TOWN=="HARTFORD" | TOWN=="NEW HAVEN" |
#                                                 TOWN=="NEW BRITAIN" | TOWN=="WEST HAVEN" | TOWN=="NEW LONDON" |
#                                                 TOWN=="WATERBURY" | TOWN=="NORWALK" | TOWN=="WATERBURY" |
#                                                 TOWN=="NORWALK" | TOWN=="ANSONIA" | TOWN=="STAMFORD"))

urban_arrests_d <- filter(arrests_new, TOWN=="BRIDGEPORT" | TOWN=="HARTFORD" | TOWN=="NEW HAVEN" |
                                                                             TOWN=="NEW BRITAIN" | TOWN=="WEST HAVEN" | TOWN=="NEW LONDON" |
                                                                             TOWN=="WATERBURY" | TOWN=="NORWALK" | TOWN=="WATERBURY" |
                                                                             TOWN=="NORWALK" | TOWN=="ANSONIA" | TOWN=="STAMFORD")

#suburban_arrests_d <- subset(arrests_new, !(TOWN=="BRIDGEPORT" | TOWN=="HARTFORD" | TOWN=="NEW HAVEN" |
#                                                     TOWN=="NEW BRITAIN" | TOWN=="WEST HAVEN" | TOWN=="NEW LONDON" |
#                                                     TOWN=="WATERBURY" | TOWN=="NORWALK" | TOWN=="WATERBURY" |
#                                                     TOWN=="NORWALK" | TOWN=="ANSONIA" | TOWN=="STAMFORD"))

suburban_arrests_d <- filter(arrests_new, TOWN!="BRIDGEPORT" & TOWN!="HARTFORD" & TOWN!="NEW HAVEN" &
                            TOWN!="NEW BRITAIN" & TOWN!="WEST HAVEN" & TOWN!="NEW LONDON" &
                            TOWN!="WATERBURY" & TOWN!="NORWALK" & TOWN!="WATERBURY" &
                            TOWN!="NORWALK" & TOWN!="ANSONIA" & TOWN!="STAMFORD")


# LETS COMPARE URBAN/SUBURBAN ARREST SUBCHARGES TO PERCENT STUFF
# NOW FOR URBAN
arr_total_u <- data.frame(table(urban_arrests_d$TOWN))
colnames(arr_total_u) <- c("TOWN", "TOTAL.ARRESTS")
arr_total_u$TOWN <- as.character(arr_total_u$TOWN)

# ARRESTS FOR SPECIFIC SUBCHARGES IN URBAN AREAS

arr_charges_u <- data.frame(table(urban_arrests_d$TOWN, urban_arrests_d$ORIGINAL_STATUTE))
colnames(arr_charges_u) <- c("TOWN", "Charge", "Freq")
arr_charges_u <- spread(arr_charges_u, Charge, Freq)
arr_charges_u$TOWN <- as.character(arr_charges_u$TOWN)

# JOIN ALL TO MAKE ONE ARRESTS DATAFRAME

arr_all_u <- left_join(percent, arr_total_u) 
arr_all_u <- left_join(arr_all_u, arr_charges_u)

# CORRELATION TIME FOR URBAN AREAS
arr_all_u$cor279a <- (cor(arr_all_u$PERCENT, arr_all_u[,7], use="pairwise.complete.obs"))
arr_all_u$cor279b <- (cor(arr_all_u$PERCENT, arr_all_u[,10], use="pairwise.complete.obs"))
arr_all_u$cor279c <- (cor(arr_all_u$PERCENT, arr_all_u[,12], use="pairwise.complete.obs"))
arr_all_u$cor279d <- (cor(arr_all_u$PERCENT, arr_all_u[,14], use="pairwise.complete.obs"))

# NEXT IS FOR SUBURBAN
arr_total_s <- data.frame(table(suburban_arrests_d$TOWN))
colnames(arr_total_s) <- c("TOWN", "TOTAL.ARRESTS")
arr_total_s$TOWN <- as.character(arr_total_s$TOWN)

# ARRESTS FOR SPECIFIC SUBCHARGES IN SUBURBAN AREAS

arr_charges_s <- data.frame(table(suburban_arrests_d$TOWN, suburban_arrests_d$ORIGINAL_STATUTE))
colnames(arr_charges_s) <- c("TOWN", "Charge", "Freq")
arr_charges_s <- spread(arr_charges_s, Charge, Freq)
arr_charges_s$TOWN <- as.character(arr_charges_s$TOWN)

# JOIN ALL TO MAKE ONE ARRESTS DATAFRAME

arr_all_s <- left_join(percent, arr_total_s) 
arr_all_s <- left_join(arr_all_s, arr_charges_s)

# CORRELATION TIME FOR SUBURBAN AREAS
arr_all_s$cor279a <- (cor(arr_all_s$PERCENT, arr_all_s[,7], use="pairwise.complete.obs"))
arr_all_s$cor279b <- (cor(arr_all_s$PERCENT, arr_all_s[,10], use="pairwise.complete.obs"))
arr_all_s$cor279c <- (cor(arr_all_s$PERCENT, arr_all_s[,12], use="pairwise.complete.obs"))
arr_all_s$cor279d <- (cor(arr_all_s$PERCENT, arr_all_s[,14], use="pairwise.complete.obs"))


# Race for all charges in urban cities
race_year_new_u <- data.frame(table(urban_arrests_d$Year,urban_arrests_d$Def_Race))

colnames(race_year_new_u) <- c("Year", "Race", "Arrests")
race_year_new_u$Race <- as.character(race_year_new_u$Race)
race_year_new_u$Race <- gsub("^$", "Not Listed", race_year_new_u$Race)

ggplot(race_year_new_u, aes(Year, Arrests, group=Race, colour=Race)) +
  geom_path(alpha=0.5) +
  ggtitle("Race of those arrested in urban cities since 1999") +
  theme_minimal()

# And by percent
ggplot(race_year_new_u, aes(Year, Arrests, group=Race, fill=Race)) + geom_area(position="fill") +
  ggtitle("Race of those arrested in urban cities since 1999 by percent")

# Types of charges over time
charges_year_new_u <- data.frame(table(urban_arrests_d$Year,urban_arrests_d$ORIGINAL_STATUTE))
colnames(charges_year_new_u) <- c("Year", "Charge", "Arrests")

ggplot(charges_year_new_u, aes(Year, Arrests, group=Charge, colour=Charge)) +
  geom_path(alpha=0.5) +
  ggtitle("Types of Possession charges in urban cities since 1999") +
  theme_minimal()

# And by percent
ggplot(charges_year_new_u, aes(Year, Arrests, group=Charge, fill=Charge)) + geom_area(position="fill") +
  ggtitle("Possession charges since 1999 in urban cities by percent")


# Comparing to convictions now

urban_convictions_d$Sentenced <- as.character(urban_convictions_d$Sentenced)
urban_convictions_d$Date <- mdy(urban_convictions_d$Sentenced)
urban_convictions_d$Year <- year(urban_convictions_d$Date)
urban_convictions_d$Year[urban_convictions_d$Year==2099] <- 1999

# Race for all convictions

urban_convictions_d$RaceOf <- paste(urban_convictions_d$Race, urban_convictions_d$Hispanic, sep="")
index <- c("", "A", "B", "BY", "C", "CY", "HY", "O", "OY")
values <- c("Unlisted", "Asian", "Black", "Hispanic", "White", "Hispanic", "Hispanic", "Other", "Hispanic")
urban_convictions_d$RaceOfConvicted <- values[match(urban_convictions_d$RaceOf, index)]

urban_convictions_d_year <- data.frame(table(urban_convictions_d$Year,urban_convictions_d$RaceOfConvicted))

colnames(urban_convictions_d_year) <- c("Year", "Race", "Convictions")
urban_convictions_d_year$Race <- as.character(urban_convictions_d_year$Race)

ggplot(urban_convictions_d_year, aes(Year, Convictions, group=Race, colour=Race)) +
  geom_path(alpha=0.5) +
  ggtitle("Race of those convicted in urban cities since 1999") +
  theme_minimal()

# And by percent
ggplot(urban_convictions_d_year, aes(Year, Convictions, group=Race, fill=Race)) + geom_area(position="fill") +
  ggtitle("Race of those convicted since 1999 by percent")


# Now going into type of convictions

# Types of charges over time

urban_convictions_d_year <- data.frame(table(urban_convictions_d$Year,urban_convictions_d$Final.Statute))
colnames(urban_convictions_d_year) <- c("Year", "Type", "Convictions")

ggplot(urban_convictions_d_year, aes(Year, Convictions, group=Type, colour=Type)) +
  geom_path(alpha=0.5) +
  ggtitle("Types of Possession convictions since 1999") +
  theme_minimal()

# And by percent
ggplot(urban_convictions_d_year, aes(Year, Convictions, group=Type, fill=Type)) + geom_area(position="fill") +
  ggtitle("Possession convictions since 1999 by percent")

# charges as percent of all arrests

arrests.charges_u <- data.frame(table(urban_arrests_d$ORIGINAL_STATUTE))
colnames(arrests.charges_u) <- c("Statute", "Arrest.Charge")
arrests.charges_u$Arrest.Charge.Percent <- round((arrests.charges_u$Arrest.Charge/sum(arrests.charges_u$Arrest.Charge)*100), digits=2)
arrests.charges_u <- subset(arrests.charges_u, (Statute=="21a-279(a)" | Statute=="21a-279(b)" | Statute=="21a-279(c)" | Statute=="21a-279(d)"))

# charges as percent of all convictions 

convictions.charges_u <- data.frame(table(urban_convictions_d$Final.Statute))
colnames(convictions.charges_u) <- c("Statute", "Conviction.Charge")
convictions.charges_u$Conviction.Charge.Percent <- round((convictions.charges_u$Conviction.Charge/sum(convictions.charges_u$Conviction.Charge)*100), digits=2)
convictions.charges_u <- subset(convictions.charges_u, (Statute=="21a-279(a)" | Statute=="21a-279(b)" | Statute=="21a-279(c)" | Statute=="21a-279(d)"))

charges_compare_u <- left_join(arrests.charges_u,convictions.charges_u)

charges_comp_u <- charges_compare_u
charges_comp_u$Arrest.Charge <- NULL
charges_comp_u$Conviction.Charge <- NULL

compared_u <- gather(charges_comp, "Type", "Percent", 2:3)

ggplot(data=compared_u, aes(x=Type, y=Percent, fill=Statute)) + geom_bar(stat="identity") + ggtitle("Urban: Percent of subcharges for 21a-279: Arrests vs. Convictions ")

compared_u$Percent

# Race for all charges in suburban cities
race_year_new_s <- data.frame(table(suburban_arrests_d$Year,suburban_arrests_d$Def_Race))

colnames(race_year_new_s) <- c("Year", "Race", "Arrests")
race_year_new_s$Race <- as.character(race_year_new_s$Race)
race_year_new_s$Race <- gsub("^$", "Not Listed", race_year_new_s$Race)

ggplot(race_year_new_s, aes(Year, Arrests, group=Race, colour=Race)) +
  geom_path(alpha=0.5) +
  ggtitle("Race of those arrested in suburban cities since 1999") +
  theme_minimal()

# And by percent
ggplot(race_year_new_s, aes(Year, Arrests, group=Race, fill=Race)) + geom_area(position="fill") +
  ggtitle("Race of those arrested in suburban cities since 1999 by percent")

# Types of charges over time
charges_year_new_s <- data.frame(table(suburban_arrests_d$Year,suburban_arrests_d$ORIGINAL_STATUTE))
colnames(charges_year_new_s) <- c("Year", "Charge", "Arrests")

ggplot(charges_year_new_s, aes(Year, Arrests, group=Charge, colour=Charge)) +
  geom_path(alpha=0.5) +
  ggtitle("Types of Possession arrests in suburban cities since 1999") +
  theme_minimal()

# And by percent
ggplot(charges_year_new_s, aes(Year, Arrests, group=Charge, fill=Charge)) + geom_area(position="fill") +
  ggtitle("Possession charges since 1999 in suburban cities by percent")


# Comparing to convictions now

suburban_convictions_d$Sentenced <- as.character(suburban_convictions_d$Sentenced)
suburban_convictions_d$Date <- mdy(suburban_convictions_d$Sentenced)
suburban_convictions_d$Year <- year(suburban_convictions_d$Date)
suburban_convictions_d$Year[suburban_convictions_d$Year==2099] <- 1999

# Race for all convictions

suburban_convictions_d$RaceOf <- paste(suburban_convictions_d$Race, suburban_convictions_d$Hispanic, sep="")
index <- c("", "A", "B", "BY", "C", "CY", "HY", "O", "OY")
values <- c("Unlisted", "Asian", "Black", "Hispanic", "White", "Hispanic", "Hispanic", "Other", "Hispanic")
suburban_convictions_d$RaceOfConvicted <- values[match(suburban_convictions_d$RaceOf, index)]

suburban_convictions_d_year <- data.frame(table(suburban_convictions_d$Year,suburban_convictions_d$RaceOfConvicted))

colnames(suburban_convictions_d_year) <- c("Year", "Race", "Convictions")
suburban_convictions_d_year$Race <- as.character(suburban_convictions_d_year$Race)

ggplot(suburban_convictions_d_year, aes(Year, Convictions, group=Race, colour=Race)) +
  geom_path(alpha=0.5) +
  ggtitle("Race of those convicted in suburban cities since 1999") +
  theme_minimal()

# And by percent
ggplot(suburban_convictions_d_year, aes(Year, Convictions, group=Race, fill=Race)) + geom_area(position="fill") +
  ggtitle("Race of those convicted since 1999 by percent")


# Now going into type of convictions

# Types of charges over time

suburban_convictions_d_year <- data.frame(table(suburban_convictions_d$Year,suburban_convictions_d$Final.Statute))
colnames(suburban_convictions_d_year) <- c("Year", "Type", "Convictions")

ggplot(suburban_convictions_d_year, aes(Year, Convictions, group=Type, colour=Type)) +
  geom_path(alpha=0.5) +
  ggtitle("Types of Possession convictions since 1999") +
  theme_minimal()

# And by percent
ggplot(suburban_convictions_d_year, aes(Year, Convictions, group=Type, fill=Type)) + geom_area(position="fill") +
  ggtitle("Possession convictions since 1999 by percent")

# charges as percent of all arrests

arrests.charges_s <- data.frame(table(suburban_arrests_d$ORIGINAL_STATUTE))
colnames(arrests.charges_s) <- c("Statute", "Arrest.Charge")
arrests.charges_s$Arrest.Charge.Percent <- round((arrests.charges_s$Arrest.Charge/sum(arrests.charges_s$Arrest.Charge)*100), digits=2)
arrests.charges_s <- subset(arrests.charges_s, (Statute=="21a-279(a)" | Statute=="21a-279(b)" | Statute=="21a-279(c)" | Statute=="21a-279(d)"))

# charges as percent of all convictions 

convictions.charges_s <- data.frame(table(suburban_convictions_d$Final.Statute))
colnames(convictions.charges_s) <- c("Statute", "Conviction.Charge")
convictions.charges_s$Conviction.Charge.Percent <- round((convictions.charges_s$Conviction.Charge/sum(convictions.charges_s$Conviction.Charge)*100), digits=2)
convictions.charges_s <- subset(convictions.charges_s, (Statute=="21a-279(a)" | Statute=="21a-279(b)" | Statute=="21a-279(c)" | Statute=="21a-279(d)"))

charges_compare_s <- left_join(arrests.charges_s,convictions.charges_s)

charges_comp_s <- charges_compare_s
charges_comp_s$Arrest.Charge <- NULL
charges_comp_s$Conviction.Charge <- NULL

compared_s <- gather(charges_comp, "Type", "Percent", 2:3)

ggplot(data=compared_s, aes(x=Type, y=Percent, fill=Statute)) + geom_bar(stat="identity") + ggtitle("Suburban: Percent of subcharges for 21a-279: Arrests vs. Convictions ")

compared_s$Percent
