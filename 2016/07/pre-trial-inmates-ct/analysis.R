# A listing, updated nightly, of individuals being held in Department of Corrections facilities 
## while awaiting trial. This data is appended on nightly basis reflecting the individual inmates 
## being held in correctional facilities each day beginning July 1, 2016.

# Field Descriptions: 

# DOWNLOAD DATE: Date in which the data were extracted and reflecting the population for that day.
# IDENITIFIER: Individual Inmate Identifier
# LATEST ADMISSION DATE: Most recent date in which the inmate has been admitted. 
## In some instances, this may reflect an original date of admission to a correctional facility. 
## Generally, if a date is more than one year old, an inmate should not be considered to have been 
## held for the entire duration of that time.
# RACE: Race of inmate
# AGE: Age of inmate
# BOND AMOUNT: Amount of bond for which the inmate is being held. 
## In some instances, for particularly low (less than $100), this bond amount may be considered a 
## place holder value
# OFFENSE: Controlling offense for which the bond amount has been set.
# FACILITY: Department of Correction facility where the inmate is currently held.
# DETAINER: Denotes whether inmate is being held at the request of another criminal justice agency, 
## or if another agency is to be notified upon release.

library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(ggalt)
library(extrafont)
detach("package:plyr", unload=TRUE)
# Total by day

total_day <- update %>%
  group_by(DOWNLOAD.DATE) %>%
  summarise(total=n())

# Total by day by race

total_day_race <- update %>%
  group_by(DOWNLOAD.DATE, RACE) %>%
  summarise(total=n()) %>%
  spread(RACE, total)


# recent date
#update$DOWNLOAD.DATE <- mdy(update$DOWNLOAD.DATE)

update <- update %>%
  filter(DOWNLOAD.DATE=="07/15/2016")

# Gender breakdown
gender_total <- update %>%
  group_by(GENDER) %>%
  summarise(total=n())

total <- sum(gender_total$total)
gender_total$percent <- round(gender_total$total/total*100,2)

# Race total
race_total <- update %>%
  group_by(RACE) %>%
  summarise(total=n())

total <- sum(race_total$total)
race_total$percent <- round(race_total$total/total*100,2)

# female race total

female_total <- update %>%
  filter(GENDER=="F") %>%
  group_by(RACE) %>%
  summarise(total=n())

total <- sum(female_total$total)
female_total$percent <- round(female_total$total/total*100,2)

# male race total

male_total <- update %>%
  filter(GENDER=="M") %>%
  group_by(RACE) %>%
  summarise(total=n())

total <- sum(male_total$total)
male_total$percent <- round(male_total$total/total*100,2)

# Average age by race
avg_age <- update %>%
  group_by(RACE) %>%
  summarise(avg_age=round(mean(AGE),0), median_age=round(median(AGE),0))

# Total by facility

facilities <- update %>%
  group_by(FACILITY) %>%
  summarise(total=n())

total <- sum(facilities$total)
facilities$percent <- round(facilities$total/total*100,2)


# Total by facility/race

facilities_race <- update %>%
  group_by(FACILITY, RACE) %>%
  summarise(total=n()) %>%
  spread(RACE, total) %>%
  mutate(total=sum(`AMER IND`,ASIAN, BLACK, HISPANIC, WHITE, na.rm=T))

facilities_race_percent <- facilities_race
facilities_race_percent$`AMER IND` <- round(facilities_race$`AMER IND`/ facilities_race$total*100,2)
facilities_race_percent$ASIAN <- round(facilities_race$ASIAN / facilities_race$total*100,2)
facilities_race_percent$BLACK <- round(facilities_race$BLACK / facilities_race$total*100,2)
facilities_race_percent$HISPANIC <- round(facilities_race$HISPANIC / facilities_race$total*100,2)
facilities_race_percent$WHITE <- round(facilities_race$WHITE / facilities_race$total*100,2)

# Bond amount total by race

bonds <- update %>%
  group_by(RACE) %>%
  summarise(avg_bond=mean(BOND.AMOUNT), median_bond=median(BOND.AMOUNT))

# Earliest entry into system

update$LATEST.ADMISSION.DATE <-mdy(update$LATEST.ADMISSION.DATE)

# Today's date

today <- Sys.Date()
update$duration <- today - update$LATEST.ADMISSION.DATE

# Time by race

time_race <- update %>%
  group_by(RACE) %>%
  summarise(avg_days=round(mean(duration),0), median_days=round(median(duration),0))

# What's up with long stays for asians

asians <- update %>%
  filter(RACE=="ASIAN")


# by date, race total


race_total_date <- update %>%
  group_by(DOWNLOAD.DATE, RACE) %>%
  summarise(total=n())

race_total_date$RACE <- gsub(" ", ".", race_total_date$RACE)

race_total_date <- race_total_date %>%
  spread(RACE, total)

total <- sum(race_total$total)
race_total$percent <- round(race_total$total/total*100,2)

# OFFENSE PRE WORK

update$offense_pre <- gsub(", .*", "", update$OFFENSE)
update$offense_degree <- gsub(".*, ", "", update$OFFENSE)

update$offense_degree <- ifelse(update$offense_pre==update$offense_degree, "", update$offense_degree)

offense_list <- update %>%
  group_by(DOWNLOAD.DATE, offense_pre) %>%
  summarise(count=n()) %>%
  group_by(offense_pre) %>%
  summarise(avg=round(mean(count),0)) %>%
  arrange(-avg) %>%
  filter(avg>50)
kable(offense_list)

# female

fem_offense_list <- update %>%
  filter(GENDER=="F") %>%
  group_by(DOWNLOAD.DATE, offense_pre) %>%
  summarise(count=n()) %>%
  group_by(offense_pre) %>%
  summarise(avg=round(mean(count),0)) %>%
  arrange(-avg) 
  
# male

mal_offense_list <- update %>%
  filter(GENDER=="M") %>%
  group_by(DOWNLOAD.DATE, offense_pre) %>%
  summarise(count=n()) %>%
  group_by(offense_pre) %>%
  summarise(avg=round(mean(count),0)) %>%
  arrange(-avg) 

# male race

mal_offense_race <- update %>%
  filter(GENDER=="M") %>%
  group_by(DOWNLOAD.DATE, offense_pre, RACE) %>%
  summarise(count=n()) %>%
  spread(RACE, count) %>%
  group_by(offense_pre) %>%
  summarise(amer.ind.avg=round(mean(`AMER IND`),0), asian.avg=round(mean(ASIAN),0), black.avg=round(mean(BLACK),0), hispanic.avg=round(mean(HISPANIC),0), white.avg=round(mean(WHITE),0)) %>%
  arrange(-white.avg) 
write.csv(mal_offense_race, "male_offenses_race.csv")

fem_offense_race <- update %>%
  filter(GENDER=="F") %>%
  group_by(DOWNLOAD.DATE, offense_pre, RACE) %>%
  summarise(count=n()) %>%
  spread(RACE, count) %>%
  group_by(offense_pre) %>%
  summarise(amer.ind.avg=round(mean(`AMER IND`),0), asian.avg=round(mean(ASIAN),0), black.avg=round(mean(BLACK),0), hispanic.avg=round(mean(HISPANIC),0), white.avg=round(mean(WHITE),0)) %>%
  arrange(-white.avg) 
write.csv(fem_offense_race, "female_offenses_race.csv")

detach("package:plyr", unload=TRUE)

mal_offense_race_percent <- update %>%
  filter(GENDER=="M") %>%
  group_by(DOWNLOAD.DATE, offense_pre, RACE) %>%
  summarise(count=n()) %>%
  spread(RACE, count) %>%
  group_by(offense_pre) %>%
  summarise(amer.ind.avg=round(mean(`AMER IND`),2), asian.avg=round(mean(ASIAN),0), black.avg=round(mean(BLACK),0), hispanic.avg=round(mean(HISPANIC),0), white.avg=round(mean(WHITE),0)) %>%
  arrange(-white.avg) %>%
  group_by(offense_pre) %>%
  mutate(total=sum(amer.ind.avg,asian.avg,black.avg,hispanic.avg,white.avg, na.rm=T)) %>%
  filter(total>20) %>%
  arrange(-total) %>%
  mutate(American.Indian.avg.per = round((amer.ind.avg/total)*100,0), Asian.avg.per = round(asian.avg/total*100,0),
Black.avg.per = round(black.avg/total*100,0), Hispanic.avg.per = round(hispanic.avg/total*100,0), White.avg.per = round(white.avg/total*100,0)) %>%
  select(offense_pre, total, American.Indian.avg.per, Asian.avg.per, Black.avg.per, Hispanic.avg.per, White.avg.per) %>%
  gather(Race, Percent, 3:7)

mal_offense_race_percent$Race <- gsub(".avg.per", "", mal_offense_race_percent$Race)
mal_offense_race_percent$Race <- gsub("\\.", " ", mal_offense_race_percent$Race)

#plotting it

library(plyr)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

Data <- mal_offense_race_percent
Data[is.na(Data)] <- 0
Data <- ddply(Data, .(offense_pre), transform, pos=cumsum(Percent)-(0.5*Percent))
#Data$Percent <- round(Data$Percent, 1)
#Data$pos[205] <- 104
#Data$pos[235] <- 100

Data$offense_pre <- gsub(" AM", "", Data$offense_pre)
Data$offense_pre <- gsub(" DF", "", Data$offense_pre)
Data$offense_pre <- gsub(" AF", "", Data$offense_pre)
Data$offense_pre <- gsub(" CF", "", Data$offense_pre)
Data$offense_pre <- gsub("  F", "", Data$offense_pre)
Data$offense_pre <- str_trim(Data$offense_pre)

Data[Data == 0] <- NA
Data <- Data[ order(-Data$total), ]

Data$offense_pre <- factor(Data$offense_pre, levels = Data$offense_pre[order(Data$total)])

gg <- ggplot(Data, aes(x = offense_pre, y = Percent)) 
gg <- gg + geom_bar(aes(fill = Race), stat="identity") 
gg <- gg + geom_text(aes(label = Percent, y = pos), size = 5) 
#gg <- gg + scale_x_discrete(limits = rev(Data$total)) 
gg <- gg + scale_fill_manual(values=cbPalette)
gg <- gg + coord_flip() 
#gg <- gg +  theme_minimal() 
gg <- gg + geom_rect(data=Data, aes(ymin=102, ymax=114, xmin=-Inf, xmax=Inf), fill="#efefe3")
gg <- gg + geom_text(data=Data, aes(label=round(total,0), x=offense_pre, y=108), fontface="bold", size=5, family="Calibri")
#gg <- gg + geom_text(data=filter(Data, offense_pre=="VIOLATION OF PROBATION OR COND DISCHG"), aes(x=offense_pre, y=108, label="TOTAL"),
#                     color="#7a7d7e", size=5, vjust=-2, fontface="bold", family="Calibri")
#gg <- gg + scale_y_continuous(expand=c(0,120), limits=c(0, 120))
#gg <- gg + scale_x_discrete(expand=c(0,120), limits=c(0, 120))
gg <- gg + labs(x=NULL, y="Percent", title="Offenses that accused pre-trial inmates face by race",
                subtitle="Percent of race and total facing offenses. Based on rolling average of inmates per day. Note: Might not add up to 100 due to rounding.",
                caption="Department of Correction\nAndrew Ba Tran/TrendCT.org")
gg <- gg + theme_bw(base_family="Calibri")
gg <- gg + theme(panel.grid.major=element_blank())
gg <- gg + theme(panel.grid.minor=element_blank())
gg <- gg + theme(panel.border=element_blank())
gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(axis.text.x=element_blank())
gg <- gg + theme(text = element_text(size=20))
gg <- gg + theme(plot.title=element_text(face="bold", family="Lato Black", size=27, hjust=-5.2))
gg <- gg + theme(plot.subtitle=element_text(face="italic", hjust=.7, size=20,margin=margin(b=12)))
gg <- gg + theme(plot.caption=element_text(size=18, margin=margin(t=16), color="#7a7d7e"))
gg

ggsave(gg, file = "offenses_race_percent.png", width = 16, height = 18, type = "cairo-png")


# BOND.AMOUNT


mal_offense_race_bond <- update %>%
  filter(GENDER=="M") %>%
  group_by(DOWNLOAD.DATE, OFFENSE, RACE) %>%
  summarise(total=n(),avg.bond=mean(BOND.AMOUNT)) %>%
  select(DOWNLOAD.DATE, OFFENSE, total, RACE, avg.bond) %>%
  spread(RACE, avg.bond) %>%
  group_by(OFFENSE) %>%
  summarise(total = sum(total), amer.ind.avg=round(mean(`AMER IND`, na.rm=T),2), asian.avg=round(mean(ASIAN, na.rm=T),0), black.avg=round(mean(BLACK, na.rm=T),0), hispanic.avg=round(mean(HISPANIC, na.rm=T),0), white.avg=round(mean(WHITE, na.rm=T),0)) %>%
  arrange(-white.avg) %>%
  filter(total>20)

mal_offense_race_bond$what <- ifelse(((mal_offense_race_bond$white.avg < mal_offense_race_bond$black.avg) & (mal_offense_race_bond$white.avg < mal_offense_race_bond$hispanic.avg)), "white", "minority")

write.csv(mal_offense_race_bond, "mal_offense_race_bond.csv")


update %>%
  group_by(DOWNLOAD.DATE) %>%
  summarise(count=n()) %>%
  summarise(avg=mean(count))