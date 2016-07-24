us <- read.csv("data/historicalpop-US.csv", stringsAsFactors=FALSE)
states <- read.csv("data/historicalpop-StatesT.csv", stringsAsFactors=FALSE)

nics <- read.csv("data/nics-firearm-background-checks.csv", stringsAsFactors=FALSE)

library(stringr)

nics$state <- str_trim(nics$state)

ct <- subset(nics, state=="Connecticut")

library(lubridate)
library(dplyr)
nics$date <- as.Date(paste(nics$month,"-01",sep=""))

# Annual per capita
nics$year <- year(nics$date)

annual <- data.frame(tapply(nics$totals, nics$year, sum))

annual$year <- rownames(annual)
rownames(annual) <- NULL
colnames(annual) <- c("Total", "Year")
annual <- annual[c("Year", "Total")]

annual <- merge(annual, us)

annual$percapita <- round((annual$Total/annual$US)*1000,2)

# Monthly totals for the US

monthly <-data.frame(tapply(nics$totals, nics$date, sum))
monthly$date <- rownames(monthly)
rownames(monthly) <- NULL
colnames(monthly) <- c("Total", "Month")
monthly<- monthly[c("Month", "Total")]
monthly$Month <- ymd(monthly$Month)
monthly$Year <- year(monthly$Month)

monthly <- left_join(monthly, us)
monthly$percapita <- round((monthly$Total/monthly$US)*1000,2)

by_month <- monthly[c("Month", "percapita")]
colnames(by_month) <- c("Month", "US")

# Monthly totals for CT

ct <- subset(nics, state=="Connecticut")
ctpop  <- read.csv("historicalpop-CT.csv", stringsAsFactors=FALSE)
ct_monthly<-data.frame(tapply(ct$totals, ct$date, sum))
ct_monthly$date <- rownames(ct_monthly)
rownames(ct_monthly) <- NULL
colnames(ct_monthly) <- c("Total", "Month")
ct_monthly<- ct_monthly[c("Month", "Total")]
ct_monthly$Month <- ymd(ct_monthly$Month)
ct_monthly$Year <- year(ct_monthly$Month)

ct_monthly <- left_join(ct_monthly, ctpop)
ct_monthly$percapita <- round((ct_monthly$Total/ct_monthly$CT)*1000,2)

ct_month <- ct_monthly[c("Month", "percapita")]
colnames(ct_month) <- c("Month", "CT")

by_month <- left_join(by_month, ct_month)

library(dplyr)
library(tidyr)


# Monthly for all states now
# Restructure data.frame 

totals_only <- nics[c("month", "state", "totals")] 

spreaded_totals <- spread(totals_only, state, totals)
spreaded_totals <- spreaded_totals[,colSums(is.na(spreaded_totals))<nrow(spreaded_totals)]
spreaded_totals$year <- substr(spreaded_totals$month, 1, 4)
spreaded_totals$year <- as.numeric(spreaded_totals$year)

# 1. Create a loop going through a list of all the state names
# 2. Subset by each state: Annual population
# 3. Subset by each state: NICS monthly background checks
# 4. Join pop to subsetted background checks
# 5. Create a new column based on per capita
# 6. Add it to a new overall dataframe

# US per capita

monthly <-data.frame(tapply(nics$totals, nics$date, sum))
monthly$date <- rownames(monthly)
rownames(monthly) <- NULL
colnames(monthly) <- c("Total", "Month")
monthly<- monthly[c("Month", "Total")]
monthly$Month <- ymd(monthly$Month)
monthly$Year <- year(monthly$Month)

monthly <- left_join(monthly, us)
monthly$percapita <- round((monthly$Total/monthly$US)*1000,2)

by_month <- monthly[c("Month", "percapita")]
colnames(by_month) <- c("Month", "US")

by_month$Month <- substr(by_month$Month, 1, 7)


states_num <- ncol(states)
states_list <- 2:states_num

for (i in states_list) {
  state_name <- colnames(states[i])
  temp_df <- states[c("Year", state_name)]
  colnames(temp_df) <- c("year", "Population")
  state_name <- gsub("\\.", " ", state_name)
  test_this <- grepl(state_name, colnames(spreaded_totals))
  test_sum <- sum(test_this)
  if (test_sum>0) {
    # looking just at totals
    nics_df <- spreaded_totals[c("month", "year", state_name)]
    temp_df <- left_join(temp_df, nics_df)
    temp_df$per_capita <- round((temp_df[,4]/temp_df[,2])*1000,2)
    temp_df <- temp_df[c("month", "per_capita")]
    colnames(temp_df) <- c("Month", state_name)
    
    by_month <- left_join(by_month, temp_df)
  }
}

## Charting out with GGPlot

test_df <- by_month

test_df$Month <- factor(test_df$Month)

test_df <- gather(test_df, "State", "Per.Capita", 2:53)

ggplot(data=test_df, aes(x=test_df$Month,y=Per.Capita, group=State)) +
  geom_line() +
  ggtitle("Background checks by state") +
  labs(x="Month", y="Per 1,000 residents")

ggplot(data=test_df, aes(x=Month,y=Per.Capita)) +
  geom_bar(stat="identity") +
  facet_wrap(~State) +
  ggtitle("Background checks by state") +
  theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=20, hjust=0, color="#555555")) +
  theme(axis.text.x = element_text(angle=90)) 