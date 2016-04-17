## This script analyzes the data pulled via ad_extractor.R and parsed with parser_totals.R
# Note: These documents are very tricky. 
# Some contracts were revised multiple times and the original ones were not deleted
# This script takes great care to toss out older versions of contracts
# If it missed any and double counted any instances, please let me know abtran@trendct.org

library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(tidyr)

# Bringing in the lines.csv file and cleaning up the numbers columns to be recongized as numbers

totals <- read.csv("data/ad_buys.csv", stringsAsFactors=FALSE)
totals$gross.amount <- gsub("\\$", "", totals$gross.amount)
totals$gross.amount <- gsub(",", "", totals$gross.amount)
totals$gross.amount <- as.numeric(totals$gross.amount)

totals$net.amount <- gsub("\\$", "", totals$net.amount)
totals$net.amount <- gsub(",", "", totals$net.amount)
totals$net.amount <- as.numeric(totals$net.amount)

totals$agency.commission <- totals$gross.amount - totals$net.amount

# making the dates recongized by R

totals$time.period.start <- mdy(totals$time.period.start)
totals$time.period.end <- mdy(totals$time.period.end)

totals$sheet <- gsub("spreadsheets/", "", totals$sheet)
totals$sheet <- gsub(".xlsx", "", totals$sheet)

# joining the data with the original dataframe from ad_extractor.R
# this will add more details like station and advertiser

presi_table <- read.csv("data/ads_dataframe.csv")
totals <- left_join(totals, presi_table, by="sheet")

# create a new column identifying the candidate based on what group bought the ad
totals$candidate <- ""

for (i in 1:nrow(totals)) {
  info <- str_to_upper(totals$`Advertiser/File Info`[i])
  if (grepl("BERNIE", info)) {
    totals$candidate[i] <- "Bernie Sanders"
  } else if (grepl("HILLARY", info)) {
    totals$candidate[i] <- "Hillary Clinton"
  } else {
    totals$candidate[i] <- "Other"
    
  }
}

# cleaning up the column names to eliminate spaces
colnames(totals) <- c("time.period.start", "time.period.end", "spots", "gross.amount", "agency.commission", "net.amount", "sheet", "tv.station", "market", "date", "type", "status", "spots2", "cost2", "advertiser", "link", "doc", "candidate")
totals <- totals[c("candidate",  "tv.station", "market", "date", "time.period.start", "time.period.end", "spots", "gross.amount", "agency.commission", "net.amount", "sheet", "advertiser", "link", "doc")]

# create a new column identifying the group
totals$group <- ""

for (i in 1:nrow(totals)) {
  info <- str_to_upper(totals$advertiser[i])
  if (grepl("HILLARY 2016", info)) {
    totals$group[i] <- "Hillary 2016"
  } else if (grepl("BERNIE SANDERS FOR PRESIDENT", info)) {
    totals$group[i] <- "Bernie Sanders for President"
  } else if (grepl("HILLARY CLINTON", info)) {
    totals$group[i] <- "Hillary Clinton" 
  } else if (grepl("BERNIE SANDERS", info)) {
    totals$group[i] <- "Bernie Sanders" 
  }  else if (grepl("BERNIE 2016", info)) {
    totals$group[i] <- "Bernie 2016" 
  } else if (grepl("HILLARY FOR AMERICA", info)) {
    totals$group[i] <- "Hillary for America" 
  }
}


# Picking out the most-recent contracts, discarding the others
check <- totals %>%
  group_by(tv.station, time.period.start, time.period.end, candidate) %>%
  arrange(-spots)

check2 <- totals
check2$mega <- paste(check2$tv.station, check2$time.period.start,check2$time.period.end,check2$candidate)

check3 <- check2 %>%
  group_by(mega) %>%
  top_n(n = 1, wt=spots)

# Making sure there are no duplicate rows
check3 <- unique(check3)


# Now check to see if there are any overlapping time periods
check3$interval <- interval(check3$time.period.start, check3$time.period.end)

check3$overlaps <- ""
rows <- nrow(check3)-1
for (i in 1:rows) {
  if ((check3$tv.station[i]==check3$tv.station[i+1]) && (check3$group[i]==check3$group[i+1])) {
    if (int_overlaps(check3$interval[i], check3$interval[i+1])) {
      check3$overlaps[i] <- "yes"
      check3$overlaps[i+1] <- "yes" } 
    else {
      check3$overlaps[i] <- "no"
    }
  } else {
    check3$overlaps[i] <- "no"
  }
}

# Alright, just a handful of overlapping contact periods
# It looks like there was one Hillary contract that was revised to expand the time period and increase ad buys
# Will take out the older contract

check3 <- subset(check3, sheet!="04_11_16WFSB14603982219469_38")


# just some preliminary analysis
tapply(check3$gross.amount, check3$candidate, sum)
tapply(check3$spots, check3$candidate, sum)
tapply(check3$spots, check3$tv.station, sum)

tapply(check3$spots, check3$time.period.start, sum)
tapply(check3$spots, check3$group, sum)
tapply(check3$gross.amount, check3$group, sum)
tapply(check3$gross.amount, check3$date, sum)

## Money spent by FCC filing date
sum_date <- check3 %>%
  group_by(date, candidate, tv.station) %>%
  summarise(total=sum(gross.amount))

sum_date_only <- check3 %>%
  group_by(date, candidate) %>%
  summarise(total=sum(gross.amount))

ggplot(data=sum_date_only, aes(x=date, y=total, fill=candidate)) +
  geom_bar(stat="identity", position=position_dodge())

sum_for_trendchart <- spread(sum_date_only, candidate, total)

sum_for_trendchart <- data.frame(sum_for_trendchart)

## All future mentions of the function trendchart won't work unless you have a specific package
# It's proprietary and for trendct.org data visualizations

trendchart(sum_for_trendchart, headline = "Total spent on ad buys by day", subhead = "Based on station FCC filing date.", src = "Federal Communications Commission, politicaladsleuth.com",
           byline = "Andrew Ba Tran/TrendCT.org", type = "column", xTitle = "", yTitle = "",
           xSuffix = "", ySuffix = "", xPrefix = "", yPrefix = "$", option = "")


ggplot(data=sum_date, aes(x=date, y=total, fill=candidate)) +
  geom_bar(stat="identity", position=position_dodge())+ facet_wrap(~tv.station)

## Money spent by start period

sum_period_start <- check3 %>%
  group_by(time.period.start, candidate, tv.station) %>%
  summarise(total=sum(gross.amount))

ggplot(data=sum_period_start, aes(x=time.period.start, y=total, fill=candidate)) +
  geom_bar(stat="identity", position=position_dodge())

ggplot(data=sum_period_start, aes(x=time.period.start, y=total, fill=candidate)) +
  geom_bar(stat="identity", position=position_dodge())+ facet_wrap(~tv.station)

## Money spent by group

## Money spent by start period

sum_group <- check3 %>%
  group_by(group, candidate) %>%
  summarise(total=sum(gross.amount))

ggplot(data=sum_group, aes(x=group, y=total, fill=candidate)) +
  geom_bar(stat="identity", position=position_dodge())

ggplot(data=sum_period_start, aes(x=time.period.start, y=total, fill=candidate)) +
  geom_bar(stat="identity", position=position_dodge())+ facet_wrap(~tv.station)

## Spots by FCC filing date

spots_date <- check3 %>%
  group_by(date, candidate, tv.station) %>%
  summarise(spots=sum(spots))

spots_date2 <- check3 %>%
  group_by(date, candidate) %>%
  summarise(spots=sum(spots))

ggplot(data=spots_date2, aes(x=date, y=spots, fill=candidate)) +
  geom_bar(stat="identity", position=position_dodge())


spots_for_trendchart <- spread(spots_date2, candidate, spots)

spots_for_trendchart <- data.frame(spots_for_trendchart)

trendchart(spots_for_trendchart, headline = "Total ads purchased by day", subhead = "Based on station FCC filing date.", src = "Federal Communications Commission, politicaladsleuth.com",
           byline = "Andrew Ba Tran/TrendCT.org", type = "column", xTitle = "", yTitle = "",
           xSuffix = "", ySuffix = "", xPrefix = "", yPrefix = "", option = "")


ggplot(data=spots_date, aes(x=date, y=spots, fill=candidate)) +
  geom_bar(stat="identity", position=position_dodge())+ facet_wrap(~tv.station)


## spots by start period

spot_period_start <- check3 %>%
  group_by(time.period.start, candidate, tv.station) %>%
  summarise(spots=sum(spots))

ggplot(data=spot_period_start, aes(x=time.period.start, y=spots, fill=candidate)) +
  geom_bar(stat="identity", position=position_dodge()) 

ggplot(data=spot_period_start, aes(x=time.period.start, y=spots, fill=candidate)) +
  geom_bar(stat="identity", position=position_dodge()) + facet_wrap(~tv.station) + theme_minimal()

# Spots and Candidates and Station

scs <- check3 %>%
  group_by(candidate, tv.station) %>%
  summarise(spots=sum(spots))

ggplot(data=scs, aes(x=tv.station, y=spots, fill=candidate)) +
  geom_bar(stat="identity", position=position_dodge()) + theme_minimal()


spots_for_trendchart2 <- spread(scs, candidate, spots)

spots_for_trendchart2 <- data.frame(spots_for_trendchart2)

trendchart(spots_for_trendchart2, headline = "Total ads purchased by station", subhead = "Based on station FCC filing date.", src = "Federal Communications Commission, politicaladsleuth.com",
           byline = "Andrew Ba Tran/TrendCT.org", type = "column", xTitle = "", yTitle = "",
           xSuffix = "", ySuffix = "", xPrefix = "", yPrefix = "", option = "")


## for datatables

for_dt <- check3
for_dt$link <- paste0("<a href='", for_dt$doc, "' target='_blank'>link</a>")
for_dt <- for_dt[c("candidate", "group", "tv.station", "date", "spots", "net.amount", "agency.commission", "gross.amount", "time.period.start", "time.period.end", "link")]
for_dt <- data.frame(for_dt)
fancytable(for_dt, headline = "Presidential candidate ad buys in CT", subhead = "As of April 15, 2016.", height = 400,
           paging = "false", sourceline = "FCC", byline = "Andrew Ba Tran/TrendCT.org", col = 3,
           desc_asc = "desc")


write.csv(check3, "data/ct_primary_ads_041516.csv")
