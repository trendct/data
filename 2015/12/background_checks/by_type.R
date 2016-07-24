library(stringr)
library(lubridate)
library(dplyr)
library(tidyr)
library(trendct)

us <- read.csv("data/historicalpop-US.csv", stringsAsFactors=FALSE)
states <- read.csv("data/historicalpop-StatesT.csv", stringsAsFactors=FALSE)

nics <- read.csv("data/nics-firearm-background-checks.csv", stringsAsFactors=FALSE)


nics$state <- str_trim(nics$state)

ct <- subset(nics, state=="Connecticut")

nics$date <- as.Date(paste(nics$month,"-01",sep=""))

nics$year <- year(nics$date)

# Sum of other
nics <- mutate(nics, Other.All = totals - handgun - long_gun-permit)

annual <- data.frame(tapply(nics$totals, nics$year, sum))
annual$year <- rownames(annual)
rownames(annual) <- NULL
colnames(annual) <- c("Total", "Year")
annual <- annual[c("Year", "Total")]

# Annual per capita
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

# Long gun for US
long_gun <-data.frame(tapply(nics$long_gun, nics$date, sum, na.rm=TRUE))
long_gun$date <- rownames(long_gun)
rownames(long_gun) <- NULL
colnames(long_gun) <- c("Long_Gun", "Month")
long_gun<- long_gun[c("Month", "Long_Gun")]
long_gun$Month <- ymd(long_gun$Month)

long_gun <- long_gun[-1,]
month_only <- long_gun$Month

long_gun2 <- ts(long_gun[,2], frequency=12,start=c(1998,11))
long_gun2 <- seas(long_gun2)
plot(long_gun2)
long_gun2 <- as.data.frame(final(long_gun2))
long_gun2 <- cbind(month_only, long_gun2)
colnames(long_gun2) <- c("Month", "Long_Gun")


# Permit
permit <-data.frame(tapply(nics$permit, nics$date, sum, na.rm=TRUE))
permit$date <- rownames(permit)
rownames(permit) <- NULL
colnames(permit) <- c("permit", "Month")
permit<- permit[c("Month", "permit")]
permit$Month <- ymd(permit$Month)

permit <- permit[-1,]
month_only <- permit$Month

permit2 <- ts(permit[,2], frequency=12,start=c(1998,11))
permit2 <- seas(permit2)
plot(permit2)
permit2 <- as.data.frame(final(permit2))
permit2 <- cbind(month_only, permit2)
colnames(permit2) <- c("Month", "permit")

# Handgun
handgun <-data.frame(tapply(nics$handgun, nics$date, sum, na.rm=TRUE))
handgun$date <- rownames(handgun)
rownames(handgun) <- NULL
colnames(handgun) <- c("handgun", "Month")
handgun<- handgun[c("Month", "handgun")]
handgun$Month <- ymd(handgun$Month)

handgun <- handgun[-1,]
month_only <- handgun$Month

handgun2 <- ts(handgun[,2], frequency=12,start=c(1998,11))
handgun2 <- seas(handgun2)
plot(handgun2)
handgun2 <- as.data.frame(final(handgun2))
handgun2 <- cbind(month_only, handgun2)
colnames(handgun2) <- c("Month", "handgun")

# Other
other <-data.frame(tapply(nics$Other.All, nics$date, sum, na.rm=TRUE))
other$date <- rownames(other)
rownames(other) <- NULL
colnames(other) <- c("other", "Month")
other<- other[c("Month", "other")]
other$Month <- ymd(other$Month)

other <- other[-1,]
month_only <- other$Month

other2 <- ts(other[,2], frequency=12,start=c(1998,11))
other2 <- seas(other2)
plot(other2)
other2 <- as.data.frame(final(other2))
other2 <- cbind(month_only, other2)
colnames(other2) <- c("Month", "other")


us_by_type <- left_join(long_gun, permit)
us_by_type <- left_join(us_by_type, handgun)
us_by_type <- left_join(us_by_type, other)

write.csv(us_by_type, "us_by_type_raw.csv")

us_by_type_adjusted <- left_join(long_gun2, permit2)
us_by_type_adjusted <- left_join(us_by_type_adjusted, handgun2)
us_by_type_adjusted <- left_join(us_by_type_adjusted, other2)


us_by_type_adjusted$handgun <- round(us_by_type_adjusted$handgun, digits=0)
us_by_type_adjusted$Long_Gun <- round(us_by_type_adjusted$Long_Gun, digits=0)
us_by_type_adjusted$other <- round(us_by_type_adjusted$other, digits=0)
us_by_type_adjusted$permit <- round(us_by_type_adjusted$permit, digits=0)


write.csv(us_by_type_adjusted, "us_by_type_adjusted.csv")

trendchart(us_by_type_adjusted, headline = "Gun backgroundchecks in the US since 1998", subhead = "Adjusted for seasonality.", src = "Data from the FBI, cleaned by <a href='https://github.com/BuzzFeedNews/nics-firearm-background-checks/'>BuzzFeed News</a>",
           byline = "Andrew Ba Tran/TrendCT.org", type = "area", xTitle = "", yTitle = "",
           xSuffix = "", ySuffix = "", xPrefix = "", yPrefix = "", option = "")

# Monthly totals for CT

ct_nics <- subset(nics, state=="Connecticut")

# Long gun for CT
long_gun <-data.frame(tapply(ct_nics$long_gun, ct_nics$date, sum, na.rm=TRUE))
long_gun$date <- rownames(long_gun)
rownames(long_gun) <- NULL
colnames(long_gun) <- c("Long_Gun", "Month")
long_gun<- long_gun[c("Month", "Long_Gun")]
long_gun$Month <- ymd(long_gun$Month)

long_gun <- long_gun[-1,]
month_only <- long_gun$Month

long_gun2 <- ts(long_gun[,2], frequency=12,start=c(1998,11))
long_gun2 <- seas(long_gun2)
plot(long_gun2)
long_gun2 <- as.data.frame(final(long_gun2))
long_gun2 <- cbind(month_only, long_gun2)
colnames(long_gun2) <- c("Month", "Long_Gun")


# Permit
permit <-data.frame(tapply(ct_nics$permit, ct_nics$date, sum, na.rm=TRUE))
permit$date <- rownames(permit)
rownames(permit) <- NULL
colnames(permit) <- c("permit", "Month")
permit<- permit[c("Month", "permit")]
permit$Month <- ymd(permit$Month)

permit <- permit[-1,]
month_only <- permit$Month

permit2 <- ts(permit[,2], frequency=12,start=c(1998,11))
permit2 <- seas(permit2)
plot(permit2)
permit2 <- as.data.frame(final(permit2))
permit2 <- cbind(month_only, permit2)
colnames(permit2) <- c("Month", "permit")

# Handgun
handgun <-data.frame(tapply(ct_nics$handgun, ct_nics$date, sum, na.rm=TRUE))
handgun$date <- rownames(handgun)
rownames(handgun) <- NULL
colnames(handgun) <- c("handgun", "Month")
handgun<- handgun[c("Month", "handgun")]
handgun$Month <- ymd(handgun$Month)

handgun <- handgun[-1,]
month_only <- handgun$Month

handgun2 <- ts(handgun[,2], frequency=12,start=c(1998,11))
handgun2 <- seas(handgun2)
plot(handgun2)
handgun2 <- as.data.frame(final(handgun2))
handgun2 <- cbind(month_only, handgun2)
colnames(handgun2) <- c("Month", "handgun")

# Other
other <-data.frame(tapply(ct_nics$Other.All, ct_nics$date, sum, na.rm=TRUE))
other$date <- rownames(other)
rownames(other) <- NULL
colnames(other) <- c("other", "Month")
other<- other[c("Month", "other")]
other$Month <- ymd(other$Month)

other <- other[-1,]
month_only <- other$Month

other2 <- ts(other[,2], frequency=12,start=c(1998,11))
other2 <- seas(other2)
plot(other2)
other2 <- as.data.frame(final(other2))
other2 <- cbind(month_only, other2)
colnames(other2) <- c("Month", "other")


ct_by_type <- left_join(long_gun, permit)
ct_by_type <- left_join(ct_by_type, handgun)
ct_by_type <- left_join(ct_by_type, other)

write.csv(ct_by_type, "ct_by_type_raw.csv")

ct_by_type_adjusted <- left_join(long_gun2, permit2)
ct_by_type_adjusted <- left_join(ct_by_type_adjusted, handgun2)
ct_by_type_adjusted <- left_join(ct_by_type_adjusted, other2)

ct_by_type_adjusted$other[ct_by_type_adjusted$other<0] <-0
ct_by_type_adjusted$handgun[ct_by_type_adjusted$handgun<0] <-0

ct_by_type_adjusted$handgun <- round(ct_by_type_adjusted$handgun, digits=0)
ct_by_type_adjusted$Long_Gun <- round(ct_by_type_adjusted$Long_Gun, digits=0)
ct_by_type_adjusted$other <- round(ct_by_type_adjusted$other, digits=0)
ct_by_type_adjusted$permit <- round(ct_by_type_adjusted$permit, digits=0)

write.csv(us_by_type_adjusted, "ct_by_type_adjusted.csv")

trendchart(ct_by_type_adjusted, headline = "Gun backgroundchecks in Connecticut since 1998", subhead = "Adjusted for seasonality.", src = "Data from the FBI, cleaned by <a href='https://github.com/BuzzFeedNews/nics-firearm-background-checks/'>BuzzFeed News</a>",
           byline = "Andrew Ba Tran/TrendCT.org", type = "area", xTitle = "", yTitle = "",
           xSuffix = "", ySuffix = "", xPrefix = "", yPrefix = "", option = "")

