library(dplyr)
library(trendct)

turnout <- read.csv("data/turnout.csv", stringsAsFactors=FALSE)
turnout$eligible.voters <- gsub(",", "", turnout$eligible.voters)
turnout$eligible.voters <- as.numeric(turnout$eligible.voters)
turnout$voted <- gsub(",", "", turnout$voted)
turnout$voted <- as.numeric(turnout$voted)

turnout$absentee <- gsub(",", "", turnout$absentee)
turnout$absentee <- as.numeric(turnout$absentee)

turnout$absentee.counted <- gsub(",", "", turnout$absentee.counted)
turnout$absentee.counted <- as.numeric(turnout$absentee.counted)

turnout$edr <- gsub(",", "", turnout$edr.counted)
turnout$edr <- as.numeric(turnout$edr)

turnout$turnout <- gsub("%", "", turnout$turnout)
turnout$turnout <- as.numeric(turnout$turnout)

turnout <- subset(turnout, Town!="Total")

turnout <- turnout[c("Town", "turnout", "voted")]

trendmap(turnout, headline="Republican voter turnout in the 2016 primary elections", subhead="Unofficial data from the state as results may still be coming in.",
         src="Office of the Secretary of the State", byline="Andrew Ba Tran/TrendCT.org", url_append="date", shape="towns", color="yellow-red")

## democrats


turnout <- read.csv("data/dem_turnout.csv", stringsAsFactors=FALSE)
turnout$eligible.voters <- gsub(",", "", turnout$eligible.voters)
turnout$eligible.voters <- as.numeric(turnout$eligible.voters)
turnout$voted <- gsub(",", "", turnout$voted)
turnout$voted <- as.numeric(turnout$voted)

turnout$absentee <- gsub(",", "", turnout$absentee)
turnout$absentee <- as.numeric(turnout$absentee)

turnout$absentee.counted <- gsub(",", "", turnout$absentee.counted)
turnout$absentee.counted <- as.numeric(turnout$absentee.counted)

turnout$edr <- gsub(",", "", turnout$edr.counted)
turnout$edr <- as.numeric(turnout$edr)

turnout$turnout <- gsub("%", "", turnout$turnout)
turnout$turnout <- as.numeric(turnout$turnout)

turnout <- subset(turnout, Town!="Total")

turnout <- turnout[c("Town", "turnout", "voted")]

trendmap(turnout, headline="Democratic voter turnout in the 2016 primary elections", subhead="Unofficial data from the state as results may still be coming in.",
         src="Office of the Secretary of the State", byline="Andrew Ba Tran/TrendCT.org", url_append="date", shape="towns", color="blues")