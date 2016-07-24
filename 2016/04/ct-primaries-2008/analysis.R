library(dplyr)
library(trendct)
library(tidyr)
dems <- read.csv("ct2008demsprimary.csv", stringsAsFactors=FALSE)

reps <- read.csv("ct2008repsprimary.csv", stringsAsFactors=FALSE)


dems$Turnout <- gsub("%", "", dems$Turnout)
dems$Turnout <- as.numeric(dems$Turnout)
party <- subset(dems, Turnout>50)

reps$Turnout <- gsub("%", "", reps$Turnout)
reps$Turnout <- as.numeric(reps$Turnout)

party <- subset(reps, Turnout>50)
turnout <- dems[c("Town", "Turnout")]
colnames(turnout) <- c("Town", "Democratic")

rep_turnout <- reps[c("Town", "Turnout")]
colnames(rep_turnout) <- c("Town", "Republican")

turnout <- left_join(turnout, rep_turnout)

turnout$Democratic <- gsub("%", "", turnout$Democratic)
turnout$Republican <- gsub("%", "", turnout$Republican)

turnout$Democratic <- as.numeric(turnout$Democratic)
turnout$Republican <- as.numeric(turnout$Republican)

trendmap(turnout, headline="2008 Presidential primaries voter turnout", subhead="Out of percent party registrations.",
         src="Office of the Secretary of the State", byline="Andrew Ba Tran/TrendCT.org", url_append="date", shape="towns", color="yellow-red")


dem_vote <- dems[c("Town", "Obama", "Clinton", "Voted")]
dem_vote$percent_obama <- round(dem_vote$Obama / dem_vote$Voted*100,2)
dem_vote$percent_clinton <- round(dem_vote$Clinton / dem_vote$Voted*100,2)

dem_vote <- dem_vote[c("Town", "percent_obama", "percent_clinton")]
colnames(dem_vote) <- c("Town", "Obama", "Clinton")

trendmap(dem_vote, headline="Percent of Obama and Clinton votes by town", subhead="Out of those who voted.",
         src="Office of the Secretary of the State", byline="Andrew Ba Tran/TrendCT.org", url_append="date", shape="towns", color="blues")






rep_vote <- reps[c("Town", "Romney", "McCain", "Huckabee", "Voted")]
rep_vote$percent_romney <- round(rep_vote$Romney / rep_vote$Voted*100,2)
rep_vote$percent_mccain <- round(rep_vote$McCain / rep_vote$Voted*100,2)
rep_vote$percent_huckabee <- round(rep_vote$Huckabee / rep_vote$Voted*100,2)
rep_vote <- rep_vote[c("Town", "percent_romney", "percent_mccain", "percent_huckabee")]

colnames(rep_vote) <- c("Town", "Romney", "McCain", "Huckabee")

trendmap(rep_vote, headline="Percent of Republican primary votes by candidate", subhead="Out of those who voted.",
         src="Office of the Secretary of the State", byline="Andrew Ba Tran/TrendCT.org", url_append="date", shape="towns", color="yellow-red")

romney <- subset(rep_vote, (Romney > McCain) & (Romney > Huckabee))
mccain <- subset(rep_vote, (McCain > Romney) & (McCain > Huckabee))


voters <- read.csv("ACTIVE_VOTER_BY_TOWN_PARTY_040716.csv", stringsAsFactors=FALSE)
voters$Affiliation <- gsub(" .*", "", voters$Affiliation)

voters <- spread(voters, Affiliation, Registered)
voters <- data.frame(voters)

colnames(voters) <- c("Town", "dem2016", "rep2016", "un2016")

voters$un2016 <- NULL

turnout2d <- dems[c("Town", "Active.Democratic.Enrollment.List")]
turnout2r <- reps[c("Town", "Active.Republican.Enrollment.List")]

turnout3 <- left_join(turnout2d, turnout2r)
turnout3 <- left_join(turnout3, voters)

turnout$dem2016 <- turnout3$dem2016-turnout3$Active.Democratic.Enrollment.List
turnout$rep2016 <- turnout3$rep2016-turnout3$Active.Republican.Enrollment.List

turnout_diff <- turnout[c("Town", "dem2016", "rep2016")]
colnames(turnout_diff) <- c("Town", "Democrats", "Republicans")
trendmap(turnout_diff, headline="Where party registrations changed since 2008", subhead="",
         src="Office of the Secretary of the State", byline="Andrew Ba Tran/TrendCT.org", url_append="date", shape="towns", color="purple-green")
