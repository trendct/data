# Lottery analysis

library(RCurl)
library(stringr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(trendct)

# gross sales https://data.ct.gov/Government/Lottery-Gross-Sales-Prizes-Agent-Commissions-And-T/sdpj-q7rw
# gross <- "https://data.ct.gov/api/views/sdpj-q7rw/rows.csv?accessType=DOWNLOAD"
# gross <- getURL(gross,.opts=list(ssl.verifypeer=FALSE))
# gross <- read.csv(textConnection(gross))
gross <- read.csv("lottery_gross.csv", stringsAsFactors=FALSE)

# transfers gaming https://data.ct.gov/Government/Transfers-to-the-State-General-fund-from-Gaming/j6mp-nsv2
transfers_gaming <- "https://data.ct.gov/api/views/j6mp-nsv2/rows.csv?accessType=DOWNLOAD"
transfers_gaming <- getURL(transfers_gaming,.opts=list(ssl.verifypeer=FALSE))
transfers_gaming <- read.csv(textConnection(transfers_gaming))

# sales https://data.ct.gov/Government/Lottery-Sales-By-Game/n9q9-yvd6
sales <- "https://data.ct.gov/api/views/n9q9-yvd6/rows.csv?accessType=DOWNLOAD"
sales <- getURL(sales,.opts=list(ssl.verifypeer=FALSE))
sales <- read.csv(textConnection(sales))

# licenses https://data.ct.gov/Business/State-Licenses-and-Credentials/ngch-56tr
licenses <- "https://data.ct.gov/api/views/ngch-56tr/rows.csv?accessType=DOWNLOAD"
licenses <- getURL(licenses,.opts=list(ssl.verifypeer=FALSE))
licenses <- read.csv(textConnection(licenses))

# GROSS analysis

gross$Gross.Sales <- gsub("\\$", "", gross$Gross.Sales)
gross$Gross.Sales <- gsub(",", "", gross$Gross.Sales)
gross$Gross.Sales <- as.numeric(as.character(gross$Gross.Sales))

gross$Prizes <- gsub("\\$", "", gross$Prizes)
gross$Prizes <- gsub(",", "", gross$Prizes)
gross$Prizes <- as.numeric(as.character(gross$Prizes))

gross$Agent.Commissions <- gsub("\\$", "", gross$Agent.Commissions)
gross$Agent.Commissions <- gsub(",", "", gross$Agent.Commissions)
gross$Agent.Commissions <- as.numeric(as.character(gross$Agent.Commissions))

gross$Transfers.to.the.General.Fund <- gsub("\\$", "", gross$Transfers.to.the.General.Fund)
gross$Transfers.to.the.General.Fund <- gsub(",", "", gross$Transfers.to.the.General.Fund)
gross$Transfers.to.the.General.Fund <- as.numeric(as.character(gross$Transfers.to.the.General.Fund))

gross$Percent <- gsub("\\%", "", gross$Percent)
gross$Percent <- as.numeric(as.character(gross$Percent))

gross$Total <- gross$Prizes+gross$Agent.Commissions+gross$Transfers.to.the.General.Fund

gross$Game <- as.factor(gross$Game)

gross_prizes <- gross %>%
  select(Fiscal.Year, Game, Prizes) %>%
  filter(Game!="Total")

ggplot(data=gross_prizes, aes(x=Fiscal.Year, y=Prizes, group=Game, colour=Game)) +
  geom_line() +
  geom_point()

gross_prizes <- gross %>%
  select(Fiscal.Year, Game, Prizes) %>%
  filter(Game!="Total")

gross_prizes_chart <- ggplot(data=gross_prizes, aes(x=Fiscal.Year, y=Prizes, group=Game, colour=Game)) +
  geom_line() +
  geom_point()

gross_ac <- gross %>%
  select(Fiscal.Year, Game, Agent.Commissions) %>%
  filter(Game!="Total")

gross_ac_chart <- ggplot(data=gross_ac, aes(x=Fiscal.Year, y=Agent.Commissions, group=Game, colour=Game)) +
  geom_line() +
  geom_point()

gross_t <- gross %>%
  select(Fiscal.Year, Game, Transfers.to.the.General.Fund) %>%
  filter(Game!="Total")

gross_t_chart <- ggplot(data=gross_t, aes(x=Fiscal.Year, y=Transfers.to.the.General.Fund, group=Game, colour=Game)) +
  geom_line() +
  geom_point()


# plot_grid(sp, bp, labels=c("A", "B"), ncol = 2, nrow = 1)

grid.arrange(gross_prizes_chart, gross_ac_chart, gross_t_chart, labels=c("A", "B"), ncol = 1, nrow = 3)

# Area chart

p <- ggplot(gross, aes(Fiscal.Year, Prizes))
p + geom_area(aes(colour=Game, fill=Game), position='stack')
p + geom_area(aes(colour = PR_Cat, fill= PR_Cat), position = 'stack')   

gross_prizes_trend <- spread(gross_prizes, Game, Prizes)

trendchart(gross_prizes_trend, headline = "Total lottery prizes by game in Connecticut", subhead = "", src = "CT Dept of Consumer Protection",
           byline = "TrendCT.org", type = "line", xTitle = "", yTitle = "",
           xSuffix = "", ySuffix = "", xPrefix = "", yPrefix = "$", option = "")

gross_t_trend <- spread(gross_t, Game, Transfers.to.the.General.Fund)

trendchart(gross_t_trend, headline = "Total lottery contribution to Connecticut's general fund", subhead = "", src = "CT Dept of Consumer Protection",
           byline = "TrendCT.org", type = "line", xTitle = "", yTitle = "",
           xSuffix = "", ySuffix = "", xPrefix = "", yPrefix = "$", option = "")


## SALES

sales <- sales[1:42,]
sales$Weekly <- gsub("\\$", "", sales$Weekly)
sales$Weekly <- as.numeric(as.character(sales$Weekly))
sales$Instant <- gsub("\\$", "", sales$Instant)
sales$Instant <- as.numeric(as.character(sales$Instant))
sales$Daily <- gsub("\\$", "", sales$Daily)
sales$Daily <- as.numeric(as.character(sales$Daily))
sales$Lotto <- gsub("\\$", "", sales$Lotto)
sales$Lotto <- as.numeric(as.character(sales$Lotto))
sales$Cash5 <- gsub("\\$", "", sales$Cash5)
sales$Cash5 <- as.numeric(as.character(sales$Cash5))
sales$Powerball <- gsub("\\$", "", sales$Powerball)
sales$Powerball <- as.numeric(as.character(sales$Powerball))
sales$Lucky.4.Life <- gsub("\\$", "", sales$Lucky.4.Life)
sales$Lucky.4.Life <- as.numeric(as.character(sales$Lucky.4.Life))
sales$Mega.Millions <- gsub("\\$", "", sales$Mega.Millions)
sales$Mega.Millions <- as.numeric(as.character(sales$Mega.Millions))
sales$CSD <- gsub("\\$", "", sales$CSD)
sales$CSD <- as.numeric(as.character(sales$CSD))
sales$Total <- gsub("\\$", "", sales$Total)
sales$Total <- as.numeric(as.character(sales$Total))

sales_total <- sales %>% select(Fiscal.Year, Total)

trendchart(sales_total, headline = "Total lottery sales in Connecticut", subhead = "", src = "CT Dept of Consumer Protection",
           byline = "TrendCT.org", type = "line", xTitle = "", yTitle = "",
           xSuffix = "", ySuffix = "", xPrefix = "", yPrefix = "$", option = "")

sales_subtotals <- sales %>% select(Fiscal.Year, Weekly, Instant, Daily, Lotto, Cash5, Powerball, Lucky.4.Life, Mega.Millions, CSD)

trendchart(sales_subtotals, headline = "Total lottery sales by type in Connecticut", subhead = "", src = "CT Dept of Consumer Protection",
           byline = "TrendCT.org", type = "line", xTitle = "", yTitle = "",
           xSuffix = "", ySuffix = "", xPrefix = "", yPrefix = "$", option = "")

# TOTAL FROM GAMING

transfers_gaming <- transfers_gaming[1:43,]
backup <- transfers_gaming
transfers_gaming$Lottery <- gsub("\\$", "", transfers_gaming$Lottery)
transfers_gaming$Lottery <- as.numeric(as.character(transfers_gaming$Lottery))
transfers_gaming$Plainfield.Greyhound <- gsub("\\$", "", transfers_gaming$Plainfield.Greyhound)
transfers_gaming$Plainfield.Greyhound <- as.numeric(as.character(transfers_gaming$Plainfield.Greyhound))
transfers_gaming$Bridgeport.Shoreline.Star <- gsub("\\$", "", transfers_gaming$Bridgeport.Shoreline.Star)
transfers_gaming$Bridgeport.Shoreline.Star <- as.numeric(as.character(transfers_gaming$Bridgeport.Shoreline.Star))
transfers_gaming$Hartford.Jai.Alai <- gsub("\\$", "", transfers_gaming$Hartford.Jai.Alai)
transfers_gaming$Hartford.Jai.Alai <- as.numeric(as.character(transfers_gaming$Hartford.Jai.Alai))
transfers_gaming$Milford.Jai.Alai <- gsub("\\$", "", transfers_gaming$Milford.Jai.Alai)
transfers_gaming$Milford.Jai.Alai <- as.numeric(as.character(transfers_gaming$Milford.Jai.Alai))
transfers_gaming$Pari.mutuel.Subtotal <- gsub("\\$", "", transfers_gaming$Pari.mutuel.Subtotal)
transfers_gaming$Pari.mutuel.Subtotal <- as.numeric(as.character(transfers_gaming$Pari.mutuel.Subtotal))
transfers_gaming$Off.Track.Betting <- gsub("\\$", "", transfers_gaming$Off.Track.Betting)
transfers_gaming$Off.Track.Betting <- as.numeric(as.character(transfers_gaming$Off.Track.Betting))
transfers_gaming$Charitable.Games <- gsub("\\$", "", transfers_gaming$Charitable.Games)
transfers_gaming$Charitable.Games <- as.numeric(as.character(transfers_gaming$Charitable.Games))
transfers_gaming$Foxwoods <- gsub("\\$", "", transfers_gaming$Foxwoods)
transfers_gaming$Foxwoods <- as.numeric(as.character(transfers_gaming$Foxwoods))
transfers_gaming$Mohegan.Sun <- gsub("\\$", "", transfers_gaming$Mohegan.Sun)
transfers_gaming$Mohegan.Sun <- as.numeric(as.character(transfers_gaming$Mohegan.Sun))
transfers_gaming$Casino.Subtotal <- gsub("\\$", "", transfers_gaming$Casino.Subtotal)
transfers_gaming$Casino.Subtotal <- as.numeric(as.character(transfers_gaming$Casino.Subtotal))
transfers_gaming$Grand.Total <- gsub("\\$", "", transfers_gaming$Grand.Total)
transfers_gaming$Grand.Total <- gsub("\\,", "", transfers_gaming$Grand.Total)
transfers_gaming$Grand.Total <- gsub("\\* ", "", transfers_gaming$Grand.Total)

transfers_gaming$Grand.Total <- as.numeric(as.character(transfers_gaming$Grand.Total))

transfers_total <- transfers_gaming %>% select(FYE.6.30, Lottery, Pari.mutuel.Subtotal, Off.Track.Betting, Charitable.Games, Casino.Subtotal, Grand.Total)

options(scipen=999)

transfers_total_chart <- transfers_total %>% select(FYE.6.30, Lottery, Pari.mutuel.Subtotal, Off.Track.Betting, Charitable.Games, Casino.Subtotal)

trendchart(transfers_total_chart, headline = "Total gaming revenue by type in Connecticut", subhead = "", src = "CT Dept of Consumer Protection",
           byline = "TrendCT.org", type = "area", xTitle = "", yTitle = "",
           xSuffix = "", ySuffix = "", xPrefix = "", yPrefix = "$", option = "")