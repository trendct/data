#sub <- read.csv("subsidies.csv", stringsAsFactors=FALSE)
sub <- read.csv("subsidies-csv.csv", stringsAsFactors=FALSE)

sub$Subsidy.Value <- gsub("\\$", "", sub$Subsidy.Value) # remove the $
sub$Subsidy.Value <- gsub("\\,", "", sub$Subsidy.Value) # remove the ,
sub$Subsidy.Value <- as.numeric(sub$Subsidy.Value)

sub$Subsidy.Value.Adjusted.For.Megadeal <- gsub("\\$", "", sub$Subsidy.Value.Adjusted.For.Megadeal) # remove the $
sub$Subsidy.Value.Adjusted.For.Megadeal <- gsub("\\,", "", sub$Subsidy.Value.Adjusted.For.Megadeal) # remove the ,
sub$Subsidy.Value.Adjusted.For.Megadeal <- as.numeric(sub$Subsidy.Value.Adjusted.For.Megadeal)

sub$Megadeal.Contribution <- gsub("\\$", "", sub$Megadeal.Contribution) # remove the $
sub$Megadeal.Contribution <- gsub("\\,", "", sub$Megadeal.Contribution) # remove the ,
sub$Megadeal.Contribution <- as.numeric(sub$Megadeal.Contribution)

sub$Loan.Value <- gsub("\\$", "", sub$Loan.Value) # remove the $
sub$Loan.Value <- gsub("\\,", "", sub$Loan.Value) # remove the ,
sub$Loan.Value <- as.numeric(sub$Loan.Value)

sub$Company <- gsub("Starwood Hotel & Resorts","Starwood Hotels and Resorts", sub$Company)
sub$Company <- gsub("Starwood Hotels & Resorts Worldwide, Inc.","Starwood Hotels and Resorts", sub$Company)



sub$Loan.Value <- gsub("\\$", "", sub$Loan.Value) # remove the $
sub$Loan.Value <- gsub("\\,", "", sub$Loan.Value) # remove the ,
sub$Loan.Value <- as.numeric(sub$Loan.Value)

sub$City <- gsub("City of ", "", sub$City)
sub$City <- gsub("Town of ", "", sub$City)
sub$City <- gsub("Borough of ", "", sub$City)
sub$City <- gsub("Port Chester ", "", sub$City)

## data analysis

library(dplyr)

by_year <- sub %>%
  group_by(Year) %>%
  summarise(Total=sum(Subsidy.Value, na.rm=TRUE), Count=n()) %>%
  mutate(Per_Year=round((Total/Count),2))

by_town <- sub %>%
  group_by(City) %>%
  summarise(Total=sum(Subsidy.Value.Adjusted.For.Megadeal, na.rm=TRUE), Count=n()) %>%
  arrange(-Total)

by_town_mega <- sub %>%
  group_by(City) %>%
  summarise(Total=sum(Subsidy.Value.Adjusted.For.Megadeal, na.rm=TRUE), Count=n()) %>%
  arrange(-Total)

company <- sub %>%
  group_by(Company) %>%
  summarise(Total=sum(Subsidy.Value, na.rm=TRUE), Count=n()) %>%
  arrange(-Total) %>%
  head(10)

company <- data.frame(company)
company <- company[c("Company", "Total")]

trendchart(company, headline = "Companies with most government subsidies in CT", subhead = "Since 1999.", src = "subsidytracker.goodjobsfirst.org",
           byline = "Andrew Ba Tran/TrendCT.org", type = "bar", xTitle = "", yTitle = "Total subsidies",
           xSuffix = "", ySuffix = "", xPrefix = "", yPrefix = "$", option = "")


parent_company <- sub %>%
  group_by(Parent.Company) %>%
  summarise(Total=sum(Subsidy.Value.Adjusted.For.Megadeal, na.rm=TRUE), Count=n()) %>%
  arrange(-Total) %>%
  head(11)

parent <- parent_company[-1,]
parent <- data.frame(parent)
parent <- parent[c("Parent.Company", "Total")]

trendchart(parent, headline = "Parent companies with most government subsidies in CT", subhead = "Since 1999.", src = "subsidytracker.goodjobsfirst.org",
           byline = "Andrew Ba Tran/TrendCT.org", type = "bar", xTitle = "", yTitle = "Total subsidies",
           xSuffix = "", ySuffix = "", xPrefix = "", yPrefix = "$", option = "")

ge <- subset(sub, Parent.Company=="General Electric")
ge2 <- subset(sub, Company=="GE Capital Corporation")

company_stamford <- sub %>%
  filter(City=="Stamford") %>%
  group_by(Company) %>%
  summarise(Total=sum(Subsidy.Value.Adjusted.For.Megadeal, na.rm=TRUE), Count=n()) %>%
  arrange(-Total) %>%
  head(11)

company_stamford <- company_stamford[c("Company", "Total")]
company_stamford <- data.frame(company_stamford)

trendchart(company_stamford , headline = "Government subsidies to Stamford companies", subhead = "Since 1999.", src = "subsidytracker.goodjobsfirst.org",
           byline = "Andrew Ba Tran/TrendCT.org", type = "bar", xTitle = "", yTitle = "Total subsidies",
           xSuffix = "", ySuffix = "", xPrefix = "", yPrefix = "$", option = "")

## charting

plot(by_year, type="l", main="Total subsidies to businesses in Connecticut", ylab="Subsidy $", xlab="Year")

plot(by_year$Year, by_year$Per_Year, type="l", main="Total average subsidies to businesses in Connecticut", ylab="Subsidy $", xlab="Year")

plot(by_year$Year, by_year$Count, type="l", main="Number of subsidies to businesses in Connecticut", ylab="Subsidies", xlab="Year")

# Towns or not

table(sub$City)

library(ggplot2)

# source

table(sub$Subsidy.Source)

# clean up town names
library(ctnamecleaner)
by_townies <- ctnamecleaner(City, by_town_mega)

by_townies2 <- subset(by_townies, !is.na(by_townies$real.town.name))
by_townies3 <- subset(by_townies, is.na(by_townies$real.town.name))

by_townies2 <- by_townies2 %>%
  group_by(real.town.name) %>%
  summarise(Subsidies=sum(Total), Count=sum(Count)) %>%
  arrange(-Subsidies)
colnames(by_townies2) <- c("town", "subsidies", "count")

# map it
mean(is.na(by_townies$real.town.name))

library(trendct)

trendmap(by_townies2, headline="Subsidies to businesses in Connecticut by town", subhead="Since 1999.",
         src="subsidytracker.goodjobsfirst.org", byline="TrendCT.org", url_append="date", shape="towns", color="yellow-red")


#

table(sub$Type.of.Subsidy)
table(sub$Awarding.Agency)
table(sub$Program.Name) # This one

tapply(sub$Subsidy.Value.Adjusted.For.Megadeal, sub$Type.of.Subsidy, sum)

write.csv(tapply(sub$Subsidy.Value.Adjusted.For.Megadeal, sub$Program.Name, sum), "program.csv")


annual <- data.frame(tapply(sub$Subsidy.Value.Adjusted.For.Megadeal, sub$Year, sum))
annual$Year <- rownames(annual)
colnames(annual) <- c("Total", "Year")
annual <- annual[c("Year", "Total")]

rownames(annual) <- NULL
trendchart(annual, headline = "Total subsidies by year", subhead = "", src = "subsidytracker.goodjobsfirst.org",
           byline = "Andrew Ba Tran/TrendCT.org", type = "column", xTitle = "", yTitle = "Total subsidies amount",
           xSuffix = "", ySuffix = "", xPrefix = "", yPrefix = "$", option = "")

stam <- data.frame(tapply(stamford$Subsidy.Value.Adjusted.For.Megadeal, stamford$Year, sum))
stam$year <- rownames(stam)
colnames(stam) <- c("subsidy", "year")
stam <- stam[c("year", "subsidy")]


company_water <- sub %>%
  dplyr::filter(City=="Waterbury") %>%
  group_by(Year) %>%
  summarise(Total=sum(Subsidy.Value.Adjusted.For.Megadeal, na.rm=TRUE)) %>%
  arrange(Year)

company_water <- data.frame(company_water)
trendchart(company_water, headline = "Subsidies to Waterbury companies by year", subhead = "", src = "",
                      byline = "TrendCT.org", type = "column", xTitle = "", yTitle = "Subsidies",
                       xSuffix = "", ySuffix = "", xPrefix = "", yPrefix = "$", option = "")

