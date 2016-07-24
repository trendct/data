## If you don't have the following packages installed, uncomment and run the line below
#devtools::install_github("ropensci/plotly")
## install.packages("plotly")
## install.packages("ggplot2")
## install.packages("tidyr")
library(plotly)
library(ggplot2)
library(tidyr)

source("prepping_data.R")
source("urban_rural_mixed.R")

## Box plot it
town_box <- town_count[c("NAME10", "Type", "perc_urban")]
colnames(town_box) <- c("id", "type", "percent_urban")
box_towns <- data.frame(drugs_total)
box_towns <- left_join(box_towns, town_box)

p <- ggplot(box_towns, aes(factor(type), Total))
p + geom_boxplot() + ggtitle("Total heroin deaths by town type")

p <- ggplot(box_towns, aes(factor(type), percapita))
p + geom_boxplot() + ggtitle("Heroin death rate by town type")

dt12 <- hdrugs_total_2012
dt12 <- subset(dt12, !is.na(percapita))
dt12 <- dt12[c("id", "Total", "percapita")]
colnames(dt12) <- c("id", "Total2012", "percapita2012")
box_towns <- left_join(box_towns, dt12)

dt13 <- hdrugs_total_2013
dt13 <- subset(dt13, !is.na(percapita))
dt13 <- dt13[c("id", "Total", "percapita")]
colnames(dt13) <- c("id", "Total2013", "percapita2013")
box_towns <- left_join(box_towns, dt13)


dt14 <- hdrugs_total_2014
dt14 <- subset(dt14, !is.na(percapita))
dt14 <- dt14[c("id", "Total", "percapita")]
colnames(dt14) <- c("id", "Total2014", "percapita2014")
box_towns <- left_join(box_towns, dt14)

dt15 <- hdrugs_total_2015
dt15 <- subset(dt15, !is.na(percapita))
dt15 <- dt15[c("id", "Total", "percapita")]
colnames(dt15) <- c("id", "Total2015", "percapita2015")
box_towns <- left_join(box_towns, dt15)


p <- ggplot(box_towns, aes(factor(type), percapita2012))
p + geom_boxplot()

p <- ggplot(box_towns, aes(factor(type), percapita2013))
p + geom_boxplot()

p <- ggplot(box_towns, aes(factor(type), percapita2014))
p + geom_boxplot()

p <- ggplot(box_towns, aes(factor(type), percapita2015))
p + geom_boxplot()

box_towns_rate <- box_towns[c("id", "percapita2012", "percapita2013", "percapita2014", "percapita2015")]
box_towns_rate <- gather(box_towns_rate, "year", "percapita", 2:5)
box_towns_rate <- left_join(box_towns_rate, town_box)
p <- ggplot(box_towns, aes(factor(type), percapita))
p + geom_boxplot()

r <- plot_ly(box_towns, y = percapita, color = type, type = "box")
r
# This line only works if you have plotly set up 
# plotly_POST(r, filename = "r-docs/heroin-overdoses")


box_towns_rate$year <- gsub("percapita2012", "2012", box_towns_rate$year)
box_towns_rate$year <- gsub("percapita2013", "2013", box_towns_rate$year)
box_towns_rate$year <- gsub("percapita2014", "2014", box_towns_rate$year)
box_towns_rate$year <- gsub("percapita2015", "2015", box_towns_rate$year)
box_towns_rate$type <- gsub("Suburb", "Suburban", box_towns_rate$type)

# heroin death rate by year

p <- ggplot(box_towns_rate, aes(x=type, y=percapita,fill=year))+
  geom_boxplot()+
  facet_grid(.~year)+
  labs(list(title="Heroin overdoses by year in Connecticut", x="Year", y="Per capita"))+
  theme(axis.text.x=element_text(angle=-90, vjust=0.4,hjust=1))
p


f <- list(
  family = "Droid Sans",
  size = 18,
  color = "#fffff"
)
titl <- list(
  title="Drug overdose rate by town type",
  titlefont=f
)
x <- list(
  title = "Year",
  titlefont = f
)
y <- list(
  title = "Per capita",
  titlefont = f
)


p <- plot_ly(box_towns_rate, x = year, y = percapita, color = type, type = "box") %>%
  layout(boxmode = "group", xaxis=x, yaxis=y)
p
# This line only works if you have plotly already set up
# plotly_POST(p, filename = "r-docs/heroin-overdoses-ct")

# per capita

per_capita_towns <- box_towns[c("id", "percapita2012", "percapita2013", "percapita2014", "percapita2015")]
# map goes here

# income
income <- read.csv("data/median-income.csv", stringsAsFactors=FALSE)
income$category <- "nope"

# This determines the range for the loop below
quantile(income$median.income) 

for (i in 1:nrow(income)) {
  if (income$median.income[i]>=90499) {
    income$category[i] <- "> 90,000"
  } else if ((income$median.income[i]>=72528 & income$median.income[i]< 90499)) {
    income$category[i] <- "80,000 - 90,000"
  } else if ((income$median.income[i]>=66143 & income$median.income[i]< 72528)) {
    income$category[i] <- "66,000 - 80,000"
  } else {
    income$category[i] <- "< 66,000"
  }
}

income$category <- as.factor(income$category)
income$category = factor(income$category,levels(income$category)[c(2,4,3,1)])

income_box <- left_join(box_towns, income)
r <- plot_ly(income_box, y = percapita, color = category, type = "box")
r
# This line only works if you have plotly already set up
# plotly_POST(r, filename = "r-docs/heroin-overdoses-income-boxplots")


# by year

income_box_years <- left_join(box_towns_rate, income)

y <- plot_ly(income_box_years, x = year, y = percapita, color = category, type = "box") %>%
  layout(boxmode = "group", xaxis=x, yaxis=y)
y

# This line only works if you already have plotly set up
# plotly_POST(y, filename = "r-docs/heroin-overdoses-income-year-boxplots")

avg_box_year <- income_box_years %>%
  group_by(year, category) %>%
  summarise(average=mean(percapita, na.rm=TRUE), median=median(percapita, na.rm=TRUE))

avg_box_year2 <- avg_box_year %>%
  select(year, category, median) %>%
  group_by(year, category) %>%
  spread(year, median)

p = ggplot(avg_box_year, aes(x = year, y = average))
p = p + geom_bar(stat='identity')
p = p + facet_grid(~category)
p


p = ggplot(avg_box_year, aes(x = year, y = median))
p = p + geom_bar(stat='identity')
p = p + facet_grid(~category)
p