library(dplyr)
library(tidyr)
library(choroplethr)
library(stringr)

# devtools::install_github("hrbrmstr/ggalt")
library(ggplot2)
library(ggalt)
library(scales)

loans <- read.csv("data/loan.csv", stringsAsFactors=FALSE)

us_map <- loans %>%
  group_by(addr_state) %>%
  summarise(loans=n(), total=sum(loan_amnt), average=round(mean(loan_amnt),2))

# Bringing in US population
uspop <- read.csv("data/uspop.csv", stringsAsFactors=FALSE)

us_map <- left_join(us_map, uspop)

us_map$loans_per_capita <- round(us_map$loans/us_map$population*10000,2)

us_map <- us_map[c("state", "loans_per_capita", "loans", "total", "average")]
us_map$state <- str_to_lower(us_map$state)
# loans per capita

lpc <- us_map[c("state", "loans_per_capita")]
colnames(lpc) <- c("region", "value")

state_choropleth(lpc, title = "Loans per 10,000 residents")

l <- us_map[c("state", "loans")]
colnames(l) <- c("region", "value")

state_choropleth(l, title = "Total loans between 2007 and 2015")

t <- us_map[c("state", "total")]
colnames(t) <- c("region", "value")

state_choropleth(t, title = "Total loan amounts between 2007 and 2015")

a <- us_map[c("state", "average")]
colnames(a) <- c("region", "value")

state_choropleth(a, title = "Average loan amounts between 2007 and 2015")

# write.csv(us_map, "us_map.csv")

# Looking specifically at Connecticut

ct_loans <- subset(loans, addr_state=="CT")

grade <- ct_loans %>%
  group_by(grade) %>%
  summarise(total=n()) %>%
  mutate(percent=round(total/sum(total),2)) %>%
  select(grade, percent) %>%
  mutate(grade=factor(grade), grade=factor(grade, levels=rev(levels(grade))))

gg <- ggplot(grade, aes(y=grade, x=percent))
gg <- gg + geom_lollipop(point.colour="royalblue", point.size=3, horizontal=TRUE)
gg <- gg + scale_x_continuous(expand=c(0,0), labels=percent,
                              breaks=seq(0, 1, by=0.2), limits=c(0, .5))

#gg <- gg + coord_flip()
gg <- gg + labs(x=NULL, y=NULL, 
                title="Loan grades in Connecticut",
                subtitle="Lending Club data between 2007 and 2015. Interest rates ranged from 5.32 percent to 28.49 percent, 
                depending on the grade assigned to the loan from A to G.",
                caption="Source: Kaggle, LendingClub")
gg <- gg + theme_minimal(base_family="Arial Narrow")
gg <- gg + theme(panel.grid.major.y=element_blank())
gg <- gg + theme(panel.grid.minor=element_blank())
gg <- gg + theme(axis.line.y=element_line(color="#2b2b2b", size=0.15))
gg <- gg + theme(axis.text.y=element_text(margin=margin(r=-5, l=0)))
gg <- gg + theme(plot.margin=unit(rep(30, 4), "pt"))
gg <- gg + theme(plot.title=element_text(face="bold"))
gg <- gg + theme(plot.subtitle=element_text(margin=margin(b=10)))
gg <- gg + theme(plot.caption=element_text(size=8, margin=margin(t=10)))
gg

purpose <- ct_loans %>%
  group_by(purpose) %>%
  summarise(total=n(), sum=sum(loan_amnt), average=mean(loan_amnt)) %>%
  mutate(percent=round(total/sum(total)*100,2)) %>%
  mutate(purpose=factor(purpose), purpose=factor(purpose, levels=rev(levels(purpose))))

gg <- ggplot(purpose, aes(y=purpose, x=total))
gg <- gg + geom_lollipop(point.colour="royalblue", point.size=3, horizontal=TRUE)
gg <- gg + scale_x_continuous(expand=c(0,0),
                              breaks=seq(0, 1, by=0.2), limits=c(0, max(total)))
#gg <- gg + coord_flip()
gg <- gg + labs(x=NULL, y=NULL, 
                title="Total loans issued in Connecticut",
                subtitle="Lending Club data between 2007 and 2015.",
                caption="Source: Kaggle, LendingClub")
gg <- gg + theme_minimal(base_family="Arial Narrow")
gg <- gg + theme(panel.grid.major.y=element_blank())
gg <- gg + theme(panel.grid.minor=element_blank())
gg <- gg + theme(axis.line.y=element_line(color="#2b2b2b", size=0.15))
gg <- gg + theme(axis.text.y=element_text(margin=margin(r=-5, l=0)))
gg <- gg + theme(plot.margin=unit(rep(30, 4), "pt"))
gg <- gg + theme(plot.title=element_text(face="bold"))
gg <- gg + theme(plot.subtitle=element_text(margin=margin(b=10)))
gg <- gg + theme(plot.caption=element_text(size=8, margin=margin(t=10)))
gg

## Avg
gg <- ggplot(purpose, aes(y=purpose, x=average))
gg <- gg + geom_lollipop(point.colour="royalblue", point.size=3, horizontal=TRUE)
gg <- gg + scale_x_continuous(expand=c(0,0),labels=dollar,
                              breaks=seq(0, 1, by=0.2), limits=c(0, max(total)))
#gg <- gg + coord_flip()
gg <- gg + labs(x=NULL, y=NULL, 
                title="Average loan amount issued in Connecticut",
                subtitle="By purpose. Lending Club data between 2007 and 2015.",
                caption="Source: Kaggle, LendingClub")
gg <- gg + theme_minimal(base_family="Arial Narrow")
gg <- gg + theme(panel.grid.major.y=element_blank())
gg <- gg + theme(panel.grid.minor=element_blank())
gg <- gg + theme(axis.line.y=element_line(color="#2b2b2b", size=0.15))
gg <- gg + theme(axis.text.y=element_text(margin=margin(r=-5, l=0)))
gg <- gg + theme(plot.margin=unit(rep(30, 4), "pt"))
gg <- gg + theme(plot.title=element_text(face="bold"))
gg <- gg + theme(plot.subtitle=element_text(margin=margin(b=10)))
gg <- gg + theme(plot.caption=element_text(size=8, margin=margin(t=10)))
gg
# write.csv(purpose, "purpose2.csv")


ct_loans$year <- gsub(".*-", "", ct_loans$issue_d)

years <- ct_loans %>%
  group_by(year) %>%
  summarise(loans=n())

ggplot(years, aes(x=year, y=loans, group=1)) + geom_line() +
  labs(x = "Year", y = "Number of loans", 
       title = "LendingClub loans issued over time in Connecticut") + 
  theme_minimal(base_family="Arial Narrow") +
 theme(panel.grid.major.y=element_blank()) +
  theme(panel.grid.minor=element_blank())

# write.csv(table(ct_loans$year), "annual.csv")
# write.csv(table(ct_loans$year, ct_loans$grade), "annual_grade.csv")
