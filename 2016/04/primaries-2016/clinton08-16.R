## Analyzing Clinton's 2016 performance with her 2008 performance

## Towns with the closest margins

total_margins <- dem_results[c("Town", "clinton_count", "sanders_count", "vote_diff")]
total_margins <- arrange(total_margins, vote_diff)

total_margins <- subset(total_margins, (vote_diff<6) & (vote_diff > -6))

## Towns with the widest margins

percent_margins <- dem_results[c("Town", "clinton_per", "sanders_per", "percent_diff")]
percent_margins <- arrange(percent_margins, percent_diff)

clinton_lead <- head(percent_margins, 5)
sanders_lead <- tail(percent_margins, 5)

## Did Hillary win the towns she won last time?

dem2008 <- read.csv("data/dem2008.csv", stringsAsFactors=FALSE)
dem2008_total <- dem2008[c("Town", "clinton_total_2008")]
dem2008_percent <- dem2008[c("Town", "clinton_per_2008")]

timechange <- dem_results[c("Town", "clinton_per", "clinton_count")]
timechange_total <- timechange[c("Town", "clinton_count")]
timechange_percent <- timechange[c("Town", "clinton_per")]

timechange_total <- left_join(dem2008_total, timechange_total)
timechange_percent <- left_join(dem2008_percent, timechange_percent)

timechange_percent <- arrange(timechange_percent, clinton_per)
timechange_percent$Town <- factor(timechange_percent$Town, levels=unique(timechange_percent$Town))

library(tidyr)
library(scales)
library(ggplot2)
devtools::install_github("hrbrmstr/ggalt")
library(ggalt)


gg <- ggplot(timechange_percent, aes(x=clinton_per, xend=clinton_per_2008, y=Town, group=Town))
gg <- gg + geom_dumbbell(color="#a3c4dc", size=0.5, point.colour.l="#0e668b")
# gg <- gg + scale_x_continuous(label=percent)
gg <- gg + labs(x=NULL, y=NULL)
gg <- gg + theme_bw()
gg <- gg+ theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=6)) 
gg <- gg + theme(plot.background=element_rect(fill="#f7f7f7"))
gg <- gg + theme(panel.background=element_rect(fill="#f7f7f7"))
gg <- gg + theme(panel.grid.minor=element_blank())
gg <- gg + theme(panel.grid.major.y=element_blank())
gg <- gg + theme(panel.grid.major.x=element_line())
gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(axis.text = element_text(size = 7))
gg <- gg + labs(title = "Percent of town votes for Hillary Clinton 2008 versus 2016")
gg <- gg + annotate("text", x = 68, y = 140, label = "2008", size=3, colour="gray30")
gg <- gg + annotate("text", x = 68, y = 143, label = "2016", size=3, colour="gray30")
gg <- gg + annotate("point", x = 66, y = 140, colour = "#a3c4dc", size = 2) 
gg <- gg + annotate("point", x = 66, y = 143, colour = "#0e668b", size = 2)
gg <- gg + theme(legend.position="top")
gg <- gg + theme(panel.border=element_blank())
gg

#timechange_percenth <- timechange_percent
#colnames(timechange_percenth) <- c("Town", "y2008", "y2016")
#timechange_percenth <- gather(timechange_percenth, "primary", "percent", 2:3)
#timechange_percenth$primary <- as.factor(timechange_percenth$primary)
#timechange_percenth <- timechange_percenth %>% arrange(Town, primary)


#gg <- ggplot(timechange_percenth)
#gg <- gg + geom_path(aes(x=percent, y=Town, group=Town, color="#a3c4dc"), size=0.55)
#gg <- gg + geom_point(aes(x=percent, y=Town, color="#0e668b"), size=1.25)
#gg <- gg + scale_color_identity()
#gg <- gg + scale_x_continuous(label=percent)
#gg <- gg + labs(x=NULL, y=NULL)
#gg <- gg + theme_bw()
#gg <- gg + theme(plot.background=element_rect(fill="#f7f7f7"))
#gg <- gg + theme(panel.background=element_rect(fill="#f7f7f7"))
#gg <- gg + theme(panel.grid.minor=element_blank())
#gg <- gg + theme(panel.grid.major.y=element_blank())
#gg <- gg + theme(panel.grid.major.x=element_line())
#gg <- gg + theme(axis.ticks=element_blank())
#gg <- gg + theme(legend.position="top")
#gg <- gg + theme(panel.border=element_blank())
#gg


clinton_towns <- subset(dem2008, obama_per_2008<clinton_per_2008)
clinton_towns <- left_join(clinton_towns, percent_margins)

## How'd Bernie do in Obama towns?

obama_towns <- subset(dem2008, obama_per_2008>clinton_per_2008)
obama_towns <- left_join(obama_towns, percent_margins)

checking <- subset(obama_towns, sanders_per>clinton_per)

## Correlation between Trump votes and what?
library(ctnamecleaner)


tru <- rep_results[c("Town", "trump_count")]
colnames(tru) <- c("town", "variable")

ctcorrelator(tru, p=.9)
# Trump .9 correlation
#Between occupied housing
#commuters

## Correlation between Hillary votes and what?

hil <- dem_results[c("Town", "clinton_count")]
colnames(hil) <- c("town", "variable")

ctcorrelator(hil, p=.9)

# Clinton .9 correlation
#Between occupied housing
#commuters
#race.total?
#all other households?

## Correlation between Kasich votes and what?

kas <- rep_results[c("Town", "kasich_count")]
colnames(kas) <- c("town", "variable")

ctcorrelator(kas, p=.9)
# Trump .9 correlation
#Between occupied housing
#commuters

## Correlation between Sanders voets and what?

san <- dem_results[c("Town", "sanders_count")]
colnames(san) <- c("town", "variable")

ctcorrelator(san, p=.9)

# Sanders.9 correlation
#Between occupied housing
#commuters


#We're going to try to analyze the town demographics as associated with who they voted for. But these next steps should be taken with a huge grain of salt because it's difficult to tell if the six percent who voted on Tuesday accurately represent the makeup of the rest of the state.

### Very strong correlations between

#**Number of Trump votes and**
  
#  * Occupied housing population
#  * Number of commuters

#  **Number of Kasich votes and**
  
#  * Nothing

# **Number of Clinton votes and**
  
#  * Occupied housing population
# * Number of commuters
# * race.total?
# * all other households?

# **Number of Sanders votes and**
  
#  * Occupied housing population
# * Number of commuters