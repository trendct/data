library(readxl)
library(corrplot)
library(dplyr)
library(stringr)
library(dplyr)
library(tidyr)
library(rgeos)
library(maptools)
library(ggplot2)   # devtools::install_github("hadley/ggplot2") only if you want subtitles/captions
library(ggalt)
library(ggthemes)
library(albersusa) # devtools::install_github("hrbrmstr/albersusa")
library(viridis)
library(scales)

y2013 <- read_excel("AIDSVu_County_Prev_2013.xlsx", sheet=1, skip=2)
y2014 <- read_excel("AIDSVu_County_NewDX_2014.xlsx", sheet=1, skip=2)

colnames(y2013) <- make.names(colnames(y2013))
colnames(y2014) <- make.names(colnames(y2014))

y2013$GEO.ID <-  ifelse(nchar(y2013$GEO.ID) == 4, paste0("0", y2013$GEO.ID), y2013$GEO.ID)
y2014$GEO.ID <-  ifelse(nchar(y2014$GEO.ID) == 4, paste0("0", y2014$GEO.ID), y2014$GEO.ID)



with(y2013, cor(Gini.Coefficient, County.Rate, use="complete.obs"))

y2013nums <- y2013
y2013nums$State.Abbreviation <- NULL
y2013nums$State <- NULL
y2013nums$County.Name <- NULL
y2013nums$County.Rate.Stability <- NULL
y2013nums$Male.Rate.Stability <- NULL
y2013nums$Female.Rate.Stability <- NULL
y2013nums$Black.Rate.Stability <- NULL
y2013nums$Male.Rate.Stability <- NULL
y2013nums$White.Rate.Stability <- NULL
y2013nums$Hispanic.Rate.Stability <- NULL
y2013nums$Age.13.24.Rate.Stability <- NULL
y2013nums$Age.25.34.Rate.Stability <- NULL
y2013nums$Age.35.44.Rate.Stability <- NULL
y2013nums$Age.45.54.Rate.Stability <- NULL
y2013nums$Age.55..Rate.Stability <- NULL

y2013nums <- cor(y2013nums, use="na.or.complete")
corrplot(y2013nums, method="number")

# Alright, no significant correlations between cases and uninsured, income, gini coefficient, poverty, high school, and correctional warnings

# State Rate?

state_sum <- y2013 %>% 
  group_by(State) %>%
  summarise(Rate=mean(County.Rate))
colnames(state_sum) <- c("County", "Overall.Rate")


state_black <- y2013 %>% 
  group_by(State) %>%
  summarise(Rate=mean(Black.Rate))
colnames(state_black) <- c("County", "Black.Rate")

state_hispanic <- y2013 %>% 
  group_by(State) %>%
  summarise(Rate=mean(Hispanic.Rate))
colnames(state_hispanic) <- c("County", "Hispanic.Rate")

state_white <- y2013 %>% 
  group_by(State) %>%
  summarise(Rate=mean(White.Rate))
colnames(state_white) <- c("County", "White.Rate")

state_all <- left_join(state_sum, state_black)
state_all <- left_join(state_all, state_hispanic)
state_all <- left_join(state_all, state_white)

# new origin dataframe
y2013b <- y2013

# Structure of story

# Viz 1: Overall US county map 2013 (multi-tabs)
# * County.Rate
# * Male.Rate
# * Female.Rate
# * Black.Rate
# * White.Rate
# * Hispanic.Rate

# Bringing map in
cmap <- fortify(counties_composite(), region="fips")


# Turning into factor and creating breaks
y2013 <- subset(y2013b, County.Rate>1)

y2013$breaks <- "NA"
y2013$breaks <- ifelse(y2013$County.Rate < 41, "0-40", y2013$breaks)
y2013$breaks <- ifelse(y2013$County.Rate >= 41 & y2013$County.Rate <= 60, "41-60", y2013$breaks)
y2013$breaks <- ifelse(y2013$County.Rate >= 61 & y2013$County.Rate <= 70, "61-70", y2013$breaks)
y2013$breaks <- ifelse(y2013$County.Rate >= 71 & y2013$County.Rate <= 90, "71-90", y2013$breaks)
y2013$breaks <- ifelse(y2013$County.Rate >= 91 & y2013$County.Rate <= 110, "91-110", y2013$breaks)
y2013$breaks <- ifelse(y2013$County.Rate >= 111 & y2013$County.Rate <= 140, "111-140", y2013$breaks)
y2013$breaks <- ifelse(y2013$County.Rate >= 141 & y2013$County.Rate <= 190, "141-190", y2013$breaks)
y2013$breaks <- ifelse(y2013$County.Rate >= 191 & y2013$County.Rate <= 260, "191-260", y2013$breaks)
y2013$breaks <- ifelse(y2013$County.Rate >= 261 & y2013$County.Rate <= 410, "260-410", y2013$breaks)
y2013$breaks <- ifelse(y2013$County.Rate >= 411, "411+", y2013$breaks)

y2013$breaks <- factor(y2013$breaks, levels=c("0-40", "41-60", "61-70", "71-90", "91-110", "111-140", "141-190", "191-260", "260-410","411+"))
#y2013$breaks <- cut(y2013$County.Rate, 5)

gg <- ggplot()
gg <- gg + geom_map(data=cmap, map=cmap,
                    aes(x=long, y=lat, map_id=id),
                    color="#2b2b2b", size=0.05, fill=NA)
gg <- gg + geom_map(data=y2013, map=cmap,
                    aes(fill=breaks, map_id=GEO.ID),
                    color="#2b2b2b", size=0.05, na.rm=TRUE)
#gg <- gg + scale_fill_viridis(name="Uninsured",labels=percent)
gg <- gg + scale_fill_manual(values = c('#f7fcf0','#e0f3db','#ccebc5','#a8ddb5','#7bccc4','#4eb3d3','#2b8cbe','#0868ac','#084081', "#081d58"), name="Rate")
#gg <- gg + scale_fill_brewer(palette="PuRd")
#gg <- gg + scale_fill_brewer(palette = "Reds")
gg <- gg + coord_proj(us_laea_proj)
gg <- gg + theme_map(base_family="Arial Narrow")
gg <- gg + theme(legend.position=c(0.8, 0.25))
gg <- gg + theme(plot.title=element_text(face="bold", size=14, margin=margin(b=6)))
gg <- gg + theme(plot.subtitle=element_text(size=10, margin=margin(b=-14)))
gg

ggsave("rate.png", width = 8, height = 6)



y2013 <- subset(y2013b, County.Cases>=1)

y2013$breaks <- "NA"
y2013$breaks <- ifelse(y2013$County.Cases >= 5 & y2013$County.Cases <=8 , "5-8", y2013$breaks)
y2013$breaks <- ifelse(y2013$County.Cases >= 9 & y2013$County.Cases <= 12, "9-12", y2013$breaks)
y2013$breaks <- ifelse(y2013$County.Cases >= 13 & y2013$County.Cases <= 18, "13-18", y2013$breaks)
y2013$breaks <- ifelse(y2013$County.Cases >= 19 & y2013$County.Cases <= 24, "19-24", y2013$breaks)
y2013$breaks <- ifelse(y2013$County.Cases >= 25 & y2013$County.Cases <= 36, "25-36", y2013$breaks)
y2013$breaks <- ifelse(y2013$County.Cases >= 37 & y2013$County.Cases <= 56, "37-56", y2013$breaks)
y2013$breaks <- ifelse(y2013$County.Cases >= 57 & y2013$County.Cases <= 96, "57-96", y2013$breaks)
y2013$breaks <- ifelse(y2013$County.Cases >= 97 & y2013$County.Cases <= 166, "97-166", y2013$breaks)
y2013$breaks <- ifelse(y2013$County.Cases >= 167 & y2013$County.Cases <= 518, "167-518", y2013$breaks)
y2013$breaks <- ifelse(y2013$County.Cases >= 519, "519+", y2013$breaks)

y2013$breaks <- factor(y2013$breaks, levels = c("5-8", "9-12", "13-18", "19-24", "25-36", "37-56", "57-96", "97-166", "167-518","519+"))

gg <- ggplot()
gg <- gg + geom_map(data=cmap, map=cmap,
                    aes(x=long, y=lat, map_id=id),
                    color="#2b2b2b", size=0.05, fill=NA)
gg <- gg + geom_map(data=y2013, map=cmap,
                    aes(fill=breaks, map_id=GEO.ID),
                    color="#2b2b2b", size=0.05)
#gg <- gg + scale_fill_viridis(name="Uninsured",labels=percent)
gg <- gg + scale_fill_manual(values = c('#f7fcf0','#e0f3db','#ccebc5','#a8ddb5','#7bccc4','#4eb3d3','#2b8cbe','#0868ac','#084081', "#081d58"), name="Cases")
gg <- gg + coord_proj(us_laea_proj)
gg <- gg + theme_map(base_family="Arial Narrow")
gg <- gg + theme(legend.position=c(0.8, 0.25))
gg <- gg + theme(plot.title=element_text(face="bold", size=14, margin=margin(b=6)))
gg <- gg + theme(plot.subtitle=element_text(size=10, margin=margin(b=-14)))
gg

ggsave("total.png", width = 8, height = 6)

# 
# 
# gg <- ggplot()
# gg <- gg + geom_map(data=cmap, map=cmap,
#                     aes(x=long, y=lat, map_id=id),
#                     color="#2b2b2b", size=0.05, fill=NA)
# gg <- gg + geom_map(data=y2013, map=cmap,
#                     aes(fill=Male.Rate, map_id=GEO.ID),
#                     color="#2b2b2b", size=0.05)
# #gg <- gg + scale_fill_viridis(name="Uninsured",labels=percent)
# gg <- gg + scale_fill_gradient(low="beige", high="red", name="Rate")
# gg <- gg + coord_proj(us_laea_proj)
# gg <- gg + theme_map(base_family="Arial Narrow")
# gg <- gg + theme(legend.position=c(0.8, 0.25))
# gg <- gg + theme(plot.title=element_text(face="bold", size=14, margin=margin(b=6)))
# gg <- gg + theme(plot.subtitle=element_text(size=10, margin=margin(b=-14)))
# gg
# 
# ggsave("male_rate.png", scale=.5)
# 
# 
# 
# gg <- ggplot()
# gg <- gg + geom_map(data=cmap, map=cmap,
#                     aes(x=long, y=lat, map_id=id),
#                     color="#2b2b2b", size=0.05, fill=NA)
# gg <- gg + geom_map(data=y2013, map=cmap,
#                     aes(fill=Female.Rate, map_id=GEO.ID),
#                     color="#2b2b2b", size=0.05)
# #gg <- gg + scale_fill_viridis(name="Uninsured",labels=percent)
# gg <- gg + scale_fill_gradient(low="beige", high="red", name="Rate")
# gg <- gg + coord_proj(us_laea_proj)
# gg <- gg + theme_map(base_family="Arial Narrow")
# gg <- gg + theme(legend.position=c(0.8, 0.25))
# gg <- gg + theme(plot.title=element_text(face="bold", size=14, margin=margin(b=6)))
# gg <- gg + theme(plot.subtitle=element_text(size=10, margin=margin(b=-14)))
# gg
# 
# ggsave("female_rate.png", scale=.5)
# 
# 
# 
# gg <- ggplot()
# gg <- gg + geom_map(data=cmap, map=cmap,
#                     aes(x=long, y=lat, map_id=id),
#                     color="#2b2b2b", size=0.05, fill=NA)
# gg <- gg + geom_map(data=y2013, map=cmap,
#                     aes(fill=Black.Rate, map_id=GEO.ID),
#                     color="#2b2b2b", size=0.05)
# #gg <- gg + scale_fill_viridis(name="Uninsured",labels=percent)
# gg <- gg + scale_fill_gradient(low="beige", high="red", name="Rate")
# gg <- gg + coord_proj(us_laea_proj)
# gg <- gg + theme_map(base_family="Arial Narrow")
# gg <- gg + theme(legend.position=c(0.8, 0.25))
# gg <- gg + theme(plot.title=element_text(face="bold", size=14, margin=margin(b=6)))
# gg <- gg + theme(plot.subtitle=element_text(size=10, margin=margin(b=-14)))
# gg
# 
# ggsave("black_rate.png", scale=.5)
# 
# 
# 
# gg <- ggplot()
# gg <- gg + geom_map(data=cmap, map=cmap,
#                     aes(x=long, y=lat, map_id=id),
#                     color="#2b2b2b", size=0.05, fill=NA)
# gg <- gg + geom_map(data=y2013, map=cmap,
#                     aes(fill=Hispanic.Rate, map_id=GEO.ID),
#                     color="#2b2b2b", size=0.05)
# #gg <- gg + scale_fill_viridis(name="Uninsured",labels=percent)
# gg <- gg + scale_fill_gradient(low="beige", high="red", name="Rate")
# gg <- gg + coord_proj(us_laea_proj)
# gg <- gg + theme_map(base_family="Arial Narrow")
# gg <- gg + theme(legend.position=c(0.8, 0.25))
# gg <- gg + theme(plot.title=element_text(face="bold", size=14, margin=margin(b=6)))
# gg <- gg + theme(plot.subtitle=element_text(size=10, margin=margin(b=-14)))
# gg
# 
# ggsave("hispanic_rate.png", scale=.5)
# 
# 
# 
# gg <- ggplot()
# gg <- gg + geom_map(data=cmap, map=cmap,
#                     aes(x=long, y=lat, map_id=id),
#                     color="#2b2b2b", size=0.05, fill=NA)
# gg <- gg + geom_map(data=y2013, map=cmap,
#                     aes(fill=White.Rate, map_id=GEO.ID),
#                     color="#2b2b2b", size=0.05)
# #gg <- gg + scale_fill_viridis(name="Uninsured",labels=percent)
# gg <- gg + scale_fill_gradient(low="beige", high="red", name="Rate")
# gg <- gg + coord_proj(us_laea_proj)
# gg <- gg + theme_map(base_family="Arial Narrow")
# gg <- gg + theme(legend.position=c(0.8, 0.25))
# gg <- gg + theme(plot.title=element_text(face="bold", size=14, margin=margin(b=6)))
# gg <- gg + theme(plot.subtitle=element_text(size=10, margin=margin(b=-14)))
# gg
# 
# ggsave("white_rate.png", scale=.5)


# Viz 2: New Diagnosis 2014
# * Rate
# * Cases
y2014b <- y2014

y2014 <- subset(y2014b, New.Diagnoses.Rate>1)

y2014$breaks <- "NA"
y2014$breaks <- ifelse(y2014$New.Diagnoses.Rate < 6, "0-6", y2014$breaks)
y2014$breaks <- ifelse(y2014$New.Diagnoses.Rate >= 7 & y2014$New.Diagnoses.Rate <= 8, "7-8", y2014$breaks)
y2014$breaks <- ifelse(y2014$New.Diagnoses.Rate >= 9 & y2014$New.Diagnoses.Rate <= 9, "9-9", y2014$breaks)
y2014$breaks <- ifelse(y2014$New.Diagnoses.Rate >= 10 & y2014$New.Diagnoses.Rate <= 11, "10-11", y2014$breaks)
y2014$breaks <- ifelse(y2014$New.Diagnoses.Rate >= 12 & y2014$New.Diagnoses.Rate <= 14, "12-14", y2014$breaks)
y2014$breaks <- ifelse(y2014$New.Diagnoses.Rate >= 15 & y2014$New.Diagnoses.Rate <= 17, "15-17", y2014$breaks)
y2014$breaks <- ifelse(y2014$New.Diagnoses.Rate >= 18 & y2014$New.Diagnoses.Rate <= 21, "18-21", y2014$breaks)
y2014$breaks <- ifelse(y2014$New.Diagnoses.Rate >= 22 & y2014$New.Diagnoses.Rate <= 28, "22-28", y2014$breaks)
y2014$breaks <- ifelse(y2014$New.Diagnoses.Rate >= 29 & y2014$New.Diagnoses.Rate <= 40, "29-40", y2014$breaks)
y2014$breaks <- ifelse(y2014$New.Diagnoses.Rate >= 41, "41+", y2014$breaks)

y2014$breaks <- factor(y2014$breaks, levels=c("0-6", "7-8", "9-9", "10-11", "12-14", "15-17", "18-21", "22-28", "29-40","41+"))



gg <- ggplot()
gg <- gg + geom_map(data=cmap, map=cmap,
                    aes(x=long, y=lat, map_id=id),
                    color="#2b2b2b", size=0.05, fill=NA)
gg <- gg + geom_map(data=y2014, map=cmap,
                    aes(fill=breaks, map_id=GEO.ID),
                    color="#2b2b2b", size=0.05)
#gg <- gg + scale_fill_viridis(name="Uninsured",labels=percent)
gg <- gg + scale_fill_manual(values = c('#fff7ec','#fee8c8','#fdd49e','#fdbb84','#fc8d59','#ef6548','#d7301f','#b30000','#7f0000', '#67000d'), name="Rate")
gg <- gg + coord_proj(us_laea_proj)
gg <- gg + theme_map(base_family="Arial Narrow")
gg <- gg + theme(legend.position=c(0.8, 0.25))
gg <- gg + theme(plot.title=element_text(face="bold", size=14, margin=margin(b=6)))
gg <- gg + theme(plot.subtitle=element_text(size=10, margin=margin(b=-14)))
gg

ggsave("2014_rate.png", width = 8, height = 6)


y2014 <- subset(y2014b, New.Diagnoses.Cases>1)

y2014$breaks <- "NA"
y2014$breaks <- ifelse(y2014$New.Diagnoses.Cases == 5, "5", y2014$breaks)
y2014$breaks <- ifelse(y2014$New.Diagnoses.Cases == 6, "6", y2014$breaks)
y2014$breaks <- ifelse(y2014$New.Diagnoses.Cases >= 7 & y2014$New.Diagnoses.Cases <= 8, "7-8", y2014$breaks)
y2014$breaks <- ifelse(y2014$New.Diagnoses.Cases >= 9 & y2014$New.Diagnoses.Cases <= 10, "9-10", y2014$breaks)
y2014$breaks <- ifelse(y2014$New.Diagnoses.Cases >= 11 & y2014$New.Diagnoses.Cases <= 13, "11-13", y2014$breaks)
y2014$breaks <- ifelse(y2014$New.Diagnoses.Cases >= 14 & y2014$New.Diagnoses.Cases <= 19, "14-19", y2014$breaks)
y2014$breaks <- ifelse(y2014$New.Diagnoses.Cases >= 20 & y2014$New.Diagnoses.Cases <= 28, "20-28", y2014$breaks)
y2014$breaks <- ifelse(y2014$New.Diagnoses.Cases >= 29 & y2014$New.Diagnoses.Cases <= 49, "29-49", y2014$breaks)
y2014$breaks <- ifelse(y2014$New.Diagnoses.Cases >= 50 & y2014$New.Diagnoses.Cases <= 111, "50-111", y2014$breaks)
y2014$breaks <- ifelse(y2014$New.Diagnoses.Cases >= 112, "112+", y2014$breaks)

y2014$breaks <- factor(y2014$breaks, levels=c("5", "6", "7-8", "9-10", "11-13", "14-19", "20-28", "29-49", "50-111","112+"))




gg <- ggplot()
gg <- gg + geom_map(data=cmap, map=cmap,
                    aes(x=long, y=lat, map_id=id),
                    color="#2b2b2b", size=0.05, fill=NA)
gg <- gg + geom_map(data=y2014, map=cmap,
                    aes(fill=breaks, map_id=GEO.ID),
                    color="#2b2b2b", size=0.05)
#gg <- gg + scale_fill_viridis(name="Uninsured",labels=percent)
gg <- gg + scale_fill_manual(values = c('#fff7ec','#fee8c8','#fdd49e','#fdbb84','#fc8d59','#ef6548','#d7301f','#b30000','#7f0000', '#67000d'), name="Cases")
gg <- gg + coord_proj(us_laea_proj)
gg <- gg + theme_map(base_family="Arial Narrow")
gg <- gg + theme(legend.position=c(0.8, 0.25))
gg <- gg + theme(plot.title=element_text(face="bold", size=14, margin=margin(b=6)))
gg <- gg + theme(plot.subtitle=element_text(size=10, margin=margin(b=-14)))
gg

ggsave("2014_cases.png", width = 8, height = 6)

# Viz 3: Interactive CT chart (or table) counties
# * County.Rate
# * Black.Rate
# * White.Rate
# * Hispanic.Rate

ct_2013 <- subset(y2013, State.Abbreviation=="CT")

ct_2013 <- ct_2013 %>%
  select(County.Name, County.Rate, Black.Rate, White.Rate, Hispanic.Rate)

ct_2013$County.Name <- gsub(" County", "", ct_2013$County.Name)
write.csv(ct_2013, "ct_2013.csv")

# Viz 4: Interactive CT (or table)  chart counties
# * New.Diagnoses.Rate
# * New.Diagnoses.Cases




ct_2014 <- subset(y2014, State.Abbreviation=="CT")

ct_2014 <- ct_2014 %>%
  select(County.Name, New.Diagnoses.Rate, New.Diagnoses.Cases)

ct_2014$County.Name <- gsub(" County", "", ct_2014$County.Name)
write.csv(ct_2014, "ct_2014.csv")


# Viz 5: Something about Hartford?
