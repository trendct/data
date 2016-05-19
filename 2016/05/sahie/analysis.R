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

sahie2013 <- read.csv("data/sahie_2013.csv", skip=78, stringsAsFactors=FALSE)
sahie2013$State.Name <- str_trim(sahie2013$State.Name)
sahie2014 <- read.csv("data/sahie_2014.csv", skip=78, stringsAsFactors=FALSE)
sahie2014$State.Name <- str_trim(sahie2014$State.Name)

sahie2013$statefips <- as.character(sahie2013$statefips)
sahie2013$countyfips <- as.character(sahie2013$countyfips)

sahie2013$statefips <- ifelse(nchar(sahie2013$statefips) == 1, paste0("0", sahie2013$statefips), sahie2013$statefips)
sahie2013$countyfips <- ifelse(nchar(sahie2013$countyfips) == 1, paste0("00", sahie2013$countyfips), sahie2013$countyfips)
sahie2013$countyfips <- ifelse(nchar(sahie2013$countyfips) == 2, paste0("0", sahie2013$countyfips), sahie2013$countyfips)
sahie2013$countyfips <- ifelse(nchar(sahie2013$countyfips) == 3, sahie2013$countyfip, sahie2013$countyfips)
sahie2013$fips <- paste0(sahie2013$statefips, sahie2013$countyfips)
ct_sahie_2013 <- subset(sahie2013, State.Name=="Connecticut")


sahie2014$statefips <- as.character(sahie2014$statefips)
sahie2014$countyfips <- as.character(sahie2014$countyfips)

sahie2014$statefips <- ifelse(nchar(sahie2014$statefips) == 1, paste0("0", sahie2014$statefips), sahie2014$statefips)
sahie2014$countyfips <- ifelse(nchar(sahie2014$countyfips) == 1, paste0("00", sahie2014$countyfips), sahie2014$countyfips)
sahie2014$countyfips <- ifelse(nchar(sahie2014$countyfips) == 2, paste0("0", sahie2014$countyfips), sahie2014$countyfips)
sahie2014$countyfips <- ifelse(nchar(sahie2014$countyfips) == 3, sahie2014$countyfip, sahie2014$countyfips)
sahie2014$fips <- paste0(sahie2014$statefips, sahie2014$countyfips)

ct_sahie_2013 <- subset(sahie2013, State.Name=="Connecticut")
ct_sahie_2014 <- subset(sahie2014, State.Name=="Connecticut")

cmap <- fortify(counties_composite(), region="fips")


counties_sahie <- function(oldyear, newyear, column="PCTELIG",
                           age_cat=0, sex_cat=0, race_cat=0, ipr_cat=0,
                           t="Temp title", s="", c="") {
  
  working_adults_2013 <- oldyear %>%
    filter(agecat==age_cat, sexcat==sex_cat, racecat==race_cat, iprcat==ipr_cat) 
  
  working_adults_2013 <- working_adults_2013[c("fips", "State.Name", column)]
  
  names(working_adults_2013)[names(working_adults_2013)==column] <- "column"
  
  
  working_adults_2013 <- working_adults_2013  %>%
    mutate(percent=as.numeric(column), column=round(as.numeric(column)/100,2))
  

  working_adults_2014 <- newyear %>%
    filter(agecat==age_cat, sexcat==sex_cat, racecat==race_cat, iprcat==ipr_cat) 
  
  working_adults_2014 <- working_adults_2014[c("fips", "State.Name", column)]
  
  names(working_adults_2014)[names(working_adults_2014)==column] <- "column"
  
  working_adults_2014 <- working_adults_2014  %>%
    mutate(percent=as.numeric(column), column=round(as.numeric(column)/100,2))
    
  # 2013-2014 diff
  
  working_adults_2014 <- select(working_adults_2014, fips, percent)
  colnames(working_adults_2014) <- c("fips", "p2014")
  
  working_adults <- left_join(working_adults_2013, working_adults_2014)
  working_adults_compare <- working_adults
  working_adults$diff <- (working_adults$percent-working_adults$p2014)
  
  working_adults <- select(working_adults, fips, State.Name, diff)
  
  gg <- ggplot()
  gg <- gg + geom_map(data=cmap, map=cmap,
                      aes(x=long, y=lat, map_id=id),
                      color="#2b2b2b", size=0.05, fill=NA)
  gg <- gg + geom_map(data=working_adults, map=cmap,
                      aes(fill=diff, map_id=fips),
                      color="#2b2b2b", size=0.05)
  # gg <- gg + scale_fill_viridis(name="Uninsured") 
  gg <- gg + scale_fill_viridis(name="Percent point change")
  gg <- gg + coord_proj(us_laea_proj)
  gg <- gg + labs(title=t,
                  subtitle=s,
                  caption=c)
  gg <- gg + theme_map(base_family="Arial Narrow")
  gg <- gg + theme(legend.position=c(0.8, 0.25))
  gg <- gg + theme(plot.title=element_text(face="bold", size=14, margin=margin(b=6)))
  gg <- gg + theme(plot.subtitle=element_text(size=10, margin=margin(b=-14)))
  print(gg)
  
  nrow(subset(working_adults, diff>=0))
  nrow(subset(working_adults, diff<0))
  
  less <- nrow(subset(working_adults_compare, p2014 < percent)) 
  more <- nrow(subset(working_adults_compare, p2014 > percent))
  
  per_less <- round(less/(more+less)*100,2)
  per_more <- round(more/(more+less)*100,2)
  cat("less: ", per_less, "% \n")
  cat("more: ", per_more, "%")
  
  ct_working_adults_compare <- subset(working_adults_compare, State.Name=="Connecticut")
  ct_working_adults_compare <- ct_working_adults_compare[c("fips", "percent", "p2014")]
  colnames(ct_working_adults_compare) <- c("fips", "y2013", "y2014")
  ct_working_adults_compare$diff <- ct_working_adults_compare$y2013 - ct_working_adults_compare$y2014
  
  write.csv(ct_working_adults_compare, paste0("data/", gsub(" ", "", t), ".csv"))
}


# 18 - 64
# agecat==1
# sexcat==0
# racecat==0
# iprcat==0
# PCTELIG


counties_sahie(sahie2013, sahie2014, column="PCTELIG",
               age_cat=1, sex_cat=0, race_cat=0, ipr_cat=0,
               t="U.S. decrease of uninsured rate by county from 2013 to 2014 (18-64)", 
               s="The estimated uninsured rate for working-age adults decreased in 72.1 percent of the nation’s counties (or 2,262 counties) from 2013 to 2014.", 
               c="SOURCE: Small Area Health Insurance Estimates, U.S. Census")

# Under 65
# agecat==0
# sexcat==0
# racecat==0
# iprcat==0
# PCTELIG

counties_sahie(sahie2013, sahie2014, column="PCTELIG",
               age_cat=0, sex_cat=0, race_cat=0, ipr_cat=0,
               t="U.S. decrease of uninsured rate by county from 2013 to 2014 (Under 64)", 
               s="For the total population under age 65, the estimated uninsured rate decreased in 74.1 percent of counties (or 2,325 counties) \nbetween 2013 and 2014. There were no statistically significant changes in 25.9 percent of counties, \nand only one county experienced an increase in its uninsured rate.", 
               c="SOURCE: Small Area Health Insurance Estimates, U.S. Census")

# working-age adults living at or below 138 percent of poverty
# saw a decrease in the uninsured in 59.3 percent of counties (or 1,860 counties)
# agecat==1
# sexcat==0
# racecat==0
# iprcat==3 
# PCTUI

counties_sahie(sahie2013, sahie2014, column="PCTUI",
               age_cat=1, sex_cat=0, race_cat=0, ipr_cat=3,
               t="U.S. Decrease of Uninsured Rate by County from 2013 to 2014 (Adults in poverty)", 
               s="Between 2013 and 2014, working-age adults living at or below 138 percent of poverty saw a decrease \nin the uninsured in 59.3 percent of counties (or 1,860 counties).", 
               c="SOURCE: Small Area Health Insurance Estimates, U.S. Census")

# Between 2013 and 2014, the uninsured rate decreased in 66.6 percent 
# of counties (or 2,089 counties) for working-age adults living between 
# 138 to 400 percent of poverty (which is the eligibility for subsidies
# to purchase health insurance coverage through the exchanges).

# agecat==1
# sexcat==0
# racecat==0
# iprcat==5 
# PCTELIG

counties_sahie(sahie2013, sahie2014, column="PCTELIG",
               age_cat=1, sex_cat=0, race_cat=0, ipr_cat=5,
               t="U.S. Decrease of Uninsured Rate by County from 2013 to 2014 (Adults in poverty)", 
               s="Between 2013 and 2014, the uninsured rate decreased in 66.6 percent of counties (or 2,089 counties) for working-age adults \nliving between 138 to 400 percent of poverty (which is the eligibility for subsidies to purchase health insurance coverage through the exchanges).", 
               c="SOURCE: Small Area Health Insurance Estimates, U.S. Census")


# Between 2013 and 2014, the uninsured rate decreased for children 
# under age 19 in 20.7 percent of counties (or 650 counties).
# agecat==4
# sexcat==0
# racecat==0
# iprcat==0
# PCTELIG

counties_sahie(sahie2013, sahie2014, column="PCTELIG",
               age_cat=4, sex_cat=0, race_cat=0, ipr_cat=0,
               t="U.S. Decrease of Uninsured Rate by County from 2013 to 2014 (Children)", 
               s="Between 2013 and 2014, the uninsured rate decreased for children under age 19 in 20.7 percent of counties (or 650 counties)", 
               c="SOURCE: Small Area Health Insurance Estimates, U.S. Census")

# Working-age males had a higher uninsured rate than females in 
# 40.1 percent of counties (or 1,259 counties); there were no statistically 
# significant differences in the remaining counties.

# agecat==1
# sexcat==1 & 2
# racecat==0
# iprcat==0 
# PCTELIG


working_m_2014 <- sahie2014 %>%
  filter(agecat==1, sexcat==1, racecat==0, iprcat==0) %>%
  select(fips, State.Name, PCTELIG) %>%
  mutate(percent=as.numeric(PCTELIG), PCTELIG=round(as.numeric(PCTELIG)/100,2))

working_f_2014 <- sahie2014 %>%
  filter(agecat==1, sexcat==2, racecat==0, iprcat==0) %>%
  select(fips, State.Name, PCTELIG) %>%
  #  mutate(PCTELIG=as.numeric(PCTELIG))
  mutate(percent=as.numeric(PCTELIG), PCTELIG=round(as.numeric(PCTELIG)/100,2))

# male-female diff

working_f_2014 <- select(working_f_2014, fips, percent)
colnames(working_f_2014) <- c("fips", "female")

working_adults <- left_join(working_m_2014, working_f_2014)
working_m_f_compare <- working_adults
working_adults$diff <- (working_adults$percent-working_adults$female)

working_adults <- select(working_adults, fips, State.Name, diff)


gg <- ggplot()
gg <- gg + geom_map(data=cmap, map=cmap,
                    aes(x=long, y=lat, map_id=id),
                    color="#2b2b2b", size=0.05, fill=NA)
gg <- gg + geom_map(data=working_adults, map=cmap,
                    aes(fill=diff, map_id=fips),
                    color="#2b2b2b", size=0.05)
# gg <- gg + scale_fill_viridis(name="Uninsured") 
gg <- gg + scale_fill_viridis(name="Percent point change")
gg <- gg + coord_proj(us_laea_proj)
gg <- gg + labs(title="Male vs Female Uninsured rate in 2014",
                subtitle="Content SOURCE: Small Area Health Insurance Estimates, U.S. Census",
                caption="Data from http://www.cdc.gov/diabetes/atlas/countydata/County_ListofIndicators.html")
gg <- gg + theme_map(base_family="Arial Narrow")
gg <- gg + theme(legend.position=c(0.8, 0.25))
gg <- gg + theme(plot.title=element_text(face="bold", size=14, margin=margin(b=6)))
gg <- gg + theme(plot.subtitle=element_text(size=10, margin=margin(b=-14)))
gg

nrow(subset(working_adults, diff>=0))
nrow(subset(working_adults, diff<0))

less <- nrow(subset(working_m_f_compare, p2014 < percent)) 
more <- nrow(subset(working_m_f_compare, p2014 > percent))

per_less <- round(less/(more+less)*100,2)
per_more <- round(more/(more+less)*100,2)



# 18 - 64 OLD VERSION
# 
# working_adults_2013 <- sahie2013 %>%
#   filter(agecat==1, sexcat==0, racecat==0, iprcat==0) %>%
#   select(fips, State.Name, PCTELIG) %>%
#   mutate(percent=as.numeric(PCTELIG), PCTELIG=round(as.numeric(PCTELIG)/100,2))
# 
# gg <- ggplot()
# gg <- gg + geom_map(data=cmap, map=cmap,
#                     aes(x=long, y=lat, map_id=id),
#                     color="#2b2b2b", size=0.05, fill=NA)
# gg <- gg + geom_map(data=working_adults_2013, map=cmap,
#                     aes(fill=PCTELIG, map_id=fips),
#                     color="#2b2b2b", size=0.05)
# # gg <- gg + scale_fill_viridis(name="Uninsured") 
# gg <- gg + scale_fill_viridis(name="Uninsured",labels=percent)
# gg <- gg + coord_proj(us_laea_proj)
# gg <- gg + labs(title="U.S. Uninsured Rate by County in 2013 (18-64))",
#                 subtitle="Content SOURCE: Small Area Health Insurance Estimates, U.S. Census",
#                 caption="Data from http://www.cdc.gov/diabetes/atlas/countydata/County_ListofIndicators.html")
# gg <- gg + theme_map(base_family="Arial Narrow")
# gg <- gg + theme(legend.position=c(0.8, 0.25))
# gg <- gg + theme(plot.title=element_text(face="bold", size=14, margin=margin(b=6)))
# gg <- gg + theme(plot.subtitle=element_text(size=10, margin=margin(b=-14)))
# gg
# 
# 
# working_adults_2014 <- sahie2014 %>%
#   filter(agecat==1, sexcat==0, racecat==0, iprcat==0) %>%
#   select(fips, State.Name, PCTELIG) %>%
#   #  mutate(PCTELIG=as.numeric(PCTELIG))
#   mutate(percent=as.numeric(PCTELIG), PCTELIG=round(as.numeric(PCTELIG)/100,2))
# 
# gg <- ggplot()
# gg <- gg + geom_map(data=cmap, map=cmap,
#                     aes(x=long, y=lat, map_id=id),
#                     color="#2b2b2b", size=0.05, fill=NA)
# gg <- gg + geom_map(data=working_adults_2014, map=cmap,
#                     aes(fill=PCTELIG, map_id=fips),
#                     color="#2b2b2b", size=0.05)
# # gg <- gg + scale_fill_viridis(name="Uninsured") 
# gg <- gg + scale_fill_viridis(name="Uninsured",labels=percent)
# gg <- gg + coord_proj(us_laea_proj)
# gg <- gg + labs(title="U.S. Uninsured Rate by County in 2014 (18-64)",
#                 subtitle="Content SOURCE: Small Area Health Insurance Estimates, U.S. Census",
#                 caption="Data from http://www.cdc.gov/diabetes/atlas/countydata/County_ListofIndicators.html")
# gg <- gg + theme_map(base_family="Arial Narrow")
# gg <- gg + theme(legend.position=c(0.8, 0.25))
# gg <- gg + theme(plot.title=element_text(face="bold", size=14, margin=margin(b=6)))
# gg <- gg + theme(plot.subtitle=element_text(size=10, margin=margin(b=-14)))
# gg
# 
# # 2013-2014 diff
# 
# working_adults_2014 <- select(working_adults_2014, fips, percent)
# colnames(working_adults_2014) <- c("fips", "p2014")
# 
# working_adults <- left_join(working_adults_2013, working_adults_2014)
# working_adults_compare <- working_adults
# working_adults$diff <- (working_adults$percent-working_adults$p2014)
# 
# working_adults <- select(working_adults, fips, State.Name, diff)
# 
# 
# gg <- ggplot()
# gg <- gg + geom_map(data=cmap, map=cmap,
#                     aes(x=long, y=lat, map_id=id),
#                     color="#2b2b2b", size=0.05, fill=NA)
# gg <- gg + geom_map(data=working_adults, map=cmap,
#                     aes(fill=diff, map_id=fips),
#                     color="#2b2b2b", size=0.05)
# # gg <- gg + scale_fill_viridis(name="Uninsured") 
# gg <- gg + scale_fill_viridis(name="Percent point change")
# gg <- gg + coord_proj(us_laea_proj)
# gg <- gg + labs(title="U.S. Decrease of Uninsured Rate by County from 2013 to 2014 (18-64)",
#                 subtitle="Content SOURCE: Small Area Health Insurance Estimates, U.S. Census",
#                 caption="Data from http://www.cdc.gov/diabetes/atlas/countydata/County_ListofIndicators.html")
# gg <- gg + theme_map(base_family="Arial Narrow")
# gg <- gg + theme(legend.position=c(0.8, 0.25))
# gg <- gg + theme(plot.title=element_text(face="bold", size=14, margin=margin(b=6)))
# gg <- gg + theme(plot.subtitle=element_text(size=10, margin=margin(b=-14)))
# gg
# 
# nrow(subset(working_adults, diff>=0))
# nrow(subset(working_adults, diff<0))
# 
# less <- nrow(subset(working_adults_compare, p2014 < percent)) 
# more <- nrow(subset(working_adults_compare, p2014 > percent))
# 
# per_less <- round(less/(more+less)*100,2)
# per_more <- round(more/(more+less)*100,2)


## UNDER 65 OLD VERSION
# 
# working_under_65_2013 <- sahie2013 %>%
#   filter(agecat==0, sexcat==0, racecat==0, iprcat==0) %>%
#   select(fips, State.Name, PCTELIG) %>%
#   mutate(percent=as.numeric(PCTELIG), PCTELIG=round(as.numeric(PCTELIG)/100,2))
# 
# gg <- ggplot()
# gg <- gg + geom_map(data=cmap, map=cmap,
#                     aes(x=long, y=lat, map_id=id),
#                     color="#2b2b2b", size=0.05, fill=NA)
# gg <- gg + geom_map(data=working_under_65_2013, map=cmap,
#                     aes(fill=PCTELIG, map_id=fips),
#                     color="#2b2b2b", size=0.05)
# # gg <- gg + scale_fill_viridis(name="Uninsured") 
# gg <- gg + scale_fill_viridis(name="Uninsured",labels=percent)
# gg <- gg + coord_proj(us_laea_proj)
# gg <- gg + labs(title="U.S. Uninsured Rate by County in 2013 (Under 65))",
#                 subtitle="Content SOURCE: Small Area Health Insurance Estimates, U.S. Census",
#                 caption="Data from http://www.cdc.gov/diabetes/atlas/countydata/County_ListofIndicators.html")
# gg <- gg + theme_map(base_family="Arial Narrow")
# gg <- gg + theme(legend.position=c(0.8, 0.25))
# gg <- gg + theme(plot.title=element_text(face="bold", size=14, margin=margin(b=6)))
# gg <- gg + theme(plot.subtitle=element_text(size=10, margin=margin(b=-14)))
# gg
# 
# 
# working_under_65_2014 <- sahie2014 %>%
#   filter(agecat==0, sexcat==0, racecat==0, iprcat==0) %>%
#   select(fips, State.Name, PCTELIG) %>%
#   #  mutate(PCTELIG=as.numeric(PCTELIG))
#   mutate(percent=as.numeric(PCTELIG), PCTELIG=round(as.numeric(PCTELIG)/100,2))
# 
# gg <- ggplot()
# gg <- gg + geom_map(data=cmap, map=cmap,
#                     aes(x=long, y=lat, map_id=id),
#                     color="#2b2b2b", size=0.05, fill=NA)
# gg <- gg + geom_map(data=working_under_65_2014, map=cmap,
#                     aes(fill=PCTELIG, map_id=fips),
#                     color="#2b2b2b", size=0.05)
# # gg <- gg + scale_fill_viridis(name="Uninsured") 
# gg <- gg + scale_fill_viridis(name="Uninsured",labels=percent)
# gg <- gg + coord_proj(us_laea_proj)
# gg <- gg + labs(title="U.S. Uninsured Rate by County in 2014 (Under 65))",
#                 subtitle="Content SOURCE: Small Area Health Insurance Estimates, U.S. Census",
#                 caption="Data from http://www.cdc.gov/diabetes/atlas/countydata/County_ListofIndicators.html")
# gg <- gg + theme_map(base_family="Arial Narrow")
# gg <- gg + theme(legend.position=c(0.8, 0.25))
# gg <- gg + theme(plot.title=element_text(face="bold", size=14, margin=margin(b=6)))
# gg <- gg + theme(plot.subtitle=element_text(size=10, margin=margin(b=-14)))
# gg
# 
# # 2013-2014 diff
# 
# working_under_65_2014 <- select(working_under_65_2014, fips, percent)
# colnames(working_under_65_2014) <- c("fips", "p2014")
# 
# working_adults <- left_join(working_under_65_2013, working_under_65_2014)
# working_under_65_compare <- working_adults
# working_adults$diff <- (working_adults$percent-working_adults$p2014)
# 
# working_adults <- select(working_adults, fips, State.Name, diff)
# 
# 
# gg <- ggplot()
# gg <- gg + geom_map(data=cmap, map=cmap,
#                     aes(x=long, y=lat, map_id=id),
#                     color="#2b2b2b", size=0.05, fill=NA)
# gg <- gg + geom_map(data=working_adults, map=cmap,
#                     aes(fill=diff, map_id=fips),
#                     color="#2b2b2b", size=0.05)
# # gg <- gg + scale_fill_viridis(name="Uninsured") 
# gg <- gg + scale_fill_viridis(name="Percent point change")
# gg <- gg + coord_proj(us_laea_proj)
# gg <- gg + labs(title="U.S. Decrease of Uninsured Rate by County from 2013 to 2014 (Under 65)",
#                 subtitle="Content SOURCE: Small Area Health Insurance Estimates, U.S. Census",
#                 caption="Data from http://www.cdc.gov/diabetes/atlas/countydata/County_ListofIndicators.html")
# gg <- gg + theme_map(base_family="Arial Narrow")
# gg <- gg + theme(legend.position=c(0.8, 0.25))
# gg <- gg + theme(plot.title=element_text(face="bold", size=14, margin=margin(b=6)))
# gg <- gg + theme(plot.subtitle=element_text(size=10, margin=margin(b=-14)))
# gg
# 
# nrow(subset(working_adults, diff>=0))
# nrow(subset(working_adults, diff<0))
# 
# less <- nrow(subset(working_under_65_compare, p2014 < percent)) 
# more <- nrow(subset(working_under_65_compare, p2014 > percent))
# 
# per_less <- round(less/(more+less)*100,2)
# per_more <- round(more/(more+less)*100,2)




# working-age adults living at or below 138 percent of poverty OLD VERSION
# 
# working_below_138_2013 <- sahie2013 %>%
#   filter(agecat==1, sexcat==0, racecat==0, iprcat==3) %>%
#   select(fips, State.Name, PCTUI) %>%
#   mutate(percent=as.numeric(PCTUI), PCTUI=round(as.numeric(PCTUI)/100,2))
# 
# gg <- ggplot()
# gg <- gg + geom_map(data=cmap, map=cmap,
#                     aes(x=long, y=lat, map_id=id),
#                     color="#2b2b2b", size=0.05, fill=NA)
# gg <- gg + geom_map(data=working_below_138_2013, map=cmap,
#                     aes(fill=PCTUI, map_id=fips),
#                     color="#2b2b2b", size=0.05)
# # gg <- gg + scale_fill_viridis(name="Uninsured") 
# gg <- gg + scale_fill_viridis(name="Uninsured",labels=percent)
# gg <- gg + coord_proj(us_laea_proj)
# gg <- gg + labs(title="U.S. Uninsured Rate by County in 2013 (Adults in poverty))",
#                 subtitle="Content SOURCE: Small Area Health Insurance Estimates, U.S. Census",
#                 caption="Data from http://www.cdc.gov/diabetes/atlas/countydata/County_ListofIndicators.html")
# gg <- gg + theme_map(base_family="Arial Narrow")
# gg <- gg + theme(legend.position=c(0.8, 0.25))
# gg <- gg + theme(plot.title=element_text(face="bold", size=14, margin=margin(b=6)))
# gg <- gg + theme(plot.subtitle=element_text(size=10, margin=margin(b=-14)))
# gg
# 
# 
# working_below_138_2014 <- sahie2014 %>%
#   filter(agecat==1, sexcat==0, racecat==0, iprcat==3) %>%
#   select(fips, State.Name, PCTUI) %>%
#   #  mutate(PCTUI=as.numeric(PCTUI))
#   mutate(percent=as.numeric(PCTUI), PCTUI=round(as.numeric(PCTUI)/100,2))
# 
# gg <- ggplot()
# gg <- gg + geom_map(data=cmap, map=cmap,
#                     aes(x=long, y=lat, map_id=id),
#                     color="#2b2b2b", size=0.05, fill=NA)
# gg <- gg + geom_map(data=working_below_138_2014, map=cmap,
#                     aes(fill=PCTUI, map_id=fips),
#                     color="#2b2b2b", size=0.05)
# # gg <- gg + scale_fill_viridis(name="Uninsured") 
# gg <- gg + scale_fill_viridis(name="Uninsured",labels=percent)
# gg <- gg + coord_proj(us_laea_proj)
# gg <- gg + labs(title="U.S. Uninsured Rate by County in 2014 (Adults in poverty))",
#                 subtitle="Content SOURCE: Small Area Health Insurance Estimates, U.S. Census",
#                 caption="Data from http://www.cdc.gov/diabetes/atlas/countydata/County_ListofIndicators.html")
# gg <- gg + theme_map(base_family="Arial Narrow")
# gg <- gg + theme(legend.position=c(0.8, 0.25))
# gg <- gg + theme(plot.title=element_text(face="bold", size=14, margin=margin(b=6)))
# gg <- gg + theme(plot.subtitle=element_text(size=10, margin=margin(b=-14)))
# gg
# 
# # 2013-2014 diff
# 
# working_below_138_2014 <- select(working_below_138_2014, fips, percent)
# colnames(working_below_138_2014) <- c("fips", "p2014")
# 
# working_adults <- left_join(working_below_138_2013, working_below_138_2014)
# working_below_138_compare <- working_adults
# working_adults$diff <- (working_adults$percent-working_adults$p2014)
# 
# working_adults <- select(working_adults, fips, State.Name, diff)
# 
# 
# gg <- ggplot()
# gg <- gg + geom_map(data=cmap, map=cmap,
#                     aes(x=long, y=lat, map_id=id),
#                     color="#2b2b2b", size=0.05, fill=NA)
# gg <- gg + geom_map(data=working_adults, map=cmap,
#                     aes(fill=diff, map_id=fips),
#                     color="#2b2b2b", size=0.05)
# # gg <- gg + scale_fill_viridis(name="Uninsured") 
# gg <- gg + scale_fill_viridis(name="Percent point change")
# gg <- gg + coord_proj(us_laea_proj)
# gg <- gg + labs(title="U.S. Decrease of Uninsured Rate by County from 2013 to 2014 (Adults in poverty)",
#                 subtitle="Content SOURCE: Small Area Health Insurance Estimates, U.S. Census",
#                 caption="Data from http://www.cdc.gov/diabetes/atlas/countydata/County_ListofIndicators.html")
# gg <- gg + theme_map(base_family="Arial Narrow")
# gg <- gg + theme(legend.position=c(0.8, 0.25))
# gg <- gg + theme(plot.title=element_text(face="bold", size=14, margin=margin(b=6)))
# gg <- gg + theme(plot.subtitle=element_text(size=10, margin=margin(b=-14)))
# gg
# 
# nrow(subset(working_adults, diff>=0))
# nrow(subset(working_adults, diff<0))
# 
# less <- nrow(subset(working_below_138_compare, p2014 < percent)) 
# more <- nrow(subset(working_below_138_compare, p2014 > percent))
# 
# per_less <- round(less/(more+less)*100,2)
# per_more <- round(more/(more+less)*100,2)



# Between 2013 and 2014, the uninsured rate decreased in 66.6 percent OLD VERSION
# 
# working_adults_2013 <- sahie2013 %>%
#   filter(agecat==1, sexcat==0, racecat==0, iprcat==5) %>%
#   select(fips, State.Name, PCTELIG) %>%
#   mutate(percent=as.numeric(PCTELIG), PCTELIG=round(as.numeric(PCTELIG)/100,2))
# 
# working_adults_2014 <- sahie2014 %>%
#   filter(agecat==1, sexcat==0, racecat==0, iprcat==0) %>%
#   select(fips, State.Name, PCTELIG) %>%
#   #  mutate(PCTELIG=as.numeric(PCTELIG))
#   mutate(percent=as.numeric(PCTELIG), PCTELIG=round(as.numeric(PCTELIG)/100,2))
# # 2013-2014 diff
# 
# working_adults_2014 <- select(working_adults_2014, fips, percent)
# colnames(working_adults_2014) <- c("fips", "p2014")
# 
# working_adults <- left_join(working_adults_2013, working_adults_2014)
# working_adults_compare <- working_adults
# working_adults$diff <- (working_adults$percent-working_adults$p2014)
# 
# working_adults <- select(working_adults, fips, State.Name, diff)
# 
# gg <- ggplot()
# gg <- gg + geom_map(data=cmap, map=cmap,
#                     aes(x=long, y=lat, map_id=id),
#                     color="#2b2b2b", size=0.05, fill=NA)
# gg <- gg + geom_map(data=working_adults, map=cmap,
#                     aes(fill=diff, map_id=fips),
#                     color="#2b2b2b", size=0.05)
# # gg <- gg + scale_fill_viridis(name="Uninsured") 
# gg <- gg + scale_fill_viridis(name="Percent point change")
# gg <- gg + coord_proj(us_laea_proj)
# gg <- gg + labs(title="U.S. Decrease of Uninsured Rate by County from 2013 to 2014 (Adults in poverty)",
#                 subtitle="Between 2013 and 2014, the uninsured rate decreased in 66.6 percent of counties (or 2,089 counties) for working-age adults \nliving between 138 to 400 percent of poverty (which is the eligibility for subsidies to purchase health insurance coverage through the exchanges).",
#                 caption="SOURCE: Small Area Health Insurance Estimates, U.S. Census")
# gg <- gg + theme_map(base_family="Arial Narrow")
# gg <- gg + theme(legend.position=c(0.8, 0.25))
# gg <- gg + theme(plot.title=element_text(face="bold", size=14, margin=margin(b=6)))
# gg <- gg + theme(plot.subtitle=element_text(size=10, margin=margin(b=-14)))
# gg
# 
# nrow(subset(working_adults, diff>=0))
# nrow(subset(working_adults, diff<0))
# 
# less <- nrow(subset(working_adults_compare, p2014 < percent)) 
# more <- nrow(subset(working_adults_compare, p2014 > percent))
# 
# per_less <- round(less/(more+less)*100,2)
# per_more <- round(more/(more+less)*100,2)

# In 2014, for the population under age 65, non-Hispanic whites had a lower 
# uninsured rate than both Hispanics and non-Hispanic blacks in every state and Washington, D.C.

# agecat==0
# sexcat==0
# racecat==1 & 2 & 3
# iprcat==0 
# PCTELIG
# PCTUI


sahie2008 <- read.csv("data/sahie_2008.csv", skip=78, stringsAsFactors=FALSE)
sahie2008$State.Name <- str_trim(sahie2008$State.Name)
sahie2014 <- read.csv("data/sahie_2014.csv", skip=78, stringsAsFactors=FALSE)
sahie2014$State.Name <- str_trim(sahie2014$State.Name)

sahie2008$statefips <- as.character(sahie2008$statefips)
sahie2008$countyfips <- as.character(sahie2008$countyfips)

sahie2008$statefips <- ifelse(nchar(sahie2008$statefips) == 1, paste0("0", sahie2008$statefips), sahie2008$statefips)
sahie2008$countyfips <- ifelse(nchar(sahie2008$countyfips) == 1, paste0("00", sahie2008$countyfips), sahie2008$countyfips)
sahie2008$countyfips <- ifelse(nchar(sahie2008$countyfips) == 2, paste0("0", sahie2008$countyfips), sahie2008$countyfips)
sahie2008$countyfips <- ifelse(nchar(sahie2008$countyfips) == 3, sahie2008$countyfip, sahie2008$countyfips)
sahie2008$fips <- paste0(sahie2008$statefips, sahie2008$countyfips)
ct_sahie_2008 <- subset(sahie2008, State.Name=="Connecticut")


sahie2009 <- read.csv("data/sahie_2009.csv", skip=78, stringsAsFactors=FALSE)
sahie2009$State.Name <- str_trim(sahie2009$State.Name)
sahie2014 <- read.csv("data/sahie_2014.csv", skip=78, stringsAsFactors=FALSE)
sahie2014$State.Name <- str_trim(sahie2014$State.Name)

sahie2009$statefips <- as.character(sahie2009$statefips)
sahie2009$countyfips <- as.character(sahie2009$countyfips)

sahie2009$statefips <- ifelse(nchar(sahie2009$statefips) == 1, paste0("0", sahie2009$statefips), sahie2009$statefips)
sahie2009$countyfips <- ifelse(nchar(sahie2009$countyfips) == 1, paste0("00", sahie2009$countyfips), sahie2009$countyfips)
sahie2009$countyfips <- ifelse(nchar(sahie2009$countyfips) == 2, paste0("0", sahie2009$countyfips), sahie2009$countyfips)
sahie2009$countyfips <- ifelse(nchar(sahie2009$countyfips) == 3, sahie2009$countyfip, sahie2009$countyfips)
sahie2009$fips <- paste0(sahie2009$statefips, sahie2009$countyfips)
ct_sahie_2009 <- subset(sahie2009, State.Name=="Connecticut")


sahie2010 <- read.csv("data/sahie_2010.csv", skip=78, stringsAsFactors=FALSE)
sahie2010$State.Name <- str_trim(sahie2010$State.Name)
sahie2014 <- read.csv("data/sahie_2014.csv", skip=78, stringsAsFactors=FALSE)
sahie2014$State.Name <- str_trim(sahie2014$State.Name)

sahie2010$statefips <- as.character(sahie2010$statefips)
sahie2010$countyfips <- as.character(sahie2010$countyfips)

sahie2010$statefips <- ifelse(nchar(sahie2010$statefips) == 1, paste0("0", sahie2010$statefips), sahie2010$statefips)
sahie2010$countyfips <- ifelse(nchar(sahie2010$countyfips) == 1, paste0("00", sahie2010$countyfips), sahie2010$countyfips)
sahie2010$countyfips <- ifelse(nchar(sahie2010$countyfips) == 2, paste0("0", sahie2010$countyfips), sahie2010$countyfips)
sahie2010$countyfips <- ifelse(nchar(sahie2010$countyfips) == 3, sahie2010$countyfip, sahie2010$countyfips)
sahie2010$fips <- paste0(sahie2010$statefips, sahie2010$countyfips)
ct_sahie_2010 <- subset(sahie2010, State.Name=="Connecticut")


sahie2011 <- read.csv("data/sahie_2011.csv", skip=78, stringsAsFactors=FALSE)
sahie2011$State.Name <- str_trim(sahie2011$State.Name)
sahie2014 <- read.csv("data/sahie_2014.csv", skip=78, stringsAsFactors=FALSE)
sahie2014$State.Name <- str_trim(sahie2014$State.Name)

sahie2011$statefips <- as.character(sahie2011$statefips)
sahie2011$countyfips <- as.character(sahie2011$countyfips)

sahie2011$statefips <- ifelse(nchar(sahie2011$statefips) == 1, paste0("0", sahie2011$statefips), sahie2011$statefips)
sahie2011$countyfips <- ifelse(nchar(sahie2011$countyfips) == 1, paste0("00", sahie2011$countyfips), sahie2011$countyfips)
sahie2011$countyfips <- ifelse(nchar(sahie2011$countyfips) == 2, paste0("0", sahie2011$countyfips), sahie2011$countyfips)
sahie2011$countyfips <- ifelse(nchar(sahie2011$countyfips) == 3, sahie2011$countyfip, sahie2011$countyfips)
sahie2011$fips <- paste0(sahie2011$statefips, sahie2011$countyfips)
ct_sahie_2011 <- subset(sahie2011, State.Name=="Connecticut")


sahie2012 <- read.csv("data/sahie_2012.csv", skip=78, stringsAsFactors=FALSE)
sahie2012$State.Name <- str_trim(sahie2012$State.Name)
sahie2014 <- read.csv("data/sahie_2014.csv", skip=78, stringsAsFactors=FALSE)
sahie2014$State.Name <- str_trim(sahie2014$State.Name)

sahie2012$statefips <- as.character(sahie2012$statefips)
sahie2012$countyfips <- as.character(sahie2012$countyfips)

sahie2012$statefips <- ifelse(nchar(sahie2012$statefips) == 1, paste0("0", sahie2012$statefips), sahie2012$statefips)
sahie2012$countyfips <- ifelse(nchar(sahie2012$countyfips) == 1, paste0("00", sahie2012$countyfips), sahie2012$countyfips)
sahie2012$countyfips <- ifelse(nchar(sahie2012$countyfips) == 2, paste0("0", sahie2012$countyfips), sahie2012$countyfips)
sahie2012$countyfips <- ifelse(nchar(sahie2012$countyfips) == 3, sahie2012$countyfip, sahie2012$countyfips)
sahie2012$fips <- paste0(sahie2012$statefips, sahie2012$countyfips)
ct_sahie_2012 <- subset(sahie2012, State.Name=="Connecticut")

working_w_2014 <- ct_sahie_2014 %>%
  filter(agecat==0, sexcat==0, racecat==1, iprcat==0, geocat==40) %>%
  select(fips, State.Name, PCTUI) %>%
  mutate(w_percent=as.numeric(PCTUI)) %>%
  select(fips, w_percent)

working_b_2014 <- ct_sahie_2014 %>%
  filter(agecat==0, sexcat==0, racecat==2, iprcat==0) %>%
  select(fips, State.Name, PCTELIG) %>%
  #  mutate(PCTELIG=as.numeric(PCTELIG))
  mutate(b_percent=as.numeric(PCTELIG)) %>%
  select(fips, b_percent)

working_h_2014 <- ct_sahie_2014 %>%
  filter(agecat==0, sexcat==0, racecat==3, iprcat==0) %>%
  select(fips, State.Name, PCTELIG) %>%
  #  mutate(PCTELIG=as.numeric(PCTELIG))
  mutate(h_percent=as.numeric(PCTELIG)) %>%
  select(fips, h_percent)

working_r_2014 <- left_join(working_w_2014, working_b_2014)
working_r_2014 <- left_join(working_r_2014, working_h_2014)




working_w_2013 <- ct_sahie_2013 %>%
  filter(agecat==0, sexcat==0, racecat==1, iprcat==0, geocat==40) %>%
  select(fips, State.Name, PCTUI) %>%
  mutate(w_percent=as.numeric(PCTUI)) %>%
  select(fips, w_percent)

working_b_2013 <- ct_sahie_2013 %>%
  filter(agecat==0, sexcat==0, racecat==2, iprcat==0) %>%
  select(fips, State.Name, PCTELIG) %>%
  #  mutate(PCTELIG=as.numeric(PCTELIG))
  mutate(b_percent=as.numeric(PCTELIG)) %>%
  select(fips, b_percent)

working_h_2013 <- ct_sahie_2013 %>%
  filter(agecat==0, sexcat==0, racecat==3, iprcat==0) %>%
  select(fips, State.Name, PCTELIG) %>%
  #  mutate(PCTELIG=as.numeric(PCTELIG))
  mutate(h_percent=as.numeric(PCTELIG)) %>%
  select(fips, h_percent)

working_r_2013 <- left_join(working_w_2013, working_b_2013)
working_r_2013 <- left_join(working_r_2013, working_h_2013)


working_w_2008 <- ct_sahie_2008 %>%
  filter(agecat==0, sexcat==0, racecat==1, iprcat==0, geocat==40) %>%
  select(fips, State.Name, PCTUI) %>%
  mutate(w_percent=as.numeric(PCTUI)) %>%
  select(fips, w_percent)

working_b_2008 <- ct_sahie_2008 %>%
  filter(agecat==0, sexcat==0, racecat==2, iprcat==0) %>%
  select(fips, State.Name, PCTELIG) %>%
  #  mutate(PCTELIG=as.numeric(PCTELIG))
  mutate(b_percent=as.numeric(PCTELIG)) %>%
  select(fips, b_percent)

working_h_2008 <- ct_sahie_2008 %>%
  filter(agecat==0, sexcat==0, racecat==3, iprcat==0) %>%
  select(fips, State.Name, PCTELIG) %>%
  #  mutate(PCTELIG=as.numeric(PCTELIG))
  mutate(h_percent=as.numeric(PCTELIG)) %>%
  select(fips, h_percent)

working_r_2008 <- left_join(working_w_2008, working_b_2008)
working_r_2008 <- left_join(working_r_2008, working_h_2008)


working_w_2009 <- ct_sahie_2009 %>%
  filter(agecat==0, sexcat==0, racecat==1, iprcat==0, geocat==40) %>%
  select(fips, State.Name, PCTUI) %>%
  mutate(w_percent=as.numeric(PCTUI)) %>%
  select(fips, w_percent)

working_b_2009 <- ct_sahie_2009 %>%
  filter(agecat==0, sexcat==0, racecat==2, iprcat==0) %>%
  select(fips, State.Name, PCTELIG) %>%
  #  mutate(PCTELIG=as.numeric(PCTELIG))
  mutate(b_percent=as.numeric(PCTELIG)) %>%
  select(fips, b_percent)

working_h_2009 <- ct_sahie_2009 %>%
  filter(agecat==0, sexcat==0, racecat==3, iprcat==0) %>%
  select(fips, State.Name, PCTELIG) %>%
  #  mutate(PCTELIG=as.numeric(PCTELIG))
  mutate(h_percent=as.numeric(PCTELIG)) %>%
  select(fips, h_percent)

working_r_2009 <- left_join(working_w_2009, working_b_2009)
working_r_2009 <- left_join(working_r_2009, working_h_2009)


working_w_2010 <- ct_sahie_2010 %>%
  filter(agecat==0, sexcat==0, racecat==1, iprcat==0, geocat==40) %>%
  select(fips, State.Name, PCTUI) %>%
  mutate(w_percent=as.numeric(PCTUI)) %>%
  select(fips, w_percent)

working_b_2010 <- ct_sahie_2010 %>%
  filter(agecat==0, sexcat==0, racecat==2, iprcat==0) %>%
  select(fips, State.Name, PCTELIG) %>%
  #  mutate(PCTELIG=as.numeric(PCTELIG))
  mutate(b_percent=as.numeric(PCTELIG)) %>%
  select(fips, b_percent)

working_h_2010 <- ct_sahie_2010 %>%
  filter(agecat==0, sexcat==0, racecat==3, iprcat==0) %>%
  select(fips, State.Name, PCTELIG) %>%
  #  mutate(PCTELIG=as.numeric(PCTELIG))
  mutate(h_percent=as.numeric(PCTELIG)) %>%
  select(fips, h_percent)

working_r_2010 <- left_join(working_w_2010, working_b_2010)
working_r_2010 <- left_join(working_r_2010, working_h_2010)


working_w_2011 <- ct_sahie_2011 %>%
  filter(agecat==0, sexcat==0, racecat==1, iprcat==0, geocat==40) %>%
  select(fips, State.Name, PCTUI) %>%
  mutate(w_percent=as.numeric(PCTUI)) %>%
  select(fips, w_percent)

working_b_2011 <- ct_sahie_2011 %>%
  filter(agecat==0, sexcat==0, racecat==2, iprcat==0) %>%
  select(fips, State.Name, PCTELIG) %>%
  #  mutate(PCTELIG=as.numeric(PCTELIG))
  mutate(b_percent=as.numeric(PCTELIG)) %>%
  select(fips, b_percent)

working_h_2011 <- ct_sahie_2011 %>%
  filter(agecat==0, sexcat==0, racecat==3, iprcat==0) %>%
  select(fips, State.Name, PCTELIG) %>%
  #  mutate(PCTELIG=as.numeric(PCTELIG))
  mutate(h_percent=as.numeric(PCTELIG)) %>%
  select(fips, h_percent)

working_r_2011 <- left_join(working_w_2011, working_b_2011)
working_r_2011 <- left_join(working_r_2011, working_h_2011)


working_w_2012 <- ct_sahie_2012 %>%
  filter(agecat==0, sexcat==0, racecat==1, iprcat==0, geocat==40) %>%
  select(fips, State.Name, PCTUI) %>%
  mutate(w_percent=as.numeric(PCTUI)) %>%
  select(fips, w_percent)

working_b_2012 <- ct_sahie_2012 %>%
  filter(agecat==0, sexcat==0, racecat==2, iprcat==0) %>%
  select(fips, State.Name, PCTELIG) %>%
  #  mutate(PCTELIG=as.numeric(PCTELIG))
  mutate(b_percent=as.numeric(PCTELIG)) %>%
  select(fips, b_percent)

working_h_2012 <- ct_sahie_2012 %>%
  filter(agecat==0, sexcat==0, racecat==3, iprcat==0) %>%
  select(fips, State.Name, PCTELIG) %>%
  #  mutate(PCTELIG=as.numeric(PCTELIG))
  mutate(h_percent=as.numeric(PCTELIG)) %>%
  select(fips, h_percent)

working_r_2012 <- left_join(working_w_2012, working_b_2012)
working_r_2012 <- left_join(working_r_2012, working_h_2012)

temp1 <- data.frame(t(working_r_2013))
temp2 <- data.frame(t(working_r_2014))
temp3 <- data.frame(t(working_r_2008))
temp4 <- data.frame(t(working_r_2009))
temp5 <- data.frame(t(working_r_2010))
temp6 <- data.frame(t(working_r_2011))
temp7 <- data.frame(t(working_r_2012))


temp <- cbind(temp3, temp4, temp5, temp6, temp7, temp1, temp2)
temp$type <- rownames(temp)
rownames(temp) <- NULL
temp <- temp[-1,]

colnames(temp) <- c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "Ethnicity")
temp <- temp[c("Ethnicity", "2008", "2009", "2010", "2011", "2012", "2013", "2014")]
temp$Ethnicity <- gsub("w_percent", "White", temp$Ethnicity)
temp$Ethnicity <- gsub("b_percent", "Black", temp$Ethnicity)
temp$Ethnicity <- gsub("h_percent", "Hispanic", temp$Ethnicity)

temp2 <- gather(temp, "Year", "Uninsured", 2:8)
temp2$Uninsured <- as.numeric(temp2$Uninsured)
temp2$Year <- as.factor(temp2$Year)

source("slopegraph.r")

df <- build_slopegraph(temp2, x="Year", y="Uninsured", group="Ethnicity", method="tufte", min.space=0.05)

## Refactor the x-axis to get the right labels, round the y values for presentation
df <- transform(df, x=factor(x, levels=c(2008, 2009, 2010, 2011, 2012, 2013,2014),
                             labels=c("2008", "2009", "2010", "2011", "2012", "2013", "2014")),
                y=round(y))
##' Generate the raw plot
gg.form <- plot_slopegraph(df) +
  labs(title="Percent of uninsured by ethnicity in Connecticut",
       subtitle="Most major provisions of the Patient Protection and Affordable Care Act were phased in by January 2014.",
       caption="SOURCE: Small Area Health Insurance Estimates, U.S. Census") 
gg.form <- gg.form + theme_text(base_family="Arial Narrow")
#gg.form <- gg.form + theme(legend.position=c(0.8, 0.25))
gg.form <- gg.form + theme(plot.title=element_text(face="bold", size=14, margin=margin(b=6,l=-20)))
gg.form <- gg.form + theme(plot.subtitle=element_text(size=10, margin=margin(b=6)))
gg.form <- gg.form + theme(plot.caption=element_text(size=9, margin=margin(t=16)))

gg.form




# write.csv(working_r_2014, "data/working_r_2014.csv")
# For the population under age 65 living at or below 138 percent of poverty, 
# non-Hispanic blacks had a lower uninsured rate than non-Hispanic whites in 28 states.

# agecat==0
# sexcat==0
# racecat==3 & 2
# iprcat==3 
# PCTELIG


## CDC 2015
cdc <- read.csv("data/cdc_2015.csv", stringsAsFactors=FALSE)

cdc_under65 <- cdc %>%
  filter(Type=="Persons under 65") %>%
  filter(grepl("full year", year_and_quarter)) %>%
  mutate(year_and_quarter=gsub(" full year", "", year_and_quarter)) %>%
  select(year_and_quarter, Uninsured1, Private, Public) %>%
  gather("Year", "Type", 2:4)

colnames(cdc_under65) <- c("Year", "Type", "Percent")
cdc_under65$Type <- gsub("Uninsured1", "Uninsured", cdc_under65$Type)


df <- build_slopegraph(cdc_under65, x="Year", y="Percent", group="Type", method="tufte", min.space=0.05)

## Refactor the x-axis to get the right labels, round the y values for presentation
df <- transform(df, x=factor(x, levels=c(2010, 2011, 2012, 2013,2014, 2015),
                             labels=c("2010", "2011", "2012", "2013", "2014", "2015")),
                y=round(y))
#write.csv(df, "fixthisslope.csv")

df <- read.csv("fixthisslope.csv")

##' Generate the raw plot
gg.form <- plot_slopegraph(df) +
  labs(title="Percent of insured and uninsured in the United States",
       subtitle="In 2015, 28.6 million persons of all ages (9.1 percent) were uninsured—\nabout 7.4 million fewer persons than in 2014.",
       caption="SOURCE: NCHS, National Health Interview Survey, 1997–2015") 
gg.form <- gg.form + theme_text(base_family="Arial Narrow")
#gg.form <- gg.form + theme(legend.position=c(0.8, 0.25))
gg.form <- gg.form + theme(plot.title=element_text(face="bold", size=14, margin=margin(b=6,l=-20)))
gg.form <- gg.form + theme(plot.subtitle=element_text(size=10, margin=margin(b=6)))
gg.form <- gg.form + theme(plot.caption=element_text(size=9, margin=margin(t=16)))

gg.form

