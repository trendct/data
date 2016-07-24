library(dplyr)
library(tidyr)

## Republicans

rep_results <- read.csv("data/RepublicanPresidentialPreferencePrimary.csv", stringsAsFactors=FALSE)
rep_results <- rep_results %>%
  group_by(Town.Name, Candidate.Name) %>%
  summarise(votes=sum(Vote.Totals))

rep_results$Candidate.Name <- gsub("Donald J. Trump", "trump_count", rep_results$Candidate.Name)
rep_results$Candidate.Name <- gsub("John R. Kasich", "kasich_count", rep_results$Candidate.Name)
rep_results$Candidate.Name <- gsub("Ben Carson", "others_count", rep_results$Candidate.Name)
rep_results$Candidate.Name <- gsub("Ted Cruz", "others_count", rep_results$Candidate.Name)
rep_results$Candidate.Name <- gsub("Uncommitted", "others_count", rep_results$Candidate.Name)

rep_results <- rep_results %>%
  group_by(Town.Name, Candidate.Name) %>%
  summarise(votes=sum(votes)) %>%
  spread(Candidate.Name, votes)

rep_results$vote_diff <- rep_results$trump_count - rep_results$kasich_count
rep_results$total <- rep_results$trump_count + rep_results$kasich_count + rep_results$others_count
rep_results$trump_per <- round(rep_results$trump_count/rep_results$total*100,1)
rep_results$kasich_per <- round(rep_results$kasich_count/rep_results$total*100,1)
rep_results$others_per <- round(rep_results$others_count/rep_results$total*100,1)
rep_results$percent_diff <- rep_results$trump_per - rep_results$kasich_per
names(rep_results)[names(rep_results) == 'Town.Name'] <- 'Town'
rep_results <- data.frame(rep_results)

## Democrats

dem_results_sos <- read.csv("data/DemocraticPresidentialPreferencePrimary.csv", stringsAsFactors=FALSE)
dem_results_sos <- dem_results_sos %>%
  group_by(Town.Name, Candidate.Name) %>%
  summarise(votes=sum(Vote.Totals))

dem_results_sos$Candidate.Name <- gsub("Bernie Sanders", "sanders_count", dem_results_sos$Candidate.Name)
dem_results_sos$Candidate.Name <- gsub("Hillary Clinton", "clinton_count", dem_results_sos$Candidate.Name)
dem_results_sos$Candidate.Name <- gsub("Roque Rocky De La Fuente", "others_count", dem_results_sos$Candidate.Name)
dem_results_sos$Candidate.Name <- gsub("Uncommitted", "others_count", dem_results_sos$Candidate.Name)

dem_results_sos <- dem_results_sos %>%
  group_by(Town.Name, Candidate.Name) %>%
  summarise(votes=sum(votes)) %>%
  spread(Candidate.Name, votes)

str(dem_results_sos)
dem_results_sos$vote_diff <- dem_results_sos$clinton_count - dem_results_sos$sanders_count
dem_results_sos$total <- dem_results_sos$clinton_count + dem_results_sos$sanders_count + dem_results_sos$others_count
dem_results_sos$clinton_per <- round(dem_results_sos$clinton_count/dem_results_sos$total*100,1)
dem_results_sos$sanders_per <- round(dem_results_sos$sanders_count/dem_results_sos$total*100,1)
dem_results_sos$others_per <- round(dem_results_sos$others_count/dem_results_sos$total*100,1)
dem_results_sos$percent_diff <- dem_results_sos$clinton_per - dem_results_sos$sanders_per

names(dem_results_sos)[names(dem_results_sos) == 'Town.Name'] <- 'Town'
dem_results <- dem_results_sos


clinton <- dem_results[c("Town", "clinton_count", "sanders_count")]
trump <- rep_results[c("Town", "trump_count", "total")]

clinton_trump <- left_join(clinton, trump)
clinton_trump$ct <- ifelse(clinton_trump$trump_count > clinton_trump$clinton_count, "trump", "clinton")

table(clinton_trump$ct)

c_t <- clinton_trump %>%
  summarise(clinton=sum(clinton_count), trump=sum(trump_count))
c_t

c_t_r <- clinton_trump %>%
  summarise(clinton=sum(clinton_count), trump=sum(total))
c_t_r

clinton_trump$trump_sanders <- clinton_trump$sanders_count+clinton_trump$trump_count

clinton_trump$cts <- ifelse(clinton_trump$trump_sanders > clinton_trump$clinton_count, "trump", "clinton")

table(clinton_trump$cts)

c_t_b <- clinton_trump %>%
  summarise(clinton=sum(clinton_count), trump=sum(trump_sanders))
c_t_b
tapply(clinton_trump$trump_count, clinton_trump$cts, sum)


## 2008

p2008 <- read.csv("data/2008presidential.csv")

p2008$winner <- ifelse(p2008$McCain.Palin>p2008$Obama.Biden, "McCain.Palin", "Obama.Biden")

table(p2008$winner)

p2008_winner <- p2008 %>%
  summarise(mccain=sum(McCain.Palin), obama=sum(Obama.Biden))
p2008_winner

library(tidyr)
library(dplyr)
library(scales)
library(ggplot2)
library(ggalt)
library(stringr)
library(knitr)

library(rgdal)
require(maptools)


town_shape <- readOGR(dsn="maps", layer="ctgeo")
town_shape_df <- fortify(town_shape, region="NAME10")

town_map <- p2008

names(town_map)[names(town_map) == 'Town'] <- 'id'

voters_map <- left_join(town_shape_df, town_map)


theme_opts <- list(theme(panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank(),
                         panel.background = element_blank(),
                         plot.background = element_rect(fill="#e6e8ed"),
                         panel.border = element_blank(),
                         axis.line = element_blank(),
                         axis.text.x = element_blank(),
                         axis.text.y = element_blank(),
                         axis.ticks = element_blank(),
                         axis.title.x = element_blank(),
                         axis.title.y = element_blank(),
                         plot.title = element_text(size=22)))

# plot map
ggplot(voters_map, aes(long,lat, group=group, fill=winner)) + 
  geom_polygon() + 
  geom_path(color = "white") +
  labs(title="2008 presidential election winners by town") + 
  coord_equal() + 
  theme_opts



mccain_town_map <- p2008 %>%
  filter(winner=="McCain.Palin")

names(mccain_town_map)[names(mccain_town_map) == 'Town'] <- 'id'

mccain_voters_map <- left_join(town_shape_df, mccain_town_map)
mccain_voters_map <- mccain_voters_map[!is.na(mccain_voters_map$winner),]

# plot map
ggplot(mccain_voters_map, aes(long,lat, group=group, fill=winner)) + 
  geom_polygon() +
  geom_path(color = "white") +
  labs(title="Towns McCain and Palin won in 2008") + 
  coord_equal() + 
  theme_opts

## registered voters

reg <- read.csv("data/ACTIVE_VOTER_BY_TOWN_PARTY_072116.csv")

library(stringr)
reg$Affiliation <- gsub(" 1", "", reg$Affiliation)
reg$Affiliation <- gsub(" 2", "", reg$Affiliation)
reg$Affiliation <- gsub(" 3", "", reg$Affiliation)
reg$Affiliation <- gsub(" 4", "", reg$Affiliation)
reg$Affiliation <- str_trim(reg$Affiliation)

reg <- reg %>%
  spread(Affiliation, Registered)


# Quinnipiac

Rep	Dem
9%	85%
  79	5

(.09 * 425839) + (.85*734572) + (.36*803358)
(.79 * 425839) + (.05*734572) + (.41*803358)


Tot Rep Dem Ind Men Wom Yes No
Clinton 41% 8% 81% 30% 32% 49% 45% 38%
  Trump 36 75 6 38 45 28 33 38

(.08 * 425839) + (.81*734572) + (.30*803358)
(.75 * 425839) + (.06*734572) + (.38*803358)aZZ
