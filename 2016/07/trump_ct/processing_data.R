
# processing the town results report from the Office of the Secretary of the State

library(dplyr)
library(stringr)
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
