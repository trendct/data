library(stringr)
library(dplyr)
schools <- read.csv("school_lead_update.csv", stringsAsFactors=FALSE)

schools_unique <- schools[!duplicated(schools$Name),]
schools_unique$Name <- str_to_upper(schools_unique$Name)

colnames(schools_unique)[colnames(schools_unique) == 'Name'] <- 'name'

violations_schools <- left_join(actions, schools_unique)
violations_schools <- violations_schools[!is.na(violations_schools$Town),]

violations_schools <- subset(violations_schools, rule=="Lead and Copper Rule")
