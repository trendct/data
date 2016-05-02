serverkey <- "breorz:CtMirror2013!@secure.ctmirror.org"


# dem_results <- read.csv("data/dem_results_ap.csv", stringsAsFactors=FALSE)
# 
# #dem_results$Town <- gsub(" .*", "", dem_results$Town)
# 
# dem_results$clinton_per <- gsub("%.*", "", dem_results$Clinton)
# dem_results$clinton_per <- as.numeric(dem_results$clinton_per)
# dem_results$clinton_count <- gsub(".*%", "", dem_results$Clinton)
# dem_results$clinton_count <- gsub(",", "", dem_results$clinton_count)
# dem_results$clinton_count <- gsub("\n", "", dem_results$clinton_count)
# dem_results$clinton_count <- as.numeric(dem_results$clinton_count)
# 
# dem_results$sanders_per <- gsub("%.*", "", dem_results$Sanders)
# dem_results$sanders_per <- as.numeric(dem_results$sanders_per)
# dem_results$sanders_count <- gsub(".*%", "", dem_results$Sanders)
# dem_results$sanders_count <- gsub(",", "", dem_results$sanders_count)
# dem_results$sanders_count <- gsub("\n", "", dem_results$sanders_count)
# dem_results$sanders_count <- as.numeric(dem_results$sanders_count)
# 
# dem_results$others_per <- gsub("%.*", "", dem_results$Others)
# dem_results$others_per <- as.numeric(dem_results$others_per)
# dem_results$others_count <- gsub(".*%", "", dem_results$Others)
# dem_results$others_count <- gsub(",", "", dem_results$others_count)
# dem_results$others_count <- gsub("\n", "", dem_results$others_count)
# dem_results$others_count <- as.numeric(dem_results$others_count)
# 
# dem_results$Town <- gsub(" of ", "", dem_results$Town)
# dem_results$Town <- gsub('[0-9]+', '', dem_results$Town)
# 
# dem_results$Town <- str_trim(dem_results$Town)
# 
# dem_results$Clinton <- NULL
# dem_results$Sanders <- NULL
# dem_results$Others <- NULL
# 
# dem_results$total <- dem_results$clinton_count + dem_results$sanders_count + dem_results$others_count
# dem_results$vote_diff <- dem_results$clinton_count - dem_results$sanders_count
# dem_results$percent_diff <- dem_results$clinton_per - dem_results$sanders_per
