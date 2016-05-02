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

# 
# rep_results <- read.csv("data/rep_results_ap.csv", stringsAsFactors=FALSE)
# 
# #rep_results$Town <- gsub(" .*", "", rep_results$Town)
# 
# rep_results$trump_per <- gsub("%.*", "", rep_results$trump)
# rep_results$trump_per <- as.numeric(rep_results$trump_per)
# rep_results$trump_count <- gsub(".*%", "", rep_results$trump)
# rep_results$trump_count <- gsub(",", "", rep_results$trump_count)
# rep_results$trump_count <- gsub("\n", "", rep_results$trump_count)
# rep_results$trump_count <- as.numeric(rep_results$trump_count)
# 
# rep_results$kasich_per <- gsub("%.*", "", rep_results$kasich)
# rep_results$kasich_per <- as.numeric(rep_results$kasich_per)
# rep_results$kasich_count <- gsub(".*%", "", rep_results$kasich)
# rep_results$kasich_count <- gsub(",", "", rep_results$kasich_count)
# rep_results$kasich_count <- gsub("\n", "", rep_results$kasich_count)
# rep_results$kasich_count <- as.numeric(rep_results$kasich_count)
# 
# rep_results$others_per <- gsub("%.*", "", rep_results$others)
# rep_results$others_per <- as.numeric(rep_results$others_per)
# rep_results$others_count <- gsub(".*%", "", rep_results$others)
# rep_results$others_count <- gsub(",", "", rep_results$others_count)
# rep_results$others_count <- gsub("\n", "", rep_results$others_count)
# rep_results$others_count <- as.numeric(rep_results$others_count)
# 
# rep_results$Town <- gsub(" of ", "", rep_results$Town)
# rep_results$Town <- gsub('[0-9]+', '', rep_results$Town)
# 
# rep_results$Town <- str_trim(rep_results$Town)
# 
# rep_results$trump <- NULL
# rep_results$kasich <- NULL
# rep_results$others <- NULL
# 
# rep_results$total <- rep_results$trump_count + rep_results$kasich_count + rep_results$others_count
# rep_results$vote_diff <- rep_results$trump_count - rep_results$kasich_count
# rep_results$percent_diff <- rep_results$trump_per - rep_results$kasich_per
