# Researchers wanted to measure traffic stops not only against the number of  minority residents in a town
# but also account for commuter traffic and for drivers who work but do not live in a town. 
# Points were assigned to a department if they exceeded certain thresholds.

# *How much a town's minority population and minority traffic-stop rates departed from state averages.
# *How the number of minorities pulled over in a town compared to the town's estimated minority driving population, including commuters and those driving through.
# *How the number of minority residents pulled over in a town compared to their representation in the town population.

# These differences were calculated in previous scripts and this one merely assigns points based on those figures

# http://trafficstops.trendct.org/story/digging-deeper-into-racial-disparities-in-ct-traffic-stops/


pre_points <- read.csv("data/mega_df.csv")

pre_points$points_sa_m <- 0
pre_points$points_sa_b <- 0
pre_points$points_sa_h <- 0
pre_points$points_edp_m <- 0
pre_points$points_edp_b <- 0
pre_points$points_edp_h <- 0
pre_points$points_res_m <- 0
pre_points$points_res_b <- 0
pre_points$points_res_h <- 0


pre_points$points_sa_m <- ifelse(pre_points$m_distance>10, 1, 0)
pre_points$points_sa_b <- ifelse(pre_points$b_distance>10, 1, 0)
pre_points$points_sa_h <- ifelse(pre_points$h_distance>10, 1, 0)

pre_points$points_edp_m <- ifelse(pre_points$edp_m_diff >10, 1,0)
pre_points$points_edp_m <- ifelse(pre_points$edp_m_diff <10 & pre_points$edp_m_diff >5 & pre_points$edp_m_ratio > 1.75, .5,pre_points$points_edp_m)
pre_points$points_edp_b <- ifelse(pre_points$edp_b_diff >10, 1,0)
pre_points$points_edp_b <- ifelse(pre_points$edp_b_diff <10 & pre_points$edp_b_diff >5 & pre_points$edp_b_ratio > 1.75, .5,pre_points$points_edp_b)
pre_points$points_edp_h <- ifelse(pre_points$edp_h_diff >10, 1,0)
pre_points$points_edp_h <- ifelse(pre_points$edp_h_diff <10 & pre_points$edp_h_diff >5 & pre_points$edp_h_ratio > 1.75, .5,pre_points$points_edp_h)

pre_points$points_res_m <- ifelse(pre_points$res_diff_m >10, 1,0)
pre_points$points_res_m <- ifelse(pre_points$res_diff_m <10 & pre_points$res_diff_m >5 & pre_points$res_ratio_m > 1.75, .5,pre_points$points_res_m)
pre_points$points_res_b <- ifelse(pre_points$res_diff_b >10, 1,0)
pre_points$points_res_b <- ifelse(pre_points$res_diff_b <10 & pre_points$res_diff_b >5 & pre_points$res_ratio_b > 1.75, .5,pre_points$points_res_b)
pre_points$points_res_h <- ifelse(pre_points$res_diff_h >10, 1,0)
pre_points$points_res_h <- ifelse(pre_points$res_diff_h <10 & pre_points$res_diff_h >5 & pre_points$res_ratio_h > 1.75, .5,pre_points$points_res_h)

subset_test <- subset(pre_points, is.na(ReportingOfficerIdentificationID))

subset_test <- subset_test[c("DepartmentName", "points_sa_m", "points_sa_b", "points_sa_h", "points_edp_m", "points_edp_b", "points_edp_h", "points_res_m", "points_res_b", "points_res_h")]

subset_test$points <- rowSums(subset_test[,c(2:10)], na.rm=TRUE)
subset_test <- subset(subset_test,points>0)
subset_test$points <- NULL

library(ggplot2)
library(dplyr)
library(tidyr)

subset_test_graph <- subset_test %>%
  gather("Disparity", "Points", 2:10)

subset_test_graph$Disparity <- gsub("points_edp_b", "Black - Estimated driving population", subset_test_graph$Disparity)
subset_test_graph$Disparity <- gsub("points_edp_m", "Minorities - Estimated driving population", subset_test_graph$Disparity)
subset_test_graph$Disparity <- gsub("points_edp_h", "Hispanic - Estimated driving population", subset_test_graph$Disparity)

subset_test_graph$Disparity <- gsub("points_res_m", "Minorities - Resident population", subset_test_graph$Disparity)
subset_test_graph$Disparity <- gsub("points_res_b", "Black - Resident population", subset_test_graph$Disparity)
subset_test_graph$Disparity <- gsub("points_res_h", "Hispanic - Resident population", subset_test_graph$Disparity)

subset_test_graph$Disparity <- gsub("points_sa_m", "Minorities - Statewide average", subset_test_graph$Disparity)
subset_test_graph$Disparity <- gsub("points_sa_b", "Black - Statewide average", subset_test_graph$Disparity)
subset_test_graph$Disparity <- gsub("points_sa_h", "Hispanic - Statewide average", subset_test_graph$Disparity)

cbPalette <- c("#006d2c", "#2ca25f", "#66c2a4", "#54278f", "#756bb1", "#9e9ac8", "#b30000", "#e34a33", "#fc8d59")

ggplot(subset_test_graph, aes(x=DepartmentName, y=Points, fill=Disparity)) + scale_fill_manual(values=cbPalette) + geom_bar(stat="identity") + coord_flip() 
  
  
#   
# `Black - Estimated driving population` + `Black - Resident population` + `Black - Statewide average` + `Hispanic - Estimated driving population` +
#   `Hispanic - Resident population` + `Hispanic - Statewide average`+ `Minorities - Estimated driving population` + `Minorities - Resident population` + `Minorities - Statewide average`
