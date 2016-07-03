# This script takes the data in the appendix of the CCSU CTRP3 report and creates a data frame used in our analysis and database
# http://www.ctrp3.org/reports/

library(dplyr)

searched <- read.csv("searched.csv")
searched$searched_p <- as.character(searched$searched_p)
searched$searched_p <- gsub("%", "", searched$searched_p)
searched$searched_p <- as.numeric(searched$searched_p)

hispanic_residents <- read.csv("hispanic_residents.csv", stringsAsFactors=FALSE)

for (i in 2:ncol(hispanic_residents)) {
  hispanic_residents[,i] <- as.character(hispanic_residents[,i])
  hispanic_residents[,i] <- gsub("%", "", hispanic_residents[,i])
  hispanic_residents[,i] <- as.numeric(hispanic_residents[,i])
  
}

minorities_residents <- read.csv("minorities_residents.csv", stringsAsFactors=FALSE)

for (i in 2:ncol(minorities_residents)) {
  minorities_residents[,i] <- as.character(minorities_residents[,i])
  minorities_residents[,i] <- gsub("%", "", minorities_residents[,i])
  minorities_residents[,i] <- as.numeric(minorities_residents[,i])
  
}

black_residents <- read.csv("black_residents.csv", stringsAsFactors=FALSE)

for (i in 2:ncol(black_residents)) {
  black_residents[,i] <- as.character(black_residents[,i])
  black_residents[,i] <- gsub("%", "", black_residents[,i])
  black_residents[,i] <- as.numeric(black_residents[,i])
  
}

residents_all <- left_join(hispanic_residents, minorities_residents)
residents_all <- left_join(residents_all, black_residents)

residents_all$DepartmentName <- gsub("\\*", "", residents_all$DepartmentName)

ccsu_data <- left_join(searched, residents_all)

black_edp <- read.csv("edp_black.csv", stringsAsFactors=FALSE)

for (i in 2:ncol(black_edp)) {
  black_edp[,i] <- as.character(black_edp[,i])
  black_edp[,i] <- gsub("%", "", black_edp[,i])
  black_edp[,i] <- gsub(",", "", black_edp[,i])
  black_edp[,i] <- as.numeric(black_edp[,i])
  
}


hispanic_edp <- read.csv("edp_hispanic.csv", stringsAsFactors=FALSE)

for (i in 2:ncol(hispanic_edp)) {
  hispanic_edp[,i] <- as.character(hispanic_edp[,i])
  hispanic_edp[,i] <- gsub("%", "", hispanic_edp[,i])
  hispanic_edp[,i] <- gsub(",", "", hispanic_edp[,i])
  hispanic_edp[,i] <- as.numeric(hispanic_edp[,i])
  
}


minorities_edp <- read.csv("edp_minorities.csv", stringsAsFactors=FALSE)

for (i in 2:ncol(minorities_edp)) {
  minorities_edp[,i] <- as.character(minorities_edp[,i])
  minorities_edp[,i] <- gsub("%", "", minorities_edp[,i])
  minorities_edp[,i] <- gsub(",", "", minorities_edp[,i])
  minorities_edp[,i] <- as.numeric(minorities_edp[,i])
  
}

edp_all <- left_join(black_edp, hispanic_edp)
edp_all <- left_join(edp_all, minorities_edp)
edp_all$DepartmentName <- gsub("Winchester", "Winsted", edp_all$DepartmentName)

ccsu_data<- left_join(ccsu_data, edp_all)


res_b <-  read.csv("res_b.csv", stringsAsFactors=FALSE)

for (i in 2:ncol(res_b)) {
  res_b[,i] <- as.character(res_b[,i])
  res_b[,i] <- gsub("%", "", res_b[,i])
  res_b[,i] <- gsub(",", "", res_b[,i])
  res_b[,i] <- as.numeric(res_b[,i])
  
}

res_h <-  read.csv("res_h.csv", stringsAsFactors=FALSE)

for (i in 2:ncol(res_h)) {
  res_h[,i] <- as.character(res_h[,i])
  res_h[,i] <- gsub("%", "", res_h[,i])
  res_h[,i] <- gsub(",", "", res_h[,i])
  res_h[,i] <- as.numeric(res_h[,i])
  
}


res_m <-  read.csv("res_m.csv", stringsAsFactors=FALSE)

for (i in 2:ncol(res_m)) {
  res_m[,i] <- as.character(res_m[,i])
  res_m[,i] <- gsub("%", "", res_m[,i])
  res_m[,i] <- gsub(",", "", res_m[,i])
  res_m[,i] <- as.numeric(res_m[,i])
  
}

res_all <- left_join(res_b, res_h)
res_all <- left_join(res_all, res_m)
names(res_all)[names(res_all) == 'Department.Name'] <- 'DepartmentName'
res_all$DepartmentName <- gsub("\\*", "", res_all$DepartmentName)

ccsu_data <- left_join(ccsu_data, res_all)


veil_it <- read.csv("veil_it.csv", stringsAsFactors=FALSE)
veil_it <- subset(veil_it, VOD.Estimate=="Coefficient")
colnames(veil_it) <- c("DepartmentName", "Estimate", "m_coeff", "b_coeff", "h_coeff", "b_h_coeff")
veil_it <- veil_it[c("DepartmentName", "m_coeff", "b_coeff", "h_coeff", "b_h_coeff")]

veil_it$DepartmentName <- gsub("State Police- ", "", veil_it$DepartmentName)
veil_it$DepartmentName <- gsub("All Other", "CSP Headquarters", veil_it$DepartmentName)

ccsu_data <- left_join(ccsu_data, veil_it)

veil_it_mv <- read.csv("veil_it_mv.csv", stringsAsFactors=FALSE)
veil_it_mv <- subset(veil_it_mv, VOD.Estimate=="Coefficient")
colnames(veil_it_mv) <- c("DepartmentName", "Estimate", "m_coeff_mv", "b_coeff_mv", "h_coeff_mv", "b_h_coeff_mv")
veil_it_mv <- veil_it_mv[c("DepartmentName", "m_coeff_mv", "b_coeff_mv", "h_coeff_mv", "b_h_coeff_mv")]
veil_it_mv$DepartmentName <- gsub("State Police- ", "", veil_it_mv$DepartmentName)
veil_it_mv$DepartmentName <- gsub("All Other", "CSP Headquarters", veil_it_mv$DepartmentName)

ccsu_data <- left_join(ccsu_data, veil_it_mv)

kpt_diff <- read.csv("kpt.csv", stringsAsFactors=FALSE)
kpt_diff <- subset(kpt_diff,kpt_estimate=="Differential")
colnames(kpt_diff) <- c("DepartmentName", "Estimate", "m_kpt_diff", "b_kpt_diff", "h_kpt_diff", "b_h_kpt_diff")
kpt_diff <- kpt_diff[c("DepartmentName", "m_kpt_diff", "b_kpt_diff", "h_kpt_diff", "b_h_kpt_diff")]
kpt_diff$DepartmentName <- gsub("State Police- ", "", kpt_diff$DepartmentName)
kpt_diff$DepartmentName <- gsub("All Other", "CSP Headquarters", kpt_diff$DepartmentName)

kpt_pv <- read.csv("kpt.csv", stringsAsFactors=FALSE)
kpt_pv <- subset(kpt_pv,kpt_estimate=="Chi2 P-value")
colnames(kpt_pv) <- c("DepartmentName", "Estimate", "m_kpt_pv", "b_kpt_pv", "h_kpt_pv", "b_h_kpt_pv")
kpt_pv <- kpt_pv[c("DepartmentName", "m_kpt_pv", "b_kpt_pv", "h_kpt_pv", "b_h_kpt_pv")]

kpt_ess <- read.csv("kpt.csv", stringsAsFactors=FALSE)
kpt_ess <- subset(kpt_ess,kpt_estimate=="ESS")
colnames(kpt_ess) <- c("DepartmentName", "Estimate", "m_kpt_ess", "b_kpt_ess", "h_kpt_ess", "b_h_kpt_ess")
kpt_ess <- kpt_ess[c("DepartmentName", "m_kpt_ess", "b_kpt_ess", "h_kpt_ess", "b_h_kpt_ess")]

kpt_all <- left_join(kpt_diff, kpt_pv)
kpt_all <- left_join(kpt_all, kpt_ess)

kpt_all$DepartmentName <- gsub("State Police- ", "", kpt_all$DepartmentName)
kpt_all$DepartmentName <- gsub("All Other", "CSP Headquarters", kpt_all$DepartmentName)

ccsu_data<- left_join(ccsu_data, kpt_all)

synth <- read.csv("synth.csv", stringsAsFactors=FALSE)
synth <- subset(synth, synth_control=="Coefficient")
colnames(synth) <- c("DepartmentName", "Estimate", "m_synth", "b_synth", "h_synth", "b_h_synth")
synth <- synth[c("DepartmentName", "m_synth", "b_synth", "h_synth", "b_h_synth")]
synth$DepartmentName <-gsub("Winchester", "Winsted",  synth$DepartmentName)

ccsu_data<- left_join(ccsu_data, synth)

ccsu_data$DepartmentName <- gsub("State Capitol Police", "CAPITOL POLICE", ccsu_data$DepartmentName)
ccsu_data$DepartmentName <- gsub("Central CT State University", "CCSU", ccsu_data$DepartmentName)
ccsu_data$DepartmentName <- gsub("CSP Headquarters", "State Police: Headquarters", ccsu_data$DepartmentName)
ccsu_data$DepartmentName <- gsub("Department of Motor Vehicle", "DMV", ccsu_data$DepartmentName)
ccsu_data$DepartmentName <- gsub("Eastern CT State University", "ECSU", ccsu_data$DepartmentName)
ccsu_data$DepartmentName <- gsub("Southern CT State University", "SCSU", ccsu_data$DepartmentName)
ccsu_data$DepartmentName <- gsub("Troop ", "State Police: Troop ", ccsu_data$DepartmentName)
ccsu_data$DepartmentName <- gsub("University of Connecticut", "UCONN", ccsu_data$DepartmentName)
ccsu_data$DepartmentName <- gsub("Western CT State University", "WCSU", ccsu_data$DepartmentName)
ccsu_data$DepartmentName <- gsub("Yale University", "Yale", ccsu_data$DepartmentName)

write.csv(ccsu_data, "data/ccsu_data.csv")
