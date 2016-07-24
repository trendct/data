

colleges <- read.csv("colleges_ct2.csv", stringsAsFactors=FALSE)

# Admission rate

rate <- colleges[c("INSTNM", "Admission.rate")]
rate <- na.omit(rate)

## Highest

rate_high <- head(rate[order(-rate$Admission.rate),], 5)

trendchart(rate_high, headline = "Institutions with the highest acceptance rates in Connecticut", subhead = "For 2013 data where available.", src = "US Department of Education",
           byline = "TrendCT.org", type = "column", xTitle = "", yTitle = "",
           xSuffix = "", ySuffix = "%", xPrefix = "", yPrefix = "", option = "")

# Lowest

rate_low <- head(rate[order(rate$Admission.rate),], 5)

trendchart(rate_low, headline = "Institutions with the lowest acceptance rates in Connecticut", subhead = "For 2013 data where available.", src = "US Department of Education",
           byline = "TrendCT.org", type = "column", xTitle = "", yTitle = "",
           xSuffix = "", ySuffix = "%", xPrefix = "", yPrefix = "", option = "")

# Completion rate

rate <- colleges[c("INSTNM", "Completion")]
rate <- na.omit(rate)

## Highest

rate_high <- head(rate[order(-rate$Completion),], 5)

trendchart(rate_high, headline = "Institutions with the highest completion rates in Connecticut", subhead = "Within 150 percent of the expected time. For 2013 data where available.", src = "US Department of Education",
           byline = "TrendCT.org", type = "column", xTitle = "", yTitle = "",
           xSuffix = "", ySuffix = "%", xPrefix = "", yPrefix = "", option = "")

# Lowest

rate_low <- head(rate[order(rate$Completion),], 5)

trendchart(rate_low, headline = "Institutions with the lowest completion rates in Connecticut", subhead = "Within 150 percent of the expected time. For 2013 data where available.", src = "US Department of Education",
           byline = "TrendCT.org", type = "column", xTitle = "", yTitle = "",
           xSuffix = "", ySuffix = "%", xPrefix = "", yPrefix = "", option = "")


# Cost

rate <- colleges[c("INSTNM", "Cost")]
rate <- na.omit(rate)

## Highest

rate_high <- head(rate[order(-rate$Cost),], 5)
colnames(rate_high) <- c("Institute", "Average cost")

trendchart(rate_high, headline = "Institutions with the highest average cost in Connecticut", subhead = "For 2013 data where available.", src = "US Department of Education",
           byline = "TrendCT.org", type = "column", xTitle = "", yTitle = "",
           xSuffix = "", ySuffix = "", xPrefix = "", yPrefix = "$", option = "")

# Lowest

rate_low <- head(rate[order(rate$Cost),], 5)
colnames(rate_low) <- c("Institute", "Average cost")

trendchart(rate_low, headline = "Institutions with the lowest average cost in Connecticut", subhead = "For 2013 data where available.", src = "US Department of Education",
           byline = "TrendCT.org", type = "column", xTitle = "", yTitle = "",
           xSuffix = "", ySuffix = "", xPrefix = "", yPrefix = "$", option = "")
# Debt

rate <- colleges[c("INSTNM", "GRAD_DEBT_MDN")]
rate <- na.omit(rate)

## Highest

rate_high <- head(rate[order(-rate$GRAD_DEBT_MDN),], 5)
colnames(rate_high) <- c("Institute", "Median grad debt")

trendchart(rate_high, headline = "Graduates with the highest median debt in Connecticut", subhead = "For 2013 data where available.", src = "US Department of Education",
           byline = "TrendCT.org", type = "column", xTitle = "", yTitle = "",
           xSuffix = "", ySuffix = "", xPrefix = "", yPrefix = "$", option = "")

# Lowest

rate_low <- head(rate[order(rate$GRAD_DEBT_MDN),], 5)
colnames(rate_low) <- c("Institute", "Median grad debt")

trendchart(rate_low, headline = "Graduates with the least median debt in Connecticut", subhead = "For 2013 data where available.", src = "US Department of Education",
           byline = "TrendCT.org", type = "column", xTitle = "", yTitle = "",
           xSuffix = "", ySuffix = "", xPrefix = "", yPrefix = "$", option = "")

# Earnings

rate <- colleges[c("INSTNM", "md_earn_wne_p10")]
rate <- na.omit(rate)

## Highest

rate_high <- head(rate[order(-rate$md_earn_wne_p10),], 5)
colnames(rate_high) <- c("Institute", "Median earnings")

trendchart(rate_high, headline = "Graduates with the highest median earnings in Connecticut", subhead = "After 10 years of enrolling in 2001.", src = "US Department of Education",
           byline = "TrendCT.org", type = "column", xTitle = "", yTitle = "",
           xSuffix = "", ySuffix = "", xPrefix = "", yPrefix = "$", option = "")

# Lowest

rate_low <- head(rate[order(rate$md_earn_wne_p10),], 5)

colnames(rate_low) <- c("Institute", "Median earnings")

trendchart(rate_low, headline = "Graduates with the least median earnings in Connecticut", subhead = "After 10 years of enrolling in 2001.", src = "US Department of Education",
           byline = "TrendCT.org", type = "column", xTitle = "", yTitle = "",
           xSuffix = "", ySuffix = "", xPrefix = "", yPrefix = "$", option = "")

# The investment

debtvsearn <- colleges[c("INSTNM", "md_earn_wne_p10", "GRAD_DEBT_MDN")]

trendchart(debtvsearn, headline = "Student earnings to student debt ratio", subhead = "After 10 years of enrolling in 2001.", src = "US Department of Education",
           byline = "TrendCT.org", type = "scatter", xTitle = "Median salary 10 years after enrolling", yTitle = "Median graduation debt",
           xSuffix = "", ySuffix = "", xPrefix = "$", yPrefix = "$", option = "")

cor(debtvsearn$GRAD_DEBT_MDN, debtvsearn$md_earn_wne_p10)

debtvsearn$edratio <- round((debtvsearn$md_earn_wne_p10/debtvsearn$GRAD_DEBT_MDN),2)

the_ratio <- debtvsearn[c("INSTNM", "edratio")]

admission <- colleges[c("INSTNM", "Admission.rate")]
sat <- colleges[c("INSTNM", "SAT_AVG")]
comp <- colleges[c("INSTNM", "Completion")]
cost <- colleges[c("INSTNM", "Cost")]

debtvsearn <- left_join(debtvsearn, admission)
debtvsearn <- left_join(debtvsearn, sat)
debtvsearn <- left_join(debtvsearn, comp)
debtvsearn <- left_join(debtvsearn, cost)

cor(debtvsearn$edratio, debtvsearn$Admission.rate,  use = "complete.obs")

cor(debtvsearn$edratio, debtvsearn$SAT_AVG,  use = "complete.obs")

adm_chart <- debtvsearn[c("INSTNM", "Admission.rate", "edratio")]

adm_chart <- na.omit(adm_chart)

# Down to 23

trendchart(adm_chart, headline = "Correlation between admission rate and earnings to debt ratio in CT", subhead = "", src = "Source: US Department of Education",
           byline = "TrendCT.org", type = "scatter", xTitle = "Admission rate", yTitle = "Earnings to debt ratio",
           xSuffix = "%", ySuffix = "", xPrefix = "", yPrefix = "", option = "")

# OK, TIME FOR COUNTRYWIDE

small_us <- y2013[c("UNITID", "INSTNM", "GRAD_DEBT_MDN", "SAT_AVG", "ADM_RATE")]
small_us <- left_join(small_us, earnings)

small_us$GRAD_DEBT_MDN <- gsub("PrivacySuppressed", "", small_us$GRAD_DEBT_MDN)
small_us$GRAD_DEBT_MDN <- gsub("NULL", "", small_us$GRAD_DEBT_MDN)

small_us$md_earn_wne_p10 <- gsub("PrivacySuppressed", "", small_us$md_earn_wne_p10)
small_us$md_earn_wne_p10 <- gsub("NULL", "", small_us$md_earn_wne_p10)

small_us$SAT_AVG <- gsub("NULL", "", small_us$SAT_AVG)
small_us$ADM_RATE <- gsub("NULL", "", small_us$ADM_RATE)

small_us$md_earn_wne_p10 <- as.numeric(small_us$md_earn_wne_p10)
small_us$GRAD_DEBT_MDN <- as.numeric(small_us$GRAD_DEBT_MDN)
small_us$SAT_AVG <- as.numeric(small_us$SAT_AVG) 
small_us$ADM_RATE <- as.numeric(small_us$ADM_RATE) 

small_us$edratio <- round((small_us$md_earn_wne_p10/small_us$GRAD_DEBT_MDN),2)

cor(small_us$edratio, small_us$SAT_AVG,  use = "complete.obs")

cor(small_us$edratio, small_us$ADM_RATE,  use = "complete.obs")

small_us_adm <- small_us[c("INSTNM", "ADM_RATE", "edratio")]
small_us_adm <- na.omit(small_us_adm)
small_us_adm$ADM_RATE <- round(small_us_adm$ADM_RATE*100,2)

cor(small_us_adm$edratio, small_us_adm$ADM_RATE)
trendchart(small_us_adm, headline = "Correlation between admission rate and earnings to debt ratio", subhead = "Across the country", src = "Source: US Department of Education",
           byline = "TrendCT.org", type = "scatter", xTitle = "Admission rate", yTitle = "Earnings to debt ratio",
           xSuffix = "%", ySuffix = "", xPrefix = "", yPrefix = "", option = "")


sat_chart <- debtvsearn[c("INSTNM", "SAT_AVG", "edratio")]

sat_chart <- na.omit(sat_chart)

# Down to 17

trendchart(sat_chart, headline = "Correlation between SAT average score and earnings to debt ratio in CT", subhead = "", src = "Source: US Department of Education",
           byline = "TrendCT.org", type = "scatter", xTitle = "Average SAT score", yTitle = "Earnings to debt ratio",
           xSuffix = "", ySuffix = "", xPrefix = "", yPrefix = "", option = "")

small_us_sat <- small_us[c("INSTNM", "SAT_AVG", "edratio")]
small_us_sat <- na.omit(small_us_sat)

trendchart(small_us_sat, headline = "Correlation between SAT average score and earnings to debt ratio", subhead = "Across the country", src = "Source: US Department of Education",
           byline = "TrendCT.org", type = "scatter", xTitle = "Average SAT score", yTitle = "Earnings to debt ratio",
           xSuffix = "", ySuffix = "", xPrefix = "", yPrefix = "", option = "")

cor(small_us_sat$edratio, small_us_sat$SAT_AVG)
cor(debtvsearn$edratio, debtvsearn$Completion,  use = "complete.obs")
cor(debtvsearn$edratio, debtvsearn$Cost,  use = "complete.obs")

the_ratio <- debtvsearn[c("INSTNM", "edratio")]

rate_top <- head(the_ratio[order(-the_ratio$edratio),], 10)

rate_bottom <- head(the_ratio[order(the_ratio$edratio),], 10)

colnames(rate_top) <- c("Institution", "Median salary to debt ratio")

colnames(rate_bottom) <- c("Institution", "Median salary to debt ratio")

trendchart(rate_top, headline = "Institutions in Connecticut with the largest median salary to graduation debt ratio", subhead = "Median salary based on 10 years after enrollment in 2001.", src = "US Department of Education",
           byline = "TrendCT.org", type = "bar", xTitle = "", yTitle = "",
           xSuffix = "", ySuffix = "", xPrefix = "", yPrefix = "", option = "")

trendchart(rate_bottom, headline = "Institutions in Connecticut with the smallest median salary to graduation debt ratio", subhead = "Median salary based on 10 years after enrollment in 2001.", src = "US Department of Education",
           byline = "TrendCT.org", type = "bar", xTitle = "", yTitle = "",
           xSuffix = "", ySuffix = "", xPrefix = "", yPrefix = "", option = "")