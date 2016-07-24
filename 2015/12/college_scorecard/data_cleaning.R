
y2013 <- read.csv("MERGED2013_PP.csv", stringsAsFactors=FALSE)

ct2013 <- subset(y2013, STABBR=="CT")

ctagencies <- data.frame(table(ct2013$AccredAgency))

# aid checks out

aid_list <- c("DEBT_MDN_SUPP",
              "GRAD_DEBT_MDN_SUPP",
              "GRAD_DEBT_MDN10YR_SUPP")

aid <- ct2013[aid_list]


#degree seeking
ct_degrees <- ct2013[c("UNITID", "INSTNM",
                           "CITY",
                           "ZIP",
                           "AccredAgency",
                           "INSTURL",
                           "LATITUDE",
                           "LONGITUDE",
                           "ADM_RATE",
                           "SAT_AVG",
                           "PCTPELL",
                           "C150_4",
                       "C150_L4",
                       "C150_4_POOLED",
                       "C150_L4_POOLED",
                           "UGDS",
                           "UGDS_WHITE",
                           "UGDS_BLACK",
                           "UGDS_HISP",
                           "UGDS_ASIAN",
                           "UGDS_AIAN",
                           "UGDS_NHPI",
                           "UGDS_2MOR",
                           "UGDS_NRA",
                           "UGDS_UNKN",
                           "NPT4_PUB",
                           "NPT4_PRIV",
                           "NUM4_PUB",
                           "NUM4_PRIV",
                           "COSTT4_A",
                           "COSTT4_P",
                           "TUITIONFEE_IN",
                           "TUITIONFEE_OUT",
                           "AVGFACSAL",
                           "PFTFAC",
                           "PCTPELL",
                           "PCTFLOAN",
                           "PAR_ED_PCT_1STGEN",
                           "PAR_ED_PCT_MS",
                           "PAR_ED_PCT_HS",
                           "PAR_ED_PCT_PS",
                           "GRAD_DEBT_MDN",
                           "WDRAW_DEBT_MDN",
                           "LO_INC_DEBT_MDN",
                           "MD_INC_DEBT_MDN",
                           "HI_INC_DEBT_MDN",
                           "PELL_DEBT_MDN",
                           "NOPELL_DEBT_MDN",
                           "FEMALE_DEBT_MDN",
                           "MALE_DEBT_MDN",
                           "FIRSTGEN_DEBT_MDN",
                           "NOTFIRSTGEN_DEBT_MDN")]

var <- read.csv("variables.csv", stringsAsFactors=FALSE)
var_list <- read.csv("variables_list.csv", stringsAsFactors=FALSE)

library(dplyr)

var_new <- left_join(var_list, var)

# Earnings

earnings <- read.csv("Most+Recent+Cohorts+(Treasury+Elements).csv", stringsAsFactors=FALSE)
earnings <- earnings[c("UNITID", "md_earn_wne_p10")]

ct_combined <- left_join(ct_degrees, earnings)

completion <- C150_4 + C150_L4