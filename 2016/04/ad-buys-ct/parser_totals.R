## This script goes through each spreadsheet in the 'spreadsheets' folder
# (which was converted from PDFs pulled via the ad_extracor.R script)
# and builds a dataframe based on how much money was spent and the time frame for each. 
# Ideally, I would have liked to also pick up the line-by-line data on when specifically
# each ad ran and how much was paid for each spot but that was too difficult to do on deadline.
# I might revisit this in the future or perhaps whoever is reading this would like to try

# Options for future versions: 
# 1) Integrate Cometdocs API http://www.cometdocs.com/conversionApiTools
# 2) Use pdftools package but figure out a way to parse the text output

#install.packages("readxl")
library(XLConnect)
library(dplyr)

# ads <- read.csv("ad_instances.csv", stringsAsFactors=FALSE)

## Getting the list of spreadsheets in the 'spreadsheets' folder

sheets <- list.files(path = "spreadsheets")
sheets <- data.frame(sheets)

## Loop to go through each spreadsheet and extract the data we're looking for
# Unfortunately, these PDFs were scanned so extracting nicely formatted tables was impossible
# This parser looks for specific keywords and patterns to isolate the basic payment and date information

# There are some lines in here to deal specifically with quirks in the data

for (t in 1:nrow(sheets)) {
  
sheet_path <- paste0("spreadsheets/", sheets[t,1])
sheet_path <- gsub("\\~", "", sheet_path)
sheet_path <- gsub("\\$", "", sheet_path)

single_sheet <- readWorksheetFromFile(sheet_path, sheet=1)

  for (i in 1:nrow(single_sheet)) {
  
    for (x in 1:ncol(single_sheet)) {
    what <- single_sheet[i,x]
    
      if (!is.na(what) && what=="Billing Cycle") {   
      who <<- (single_sheet[i-1,x])
      } 
    
      if (!is.na(what) && what=="Gross Amount") {
        line1 <- single_sheet[i+1,]
        line2 <- single_sheet[i+2,]
        
        line1 <- line1[,colSums(is.na(line1))<nrow(line1)]
        line2 <- line2[,colSums(is.na(line2))<nrow(line2)]
        l1 <- ncol(line1)
        l2 <- ncol(line2)
        if (line2[1,1] !="Totals" && l1==l2) {
        lines <- rbind(line1, line2)
        } else if ((l1 == 5) | (l1 == 6)) {
          lines <- line1
        } else {
          lines <- line2
        }
        
        if (ncol(lines)==6) {
        colnames(lines) <- c("time.period.start", "time.period.end", "spots", "gross.amount", "agency.commission", "net.amount")
        } else {
          colnames(lines) <- c("time.period.start", "time.period.end", "spots", "gross.amount", "net.amount")
          lines$agency.commission <- ""
          lines[c("time.period.start", "time.period.end", "spots", "gross.amount", "agency.commission", "net.amount")]
        }
        lines$sheet <- sheet_path      
      } 
    }
  }

  if (t > 1) {
    lines_all <- rbind(lines_all, lines)
  } else {
    lines_all <- lines
  }

}

lines_all$time.period.end <- gsub("-", "", lines_all$time.period.end)
lines_all$time.period.end <- gsub(" ", "", lines_all$time.period.end)


# Writing the results to a csv file
write.csv(lines_all, "data/ad_buys.csv")

# The relevant info from the half dozen or so PDFs that would not convert 
# to spreadsheet will now be added to lines.csv by hand