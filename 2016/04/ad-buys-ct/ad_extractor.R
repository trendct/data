## This script scrapes the data aggregated by the Sunlight Foundation's PoliticalAdSleuth.com

## Be sure that you've installed these packages below before running the commands
library(rvest)
library(dplyr)
require(XML)
library(pdftools)

## 100 rows are listed at a time 
# The candidates have barely paid attention to CT so it was simple browse and see that 
# the scraper did not need to go deeper than two pages to get all the information sought

url1 <- "http://politicaladsleuth.com/political-files/state/CT/?page=1"
url2 <- "http://politicaladsleuth.com/political-files/state/CT/?page=2"

##  This cluster takes the link and isolates the tables

w_table <- url1 %>% read_html() %>%
  html_nodes("table.table.table-striped") %>%
  html_table()

## We just want the first table turned into a dataframe
w_table1 <- w_table[[1]]

## The table extractor did not pick up the links in the last column
# We'll have to strip those out by themselves

link <- url1 %>% read_html() %>%
  html_nodes(".table-striped a") %>%
  html_attr("href")

## Joining the table dataframe with the links 
w_table1 <- cbind(w_table1, link)

## Cleaning up the links by adding the correct url prefix
w_table1$link <- paste0("http://politicaladsleuth.com", w_table1$link)

## Repeating the steps above but for page 2
w_table <- url2 %>% read_html() %>%
  html_nodes("table.table.table-striped") %>%
  html_table()

w_table <- w_table[[1]]

link <- url2 %>% read_html() %>%
  html_nodes(".table-striped a") %>%
  html_attr("href")

w_table <- cbind(w_table, link)

w_table$link <- paste0("http://politicaladsleuth.com", w_table$link)

## Joining the two pages of datframes together
w_table1 <- rbind(w_table1, w_table)

## Throwing out anything that isn't categorized as ads for presidential campaigns
presi_table <- subset(w_table1, Type=="President")

## Alright, every link we scraped doesn't point to the document. 
# It points to an intermediate page that links to FCC document
# We need to now loop through every link and scrape the link to the FCC document

## Setting up a blank column that will be filled in with the correct link
presi_table$doc <- ""

## This is the loop that goes through each line of the link column, visits the URl, 
# And then scrapes the FCC link to the station contract/schedule

for (i in 1:nrow(presi_table)) {

  presi_table$doc[i] <- presi_table$link[i] %>% read_html() %>%
    html_nodes("#internalLinks a") %>%
    html_attr("href")
  
}

## This loop goes through the new column and downloads each PDF to the "pdfs" folder

for (i in 1:nrow(presi_table)) {
  
  url <- presi_table$doc[i]
  # replace spaces in links with URL-readable text (%20)
  url <- gsub(" ", "%20", url)
  filename <- gsub(".*\\(", "", presi_table$doc[i])
  filename <- gsub("\\).*", "", filename)
  filedate <- gsub("\\/", "_", presi_table$Date[i])
  # renaming the filename slightly so it's easier to track
  filename <- paste0("pdfs/",filedate, presi_table$`TV Station`[i], filename, "_",i, ".pdf")
  download.file(url, filename, method="auto")
  Sys.sleep(10)
}

## Adding another column to keep track of the new file name (so we can join it in the future)
presi_table$sheet <- ""

for (i in 1:nrow(presi_table)) {
  
  url <- presi_table$doc[i]
  url <- gsub(" ", "%20", url)
  filename <- gsub(".*\\(", "", presi_table$doc[i])
  filename <- gsub("\\).*", "", filename)
  filedate <- gsub("\\/", "_", presi_table$Date[i])
  presi_table$sheet[i] <- paste0(filedate, presi_table$`TV Station`[i], filename, "_",i)

}

write.csv(presi_table, "data/ads_dataframe.csv")

## Next steps

# * Split out the PDFs that just have contract info versus the PDFs with expense information
# * Convert the tables within the PDFs into a machine readable format

## I considered using the pdftools package in R but it did not extract tables well

# So I had to convert them individually using cometdocs.com
# I uploaded the PDFs to through the browswer and downloaded the files as .xlsx files
# They were downloaded to the spreadsheets folder
# Check out parser_totals.R to see the script that took the data from the spreadsheets
# and turned it into a dataframe
# A half dozen failed to convert so I had to input the information I was looking for by hand

# Options for future versions: 
# 1) Integrate Cometdocs API http://www.cometdocs.com/conversionApiTools
# 2) Use pdftools package but figure out a way to parse the text output
