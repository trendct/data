library(rvest)
library(dplyr)
require(XML)


url1 <- "http://www.guroo.com/#!care-bundles/XR027-mammogram-diagnostic-digital/35980-norwich-connecticut"

us_path <- '//*[contains(concat( " ", @class, " " ), concat( " ", "col-xs-6", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "package-cost", " " ))]'
ct_path <- '//*[contains(concat( " ", @class, " " ), concat( " ", "col-xs-5", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "package-cost", " " ))]'

h1_path <- "//h1"
ct <- url1 %>% read_html() %>%
  html_nodes(xpath=h1_path) %>%
  html_text() %>%
  as.numeric()