library(rvest)
library(dplyr)


## Scraping water sources list from the EPA
## Change the state variable to pull the list from a different state
state <- "CT"
url <- paste0("https://oaspub.epa.gov/enviro/sdw_query_v3.get_list?wsys_name=&fac_search=fac_beginning&fac_county=&fac_city=&pop_serv=500&pop_serv=3300&pop_serv=10000&pop_serv=100000&pop_serv=100001&sys_status=active&pop_serv=&wsys_id=&fac_state=", state ,"&last_fac_name=&page=1&query_results=&total_rows_found=")

link_path <- "//*[@id='results2']/tbody/tr[1]/td[1]/a"

w_table <- url %>% read_html() %>%
  html_nodes(xpath='//*[@id="results2"]') %>%
  html_table()

w_table1 <- w_table[[1]]
w_table2 <- w_table[[2]]
w_table3 <- w_table[[3]]

w_table1$type <- "Community Water Systems"
w_table2$type <- "Non-Transient Non-Community Water Systems"
w_table3$type <- "Transient Non-Community Water Systems"

w_table_all <- rbind(w_table1, w_table2, w_table3)

colnames(w_table_all) <- c("name", "county.s.served", "city.s.served", 
                           "population.served", "primary.water.source.type",
                           "pws.activity", "water.system.id", "type")

w_table_all$source.adjusted <- gsub(" ", "%20", w_table_all$primary.water.source.type)

w_table_all$link <- paste0("https://oaspub.epa.gov/enviro/sdw_report_v3.first_table?pws_id=",
                           w_table_all$water.system.id, "&state=CT&source=",
                           w_table_all$source.adjusted, "&population=", 
                           w_table_all$population.served)

