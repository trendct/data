# This script parses the republican district election results report from the Office of the Secretary of the State


library(gdata)
library(dplyr)
library(jsonlite)
library(RCurl)

source("keys.R")
#report2 <- read.xls("data/ELECTIONVOTINGDISTRICT-1336.xls",
#                    sheet=1)

report <- read.xls("data/rELECTIONVOTINGDISTRICT-20.xls",
sheet=1)

time <- "2200"
time2 <- "10:00 p.m."

towns <- data.frame(report$X.9)
towns <- towns %>% filter(grepl("Town",report.X.9))
towns <- subset(towns, report.X.9!="")
towns$report.X.9 <- gsub("Town of ", "", towns$report.X.9)
towns$report.X.9 <- gsub(",.*", "", towns$report.X.9)


for (i in 1:nrow(report)) { 
  
  if (grepl("Town",report$X.9[i])) {
    
    town <- report$X.9[i]
    town <- gsub("Town of ", "", town)
    town <- gsub(",.*", "", town)
    start <- i
    end <- i+45
    town_df <- report[start:end,]
    poll_list <- town_df[4,]
    poll_list[] <- lapply(poll_list, as.character)
    
    colnames(poll_list) <- poll_list[1,]
    
    poll_list <- poll_list[ !duplicated(names(poll_list)) ]
    
    poll_list$`NA` <- NULL
    poll_list$`1` <- NULL
    poll_list[,1] <- NULL
    poll_list[1,] <- sub('-.*', '', poll_list[1,])
    # poll_list[1,] <- sub('-(?=[^\\-]+$).*', '', poll_list[1,], perl=T)
    
    
    
    
    polls <- town_df[4:31,7:89]
    
    polls[] <- lapply(polls, as.character)
    
    for (x in 1:ncol(poll_list)) {
      poll_num <- poll_list[1,x]
      for (y in 1:ncol(polls)) {
        if (!is.na(polls[1,y]) && (sub('-.*', '', polls[1,y])==poll_num)) {
          cruz_m <- polls[6,y]
          cruz_a <- polls[7,y]
          cruz_t <- polls[8,y]
          carson_m <- polls[10,y]
          carson_a <- polls[11,y]
          carson_t <- polls[12,y]
        trump_m <- polls[14,y]
        trump_a <- polls[15,y]
        trump_t <- polls[16,y] 
        kasich_m <- polls[18,y]
        kasich_a <- polls[19,y]
        kasich_t <- polls[20,y]
        uncom_m <- polls[22,y]
        uncom_a <- polls[23,y]
        uncom_t <- polls[24,y] 

        temp_array <- data.frame(c(town, poll_num, cruz_m, cruz_a, cruz_t, carson_m, carson_a, carson_t, 
                                   trump_m, trump_a, trump_t, kasich_m, kasich_a, kasich_t,
                                   uncom_m, uncom_a, uncom_t))
        
        if (x==1) {
          row_array <- temp_array
        } else {
          row_array <- cbind(row_array, temp_array)
        }
      }
    }
    }
    row_array <- t(row_array)
    rownames(row_array) <- NULL
    row_array <- data.frame(row_array)
    colnames(row_array) <- c("town", "poll_num", "cruz_m", "cruz_a", "cruz_t", "carson_m", "carson_a", "carson_t",
                             "trump_m", "trump_a", "trump_t", "kasich_m", "kasich_a", "kasich_t",
                              "uncom_m", "uncom_a", "uncom_t")
    
    mega_array <- row_array
    polls <- town_df[28:46,7:89]
    
    polls[] <- lapply(polls, as.character)
    
    poll_list2 <- poll_list
    poll_list2[1,] <- gsub("for Town", "for Towns", poll_list2[1,])

    
        # problems here
    
    for (z in 1:ncol(poll_list)) {
      poll_stats <- poll_list2[1,z]
      for (a in 1:ncol(polls)) { 
        if (!is.na(polls[1,a]) && (sub('-.*', '',polls[1,a])==poll_stats)) {
            names_registry <- polls[3,a]
            checked_voted <- polls[4,a]
            overseas <- polls[5,a]
            polling <- polls[8,a]
            absentee <- polls[9,a]
            edr <- polls[10,a]    
            total_votes <- polls[11,a]
            
            temp_array <- data.frame(c(poll_stats, names_registry, checked_voted, overseas,
                                       polling, absentee, edr, total_votes))
            
            if (z==1) {
              row_array <- temp_array
            } else {
              row_array <- cbind(row_array, temp_array)
            }
          }
        }
      }
    
    
      
      row_array <- t(row_array)
      rownames(row_array) <- NULL
      row_array <- data.frame(row_array)
      colnames(row_array) <- c("poll_num", "names_registry", "checked_voted", "overseas",
                               "polling", "absentee", "edr", "total_votes")
      
      row_array$poll_num <- gsub("for Towns", "for Town", row_array$poll_num)
      
      
      mega_array <- left_join(mega_array, row_array)
      }
    
    if (i==1) {
      mega_mega_array <- mega_array
    } else {
      mega_mega_array <- rbind(mega_mega_array, mega_array)
    
    }
  mega_mega_array <- unique(mega_mega_array)
  
  print(town)
}

rep_towns <- subset(mega_mega_array, poll_num=="Grand Total for Town")
rep_precincts <- subset(mega_mega_array, poll_num!="Grand Total for Town")

write.csv(rep_towns, paste0("data/rep_towns_", time, ".csv"))
write.csv(rep_precincts, paste0("data/rep_precincts_", time, ".csv"))

rep_towns_json <- toJSON(rep_towns)
rep_precincts_json <- toJSON(rep_precincts)

write(rep_towns_json, "json/rep_towns.json")
write(rep_precincts_json, "json/rep_precincts.json")

url_towns <- paste0("ftp://", serverkey, "/projects.ctmirror.org/content/trend/2016/04/primaries/rep_towns.json")
url_precincts <- paste0("ftp://", serverkey, "/projects.ctmirror.org/content/trend/2016/04/primaries/rep_precincts.json")


ftpUpload("json/rep_towns.json",url_towns)
ftpUpload("json/rep_precincts.json",url_precincts)


## tables

## table time


rep_precincts$cruz_m <- gsub(",", "", rep_precincts$cruz_m)
rep_precincts$cruz_m <- as.numeric(as.character(rep_precincts$cruz_m))
rep_precincts$cruz_a <- gsub(",", "", rep_precincts$cruz_a)
rep_precincts$cruz_a <- as.numeric(as.character(rep_precincts$cruz_a))
rep_precincts$cruz_t <- gsub(",", "", rep_precincts$cruz_t)
rep_precincts$cruz_t <- as.numeric(as.character(rep_precincts$cruz_t))
rep_precincts$carson_m <- gsub(",", "", rep_precincts$carson_m)
rep_precincts$carson_m <- as.numeric(as.character(rep_precincts$carson_m))
rep_precincts$carson_a <- gsub(",", "", rep_precincts$carson_a)
rep_precincts$carson_a <- as.numeric(as.character(rep_precincts$carson_a))

rep_precincts$carson_t <- gsub(",", "", rep_precincts$carson_t)
rep_precincts$carson_t <- as.numeric(as.character(rep_precincts$carson_t))
rep_precincts$trump_m <- gsub(",", "", rep_precincts$trump_m)
rep_precincts$trump_m <- as.numeric(as.character(rep_precincts$trump_m))

rep_precincts$trump_a <- gsub(",", "", rep_precincts$trump_a)
rep_precincts$trump_a <- as.numeric(as.character(rep_precincts$trump_a))
rep_precincts$trump_t <- gsub(",", "", rep_precincts$trump_t)

rep_precincts$trump_t <- as.numeric(as.character(rep_precincts$trump_t))
rep_precincts$kasich_m <- gsub(",", "", rep_precincts$kasich_m)
rep_precincts$kasich_m <- as.numeric(as.character(rep_precincts$kasich_m))

rep_precincts$kasich_a <- gsub(",", "", rep_precincts$kasich_a)
rep_precincts$kasich_a <- as.numeric(as.character(rep_precincts$kasich_a))
rep_precincts$kasich_t <- gsub(",", "", rep_precincts$kasich_t)
rep_precincts$kasich_t <- as.numeric(as.character(rep_precincts$kasich_t))
rep_precincts$uncom_m <- gsub(",", "", rep_precincts$uncom_m)
rep_precincts$uncom_m <- as.numeric(as.character(rep_precincts$uncom_m))
rep_precincts$uncom_a <- gsub(",", "", rep_precincts$uncom_a)
rep_precincts$uncom_a <- as.numeric(as.character(rep_precincts$uncom_a))
rep_precincts$uncom_t <- gsub(",", "", rep_precincts$uncom_t)
rep_precincts$uncom_t <- as.numeric(as.character(rep_precincts$uncom_t))
rep_precincts$names_registry <- gsub(",", "", rep_precincts$names_registry)
rep_precincts$names_registry <- as.numeric(as.character(rep_precincts$names_registry))
rep_precincts$checked_voted <- gsub(",", "", rep_precincts$checked_voted)
rep_precincts$checked_voted <- as.numeric(as.character(rep_precincts$checked_voted))
rep_precincts$overseas <- gsub(",", "", rep_precincts$overseas)
rep_precincts$overseas <- as.numeric(as.character(rep_precincts$overseas))
rep_precincts$polling <- gsub(",", "", rep_precincts$polling)
rep_precincts$polling <- as.numeric(as.character(rep_precincts$polling))
rep_precincts$absentee <- gsub(",", "", rep_precincts$absentee)
rep_precincts$absentee <- as.numeric(as.character(rep_precincts$absentee))
rep_precincts$edr <- gsub(",", "", rep_precincts$edr)
rep_precincts$edr <- as.numeric(as.character(rep_precincts$edr))
rep_precincts$total_votes <- gsub(",", "", rep_precincts$total_votes)
rep_precincts$total_votes <- as.numeric(as.character(rep_precincts$total_votes))

precinct_count <- rep_precincts %>%
  group_by(town) %>%
  summarise(count=n())

precinct_sum <- rep_precincts %>%
  group_by(town) %>%
  summarise(total=sum(total_votes))

precinct_sum2 <- rep_precincts %>%
  group_by(town) %>%
  filter(total_votes > 0) 

precinct_sum2 <- precinct_sum2 %>%
  group_by(town) %>%
  summarise(precincts_voted=n())
  
if (nrow(precinct_sum2>0)) {

  precinct_count <- left_join(precinct_count, precinct_sum2)  
  precinct_count$precincts_voted[is.na(precinct_count$precincts_voted)] <- 0
  
} else {
  precinct_count$precincts_voted <-0
}

precinct_count$tally <- paste0(precinct_count$precincts_voted, " of ", precinct_count$count)

precinct_votes <- rep_precincts

precinct_votes$cruz <-precinct_votes$cruz_m + precinct_votes$cruz_a + precinct_votes$cruz_t
precinct_votes$trump <-precinct_votes$trump_m + precinct_votes$trump_a + precinct_votes$trump_t
precinct_votes$kasich <-precinct_votes$kasich_m + precinct_votes$kasich_a + precinct_votes$kasich_t
precinct_votes$poll_num <- NULL

precinct_votes <- precinct_votes %>%
  group_by(town) %>%
  summarise_each(funs(sum)) 
  
precinct_votes %>%
  summarise(total=sum(trump, cruz, kasich))

precinct_votes$trump_per <- round(precinct_votes$trump/precinct_votes$total*100, 2)
precinct_votes$cruz_per <- round(precinct_votes$cruz/precinct_votes$total*100, 2)
precinct_votes$kasich_per <- round(precinct_votes$kasich/precinct_votes$total*100, 2)

precinct_votes$trump_per[is.na(precinct_votes$trump_per)] <- 0
precinct_votes$cruz_per[is.na(precinct_votes$cruz_per)] <- 0
precinct_votes$kasich_per[is.na(precinct_votes$kasich_per)] <- 0

precinct_votes$trump_per[is.nan(precinct_votes$trump_per)] <- 0
precinct_votes$cruz_per[is.nan(precinct_votes$cruz_per)] <- 0
precinct_votes$kasich_per[is.nan(precinct_votes$kasich_per)] <- 0

precinct_votes <- left_join(precinct_votes, precinct_count)

precinct_votes$total <- NULL
precinct_votes$count <- NULL
precinct_votes$precincts_voted <- NULL

js_row <- ""
for (i in 1:nrow(precinct_votes)) {
  js_row <- paste0(js_row, "[\"",precinct_votes$town[i],"\", ", precinct_votes$trump_per[i], ",", 
                   precinct_votes$cruz_per[i],  ",", precinct_votes$kasich_per[i], ",",
                    precinct_votes$trump[i], ",", precinct_votes$cruz[i],  ",", 
                    precinct_votes$kasich[i], ", \"", precinct_votes$tally[i], "\"],") 
}

js_row <- substr(js_row, 1, nchar(js_row)-1)


header <- paste0('{ "headline": "Town by town results", "subhead": "Unofficial results as reported to the Office of the Secretary of the State.", "data": [
')

footer <- '    ], "column_names": [{ "title": "Town" }, { "title": "Trump (%)" }, { "title": "Cruz (%)" }, { "title": "Kasich (%)" }, { "title": "Trump votes" }, { "title": "Cruz votes" }, { "title": "Kasich votes" }, { "title": "Precincts reporting" }], "height": "500", "paging": false, "sourceline": "Office of the Secretary of the State", "byline": "Andrew Ba Tran/TrendCT.org", "col": "0", "desc_asc": "asc" }'


text <- paste0(header, js_row, footer)

write(text, "rep_town_results.js")

url_towns <- paste0("ftp://", serverkey, "/projects.ctmirror.org/tools/fancytable/data/rep_town_results.js")


ftpUpload("rep_town_results.js",url_towns)
