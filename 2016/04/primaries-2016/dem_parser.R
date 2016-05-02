# This script parses the democratic district election results report from the Office of the Secretary of the State

library(gdata)
library(dplyr)
library(jsonlite)
library(RCurl)

source("keys.R")
#report2 <- read.xls("data/ELECTIONVOTINGDISTRICT-1336.xls",
 #                   sheet=1)
# time <- "1711"

report <- read.xls("data/dELECTIONVOTINGDISTRICT-25.xls", sheet=1)
time <- "2205"
time2 <- "10:05 p.m."

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
    end <- i+37
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
    
    
    
    
    polls <- town_df[4:23,7:89]
    
    polls[] <- lapply(polls, as.character)
    
    for (x in 1:ncol(poll_list)) {
      poll_num <- poll_list[1,x]
      for (y in 1:ncol(polls)) {
        if (!is.na(polls[1,y]) && (sub('-.*', '', polls[1,y])==poll_num)) {
          roque_m <- polls[6,y]
          roque_a <- polls[7,y]
          roque_t <- polls[8,y]
          clinton_m <- polls[10,y]
          clinton_a <- polls[11,y]
          clinton_t <- polls[12,y]
          sanders_m <- polls[14,y]
          sanders_a <- polls[15,y]
          sanders_t <- polls[16,y] 
          uncom_m <- polls[18,y]
          uncom_a <- polls[19,y]
          uncom_t <- polls[20,y] 
          
          temp_array <- data.frame(c(town, poll_num, roque_m, roque_a, roque_t, clinton_m, clinton_a, clinton_t, 
                                     sanders_m, sanders_a, sanders_t, 
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
    colnames(row_array) <- c("town", "poll_num", "roque_m", "roque_a", "roque_t", "clinton_m", "clinton_a", "clinton_t",
                             "sanders_m", "sanders_a", "sanders_t", "uncom_m", "uncom_a", "uncom_t")
    
    mega_array <- row_array
    polls <- town_df[24:38,7:89]
    
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

dem_towns <- subset(mega_mega_array, poll_num=="Grand Total for Town")
dem_precincts <- subset(mega_mega_array, poll_num!="Grand Total for Town")

write.csv(dem_towns, paste0("data/dem_towns_", time, ".csv"))
write.csv(dem_precincts, paste0("data/dem_precincts_", time, ".csv"))

dem_towns_json <- toJSON(dem_towns)
dem_precincts_json <- toJSON(dem_precincts)

write(dem_towns_json, "json/dem_towns.json")
write(dem_precincts_json, "json/dem_precincts.json")

url_towns <- paste0("ftp://", serverkey, "/projects.ctmirror.org/content/trend/2016/04/primaries/dem_towns.json")
url_precincts <- paste0("ftp:", serverkey, "/projects.ctmirror.org/content/trend/2016/04/primaries/dem_precincts.json")


ftpUpload("json/dem_towns.json",url_towns)
ftpUpload("json/dem_precincts.json",url_precincts)

## Table time


dem_precincts$roque_m <- gsub(",", "", dem_precincts$roque_m)
dem_precincts$roque_m <- as.numeric(as.character(dem_precincts$roque_m))
dem_precincts$roque_a <- gsub(",", "", dem_precincts$roque_a)
dem_precincts$roque_a <- as.numeric(as.character(dem_precincts$roque_a))
dem_precincts$roque_t <- gsub(",", "", dem_precincts$roque_t)
dem_precincts$roque_t <- as.numeric(as.character(dem_precincts$roque_t))
dem_precincts$clinton_m <- gsub(",", "", dem_precincts$clinton_m)
dem_precincts$clinton_m <- as.numeric(as.character(dem_precincts$clinton_m))
dem_precincts$clinton_a <- gsub(",", "", dem_precincts$clinton_a)
dem_precincts$clinton_a <- as.numeric(as.character(dem_precincts$clinton_a))

dem_precincts$clinton_t <- gsub(",", "", dem_precincts$clinton_t)
dem_precincts$clinton_t <- as.numeric(as.character(dem_precincts$clinton_t))
dem_precincts$sanders_m <- gsub(",", "", dem_precincts$sanders_m)
dem_precincts$sanders_m <- as.numeric(as.character(dem_precincts$sanders_m))

dem_precincts$sanders_a <- gsub(",", "", dem_precincts$sanders_a)
dem_precincts$sanders_a <- as.numeric(as.character(dem_precincts$sanders_a))
dem_precincts$sanders_t <- gsub(",", "", dem_precincts$sanders_t)

dem_precincts$sanders_t <- as.numeric(as.character(dem_precincts$sanders_t))
dem_precincts$uncom_m <- gsub(",", "", dem_precincts$uncom_m)
dem_precincts$uncom_m <- as.numeric(as.character(dem_precincts$uncom_m))

dem_precincts$uncom_a <- gsub(",", "", dem_precincts$uncom_a)
dem_precincts$uncom_a <- as.numeric(as.character(dem_precincts$uncom_a))
dem_precincts$uncom_t <- gsub(",", "", dem_precincts$uncom_t)
dem_precincts$uncom_t <- as.numeric(as.character(dem_precincts$uncom_t))

dem_precincts$names_registry <- gsub(",", "", dem_precincts$names_registry)
dem_precincts$names_registry <- as.numeric(as.character(dem_precincts$names_registry))
dem_precincts$checked_voted <- gsub(",", "", dem_precincts$checked_voted)
dem_precincts$checked_voted <- as.numeric(as.character(dem_precincts$checked_voted))
dem_precincts$overseas <- gsub(",", "", dem_precincts$overseas)
dem_precincts$overseas <- as.numeric(as.character(dem_precincts$overseas))
dem_precincts$polling <- gsub(",", "", dem_precincts$polling)
dem_precincts$polling <- as.numeric(as.character(dem_precincts$polling))
dem_precincts$absentee <- gsub(",", "", dem_precincts$absentee)
dem_precincts$absentee <- as.numeric(as.character(dem_precincts$absentee))
dem_precincts$edr <- gsub(",", "", dem_precincts$edr)
dem_precincts$edr <- as.numeric(as.character(dem_precincts$edr))
dem_precincts$total_votes <- gsub(",", "", dem_precincts$total_votes)
dem_precincts$total_votes <- as.numeric(as.character(dem_precincts$total_votes))

dem_precincts$combined <- paste(dem_precincts$town, dem_precincts$poll_num, sep=" ")

dem_precincts <- dem_precincts[unique(dem_precincts$combined),]

precinct_count <- dem_precincts %>%
  group_by(town) %>%
  summarise(count=n())

precinct_sum <- dem_precincts %>%
  group_by(town) %>%
  summarise(total=sum(total_votes))

precinct_sum2 <- dem_precincts %>%
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

precinct_votes <- dem_precincts

precinct_votes$roque <-precinct_votes$roque_m + precinct_votes$roque_a + precinct_votes$roque_t
precinct_votes$sanders <-precinct_votes$sanders_m + precinct_votes$sanders_a + precinct_votes$sanders_t
precinct_votes$clinton <-precinct_votes$clinton_m + precinct_votes$clinton_a + precinct_votes$clinton_t
precinct_votes$poll_num <- NULL


precinct_votes <- precinct_votes %>%
  group_by(town) %>%
  summarise_each(funs(sum)) 

precinct_votes %>%
  summarise(total=sum(sanders, roque, clinton))

precinct_votes$clinton_per <- round(precinct_votes$clinton/precinct_votes$total*100, 2)
precinct_votes$roque_per <- round(precinct_votes$cruz/roque_votes$total*100, 2)
precinct_votes$sanders_per <- round(precinct_votes$sanders/precinct_votes$total*100, 2)

precinct_votes$sanders_per[is.na(precinct_votes$sanders_per)] <- 0
precinct_votes$roque_per[is.na(precinct_votes$roque_per)] <- 0
precinct_votes$clinton_per[is.na(precinct_votes$clinton_per)] <- 0

precinct_votes$sanders_per[is.nan(precinct_votes$sanders_per)] <- 0
precinct_votes$roque_per[is.nan(precinct_votes$roque_per)] <- 0
precinct_votes$clinton_per[is.nan(precinct_votes$clinton_per)] <- 0

precinct_votes <- left_join(precinct_votes, precinct_count)

precinct_votes$total <- NULL
precinct_votes$count <- NULL
precinct_votes$precincts_voted <- NULL

js_row <- ""
for (i in 1:nrow(precinct_votes)) {
  js_row <- paste0(js_row, "[\"",precinct_votes$town[i],"\", ", precinct_votes$clinton_per[i], ",", 
                   precinct_votes$sanders_per[i],  ",", precinct_votes$clinton[i], ",",
                   precinct_votes$sanders[i], ", \"", precinct_votes$tally[i], "\"],") 
}

js_row <- substr(js_row, 1, nchar(js_row)-1)


header <- paste0('{ "headline": "Town by town results", "subhead": "Unofficial results as reported to the Office of the Secretary of the State.", "data": [
                 ')

footer <- '    ], "column_names": [{ "title": "Town" }, { "title": "Clinton (%)" }, { "title": "Sanders (%)" }, { "title": "Clinton votes" }, { "title": "Sanders votes" }, { "title": "Precincts reporting" }], "height": "500", "paging": false, "sourceline": "Office of the Secretary of the State", "byline": "Andrew Ba Tran/TrendCT.org", "col": "0", "desc_asc": "asc" }'


text <- paste0(header, js_row, footer)

write(text, "json/dem_town_results.js")

url_towns <- paste0("ftp://", serverkey, "/projects.ctmirror.org/tools/fancytable/data/dem_town_results.js")


ftpUpload("json/dem_town_results.js",url_towns)
