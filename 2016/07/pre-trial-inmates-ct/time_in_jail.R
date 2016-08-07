# First Day
# Last Day
# Length of stay
library(dplyr)
library(lubridate)

update <- read.csv("https://data.ct.gov/api/views/b674-jy6w/rows.csv")


days_list <- unique(update$DOWNLOAD.DATE)
update$auth.date <- mdy(update$DOWNLOAD.DATE)
update$first.date <- ifelse(mdy(update$LATEST.ADMISSION.DATE)>mdy("07/01/2016"), as.character(mdy(update$LATEST.ADMISSION.DATE)), as.character(mdy("07/01/2016"))) 
update$last.date <- ""

#3427

for (i in 2:length(days_list)) {
  date <- days_list[i-1]
  update_sub <- subset(update, DOWNLOAD.DATE==date)
  update_sub_solo <- update_sub[c("DOWNLOAD.DATE", "IDENTIFIER", "first.date")]
  
  date2 <- days_list[i]
  update_sub2 <- subset(update, DOWNLOAD.DATE==date2)
  update_sub_solo2 <- update_sub2[c("DOWNLOAD.DATE", "IDENTIFIER", "first.date")]
  update_sub_combined <- full_join(update_sub_solo, update_sub_solo2, by="IDENTIFIER")

  update_sub_combined$first.date <- ifelse(is.na(update_sub_combined$DOWNLOAD.DATE.x), as.character(mdy(as.character(update_sub_combined$DOWNLOAD.DATE.y))), as.character(ymd(update_sub_combined$first.date.x)))
  update_sub_combined$last.date <- ifelse(is.na(update_sub_combined$DOWNLOAD.DATE.y), as.character(mdy(as.character(update_sub_combined$DOWNLOAD.DATE.x))), "")
  
  update_sub_combined <- update_sub_combined[c("IDENTIFIER","first.date","last.date")]
  
  if (i == 2) {
    update_all <- update_sub_combined
  } else {
    update_all <- rbind(update_all, update_sub_combined)
  }
  update_all_complete <- update_all[!duplicated(update_all), ]
  
}

dupes <- update_all_complete[duplicated(update_all_complete$IDENTIFIER),]
dupes$dupe <- "duplicate"
dupes <- dupes[c("IDENTIFIER", "dupe")]

update_all_complete <- left_join(update_all_complete, dupes)

duped <- subset(update_all_complete, dupe=="duplicate")
duped <- arrange(duped, IDENTIFIER)

for (i in 1:nrow(duped)) {
  if ((duped$IDENTIFIER[i]== duped$IDENTIFIER[i+1]) & (i !=nrow(duped))) {
    
    if (duped$first.date[i] == duped$first.date[i+1]) {
      duped$last.date[i] <- ifelse(duped$last.date[i]=="", duped$last.date[i+1], duped$last.date[i])
      duped$last.date[i+1] <- ifelse(duped$last.date[i]=="", duped$last.date[i+1], duped$last.date[i])
      
    }
    
    if (duped$first.date[i] > duped$first.date[i+1]) {
      duped$first.date[i] <- duped$first.date[i+1]
    }
    
    if (duped$first.date[i] < duped$first.date[i+1]) {
      duped$first.date[i + 1] <- duped$first.date[i]
    }
    
  }
  
}

duped <- unique(duped)

update_all_complete_final <- subset(update_all_complete, is.na(dupe))
update_all_complete_final <- rbind(update_all_complete_final, duped)

update$auth.date <- NULL
update$first.date <- NULL
update$last.date <- NULL

update <- left_join(update, update_all_complete_final, by="IDENTIFIER")
update$dupe <- NULL
update$first.date[update$first.date == "2016-07-01"] <- NA
update$total.days <- ymd(update$last.date) - ymd(update$first.date)

write.csv(table(update$total.days, update$RACE), "race_days.csv")
tapply(update$total.days, update$RACE, median, na.rm=T)
tapply(update$total.days, update$RACE, mean, na.rm=T)

out <- subset(update, !is.na(total.days))
still_in <- subset(update, is.na(total.days))
out_yet <- subset(still_in, last.date=="")
still_in <- subset(still_in, last.date!="")


out2 <- count(out[unique(out$IDENTIFIER),])
out_yet2 <- count(out_yet[unique(out_yet$IDENTIFIER),])
still_in2 <- count(still_in[unique(still_in$IDENTIFIER),])

total_inmates <- length(unique(update$IDENTIFIER))

still_in[unique(still_in$IDENTIFIER),] %>%
  group_by(RACE) %>%
  summarise(count=n())

out[unique(out$IDENTIFIER),] %>%
  group_by(RACE) %>%
  summarise(count=n())

out_yet[unique(out_yet$IDENTIFIER),] %>%
  group_by(RACE) %>%
  summarise(count=n())

cat("total: ")
