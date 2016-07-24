# install.packages("plyr") first
# install.packages("readxl") first
# install.packages("xlsx") first
# install.packages("lubridate") first
# install.packages("dplyr") first

library(plyr)
library(readxl)
library(xlsx)
library(lubridate)
library(dplyr)

# FILE NAMES
old <- read_excel("JobExecutionSearchResults-Old-Prod-12222015.xls", sheet = "SearchResults")
new <- read_excel("JobExecutionSearchResults-New-Prod-12222015.xls", sheet = "SearchResults")

# START TIME AND END TIME HERE
start_time <-  mdy_hms("12/22/2015 09:00:00")
end_time <- mdy_hms("12/23/2015 08:00:00")

## Cleaning up Old data
old_2 <- subset(old, Status!="PENDING")
old_2 <- subset(old_2, `Job Instance Code`!="CP3.2-P3A")
old_2 <- subset(old_2, `Job Instance Code`!="CL3")
old_2 <- subset(old_2, `Job Instance Code`!="CP3.2-P3B")
old_2 <- subset(old_2, `Job Instance Code`!="CP3.2-P3C")
old_2 <- subset(old_2, `Job Instance Code`!="CP3.2-P3D")

old_2$`Execution Start TS` <- mdy_hms(old_2$`Execution Start TS`)
old_2$`Execution End TS` <- mdy_hms(old_2$`Execution End TS`)





old_3 <- subset(old_2, (`Execution Start TS`>=start_time & `Execution Start TS`<=end_time)) 

## Cleaning up New data

new_2 <- subset(new, Status!="PENDING")
new_2 <- subset(new_2, `Job Instance Code`!="CP3.2-P3A")
new_2 <- subset(new_2, `Job Instance Code`!="CL3")
new_2 <- subset(new_2, `Job Instance Code`!="CP3.2-P3B")
new_2 <- subset(new_2, `Job Instance Code`!="CP3.2-P3C")
new_2 <- subset(new_2, `Job Instance Code`!="CP3.2-P3D")

new_2$`Execution Start TS` <- mdy_hms(new_2$`Execution Start TS`)
new_2$`Execution End TS` <- mdy_hms(new_2$`Execution End TS`)

# START TIME AND END TIME HERE
start_time <-  mdy_hms("12/22/2015 09:00:00")
end_time <- mdy_hms("12/23/2015 08:00:00")

new_3 <- subset(new_2, (`Execution Start TS`>=start_time & `Execution Start TS`<=end_time)) 

# Reformating column names to limit errors
colnames(old_3) <- c("Execution.Id", "Job.Code", "Job.Instance.Code", "Job.Type", "Trigger.Type", "Status",
                     "Schedule.TS", "Execution.Start.TS", "Execution.End.TS")

colnames(new_3) <- c("Execution.Id", "Job.Code", "Job.Instance.Code", "Job.Type", "Trigger.Type", "Status",
                     "Schedule.TS", "Execution.Start.TS", "Execution.End.TS")

# Determining Elapsed time
old_3$Elapsed.Time.Seconds <- as.duration(old_3$Execution.End.TS - old_3$Execution.Start.TS)
old_3$Elapsed.Time <- seconds_to_period(old_3$Elapsed.Time.Seconds)

new_3$Elapsed.Time.Seconds <- as.duration(new_3$Execution.End.TS - new_3$Execution.Start.TS)
new_3$Elapsed.Time <- seconds_to_period(new_3$Elapsed.Time.Seconds)


## WS_Stop / PPD8

new_4 <- subset(new_3, (Job.Instance.Code=="PPD8" | Job.Instance.Code=="WS_Stop"))
old_4 <- subset(old_3, (Job.Instance.Code=="PPD8" | Job.Instance.Code=="WS_Stop"))

new_4 <- arrange(new_4, Execution.Start.TS)
old_4 <- arrange(old_4, Execution.Start.TS)

process_time_new <-  as.duration(new_4$Execution.End.TS[2] - new_4$Execution.Start.TS[1])
process_time_old <-  as.duration(old_4$Execution.End.TS[2] - old_4$Execution.Start.TS[1])

process_time_new <- seconds_to_period(process_time_new)
process_time_old <- seconds_to_period(process_time_old)

line4 <- c("From WS-STOP to PPD8",  as.character(new_4$Execution.Start.TS[1]), as.character(new_4$Execution.End.TS[2]), as.character(process_time_new), "", as.character(old_4$Execution.Start.TS[1]), as.character(old_4$Execution.End.TS[2]), as.character(process_time_old))


## J4.1-01 / PPD63

new_5 <- subset(new_3, (Job.Instance.Code=="J4.1-01" | Job.Instance.Code=="PPD63"))
old_5 <- subset(old_3, (Job.Instance.Code=="J4.1-01" | Job.Instance.Code=="PPD63"))

new_5 <- arrange(new_5, Execution.Start.TS)
old_5 <- arrange(old_5, Execution.Start.TS)

process_time_new <-  as.duration(new_5$Execution.End.TS[2] - new_5$Execution.Start.TS[1])
process_time_old <-  as.duration(old_5$Execution.End.TS[2] - old_5$Execution.Start.TS[1])

process_time_new <- seconds_to_period(process_time_new)
process_time_old <- seconds_to_period(process_time_old)

line5 <- c("From UNEARNED_RESERVE to PPD63",  as.character(new_5$Execution.Start.TS[1]), as.character(new_5$Execution.End.TS[2]), as.character(process_time_new), "", as.character(old_5$Execution.Start.TS[1]), as.character(old_5$Execution.End.TS[2]), as.character(process_time_old))

## CP3.2-P1_PA55 / CP3.2-P1_PA63

new_6 <- subset(new_3, (Job.Instance.Code=="CP3.2-P1_PA55" | Job.Instance.Code=="CP3.2-P1_PA63"))
old_6 <- subset(old_3, (Job.Instance.Code=="CP3.2-P1_PA55" | Job.Instance.Code=="CP3.2-P1_PA63"))

new_6 <- arrange(new_6, Execution.Start.TS)
old_6 <- arrange(old_6, Execution.Start.TS)

process_time_new <-  as.duration(new_6$Execution.End.TS[2] - new_6$Execution.Start.TS[1])
process_time_old <-  as.duration(old_6$Execution.End.TS[2] - old_6$Execution.Start.TS[1])

process_time_new <- seconds_to_period(process_time_new)
process_time_old <- seconds_to_period(process_time_old)

line6 <- c("From PA55 to PA63",  as.character(new_6$Execution.Start.TS[1]), as.character(new_6$Execution.End.TS[2]), as.character(process_time_new), "", as.character(old_6$Execution.Start.TS[1]), as.character(old_6$Execution.End.TS[2]), as.character(process_time_old))

### Exporting

overall <- cbind(line4, line5, line6)
overall <- data.frame(overall)
overall <- t(overall)
overall <- data.frame(overall)
rownames(overall) <- NULL
colnames(overall) <- c("Path", "Old Start Time", "Old End Time", "Elapsed Time", "", "New Start Time", "New End Time", "Elapsed Time")

# write.csv(line4, "report_date.csv")
# end time ppd8 - wstop
## Joining to compare


# merging 
merged <- left_join(old_3, new_3, by="Job.Instance.Code" )

merged$Elapsed.Time.y > merged$Elapsed.Time.x

merged$slower <- ifelse(merged$Elapsed.Time.Seconds.y>merged$Elapsed.Time.Seconds.x, "*", "")

# slower, merged$Elapsed.Time.y, merged$Elapsed.Time.x, job.code, job.instance.code

merged$Elapsed.Time.New <- as.character(seconds_to_period(merged$Elapsed.Time.Seconds.y))
merged$Elapsed.Time.Old <- as.character(seconds_to_period(merged$Elapsed.Time.Seconds.x))

merged <- merged[c("Job.Code.x", "Job.Instance.Code", "Elapsed.Time.New", "Elapsed.Time.Old", "slower")]

# Master list
master <- read.csv("master_list.csv", stringsAsFactors=FALSE)
master_new <- left_join(master, merged, by="Job.Instance.Code")

# CHANGE FILE NAME

write.xlsx(overall, file="FILENAME_GOES_HERE.xls", sheetName="Summary")
write.xlsx(master_new, file="FILENAME_GOES_HERE.xls", sheetName="Overall", append=TRUE)

