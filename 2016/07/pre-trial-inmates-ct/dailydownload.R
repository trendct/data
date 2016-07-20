update <- read.csv("https://data.ct.gov/api/views/b674-jy6w/rows.csv")

date <- tail(update$DOWNLOAD.DATE,n=1)
date <- gsub("/", "", date)

filename <- paste0("data/file", date, ".csv" )
write.csv(update, filename)