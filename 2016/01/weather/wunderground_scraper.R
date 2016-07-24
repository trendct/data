# This scraper was inspired by Randy Olson's weather chart python scripts
# http://www.randalolson.com/2015/08/13/the-new-york-times-weather-chart-redux/

# Generates an array of dates based on what you're looking at
date <- seq(as.Date("2015/1/1"), as.Date("2016/1/1"), "day")
dates <- data.frame(date)
dates$date <- as.character(dates$date)
dates$date <- gsub("\\-", "/", dates$date)

# Run these lines if you need to try again
# tryagain <- read.csv("leftovers.csv", stringsAsFactors=FALSE)
# dates <- tryagain

dates_list <- 1:nrow(dates)



# Just a list of cities and airport codes for reference
# Hartford, KHFD
# New Haven, KHVN !NOPE
# Stamford, KHPN !NOPE
# Groton - New London, KGON
# Stratford, KBDR

# INSERT AIRPORT CODE HERE
ap <- "KGON"

# If you're rerunning this script with another airport code, 
# be sure to run this command below to clear the memory

rm(master_list)
# Loop to go scrape wunderground.com historical data based on the dates in the array
# Using the rvest package

library(rvest)

  for (i in dates_list) {
    
    actual_mean_temp <-0
    actual_max_temp <- 0
    actual_min_temp <- 0
    average_max_temp <- 0
    average_min_temp <- 0
    record_max_temp <- 0
    record_min_temp  <- 0
    record_max_temp_year <- 0
    record_min_temp_year <- 0
    
    date <- dates$date[i]
    
    link <- paste0("http://www.wunderground.com/history/airport/",ap,"/",date,"/DailyHistory.html")
    
    wu <- html(link)
    
    actual_mean_temp <- wu %>% 
      html_node("tr:nth-child(2) .wx-value") 
    if (is.null(actual_mean_temp)) {
      actual_mean_temp <- 999
    } else {
      actual_mean_temp <- actual_mean_temp  %>%
        html_text() %>%
        as.numeric()
    }

    # Sometimes the code picked up isn't consistent. So to prevent errors,
    # it's replaced with a 999 so it can be identified later to be replaced
    
    actual_max_temp <- wu %>% 
      html_node("tr:nth-child(3) .indent+ td .wx-value") 
    if (is.null(actual_max_temp)) {
      actual_max_temp <- 999
    } else {
      actual_max_temp <- actual_max_temp %>%
        html_text() %>%
        as.numeric()
    }
    
    actual_min_temp <- wu %>% 
      html_node("tr:nth-child(4) .indent+ td .wx-value")     
    if (is.null(actual_min_temp)) {
      actual_min_temp <- 999
    } else {
      actual_min_temp <- actual_min_temp %>%
        html_text() %>%
        as.numeric()
    }
    
    average_max_temp <- wu %>% 
      html_node("tr:nth-child(3) td:nth-child(3) .wx-value") 
    if (is.null(average_max_temp)) {
      average_max_temp <- 999
    } else {
      average_max_temp <- average_max_temp %>%
        html_text() %>%
        as.numeric()
    }
    
    average_min_temp <- wu %>% 
      html_node("tr:nth-child(4) td:nth-child(3) .wx-value") 
    if (is.null(average_min_temp)) {
      average_min_temp <- 999
    } else {
      average_min_temp <- average_min_temp %>%
        html_text() %>%
        as.numeric()
    }
    
    record_max_temp <- wu %>% 
      html_node("tr:nth-child(3) td:nth-child(4) .wx-value") 
    if (is.null(record_max_temp)) {
      record_max_temp <- 999
    } else {
      record_max_temp <- record_max_temp %>%
        html_text() %>%
        as.numeric()
    }
    
    record_min_temp  <- wu %>% 
      html_node("tr:nth-child(4) td:nth-child(4) .wx-value") 
    if (is.null(record_min_temp)) {
      record_min_temp <- 999
    } else {
      record_min_temp <- record_min_temp %>%
        html_text() %>%
        as.numeric()
    }
    
    record_max_temp_year<- wu %>% 
      html_node("tr:nth-child(3) td:nth-child(4)")    
    if (is.null(record_max_temp_year)) {
      record_max_temp_year <- 999
    } else {
      record_max_temp_year <- record_max_temp_year %>%
        html_text() 
      record_max_temp_year <- gsub('.*\\(', '', record_max_temp_year)
      record_max_temp_year <- gsub('\\)', '', record_max_temp_year)
      record_max_temp_year <- as.numeric(record_max_temp_year)
    }
    
    record_min_temp_year<- wu %>% 
      html_node("tr:nth-child(4) td:nth-child(4)") 
    if (is.null(record_min_temp_year)) {
      record_min_temp_year <- 999
    } else {
      record_min_temp_year <- record_min_temp_year %>%
        html_text() 
      record_min_temp_year <- gsub('.*\\(', '', record_min_temp_year)
      record_min_temp_year <- gsub('\\)', '', record_min_temp_year)
      record_min_temp_year <- as.numeric(record_min_temp_year)
    }
    
    city_df <- data.frame(date, actual_mean_temp, actual_min_temp, actual_max_temp, average_min_temp,
                          average_max_temp, record_min_temp, record_max_temp, record_min_temp_year, 
                          record_max_temp_year)
    
    if (exists("master_list")) {
      master_list <- rbind(master_list, city_df)
    } else { master_list <- city_df }
  }
  
  # Two CSV files are generated. One with errors and one without.

  clean <- subset(master_list, record_min_temp_year<=2016 & record_min_temp_year >=1800)
  leftovers <- subset(master_list, !(record_min_temp_year<=2016 & record_min_temp_year >=1800))
  
  # Rename this csv below if you're running this a second time for cleaner results
  filename <- paste0(ap, ".csv")
  write.csv(clean, filename)
  write.csv(leftovers, "leftovers.csv")
  
  # Clean up the errors by visiting the dates manually
  # or take the leftovers csv and run it through the script above
  # If you do so, be sure to rename the output so it doesn't overwrite what you've already generated