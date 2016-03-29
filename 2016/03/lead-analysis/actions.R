library(rvest)
library(dplyr)


## Scraping individual tables from water sources pages via EPA

for (x in 1:nrow(w_table_all)) {
  
  i_url <- w_table_all$link[x] %>% read_html()
  
  name <- i_url %>% html_node("td b") %>%
    html_text()
  print(c(name, x))
  
  address <- i_url %>% html_node("tr:nth-child(3) td") %>%
    html_text()
  
  phone <- i_url %>% html_node("tr:nth-child(4) td") %>%
    html_text()
  
  list_length <- i_url %>%
    html_nodes("table") %>%
    html_table(fill=TRUE) %>%
    length()
  
  if (list_length ==4) {
    
    looper <- i_url %>%
      html_nodes("table") %>%
      .[[4]] %>%
      html_table(fill=TRUE)
    
    if (ncol(looper)==5) {

      violation <- looper[1,1]
      compliance.begin <- looper[1,2]
      compliance.end <- looper[1,3]
      rule <- looper[1,4]
      analytic <- ""
      vio.id <- looper[1,5]
    } else if (ncol(looper)==6) {
      name <- name
      address <- address
      phone <- phone
      violation <- looper[1,1]
      compliance.begin <- looper[1,2]
      compliance.end <- looper[1,3]
      rule <- looper[1,4]
      analytic <- looper[1,5]
      vio.id <- looper[1,6]
    }
    follow.up.action <- ""
    response.date <- ""
    
    v_array <- data.frame(follow.up.action, response.date, name, address, phone,
                          violation, compliance.begin, compliance.end, rule, analytic, vio.id)

    } else if (list_length >=5) {
    
    
    for (i in 5:list_length) {
      looped <- i_url %>%
        html_nodes("table") %>%
        .[[i]] %>%
        html_table(fill=TRUE)
      
      if (ncol(looped)==2) {
        colnames(looped) <- c("follow.up.action", "response.date")
        
        loopy <- i_url %>%
          html_nodes("table") %>%
          .[[i-1]] %>%
          html_table(fill=TRUE)
        if (ncol(loopy)==5) {
        looped$name <- name
        looped$address <- address
        looped$phone <- phone
        looped$violation <- loopy[1,1]
        looped$compliance.begin <- loopy[1,2]
        looped$compliance.end <- loopy[1,3]
        looped$rule <- loopy[1,4]
        looped$analytic <- ""
        looped$vio.id <- loopy[1,5]
        } else if (ncol(loopy)==6) {
          looped$name <- name
          looped$address <- address
          looped$phone <- phone
          looped$violation <- loopy[1,1]
          looped$compliance.begin <- loopy[1,2]
          looped$compliance.end <- loopy[1,3]
          looped$rule <- loopy[1,4]
          looped$analytic <- loopy[1,5]
          looped$vio.id <- loopy[1,6]
        }
        v_array <- looped
        
        if (i == 5) { 
          v_dataframe <- v_array 
        } else {
          v_dataframe <- rbind(v_dataframe, v_array) }
      } 
      print(i)
    }
  } else {
    
    follow.up.action <- ""
    response.date <- ""
    violation <- ""
    compliance.begin <- ""
    compliance.end <- ""
    rule <- ""
    analytic <- ""
    vio.id <- 0

    v_array <- data.frame(follow.up.action, response.date, name, address, phone,
                          violation, compliance.begin, compliance.end, rule, analytic, vio.id)
    if (i == 5) { 
      v_dataframe <- v_array 
    } else {
      v_dataframe <- rbind(v_dataframe, v_array) }
  }
  if (x==1) {
    v_dataframe_all <- v_dataframe
  } else {
    v_dataframe_all <- rbind(v_dataframe_all, v_dataframe)
  }
  Sys.sleep(15)
}

v_dataframe_count <- left_join(v_dataframe_count, w_table_all)