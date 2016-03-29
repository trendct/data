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
    
    if (list_length >=4) {
      for (i in 4:list_length) {
        looped <- i_url %>%
          html_nodes("table") %>%
          .[[i]] %>%
          html_table(fill=TRUE)
        
        if (ncol(looped)==5) {
          
          violation <- looped[1,1]
          compliance.begin <- looped[1,2]
          compliance.end <- looped[1,3]
          rule <- looped[1,4]
          analytic <- ""
          vio.id <- looped[1,5]
          if (i != list_length) {
              loopy <- i_url %>%
                html_nodes("table") %>%
                .[[i+1]] %>%
                html_table(fill=TRUE)
              if (ncol(loopy)==2) {
              actions <- nrow(loopy) 
              } else {
                actions <- 0
              }
          } else {
            actions <- 0
          }
          v_array <- data.frame(name, address, phone, violation, compliance.begin,
                                compliance.end, rule, analytic, vio.id, actions)
          if (i == 4) { 
            v_dataframe <- v_array 
          } else {
            v_dataframe <- rbind(v_dataframe, v_array) }
        } else if (ncol(looped)==6) {
          
          violation <- looped[1,1]
          compliance.begin <- looped[1,2]
          compliance.end <- looped[1,3]
          rule <- looped[1,4]
          analytic <- looped[1,5]
          vio.id <- looped[1,6]
          if (i != list_length) {
            loopy <- i_url %>%
              html_nodes("table") %>%
              .[[i+1]] %>%
              html_table(fill=TRUE)
            if (ncol(loopy)==2) {
              actions <- nrow(loopy) 
            } else {
              actions <- 0
            }
          } else {
            actions <- 0
          }
          v_array <- data.frame(name, address, phone, violation, compliance.begin,
                                compliance.end, rule, analytic, vio.id, actions)
          if (i == 4) { 
            v_dataframe <- v_array 
          } else {
            v_dataframe <- rbind(v_dataframe, v_array) }
        } else if ((ncol(looped)!=5) | (ncol(looped)!=6)){
            print("nothing to see here!")
          }
      
        print(i)
      }
    } else {
      violation <- ""
      compliance.begin <- ""
      compliance.end <- ""
      rule <- ""
      analytic <- ""
      vio.id <- 0
      actions <- 0
      v_array <- data.frame(name, address, phone, violation, compliance.begin,
                            compliance.end, rule, analytic, vio.id, actions)
      if (i == 4) { 
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

v_dataframe_count <- v_dataframe_all
v_dataframe_count <- left_join(v_dataframe_all, w_table_all)