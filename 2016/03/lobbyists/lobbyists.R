## Lobby name column
# delete $ on left side 
# delete everything after space or -
# Starts with Compensation, 
# Admin $
# Leg $
# Total $


lobby <- read.csv("lobbyists.csv", stringsAsFactors=FALSE)

row <- c(1,2,3)
lobby_group <- c("Group1", "Group2", "Group3")
#admin <- c("$100" ,"$200", "$300")
#lege <- c("$100" ,"$200", "$300")
total <- c("$100", "$200", "$300")

#lobby_all <- data.frame(row, lobby_group, admin, lege, total)
lobby_all <- data.frame(row, lobby_group, total)
lobby_list <- 1:nrow(lobby)

for (i in lobby_list) {
  
  if (!grepl("Admin ", lobby$lobby.group[i]) & !grepl("Compensation",lobby$lobby.group[i]) & !grepl("Leg ", lobby$lobby.group[i]) & !grepl( "Total ",lobby$lobby.group[i],)) {
     row <- lobby$X[i]
     lobby_group <- lobby$lobby.group[i]
     #admin <- "nothing yet"
     #lege <- "nothing yet"
     total <- "nothing yet"
     #lobby_ish <- data.frame(row, lobby_group, admin, lege, total)
     lobby_ish <- data.frame(row, lobby_group, total)
     lobby_all <- rbind(lobby_all, lobby_ish)
  }
  
}

lobby_all = lobby_all[-1,]
lobby_all = lobby_all[-1,]
lobby_all = lobby_all[-1,]
#lobby_all$admin <- as.character(lobby_all$admin)
#lobby_all$lege <- as.character(lobby_all$lege)
lobby_all$total <- as.character(lobby_all$total)

lobby_all$row <- as.numeric(as.character(lobby_all$row))
lobby_list_all <- 1:nrow(lobby_all)

for (i in lobby_list_all) {
  if (lobby_all$row[i] > 1 & lobby_all$row[i] < 3510) {
    new_row <- lobby_all$row[i] - 1
    the_total <- lobby$x7[new_row]
    lobby_all$total[i-1] <- the_total
  } else if (lobby_all$row[i] > 3510) {
    new_row <- lobby_all$row[i] - 2
    the_total <- lobby$x7[new_row]
    lobby_all$total[i-1] <- the_total
  } else if (lobby_all$row[i] == 1) {
    the_total <- lobby$x7[4]
    lobby_all$total[i] <- the_total
  } else if (lobby_all$row[i] == 3971) {
    the_total <- lobby$x7[3973]
    lobby_all$total[i] <- the_total
  }
}


lobby_all$total <- gsub("\\$", "", lobby_all$total)
lobby_all$total <- gsub("\\,", "", lobby_all$total)
lobby_all$total <- as.numeric(lobby_all$total)
library(DT)
datatable(lobby_all)
write.csv(lobby_all, "lobby_all.csv")
