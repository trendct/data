
library(RCurl)
library(readxl)
library(dplyr)
library(stringr)

# file <- getURLContent("http://nlihc.org/sites/default/files/oor/files/reports/state/2016-OOR-CT.xls")
# 
# download.file("http://nlihc.org/sites/default/files/oor/files/reports/state/2016-OOR-CT.xls", "data/2016-OOR-CT.xls", mode="wb")
# ct <- read_excel("2016-OOR-CT.xls", sheet=1)
# 
# ct <- read_excel(file, sheet=1)


for (i in 1:length(state.abb)) {
  file_name <- paste0("2016-OOR-", state.abb[i], ".xls")
  file_url <- paste0("http://nlihc.org/sites/default/files/oor/files/reports/state/", file_name)

  download.file(file_url, paste0("data/", file_name), mode="wb")
  state <- read_excel(paste0("data/", file_name), sheet=1)
  state2 <- read_excel(paste0("data/", file_name), sheet=5)
  state3 <- read_excel(paste0("data/", file_name), sheet=4)
  
  state_only <- subset(state, TYPE=="STATE")
  state_only2 <- subset(state2, TYPE=="STATE")
  state_only3 <- subset(state3, TYPE=="STATE")
  
  if (i==1) {
    states_all <- state_only
    states_all2 <- state_only2
    states_all3 <- state_only3
    
  } else {
    states_all <- rbind(states_all, state_only)
    states_all2 <- rbind(states_all2, state_only2)
    states_all3 <- rbind(states_all3, state_only3)
    
  }
}

colnames(states_all) <- make.names(colnames(states_all))
states_all <- states_all[c("STNAME", "Housing.Wage.for.2.bdrm.FMR")]
colnames(states_all) <- c("State", "Housing Wage")
write.csv(states_all, "data/housing.csv")

colnames(states_all2) <- make.names(colnames(states_all2))
states_all2 <- states_all2[c("STNAME", "Estimated.mean.renter.wage")]
colnames(states_all2) <- c("State", "Renter Wage")
write.csv(states_all2, "data/wage.csv")


colnames(states_all3) <- make.names(colnames(states_all3))
states_all3 <- states_all3[c("STNAME", "Work.hours.per.week.at.min..wage.needed.to.afford.1.bdrm.FMR")]
colnames(states_all3) <- c("State", "Hours")
write.csv(states_all3, "data/hours.csv")

states_all4 <- left_join(states_all, states_all2)
states_all4$gap <- states_all4$`Housing Wage` - states_all4$`Renter Wage`

states_all4 <- arrange(states_all4, gap)
states_all4$State <- factor(states_all4$State, levels=unique(states_all4$State))

library(ggalt)
library(ggplot2)

gg <- ggplot()
# doing this vs y axis major grid line
gg <- gg + geom_segment(data=states_all4, aes(y=State, yend=State, x=9, xend=35.175), color="#b2b2b2", size=0.15)
# dum…dum…dum!bell


gg <- gg + geom_dumbbell(data=states_all4, aes(y=State, x=`Renter Wage`, xend=`Housing Wage`),
                         size=1.5, color="#b2b2b2", point.size.l=3, point.size.r=3,
                         point.colour.l="#476b6b", point.colour.r="#cc0052")
# text below points
gg <- gg + geom_text(data=filter(states_all4, State=="Hawaii"),
                     aes(x=`Renter Wage`, y=State, label="Renter wage"),
                     color="#476b6b", size=3, vjust=-2, fontface="bold", family="Calibri")
gg <- gg + geom_text(data=filter(states_all4, State=="Hawaii"),
                     aes(x=`Housing Wage`-2, y=State, label="Two-Bedroom housing wage"),
                     color="#cc0052", size=3, vjust=-2, fontface="bold", family="Calibri")
# text above points
gg <- gg + geom_text(data=states_all4, aes(x=`Renter Wage`, y=State, label=paste("$",round(`Renter Wage`,2))),
                     color="#476b6b", size=2.75, vjust=2.5, family="Calibri")
gg <- gg + geom_text(data=states_all4, color="#cc0052", size=2.75, vjust=2.5, family="Calibri",
                     aes(x=`Housing Wage`, y=State, label=paste("$",round(`Housing Wage`, 2))))
# difference column
gg <- gg + geom_rect(data=states_all4, aes(xmin=36.05, xmax=38.175, ymin=-Inf, ymax=Inf), fill="#efefe3")
gg <- gg + geom_text(data=states_all4, aes(label=round(gap,2), y=State, x=37.1125), fontface="bold", size=3, family="Calibri")
gg <- gg + geom_text(data=filter(states_all4, State=="Hawaii"), aes(x=41.1125, y=State, label="Gap"),
                     color="#7a7d7e", size=3.1, vjust=-2, fontface="bold", family="Calibri")
gg <- gg + scale_x_continuous(expand=c(0,0), limits=c(9, 38.175))
gg <- gg + scale_y_discrete(expand=c(0.075,0))
gg <- gg + labs(x=NULL, y=NULL, title="Gaps between hourly wages and housing wages",
                caption="Source: National Low Income Housing Coalition \nAndrew Ba Tran/TrendCT.org")
gg <- gg + theme_bw(base_family="Calibri")
gg <- gg + theme(panel.grid.major=element_blank())
gg <- gg + theme(panel.grid.minor=element_blank())
gg <- gg + theme(panel.border=element_blank())
gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(axis.text.x=element_blank())
gg <- gg + theme(plot.title=element_text(face="bold"))
gg <- gg + theme(plot.subtitle=element_text(face="italic", size=9, margin=margin(b=12)))
gg <- gg + theme(plot.caption=element_text(size=7, margin=margin(t=12), color="#7a7d7e"))
gg


library(choroplethr)

colnames(states_all3) <- c("region", "value")
states_all3$region <- str_to_lower(states_all3$region)
state_choropleth(states_all3, title = "Work hours per week at minimum wage needed to afford 1 bedroom")
