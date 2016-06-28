
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
  state <- read_excel(paste0("data/", file_name), sheet=2)

  
  state_only <- subset(state, TYPE=="METRO")
  counties <- subset(state, TYPE=="COUNTY")
  
  if (i==1) {
    states_all <- state_only
    counties_all <- counties
    
  } else {
    states_all <- rbind(states_all, state_only)
    counties_all <- rbind(counties_all, counties)
    
  }
}

colnames(states_all) <- make.names(colnames(states_all))
colnames(counties_all) <- make.names(colnames(counties_all))

states_all <- states_all[c("STNAME", "COUNTY.METRO", "Two.bedroom.FMR", "Income.needed.to.afford.2.bdrm.FMR")]
counties_all <- counties_all[c("STNAME", "COUNTY.METRO", "Two.bedroom.FMR", "Income.needed.to.afford.2.bdrm.FMR")]

colnames(states_all) <- c("State", "metro", "two.bed.cost", "income.two.bed")
colnames(counties_all) <- c("State", "county", "two.bed.cost", "income.two.bed")

library(stringr)
library(dplyr)
library(tidyr)
library(rgeos)
library(maptools)
library(ggplot2)   # devtools::install_github("hadley/ggplot2") only if you want subtitles/captions
library(ggalt)
library(ggthemes)
library(albersusa) # devtools::install_github("hrbrmstr/albersusa")
library(viridis)
library(scales)

counties_all$county_name <- paste(counties_all$county, counties_all$State)

cmap <- fortify(counties_composite(), region="fips")

cmap$state_id <- substr(cmap$id, 0,2)
substr("abcdef", 2, 4)

cmap2 <-fortify(counties_composite(), region="state") 

county_fix <- county.fips


county_fix$fips <- as.character(county_fix$fips)

county_fix$fips <- ifelse(nchar(county_fix$fips)==4, paste0("0", county_fix$fips), county_fix$fips)
county_fix$polyname <- gsub("\\.", "", county_fix$polyname)


counties_all$fips_fix <- gsub(" County", "", counties_all$county)
counties_all$fips_fix <- paste0(counties_all$State, ",", counties_all$fips_fix)
counties_all$fips_fix <- str_to_lower(counties_all$fips_fix)
counties_all$fips_fix <- gsub("\\.", "", counties_all$fips_fix)
counties_all$fips_fix <- gsub("\\*", "", counties_all$fips_fix)
counties_all$fips_fix <- gsub("\\†", "", counties_all$fips_fix)
counties_all$fips_fix <- gsub(" parish", "", counties_all$fips_fix)
counties_all$fips_fix <- gsub(" city", "", counties_all$fips_fix)
counties_all$fips_fix <- gsub("dekalb", "de kalb", counties_all$fips_fix)
counties_all$fips_fix <- gsub("\\'", "", counties_all$fips_fix)
counties_all$fips_fix <- gsub("desoto", "de soto", counties_all$fips_fix)
counties_all$fips_fix <- gsub("dupage", "du page", counties_all$fips_fix)
counties_all$fips_fix <- gsub("laporte", "la porte", counties_all$fips_fix)
counties_all$fips_fix <- gsub("louisiana,st martin", "louisiana,st martin:north", counties_all$fips_fix)
counties_all$fips_fix <- gsub("washington,pierce", "washington,pierce:main", counties_all$fips_fix)
counties_all$fips_fix <- gsub("washington,san juan", "washington,san juan:lopez island", counties_all$fips_fix)
counties_all$fips_fix <- gsub("galveston", "galveston:main", counties_all$fips_fix)
counties_all$fips_fix <- gsub("dewitt", "de witt", counties_all$fips_fix)
counties_all$fips_fix <- gsub("lamoure", "la moure", counties_all$fips_fix)
counties_all$fips_fix <- gsub("currituck", "currituck:knotts", counties_all$fips_fix)
counties_all$fips_fix <- gsub("nevada,carson", "nevada,carson city", counties_all$fips_fix)
counties_all$fips_fix <- gsub("okaloosa", "okaloosa:main", counties_all$fips_fix)


counties_all$fips_fix <- str_trim(counties_all$fips_fix)

colnames(county_fix) <- c("fips", "fips_fix")

hiak <- data.frame(fips=c("02013", "02016", "02020", "02050", "02060", "02068", "02070", "02090", "02100", "02110", "02122", "02130", "02150", "02164", "02170", "02180", "02185", "02188", "02201", "02220", "15001", "15003", "15005", "15007", "15009", "02232", "02240", "02261", "02270", "02280", "02282", "02290", "02105", "02280", "02280"),
                   fips_fix=c("alaska,aleutians east borough", "alaska,aleutians west census area", "alaska,anchorage municipality", "alaska,bethel census area", "alaska,bristol bay borough", "alaska,denali borough", "alaska,dillingham census area", "alaska,fairbanks north star borough", "alaska,haines borough", "alaska,juneau and borough", "alaska,kenai peninsula borough", "alaska,ketchikan gateway borough", "alaska,kodiak island borough", "alaska,lake and peninsula borough", "alaska,matanuska-susitna borough", "alaska,nome census area", "alaska,north slope borough", "alaska,northwest arctic borough", "alaska,prince of wales-hyder census area", "alaska,sitka and borough", "hawaii,hawaii", "hawaii,honolulu", "hawaii,kalawao", "hawaii,kauai", "hawaii,maui", "alaska,skagway municipality", "alaska,southeast fairbanks census area", "alaska,valdez-cordova census area", "alaska,wade hampton census area", "alaska,wrangell-petersburg census area", "alaska,yakutat and borough", "alaska,yukon-koyukuk census area", "alaska,hoonah-angoon census area", "alaska,wrangell and borough", "alaska,petersburg census area"))

county_fix <- rbind(county_fix, hiak)

counties_all <- left_join(counties_all, county_fix)

# test <- subset(counties_all, is.na(fips))


gg <- ggplot()
gg <- gg + geom_map(data=cmap, map=cmap,
                    aes(x=long, y=lat, map_id=id),
                    color="#2b2b2b", size=0.05, fill=NA)
gg <- gg + geom_map(data=counties_all, map=cmap,
                    aes(fill=two.bed.cost, map_id=fips),
                    color="#2b2b2b", size=0.05)
gg <- gg + scale_fill_gradient(name="Cost",labels=dollar_format(), low = "#ce1256", high = "#4eb3d3")
gg <- gg + coord_proj(us_laea_proj)
gg <- gg + labs(title="Average monthly cost of a two-bedroom dwelling by county",
                subtitle="Some county-level data, mostly in New England, were not available",
                caption="SOURCE: National Low Income Housing Coalition\nAndrew Ba Tran/TrendCT.org")
gg <- gg + theme_map(base_family="Arial Narrow")
gg <- gg + theme(legend.position=c(0.8, 0.25))
gg <- gg + theme(plot.title=element_text(face="bold", size=14, margin=margin(b=6)))
gg <- gg + theme(plot.subtitle=element_text(size=10, margin=margin(b=-14)))
gg

ct_only <- subset(states_all, State=="Connecticut")

ct_only$State <- NULL
ct_only$income.two.bed <- NULL
ct_only$metro <- gsub(" HMFA", "", ct_only$metro)
ct_only$metro <- gsub("\\*", "", ct_only$metro)
ct_only$metro <- str_trim(ct_only$metro)

write.csv(ct_only, "ct_only_2_bed_cost.csv")

states_all$two.bed.cost <- round(states_all$two.bed.cost, 2)
states_all$metro <- gsub(" HMFA", "", states_all$metro)
states_all$metro <- gsub(" MSA", "", states_all$metro)
states_all$metro <- gsub("\\*", "", states_all$metro)

write.csv(states_all, "metros_for_table.csv")
