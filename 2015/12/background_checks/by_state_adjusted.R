

library(seasonal)
library(tidyr)
library(ggplot2)

ct_seas <- ts(ct_month[,2],frequency=12,start=c(1998,11))
m <- seas(ct_seas)
plot(ct_seas)
series(m, "forecast.forecasts")
write.csv(m, "data/m.csv")
whatwhat <- final(m)

dfct <- as.data.frame(ct_seas)

ct_months_only <- ct_month$Month

dfct <- cbind(ct_months_only, dfct)
colnames(dfct) <- c("Month", "Original")
dfct2 <- data.frame(final(m))
dfct <- cbind(dfct, dfct2)

colnames(dfct) <- c("Month", "Original", "Adjusted")
dfct$Adjusted <- round(dfct$Adjusted, 2)

dfct = dfct[-1,]

library(trendct)

trendchart(dfct, headline="Firearm background checks per capita in CT", subhead = "", src = "",
           byline = "TrendCT.org", type = "line", xTitle = "", yTitle = "",
           xSuffix = "", ySuffix = "", xPrefix = "", yPrefix = "", option = "")

### US now

## Prep dataframe 
us_col <- monthly[c("Month", "percapita")]
us_col <- us_col[-1,]

## Turn it into a time series
us_tl <- ts(us_col[,2],frequency=12,start=c(1998,11))

## Adjust it for seasonality
us_m <- seas(us_tl)
plot(us_m)

## new data.frame specifically for US
colnames(us_col) <- c("Month", "Original") 
us_df <- as.data.frame(final(us_m))
us_col <- cbind(us_col, us_df)
colnames(us_col) <- c("Month", "Original", "Adjusted")
us_col$Adjusted <- round(us_col$Adjusted, 2)

trendchart(us_col, headline="Firearm background checks per capita in US", subhead = "", src = "",
           byline = "TrendCT.org", type = "line", xTitle = "", yTitle = "",
           xSuffix = "", ySuffix = "", xPrefix = "", yPrefix = "", option = "")

# US and CT adjusted
usct <- us_col[c("Month", "Adjusted")]
colnames(usct) <- c("Month", "US.Adjusted")
ct_adjusted <- dfct[c("Month", "Adjusted")]
colnames(ct_adjusted) <- c("Month", "CT.Adjusted")

usct <- left_join(usct, ct_adjusted)

trendchart(usct, headline="Firearm background checks per capita in the US and CT", subhead = "Adjusted for seasonality.", src = "",
           byline = "TrendCT.org", type = "line", xTitle = "", yTitle = "",
           xSuffix = "", ySuffix = "", xPrefix = "", yPrefix = "", option = "")

# OK, adjust per capita figures for seasonality
# Start with data.frame by_month


us_seas_only <- us_col[c("Month", "Adjusted")]
us_seas_only$Month <- as.character(us_seas_only$Month)
colnames(us_seas_only) <- c("Month", "US")
us_seas_only$US <- as.numeric(as.character(us_seas_only$US))
months_only <- as.character(us_seas_only$Month)

adj_seas <- by_month
adj_seas_num <- 2:ncol(adj_seas)
adj_seas_list <- colnames(adj_seas)

adj_seas$Month <- as.Date(adj_seas$Month, format="%Y-%m")


for (i in adj_seas_num, echo=FALSE){
  adj_seas_col <- colnames(adj_seas[i])
  seas_df_subset <- adj_seas[c("Month", adj_seas_col)]
  subset_series <- ts(seas_df_subset[,2],frequency=12,start=c(1998,11))
  subset_seas <- seas(subset_series)
  subset_seas_df <- as.data.frame(final(subset_seas))
  
  subset_seas_df <- subset_seas_df[-1,]
  months_only_df <- months_only
  months_only_df <- as.data.frame(cbind(months_only_df, subset_seas_df))
  colnames(months_only_df) <- c("Month", adj_seas_col)
  months_only_df$Month <- as.character(months_only_df$Month)
  months_only_df[,2] <- as.numeric(as.character(months_only_df[,2]))
  months_only_df[,2] <- round(months_only_df[,2], 2)
  
  if (!exists("months_only_all")) {
    months_only_all <- left_join(us_seas_only, months_only_df)
  } else {
    months_only_all <- left_join(months_only_all, months_only_df)
    
  }
  
}

# Exporting seasonally-adjusted figures to csv
write.csv(months_only_all, "data/states_monthly_adjusted.csv")

# Charting out with ggplot 
for_gg <- months_only_all

# Adjusting timestamp just for cosmetic reasons
for_gg$Month <- substring(for_gg$Month, 1,7)

# Prepping the time stamps for ggplot
for_gg$Month <- factor(for_gg$Month)

# Reshaping the dataframe for ggplot and also D3

for_gg <- gather(for_gg, "State", "Per.Capita", 2:53)

## Charting it in one line graph
ggplot(data=for_gg, aes(x=for_gg$Month,y=Per.Capita, group=State)) +
  geom_line() +
  ggtitle("Background checks by state") +
  labs(x="Month", y="Per 1,000 residents")

## Charting it using small multiples
ggplot(data=for_gg, aes(x=Month,y=Per.Capita)) +
  geom_bar(stat="identity") +
  facet_wrap(~State) +
  ggtitle(expression(atop("Background checks by state", atop(italic("Adjusted for seasonality"), "")))) +
  theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=20, hjust=0, color="#555555")) +
  theme(axis.text.x = element_text(angle=90)) 

# Exporting for D3
write.table(for_gg, file="seasonally_adjusted.tsv", quote=FALSE, sep='\t', col.names=NA)


