# Most of this code is from a great tutorial from Bradley Boehmke
# (https://rpubs.com/bradleyboehmke/weather_graphic)
# but with minor style modifications and using a different data set

# Preprocessing & summarizing data
library(dplyr)
library(tidyr)

# Visualization package
library(ggplot2)

# Cities and airport codes for reference
# Hartford, KHFD
# New Haven, KHVN
# Stamford, KHPN
# New London-Groton, KGON
# Stratford, KBDR

# Pull in the weather data that you scraped from Weather Underground
airp <- read.csv("KGON.csv", stringsAsFactors=FALSE)

# Restructuring the data so it plays nice with ggplot2
# airp2 <- gather(airp, "type", "temp", 4:12)

# Label the town for the chart title
town <- "New London-Groton"

# function to turn y-axis labels into degree formatted values
dgr_fmt <- function(x, ...) {
  parse(text = paste(x, "*degree", sep = ""))
}
a <- dgr_fmt(seq(-20,100, by=10))

# Bringing in a package that will allow use of Google fonts
library(extrafont)

p <- ggplot(airp, aes(row, average_min_temp)) +
  theme(plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()) +
  geom_linerange(airp, mapping=aes(x=row, ymin=record_min_temp, ymax=record_max_temp), colour = "sienna", alpha=.5)

p <- p + 
  geom_linerange(airp, mapping=aes(x=row, ymin=average_min_temp, ymax=average_max_temp), colour = "sienna1", alpha=.8)

p <- p + 
  geom_linerange(airp, mapping=aes(x=row, ymin=actual_min_temp, ymax=actual_max_temp), colour = "sienna4") +
  geom_vline(xintercept = 0, colour = "sienna4", linetype=1, size=1)

# The colors used in the chart layers above can be replaced with any from
# http://sape.inf.usi.ch/quick-reference/ggplot2/colour

# Make the grid look pretty

p <- p + 
  geom_hline(yintercept = -20, colour = "white", linetype=1) +
  geom_hline(yintercept = -10, colour = "white", linetype=1) +
  geom_hline(yintercept = 0, colour = "white", linetype=1) +
  geom_hline(yintercept = 10, colour = "white", linetype=1) +
  geom_hline(yintercept = 20, colour = "white", linetype=1) +
  geom_hline(yintercept = 30, colour = "white", linetype=1) +
  geom_hline(yintercept = 40, colour = "white", linetype=1) +
  geom_hline(yintercept = 50, colour = "white", linetype=1) +
  geom_hline(yintercept = 60, colour = "white", linetype=1) +
  geom_hline(yintercept = 70, colour = "white", linetype=1) +
  geom_hline(yintercept = 80, colour = "white", linetype=1) +
  geom_hline(yintercept = 90, colour = "white", linetype=1) +
  geom_hline(yintercept = 100, colour = "white", linetype=1)

# Identifying the months based on number of days

p <- p + 
  # January - 31
  geom_vline(xintercept = 31, colour = "wheat4", linetype=3, size=.5) +
  # February - 28
  geom_vline(xintercept = 59, colour = "wheat4", linetype=3, size=.5) +
  # March - 31
  geom_vline(xintercept = 90, colour = "wheat4", linetype=3, size=.5) +
  # April - 30
  geom_vline(xintercept = 120, colour = "wheat4", linetype=3, size=.5) +
  # May - 31
  geom_vline(xintercept = 151, colour = "wheat4", linetype=3, size=.5) +
  # June - 30
  geom_vline(xintercept = 181, colour = "wheat4", linetype=3, size=.5) +
  # July - 31
  geom_vline(xintercept = 212, colour = "wheat4", linetype=3, size=.5) +
  # August - 31 
  geom_vline(xintercept = 243, colour = "wheat4", linetype=3, size=.5) +
  # September - 30
  geom_vline(xintercept = 273, colour = "wheat4", linetype=3, size=.5) +
  # October 31
  geom_vline(xintercept = 304, colour = "wheat4", linetype=3, size=.5) +
  # November - 30
  geom_vline(xintercept = 334, colour = "wheat4", linetype=3, size=.5) +
  # December - 31
  geom_vline(xintercept = 365, colour = "wheat4", linetype=3, size=.5) 
  
  
# Establishing the x axis
p <- p +
  coord_cartesian(ylim = c(-20,100)) +
  scale_y_continuous(breaks = seq(-20,100, by=10), labels = a) +
  scale_x_continuous(expand = c(0, 0), 
                     breaks = c(15,45,75,105,135,165,195,228,258,288,320,350),
                     labels = c("JAN", "FEB", "MAR", 
                                "APR", "MAY", "JUN", "JUL", "AUG", "SEP","OCT", 
                                "NOV", "DEC"))

# Identifying the record-breaking days by comparing actual vs record
rlow3 <- airp[airp$actual_min_temp<=airp$record_min_temp,]
rhigh3 <- airp[airp$actual_max_temp>=airp$record_max_temp,]

# Adding them to the chart with specific colors
p <- p +
  geom_point(data=rlow3, aes(x=row, y=record_min_temp), colour="blue1") +
  geom_point(data=rhigh3, aes(x=row, y=record_max_temp), colour="red1")

# Adding a title based on the variable set above
title <- paste0(town, "'s weather in 2015")

# Setting the title
p <- p +
  ggtitle(title) +
#   theme(plot.title=element_text(face="bold",hjust=.012,vjust=.8,colour="#3C3C3C",size=20, family="Lato")) +
#   annotate("text", x = 28, y = 98, label = "Temperature", size=4, fontface="bold", family="Lato Black") 
  theme(plot.title=element_text(face="bold",hjust=.012,vjust=.8,colour="#3C3C3C",size=20)) +
  annotate("text", x = 28, y = 98, label = "Temperature", size=4, fontface="bold") 

# Now for the legend
p <- p +
  annotate("segment", x = 165, xend = 165, y = -6, yend = 16, colour = "sienna", , alpha=.5, size=3) +
  annotate("segment", x = 165, xend = 165, y = 0, yend = 10, colour = "sienna1", , alpha=.8, size=3) +
  annotate("segment", x = 165, xend = 165, y = 2, yend = 8, colour = "sienna4", size=3) +
  
  annotate("segment", x = 158, xend = 162, y = 10, yend = 10, colour = "gray30", size=.5) +
  annotate("segment", x = 158, xend = 162, y = 0, yend = 0, colour = "gray30", size=.5) +
  annotate("segment", x = 160, xend = 160, y = 10, yend = 0, colour = "gray30", size=.5) +
  
  annotate("text", x = 132, y = 5, label = "AVERAGE RANGE", size=3, colour="gray30") +
  
  annotate("segment", x = 168, xend = 172, y = 8, yend = 8, colour = "gray30", size=.5) +
  annotate("segment", x = 168, xend = 172, y = 2, yend = 2, colour = "gray30", size=.5) +
  annotate("segment", x = 170, xend = 170, y = 8, yend = 2, colour = "gray30", size=.5) +
  
  annotate("text", x = 204, y = 5, label = "2015 RANGE", size=3.5, colour="gray30") +
  annotate("text", x = 142, y = 13, label = "RECORD HIGH", size=3, colour="gray30") +
  annotate("text", x = 143, y = -3, label = "RECORD LOW", size=3, colour="gray30") +
  
  annotate("segment", x = 167, xend = 176, y = 17, yend = 17, colour = "gray30", size=.5) +
  annotate("segment", x = 167, xend = 176, y = -7, yend = -7, colour = "gray30", size=.5) +
  
  annotate("point", x = 165, y = 17, colour = "red", size = 2) + 
  annotate("point", x = 165, y = -7, colour = "blue", size = 2) +
  annotate("text", x = 206, y = 17, label = "NEW RECORD HIGH", size=3, colour="gray30") +
  annotate("text", x = 206, y = -7, label = "NEW RECORD LOW", size=3, colour="gray30") +
  annotate("text", x = 290, y = -15, label = "Source: Weather Underground", size=4, fontface="italic", colour="gray30") 

print(p)
town <- gsub(" ", "", town)

# Exporting the charts
filenamesvg <- paste0(town, "-dec.svg")
filenamepng <- paste0(town, "-dec.png")
ggsave(file=filenamesvg, plot=p, width=10, height=6)
ggsave(file=filenamepng, plot=p, width=10, height=5)



