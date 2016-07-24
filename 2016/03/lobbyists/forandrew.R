#Load libraries
library(ggplot2)
library(scales)

B16002.all <- read.csv('B16002.all.csv')

#Sort results by town level results
#LEP.x = tract results
#LEP.y = town results
B16002.all$NAME10_1 <- reorder(B16002.all$NAME10_1, B16002.all$LEP.y)

#Make chart of the data by town and by tract
#Green dots are town results, black ones are individual tracts
#Should add a legend for this but have not done it yet
#Plot displays only those towns with > 10% in some tract
p <- ggplot(data = subset(B16002.all, NAME10_1 %in% unique(subset(B16002.all, LEP.x > .1)$NAME10_1)), aes(y = NAME10_1)) + 
  geom_point(aes(group = NAME10_1, x = LEP.x)) + 
  geom_point(aes(group = NAME10_1, x = LEP.y), size = 5, colour = "green") + 
  labs(y = NULL, x = "% limited English proficiency households (town + tracts)", 
       title = "Towns with tracts with > 10% linguistically-isolated households") + 
  scale_x_continuous(labels = percent) + 
  theme_minimal()

library(plotly)
py <- plotly()
py <- plotly(p)
