
fancytable(by_townies2, headline = "Subsidies to businesses in CT since 1999", subhead = "", height = 400,
           paging = "false", sourceline = "", byline = "TrendCT.org", col = 0,
           desc_asc = "desc")

by_townies_head <- head(by_townies2, 7)

trendchart(by_townies_head, headline = "Towns with the most subsidies in CT", subhead = "", src = "",
           byline = "TrendCT.org", type = "column", xTitle = "", yTitle = "",
           xSuffix = "", ySuffix = "", xPrefix = "", yPrefix = "", option = "")


trendmap(by_townies2, headline="Subsidies to businesses in Connecticut by town", subhead="Since 1999.",
         src="subsidytracker.goodjobsfirst.org", byline="TrendCT.org", url_append="extra", shape="towns", color="yellow-red")
