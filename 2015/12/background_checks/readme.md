
An [analysis in RMarkdown](http://trendct.github.io/background_checks/) for the TrendCT story: [Increase in gun purchases triggered by Connecticut state legislation](http://trendct.org/2015/12/14/increase-in-gun-purchases-triggered-by-connecticut-state-legislation/)

The data is/are from the [Buzzfeed repo](https://github.com/BuzzFeedNews/nics-firearm-background-checks) that scraped the [FBI NICS](https://www.fbi.gov/about-us/cjis/nics) PDFs on the number of firearm checks by month, state, and type.

Check the Trend CT [repo](https://github.com/trendct/background_checks/) for the scripts and [data sets](https://github.com/trendct/background_checks/tree/master/data) that were used and generated. Just want the seasonally adjusted state per capita data? Here's [the csv](https://github.com/trendct/background_checks/blob/master/data/states_monthly_adjusted.csv).

There are many caveats to the data, but the basic thing to take away is that a background check does not necessarily mean a gun sale. The [Buzzfeed repo](https://github.com/BuzzFeedNews/nics-firearm-background-checks) has more details.

This [analysis](http://trendct.github.io/background_checks/) will go over the process used to put together [the story](http://trendct.org/2015/12/14/increase-in-gun-purchases-triggered-by-connecticut-state-legislation/). In short, the background checks were normalized for annual population by state, adjusted for seasonality, and then charted.
