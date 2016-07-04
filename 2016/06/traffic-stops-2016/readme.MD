# Analyzing racial profiling from Connecticut's traffic stop data

### This repository contains source data and analysis code for the Trend CT stories and database:

* [Explore: Trafic stops across Connecticut](http://trafficstops.trendct.org/data/)
* [Where and why drivers are pulled over in Connecticut](http://trafficstops.trendct.org/story/where-and-why-drivers-are-pulled-over-in-connecticut/)
* [Digging deeper into racial disparities in Connecticut traffic stops](http://trafficstops.trendct.org/story/digging-deeper-into-racial-disparities-in-ct-traffic-stops/)
* [How the 'Veil of Darkness' sheds light on racial profiling](http://trafficstops.trendct.org/story/how-veil-of-darkness-sheds-light-on-racial-profiling/)
* [Case studies: Which neighborhoods police focus traffic enforcement](http://trafficstops.trendct.org/story/case-studies-which-neighborhoods-police-focus-traffic-enforcement/)
* [Stopped drivers let go about half the time](http://trafficstops.trendct.org/story/stopped-drivers-let-go-about-half-the-time/)
* [Black and Hispanic drivers searched twice as often as white drivers in Connecticut](http://trafficstops.trendct.org/story/black-hispanic-drivers-searched-twice-as-often-as-white-drivers-in-connecticut/)
## Data

#### Some of the data is too large to be listed in this repo, but can be downloaded elsewhere
* Stops between October 2014 and September 2015 - [CT Data Collaborative](http://ctrp3.ctdata.org/rawdata/)
* `[data](https://github.com/trendct/data/tree/master/2016/06/traffic-stops-2016/data)` - Folder where all dependent and generated data resides

## Walkthroughs

* `location_analysis` - [[rmd](https://github.com/trendct/data/blob/master/2016/06/traffic-stops-2016/location_analysis.Rmd)][[html](http://trendct.github.io/data/2016/06/traffic-stops-2016/location_analysis.html)]] - Creates many exploratory charts and maps-- many of which ended up being used in the story [CASE STUDIES: WHICH NEIGHBORHOODS POLICE FOCUS TRAFFIC ENFORCEMENT](http://trafficstops.trendct.org/story/case-studies-which-neighborhoods-police-focus-traffic-enforcement/)
* `map` - [[rmd](https://github.com/trendct/data/blob/master/2016/06/traffic-stops-2016/map.Rmd)][[html](http://trendct.github.io/data/2016/06/traffic-stops-2016/map.html)]] - Generated exploratory maps from the script `town_borders_location_analysis.R`. None ultimately used in the published series.
* `mockup` - [[rmd](https://github.com/trendct/data/blob/master/2016/06/traffic-stops-2016/mockup.Rmd)][[html](http://trendct.github.io/data/2016/06/traffic-stops-2016/mockup.html)]] - Multiple exploratory charts analyzing a single police department to see what was most effective and narrow down what to include in the [overall database](http://trafficstops.trendct.org/data/).
* `mockup_officer` - [[rmd](https://github.com/trendct/data/blob/master/2016/06/traffic-stops-2016/mockup_officer.Rmd)][[html](http://trendct.github.io/data/2016/06/traffic-stops-2016/mockup+officer.html)]] - Multiple exploratory charts analyzing a single police officer to see what was most effective and narrow down what to include in the [overall database](http://trafficstops.trendct.org/data/).
* `search_contraband` - [[rmd](https://github.com/trendct/data/blob/master/2016/06/traffic-stops-2016/search_contraband.Rmd)][[html](http://trendct.github.io/data/2016/06/traffic-stops-2016/search_contraband.html)]] - Multiple exploratory charts focusing on data used in story 5: [BLACK AND HISPANIC DRIVERS SEARCHED TWICE AS OFTEN AS WHITE DRIVERS IN CONNECTICUT](http://trafficstops.trendct.org/story/black-hispanic-drivers-searched-twice-as-often-as-white-drivers-in-connecticut/)
* `story1` - [[rmd](https://github.com/trendct/data/blob/master/2016/06/traffic-stops-2016/story1.Rmd)][[html](http://trendct.github.io/data/2016/06/traffic-stops-2016/story1.html)]] - Multiple exploratory charts focusing on data used in story 1: [DIGGING DEEPER INTO RACIAL DISPARITIES IN CONNECTICUT TRAFFIC STOPS](http://trafficstops.trendct.org/story/digging-deeper-into-racial-disparities-in-ct-traffic-stops/)
* `story1b` - [[rmd](https://github.com/trendct/data/blob/master/2016/06/traffic-stops-2016/story1b.Rmd)][[[html](http://trendct.github.io/data/2016/06/traffic-stops-2016/story1b.html)]] - Even more exploratory charts focusing on data used in story 1: [DIGGING DEEPER INTO RACIAL DISPARITIES IN CONNECTICUT TRAFFIC STOPS](http://trafficstops.trendct.org/story/digging-deeper-into-racial-disparities-in-ct-traffic-stops/)
* `veil2` - [[rmd](https://github.com/trendct/data/blob/master/2016/06/traffic-stops-2016/veil2.Rmd)][[html](http://trendct.github.io/data/2016/06/traffic-stops-2016/veil2.html)]] - Exploratory charts illustrating the Veil of Darkness used in the story [HOW THE 'VEIL OF DARKNESS' SHEDS LIGHT ON RACIAL PROFILING](http://trafficstops.trendct.org/story/how-veil-of-darkness-sheds-light-on-racial-profiling/)
* `veilofdarkness` - [[rmd](https://github.com/trendct/data/blob/master/2016/06/traffic-stops-2016/veilofdarkness.Rmd)][[html](http://trendct.github.io/data/2016/06/traffic-stops-2016/veilofdarkness.html)]] - Exploratory charts illustrating the Veil of Darkness used in the story [HOW THE 'VEIL OF DARKNESS' SHEDS LIGHT ON RACIAL PROFILING](http://trafficstops.trendct.org/story/how-veil-of-darkness-sheds-light-on-racial-profiling/)

## Scripts

* `bubble_chart_maker.R` - Converts department-level summaries into a json array for a highcharts connected-scatterplot visualization
* `location_prep.R` - Brings in data from an Excel sheet that includes latitude and longitude data for traffic stops from 8 departments. Creates `mega.csv` 
* `town_borders_location_analysis.R` - Some exploratory maps looking at location-specific traffic stops along borders of towns
* `officers.R` - Run after `towns.R`. Adds officer-level analysis and adds it to the department-level summary to create `mega_df.csv`.
* `official_data_merging.R` - Creates `ccsu_data.csv` from various spreadsheets of figures calculated by CCSU's Institute for Municipal and Regional Policy, such as coefficients and k means values.
* `overall_analysis.R` - Script that creates a couple bar charts exploring overall stops by day of week and hour of the day
* `points.R` - Generates [exploratory chart](http://trendct.org/wp-content/blogs.dir/60/files/2016/05/disp_points2-copy-771x411.png) of disparity points based on `mega_df.csv` 
* `scatter_maker.R` - Writes three javascript arrays that created scatter plots used in [this story](http://trafficstops.trendct.org/story/digging-deeper-into-racial-disparities-in-ct-traffic-stops/)
* `time.R` - Testing chart visualization of Veil of Darkness data using Hamden locations
* `time_all.R` - Adds columns determining if a stop happened in sunlight or darkness, adds time of sunrise, sunset, dusk, dawn, and adds other date-specific info to help chart later on
* `time_version_a.R` - Same as above but for 2013-2014 data instead of 2014-2015.
* `towns.R` - Run before `officers.R`. Create a town-level analysis dataframe (`mega_town_df11.csv`), brings in `ccsu_data.csv` to replace calculations with CCSU's "official" figures.

### Images

* `img` folder inlcudes many generated exploratory charts, some of which was used in final stories

### Maps

* `maps` folder includes shape files used to generate choropleths in the walkthroughs and scriopts
