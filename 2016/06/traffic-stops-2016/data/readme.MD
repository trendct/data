## What's in the data folder

The generated dataframes used in the Trend CT series: [Cops, Stops & Race](http://trafficstops.trendct.org/).

Also, the data listed in the appendix of the CCSU Connecticut Racial Profiling Prohibition Project [report](http://www.ctrp3.org/reports/)

The raw data for all ~600,000 traffic stops conducted in Connecticut between 2014 and 2015 is too large to host on Github. Download that at [CTData.org](http://ctrp3.ctdata.org/).

**Note:** If you try to run these scripts, it will not work until you create a `ignore` folder and place the spreadsheet there first.

-----

* `2013-2014-Department-Location-Data.xlsx` - Traffic stops with location data for 10 towns (between 2013 and 2014)
* `adjacent_search.csv` - A generated dataframe exploring police departments that made traffic stops in adjacent towns
* `black_residents.csv` - Culled from the appendix of the CCSU  Connecticut Racial Profiling Prohibition Project report. Includes population, difference between state and town average population, difference between state and town average traffic stops, distance between those differences, as well as ratio.
* `dept_points.csv` - As calculated based on the weighted scores explained in the CCSU report.
* `dictionary.csv` - data dictionary of `mega_df.csv`
* `edp_black.csv` - Culled from the appendix of the CCSU  Connecticut Racial Profiling Prohibition Project report. Includes estimated black driving population, estimated black driving population percent, estimated driving population stop percent, the difference and ratio
* `edp_hispanic.csv` - Culled from the appendix of the CCSU  Connecticut Racial Profiling Prohibition Project report. Includes estimated Hispanic driving population, estimated Hispanic driving population percent, estimated driving population stop percent, the difference and ratio
* `edp_minorities.csv` - Culled from the appendix of the CCSU  Connecticut Racial Profiling Prohibition Project report. Includes estimated minority driving population, estimated minority driving population percent, estimated driving population stop percent, the difference and ratio
* `fbi-department-data.csv` - Curated from the FBI UCR data
* `forcarto.csv` - Dataframe of all incidents from the 10 or so towns geolocated and condensed to a single day-- simplified for our [visualization on CartoDB](https://abtran.cartodb.com/viz/db57e306-3359-11e6-870e-0e674067d321/embed_map).
* `groton.csv` - Extracted sheet from `2013-2014-Department-Location-Data.xlsx` specific to Groton
* `hamden.csv` - Extracted sheet from `2013-2014-Department-Location-Data.xlsx` specific to Hamden
* `hispanic_residents.csv` - Culled from the appendix of the CCSU  Connecticut Racial Profiling Prohibition Project report. Includes population, difference between state and town average population, difference between state and town average traffic stops, distance between those differences, as well as ratio.
* `kpt.csv` - Culled from the appendix of the CCSU  Connecticut Racial Profiling Prohibition Project report. KPT means estimates.
* `mega_df.csv` - The whole shebang. The dataframe that is the basis of [our database](http://trafficstops.trendct.org/).
 * Here's the data dictionary for `mega_df.csv` - [Google Sheets](https://docs.google.com/spreadsheets/d/1DPgZVkltTwWBSlVPWruZfmp9T1VPRboLAI4BOzFMTyo/edit?usp=sharing)
* `mega_town_df11.csv` - Town-level summary of data. When joined with officer-level data, leads to `mega_df.csv`.
* `mega_towns.csv` - An extremely early version of the summary data. Not used in the final version
* `mega.csv` - One single dataframe of the spreadsheet above (Traffic stops with location data for 10 towns)
* `minorities_residents.csv` - Culled from the appendix of the CCSU  Connecticut Racial Profiling Prohibition Project report. Includes population, difference between state and town average population, difference between state and town average traffic stops, distance between those differences, as well as ratio.
* `over15_race.csv` - Raw data from Census ACS 2014 5-year data on residents over the age of 15. This was used in early versions of the analysis but was eventually replaced by the figures from the appendix of the CCSU report.
* `race_2014.csv` - Culled from the appendix of the CCSU  Connecticut Racial Profiling Prohibition Project report. Racial breakdown of Connecticut and towns
* `race_over15.csv` - Cleaned up data from `over15_race.csv` but ultimately not used in the final version of the analysis.
* `race_tracts.csv` - Race makeup of every census tract in Connecticut pulled from the Census ACS 2014 5-year data.
* `res_b.csv` - Culled from the appendix of the CCSU  Connecticut Racial Profiling Prohibition Project report. Includes black resident population and black residents stopped, as well as difference and ratio between those two stats.
* `res_h.csv` - Culled from the appendix of the CCSU  Connecticut Racial Profiling Prohibition Project report. Includes Hispanic resident population and Hispanic residents stopped, as well as difference and ratio between those two stats.
* `res_m.csv` - Culled from the appendix of the CCSU  Connecticut Racial Profiling Prohibition Project report. Includes minority resident population and minority residents stopped, as well as difference and ratio between those two stats.
* `searched.csv` - List of total searches by department, as well as percent of stops that resulted in searches.
* `searchhitrate.csv` - Searches versus contraband found for the 10 or so departments that provided specific lat/lon location data.
* `state_mega.csv` - State averages dataframe. Created in the early stages of the analysis and not used in the final version.
* `synth.csv` - Culled from the appendix of the CCSU  Connecticut Racial Profiling Prohibition Project report. Synthesis control coefficients and treatments by department and race.
* `town_age_group_summary.csv` - A slice of `mega_df.csv` but looking at age groups by town. Does not include officer-level data.
* `town_disposition_summary.csv` - A slice of `mega_df.csv` but looking at results post-stops. Does not include officer-level data.
* `town_gender_summary.csv` - A slice of `mega_df.csv` but looking at gender breakdown of stops. Does not include officer-level data.
* `town_intervention_stops.csv` - A slice of `mega_df.csv` but looking at type of stops. Does not include officer-level data.
* `town_race_summary.csv` - A slice of `mega_df.csv` but breaking out racial makeup of stops. Does not include officer-level data.
* `town_residents_stops.csv` - A slice of `mega_df.csv` but looking at how often residents were stopped. Does not include officer-level data.
* `town_searches_race_stops.csv` - A slice of `mega_df.csv` but looking at searches by race. Does not include officer-level data.
* `town_searches_stops.csv` - A slice of `mega_df.csv` but looking at how often residents were searched. Does not include officer-level data.
* `town_spots.checks_race_stops.csv` - A slice of `mega_df.csv` but looking at spot checks. Does not include officer-level data.
* `town_statute_code_summary.csv` - A slice of `mega_df.csv` but looking at the reasons why drivers were pulled over. Does not include officer-level data.
* `tracts_disparity.csv` - Comparing stops by town tracts between whites and other races.
* `tracts_population.csv` - Comparing population by town tracts between whites and other races.
* `tracts_stops.csv` - Racial percent of stops by tract in the towns that provided lat/lon data.
* `veil_it.csv` - Culled from the appendix of the CCSU  Connecticut Racial Profiling Prohibition Project report. Veil of Darkness estimate includes coefficient, SE, and ESS. Not used in our stories.
* `veil.csv` - Culled from the appendix of the CCSU  Connecticut Racial Profiling Prohibition Project report. Veil of Darkness estimate includes coefficient, SE, and ESS. Not used in our stories.



