## How we put together the opioid epidemic data analysis
Trend CT spent several weeks pulling, requesting, cleaning and analyzing different data sets to better understand Connecticut's intractable drug-overdose problem. We generated hundreds, if not thousands, of exploratory charts and maps before attempting to publish the [first story](http://overdose.trendct.org/story/main/) in our series.

### Overdose data
We had to rely on overdose data from the following sources:

* Published [raw data](https://data.ct.gov/Health-and-Human-Services/Accidental-Drug-Related-Deaths-January-2012-Sept-2/rybz-nyjw) from the Office of the Chief Medical Examiner
* Data cleaned by the Connecticut Data Collaborative [[individual](http://data.ctdata.org/dataset/accidental-drug-related-deaths-by-individual-drugs-detected)] [[by type](http://data.ctdata.org/dataset/accidental-drug-related-deaths-by-drug-type)]
* Underlying raw [map data](https://github.com/trendct/ct_opioid_epidemic/blob/master/data/raw_data_2014.csv) specific to overdoses in 2014 from the state that contained specific address and latitude and longitude details
* Accidental deaths from [1999 to 2013](https://github.com/trendct/ct_opioid_epidemic/blob/master/data/deaths_in_ct_since_1999.csv) by age group, gender, and race

### Scripts
The reproducible scripts detailing our methodology can be found on the Trend CT github [repo](https://github.com/trendct/ct_opioid_epidemic).

One script [marked](https://github.com/trendct/ct_opioid_epidemic/blob/master/calendar_town.R) the overdose deaths over time by town. Another created different combinations of small multiple charts for race, gender, and towns based on [individual drugs](https://github.com/trendct/ct_opioid_epidemic/blob/master/ind_drug.R). For the second half of the series, we used our own [method](https://github.com/trendct/ct_opioid_epidemic/blob/master/prepping_data.R) of cleaning up the data to categorize deaths by prescription opioids, heroin, and fentanyl in a way that was more consistent than what was noted in the raw data.

Those who are interested in the analysis process can see the dozens of scripts in the repo to see how it was done and [exploratory angles](https://github.com/trendct/ct_opioid_epidemic/blob/master/geospatial_analysis.R) that never made it into the final series but might be revisited in the future. Please let us know if there are any suggestions to improve our analysis.

We'd like to extend our thanks to the many researchers and health officials who gave us ideas on how to approach our analysis.

### RMarkdown files

Here's a list of some scripts that were turned into RMarkdown for web publishing/sharing during the analysis. Some files are really large so please don't open on a phone.

* Town death timelines [[28.4 mb](http://overdose.trendct.org/analysis/town_timelines.html)]
* Initial drug overdose deaths analysis [[6.1 mb](http://overdose.trendct.org/analysis/initial_analysis.html)]
* Individual drugs by town [[56.8 mb](http://overdose.trendct.org/analysis/ind_drug_towns.html)]
* Individual drugs by demographic [[8.7 mb](http://overdose.trendct.org/analysis/ind_drug_demo.html)]
* Geospatial analysis [[15.4 mb](http://overdose.trendct.org/analysis/geospatial_analysis.html)]
* Drug type by town [[34 mb](http://overdose.trendct.org/analysis/drug_type_towns.html)]
* Drug type by demographic [[5.5 mb](http://overdose.trendct.org/analysis/drug_type_demo.html)]

### Trend CT story links

* Why Connecticut's drug overdose crisis isn't slowing down [[link](http://overdose.trendct.org/story/main/)]
* Who is dying in Connecticut’s opioid overdose crisis? [[link](http://overdose.trendct.org/story/who)]
* Where drug abusers overdose in Connecticut [[link](http://overdose.trendct.org/story/where)]
* What can be done to curb the drug overdose deaths [[link](http://overdose.trendct.org/story/what)]
* How we put together the opioid epidemic data analysis[[link](http://trendct.org/2016/03/15/how-we-put-together-the-opioid-epidemic-data-analysis/)]
