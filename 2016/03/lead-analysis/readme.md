
## Lead in water data analysis repository

This repository contains source data and analysis code for the following stories published on TrendCT.org. 

* [Drinking water violations across Connecticut](http://trendct.org/2016/04/06/drinking-water-violations-across-connecticut)
* [Where lead was detected in Connecticut’s drinking water](http://trendct.org/2016/04/04/where-lead-was-detected-in-connecticuts-drinking-water) 
* [Most CT schools don’t test water for lead, but that could change](http://ctmirror.org/2016/04/01/most-ct-schools-dont-test-water-for-lead-but-that-could-change)

It may be too technical for the average reader, but we make it accessible because we believe it is important to be as transparent as possible about our methodology so our work can be checked or expanded upon.

Check us out on Twitter [@TrendCT](http://www.trendct.org) and on [Facebook/TrendCT](https://www.facebook.com/trendct/).

### What's in this repo

* Folder with raw data, data dictionary, as well as cleaned up data used in filterable table
* Scripts used to pull violations and action data from the EPA drinking water database
* Methodology and process (RMarkdown) of analysis [[Lead](http://trendct.github.io/data/2016/03/lead-analysis/elevated_lead_levels.html)][[Violations](http://trendct.github.io/data/2016/03/lead-analysis/violations.html)]
* HTML of leaflet mapping output used in [Where lead was detected in Connecticut’s drinking water](http://trendct.org/2016/04/04/where-lead-was-detected-in-connecticuts-drinking-water) 

### How to get the list of data from the EPA

Lead ALE violations in CT

1. Visit the SDWIS [Federal Reports Search](https://ofmpub.epa.gov/apex/sfdw/f?p=108:200:::NO)
2. Click the 'Advanced Search Options' button
3. In the top-most section, select 'Lead and Copper Report'
4. Select 'Lead ALE Samples' under Choose Report
5. Select 'Connecticut' from 'Primary Agency'
6. Set Sample Start Date and Sample End Date to '1/1/2000' and '1/1/2016'
7. Click the 'View Reports' button at the top right




