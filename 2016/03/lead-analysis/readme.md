
## Lead in water data analysis repository

This repository contains source data and analysis code for the following stories published on TrendCT.org. 
* [Which neighborhoods have the highest risk of lead poisoning in CT?](http://trendct.org/2016/04/08/where-the-risk-for-lead-poisoning-in-connecticut-is-highest/)
* [Drinking water violations across Connecticut](http://trendct.org/2016/04/06/drinking-water-violations-across-connecticut)
* [Where lead was detected in Connecticut’s drinking water](http://trendct.org/2016/04/04/where-lead-was-detected-in-connecticuts-drinking-water) 
* [Most CT schools don’t test water for lead, but that could change](http://ctmirror.org/2016/04/01/most-ct-schools-dont-test-water-for-lead-but-that-could-change)

It may be too technical for the average reader, but we make it accessible because we believe it is important to be as transparent as possible about our methodology so our work can be checked or expanded upon.

Check us out on Twitter [@TrendCT](http://www.trendct.org) and on [Facebook/TrendCT](https://www.facebook.com/trendct/).

### What's in this repo

* Folder with raw data, data dictionary, as well as cleaned up data used in filterable table
* Scripts used to pull violations and action data from the EPA drinking water database
* [Walkthrough](http://trendct.github.io/data/2016/03/lead-analysis/elevated_lead_levels.html): Methodology and process (RMarkdown) of analysis behind the story [Where lead was detected in Connecticut’s drinking water](http://trendct.org/2016/04/04/where-lead-was-detected-in-connecticuts-drinking-water)
* HTML of leaflet mapping output used in [Where lead was detected in Connecticut’s drinking water](http://trendct.org/2016/04/04/where-lead-was-detected-in-connecticuts-drinking-water) 
* [Walkthrough](http://trendct.github.io/data/2016/03/lead-analysis/lead_risk_analysis.html): Data and methodology behind the story [Which neighborhoods have the highest risk of lead poisoning in CT?](http://trendct.org/2016/04/08/where-the-risk-for-lead-poisoning-in-connecticut-is-highest/) which was based on Vox's [lead exposure project](https://github.com/voxmedia/data-projects/tree/master/vox-lead-exposure-risk).


### How to get the list of lead violations from the EPA

Lead ALE violations in CT

1. Visit the SDWIS [Federal Reports Search](https://ofmpub.epa.gov/apex/sfdw/f?p=108:200:::NO)
2. Click the 'Advanced Search Options' button
3. In the top-most section, select 'Lead and Copper Report'
4. Select 'Lead ALE Samples' under Choose Report
5. Select 'Connecticut' from 'Primary Agency'
6. Set Sample Start Date and Sample End Date to '1/1/2000' and '1/1/2016'
7. Click the 'View Reports' button at the top right

### How to get the list of all drinking water violations from the EPA

1. Visit the SDWIS [Federal Reports Search](https://ofmpub.epa.gov/apex/sfdw/f?p=108:200:::NO)
2. Click the 'Advanced Search Options' button
3. In the top-most section, select 'Violations'
4. Select 'Connecticut' from 'Primary Agency'
5. Set Compliance Period Start Date and Compliance Period End Date to '1/1/2000' and '1/1/2016'
6. Click the 'View Reports' button at the top right

