
# Connecticut's primary presidential election

This repository contains source data and analysis code for the Trend CT and Connecticut Mirror stories:

* [Analyzing Connecticut’s Democratic primary results](http://trendct.org/2016/04/29/analyzing-connecticuts-democratic-primary-results/)
* [Analyzing Trump’s win in Connecticut’s Republican primary](http://trendct.org/2016/05/02/analyzing-trumps-win-in-connecticuts-primary-election/).

# Walkthroughs

* Analyzing Connecticut’s Democratic primary results - [[RMarkdown](http://trendct.github.io/data/2016/04/primaries-2016/analyzing2008-2016.html)]
* Analyzing Connecticut’s Republican primary results - [[RMarkdown](http://trendct.github.io/data/2016/04/primaries-2016/rep_analysis.html)]

### The Data

* From reports generated via the new [Election Center](http://ctemspublic.pcctg.net/#/home) data portal from the Connecticut Secretary of the State.

* **Note:** Data is only as recent as 5/02/16.
* Attorneys still need to go through the figures before results can be 'official'.

### What's in this repo

* Folder with [raw data](https://github.com/trendct/data/tree/master/2016/04/primaries-2016/data), data dictionary, as well as cleaned up data used in story visualizations.
* `clinton08-16.R` is an R script comparing Hillary Clinton's primary performance in 2008 and 2016.
* `dem_parser.R` is an R script that parses the Democratic district-wide report from the Office of the Secretary of the State and generateds a JSON file.
* `processing_results.R` is an R script that cleans and creates a new dataframe based on town-wide reports from the Office of the Secretary of the State.
* `rep_parser.R` is an R script that parses the Republican district-wide report from the Office of the Secretary of the State and generateds a JSON file.
* `turnout.R` analyzes primary voter turnout by town.


### Philosophy

We believe in open data. Our scripts may be too technical for the average reader, but we make it accessible because we believe it is important to be as transparent as possible about our methodology so our work can be checked or expanded upon. 

Check us out on Twitter [@TrendCT](http://www.trendct.org) and on [Facebook/TrendCT](https://www.facebook.com/trendct/).

### Usage

If you use our data or methodology, please give us a shout out in your story. It'd also be nice if you gave me a heads up: [@abtran](http://www.twitter.com/abtran) or abtran@trendct.org.

