
### This is an exploratory analysis of ad buys in Connecticut via presidential candidates leading up to the primary election

This repository contains source data and analysis code for the Trend CT story:

* [Bernie Sanders, Hillary Clinton campaigns increasing ad buys before CT primaries](http://trendct.org/?p=11872)

This analysis is based off data aggregated by the Sunlight Foundation's [PoliticalAdSleuth.com](http://politicaladsleuth.com).

* **Note:** Data is only as recent as 4/15/16.
* Primary election day is 4/26/16, so expect more filings between then.

It may be too technical for the average reader, but we make it accessible because we believe it is important to be as transparent as possible about our methodology so our work can be checked or expanded upon. 

Check us out on Twitter [@TrendCT](http://www.trendct.org) and on [Facebook/TrendCT](https://www.facebook.com/trendct/).

### What's in this repo

* Folder with raw data, data dictionary, as well as cleaned up data used in filterable table
* `ad_extractor.R` pulled the dataframe from politicaladsleuth.com and PDFs from the FCC
* `parser_totals.R` extracted data from the converted PDFs and pulled it into a dataframe
* [Walkthrough](http://trendct.github.io/data/2016/04/ad-buys-ct/ad-buys-analysis.html): Methodology and process (RMarkdown) of analysis behind the story.
* PDFs folder of tv station contracts for presidential ad buys. Includes schedules
* Spreadsheets of converted PDFs via the Cometdocs.com service

### Usage

If you use our data or methodology, please give us a shout out in your story. It'd also be nice if you gave me a heads up: [@abtran](http://www.twitter.com/abtran) or abtran@trendct.org.
