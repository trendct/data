
# Analyzing pre-trial inmates in Connecticut

This repository contains source data and analysis code for the Trend CT story:

* [Who’s sitting in pretrial detention in Connecticut](http://trendct.org/2016/07/20/pre-trial-inmates/). 

# Walkthroughs

* Data analysis of pre-trial jail inmates- [[RMarkdown](https://github.com/trendct/data/blob/master/2016/07/pre-trial-inmates-ct/index.Rmd)] [[HTML](http://trendct.github.io/data/2016/07/pre-trial-inmates-ct/)]

### The Data

* assorted summary dataframes on race and offenses
* daily downloads of the listed inmates (updated list is on the [CT data portal](https://data.ct.gov/Public-Safety/Accused-Pre-Trial-Inmates-in-Correctional-Faciltie/b674-jy6w?category=Public-Safety))

### What's in this repo

* `data` [folder]((https://github.com/trendct/data/tree/master/2016/07/pre-trial-inmates-ct/data)) with raw data and modified data.
* `analysis.R` is an R script analyzing the datasets and exploratory visualizations.
* `dailydownload.R` is an R script that will download the latest version of the inmate dataset and download it to the `data` folder.
* `time_in_jail.R` is a script that will parse through the overall dataset and determine when inmates entered and exited the system, as well as if they're still in the system. This is appended to the downloaded dataset. 
* `.png` files of various chart outputs

----

### Philosophy

We believe in open data. Our scripts may be too technical for the average reader, but we make it accessible because we believe it is important to be as transparent as possible about our methodology so our work can be checked or expanded upon. 

Check out the reproducible scripts and data behind many of our other stories in our [central repo](https://github.com/trendct/data)

Follow us out on Twitter [@TrendCT](http://www.trendct.org) and on [Facebook/TrendCT](https://www.facebook.com/trendct/).

### Usage

If you use our data or methodology, please give us a shout out in your story. It'd also be nice if you gave me a heads up: [@abtran](http://www.twitter.com/abtran) or abtran@trendct.org.