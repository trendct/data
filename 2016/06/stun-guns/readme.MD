
# Analyzing police stun gun deployments in Connecticut

This repository contains source data and analysis code for the Trend CT story:

* [Minorities more likely to be shot with stun guns in Connecticut](http://trendct.org/2016/06/30/minorities-more-likely-to-be-shot-with-stun-guns-in-connecticut/)
* [Who, where and how often people are stunned by police in Connecticut](http://trendct.org/2016/07/19/who-where-and-how-often-people-are-stunned-by-police-in-connecticut/)

# Walkthroughs

* Data analysis of police stun gun use in CT - [[RMarkdown](https://github.com/trendct/data/blob/master/2016/06/stun-guns/index.Rmd)] [[HTML](http://trendct.github.io/data/2016/06/stun-guns/)]

### The Data

* 2015 Reported Taser Data - [CCSU](https://github.com/trendct/data/blob/master/2016/06/stun-guns/data/2015%20Reported%20Taser%20Data.xlsx) 
* deployed - stun deployment frequency by race - [CSV](https://github.com/trendct/data/blob/master/2016/06/stun-guns/data/deployed.csv)
* weapons wielded by suspect- [CSV](https://github.com/trendct/data/blob/master/2016/06/stun-guns/data/weapons.csv)

### What's in this repo

* `data` [folder]((https://github.com/trendct/data/tree/master/2016/06/stun-guns/data)) with raw data, modified data.
* `analysis.R` is an R script analyzing the datasets.
* `.png` files of various chart outputs

*Bonus:* As an interesting exercise, my colleague Jake Kara, also worked did a data analysis for story but [with Python](https://github.com/trendct/data/tree/master/2016/06/taser-incidents).

We livestreamed our process. Jake with Python [[video](https://www.livecoding.tv/jakekara/videos/aEEG3-ct-stun-gun-analysis-3)]. Me with R (Sorry, I didn't realize I was on mute for the first part) [[video](https://www.livecoding.tv/abtran/videos/BMMN3-analyzing-stun-gun-use-data-with-r/)].

----

### Philosophy

We believe in open data. Our scripts may be too technical for the average reader, but we make it accessible because we believe it is important to be as transparent as possible about our methodology so our work can be checked or expanded upon. 

Check out the reproducible scripts and data behind many of our other stories in our [central repo](https://github.com/trendct/data)

Follow us out on Twitter [@TrendCT](http://www.trendct.org) and on [Facebook/TrendCT](https://www.facebook.com/trendct/).

### Usage

If you use our data or methodology, please give us a shout out in your story. It'd also be nice if you gave me a heads up: [@abtran](http://www.twitter.com/abtran) or abtran@trendct.org.
