# Style guide for Trend CT data repositories

**Structure for including analysis data and code behind TrendCT.org stories**

We believe in open data. Our scripts may be too technical for the average reader, but we make it accessible because we believe it is important to be as transparent as possible about our methodology so our work can be checked or expanded upon. [[Read more on our data sharing philosophy](https://github.com/trendct/data)]

### Requirements
* The raw data or a link to the raw data if too large
*  Downloading and running the code as instructed will produce the data and exploratory charts used in our stories
* The final polished data set itself will be an easily found location in the repos for those who don't care about the transformation or analysis process
* Our scripts will be clearly documented and can serve as a way for readers to learn advanced data analysis techniques
* A data dictionary that summarizes all the data frames in the data folder of the repo
  * Optional data dictionary summarizing each individual column in the dataframes
* Link to source data

**The root project repo folder should have a readme.MD file with the following:**

* Title of the analysis
* Explicit link to the story the analysis supports
* Explicit link to the scripts or walkthroughs (RMarkdown or iPython file) 
* List of raw data used and link to source
* List of what’s in the repo
  * Data folder, specific scripts, specific files
* Philosophy and Usage paragraphs
* Example [[link](https://github.com/trendct/data/tree/master/2016/06/hiv_ct)]

** The `data` folder should also have a readme.MD file with the following: **

* Where the data came from and link to source
* List of files in the data repo and a line describing each one
* Example [[link](https://github.com/trendct/data/tree/master/2016/06/hiv_ct/data)]

**Update the readme.MD file on the root data page at https://github.com/trendct/data**

* Update the top of this [Google Spreadsheet](https://docs.google.com/spreadsheets/u/1/d/12OOSO9-cJyqGZ-jM8xmBRZloartq0-Wjn4StYOp7C_Y/edit) with
  * Story headline and link
  * Repo name and link
  * Data folder name and link
  * Methodology script name and link
* Use the TableMarkdownMaker plugin to copy the code over to the readme.MD file