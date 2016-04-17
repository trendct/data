## What's in the data

-----

**ad_buys.csv**: Data parsed with `parser_totals.R` of dozens of spreadsheets converted from PDFs with the cometdocs.com service

* `time.period.start`: When ads would start, according to TV station
* `time.period.end`: When ads would end, according to TV station
* `spots`: Total number of spots purchased in that time period
* `gross.amount`: Total spent on ads 
* `agency.commission`: How much salesperson made
* `net.amount`: Total spent on ads minus commissoin
* `sheet`: Identifying PDF name to easily join it with new dataframe

**ads_dataframe.csv**: Dataframe pulled from the Sunlight Foundation's [PoliticalAdSleuth.com](http://politicaladsleuth.com)

* `TV Station`: TV station that filed contract to FCC
* `Market`: Local markets the station covers
* `Date`: When the TV station filed the contract to the FCC
* `Type`: What type of political ad. This one was all presidential.
* `Status`: If loaded in PAS system
* `Spots`: Number of spots if data available (usually input by volunteers)
* `Cost` Cost of spots if data available (usually input by volunteers)
* `Advertiser/File Info`: What group bought the ads
* `link`: Link to PAS page which has link to FCC PDF
* `doc`: Link to FCC PDF
* `sheet`: Identifying PDF name to easily join it with new dataframe


**ct_primary_ads_041516.csv**: The two dataframes joined above