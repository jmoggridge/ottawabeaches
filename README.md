# Ottawa Beaches dashboard

This is the repository for the Ottawa Beaches dashboard at https://jmoggridge.shinyapps.io/OttawaBeaches/.

---

## About 

**The goal of the project is to create a *forecast for beach swimming***: a tool that predicts whether fecal indicator bacteria (FIB, ie. *E. coli*) abundance will be in excess of safety thresholds in real-time and into the future.  The motivation for this project is curiosity; I am not affiliated with the city or being compensated by anyone for this work. I am currently a grad student in Bioinformatics; this side project provides an avenue for me to learn and apply statistics, coding, and science communication skills with real-world datasets, a tangible goal, and potential benefits for people in my hometown.

I plan on **modelling & forecasting for E. coli abundance during the 2020 season** based on environmental factors, location, previous day's count, etc. Predictions will be evaluated against the accuracy of the current persistence model for testing. If a predictive model is shown to be useful, I hope to publish the results and work in collaboration with the city to integrate modelling into their beach water monitoring system.

**The dashboard currently shows visualizations of beach water testing data from the City of Ottawa 2014-2019**. To my knowledge, no other comprehensive set of figures exists in the public domain for this data (as of May 29th, 2020); the dashboard should be very useful to anyone interested in beach water monitoring in the region.

---


## Creating a Predictive tool for Fecal Indicator Bacteria (FIB)


#### A six-step process from USGS to create the modelling system**

I am using [this guide from USGS](https://www.epa.gov/beach-tech/six-key-steps-developing-and-using-predictive-tools-your-beach) to create the FIB prediction tool for Ottawa beaches. Many other places have developed their tools with this guide and often it has been grad students (like me) doing the testing and/or modelling work for thesis projects, in collaboration with local beach administrators.

Below is documentation of my process through their checklist (*italics*). I've added notes with information about Ottawa beaches and how I'm applying the information from the USGS guide. The process will eventually build the case for a predictive model or demonstrate why it isn't appropriate for Ottawa beaches, given the data (we'll find out!).



**1. *Evaluate the appropriateness of a FIB predictive tool***

  - [x] *Is there a need for the tool?*
  
    - Ottawa beaches often close due to large E. coli counts (10% of total beach-days 2014-2019); recreational swimming carries the some risk of illness that we would like to minimize through closures while still maximizing safe swimming days.
    - The current daily testing system assumes a 'persistence model' with FIB remaining roughly constant over the testing interval (24hr)
       - in short: The advisory for today is based on yesterday's FIB. We don't *know* current level of risk for today.
    - but FIB could change substantially during the 24 hr lab time leading to an inaccurate advisory.
    - The main purpose of the tool is to predict events where FIB might surpass safety thresholds during testing lag-time.
    - The 'persistence model' might be complemented such a predictive tool
   
  - [x] *Are beach and monitoring program characteristics compatible with predictive tools?*
  
      - *'The beach operates under a constant range of "normal" conditions'*
        - Ottawa beaches conditions during summer are fairly stable with the exception of major precipitation events. Range of 'normal conditions' that do not vary greatly from year to year.
        - River beaches are mostly impacted by rainfall causing agricultual runoff and sewage outflows.
        - The number of dry days preceding rainfall influences the effect of rainfall ('first-flush' phenomenon).
    
      - *'Exceedences of beach notification thresholds occur occasionally but are not a chronic problem'*
        - Ottawa beaches have ~10% FIB closures and ~5% rainfall closures - I think our beaches fit this description well.
        
      - *'FIB densities change over relatively short periods of time (time-lag problem)'*
        - To be assessed and demonstrated
        - I looked at the 24hr differences between all observations and plotted these for each beach location.
        - FIB can change greatly over a 24 hr period. Persistence model prediction is that FIB_today = FIB_yesterday
        - Ie. 'persistent model' predictions vs actual timeseries by shifting the E.coli abundance lines 1 day to right and take the difference. Plot differences in a histogram.
        - Find number of days where persistence model provides accurate positive, false positive, accurate negative, false negative.

     - *'Sufficent amount of historical FIB and independent variables data exists'*
       - Six years of beach data for 5 locations. Daily testing in season will need to be available.
       - lots of meterological data can be linked to FIB observations. Real-time available.
         - Currently I have daily data but hourly data could improve the model later
       - Some hydrology data: flow and level; for both rivers at one station each. Real-time data available from EC.
       - no data on beach use/ number of bathers?
       - might be able to find more useful data about river turbity and chemistry: pH, solutes, etc. ?
       - other orgs: OttawaRiverKeeper, Universities, more weather info?
       - Overall, a large amount of data is already publicly available.

     - *'Funding, Monitoring, Resources'*
       - Monitoring already being performed. Advisories are issued daily.
       - I am personally contributing time for development and using personal resources as well as free software and hosting.
       - If the model is to go into action for public, would need to be maintained over time and updated.
       - I'm thinking about writing a grant proposal after building the models and a testing'
       - Possible COVID19 stimulus project for the City.
       - Possible collaboration with local univeristies and government organizations to increase resources available
       - Possible collaboration with the public for data collection (*eg.* count bathers and enter online)
      
      
      
**2. *Identify variables and collect data*** 

  - *USGS guide recommends 2 years for training and a year for testing models*
    - As noted above, daily testing is already in place and there is six years of historical data (possibly more not publicly available).
    
  - *Data needs to be high quality, consistent, easily-obtained, & temporally-relevant*
    - I feel that the rigorous water testing & available environmental data fulfill these requirements.
    - Need to confirm that testing methods were consistent over period 2014-2019
    - Need to ensure that this information will be able to be scraped for up-to-date predictions when model is deployed
    
  - *Identify independent variables of interest for modelling*
    - Beach location
    - previous FIB measurements
    - Meterological: temperature, precipitation; possibly also wind, UV, cloud-cover
    - Hydrological: river flow and level. Also interested in obtaining turbity and pH data if possible.
    - Time of year (seasonality)
   
   
   
 **3. *Perform Exploratory Data Analysis***
   
  - finished:
     - plot FIB distributions for different beach locations and years
       - Dashboard > Statistics > Distributions
       - there are some interesting differences here between locations, years, month of year
     - get basic statistics for FIB & summarize
       - geomtric means, medians, variance, min/max info
       - need to add to dashboard under 'Statistics'>'Distributions'>'Summary tables'
       
  - in progress:
     - I am working on looking at the interactions between environmental variables and FIB
     - trying different statistics from meterological data that have most predictive value
       - Recent rainfall statistic: USGS example is 3d weighted sum; can also try 2d, 1d; unweighted.
       - Recent temperatures statistics: means, highs, lows.
       
  - to do:
     - 24-hr change statistics & summary table
  
 **4. *Develop and test a predictive model***

  - *evaluate accuracy of the 'persistence model'*
  - *develop predictive model*
    - *train predictive model / test predictive model*
  -  *evaluate predictive model vs persistence model for accuracy*
  
  
   - a hypothetical generalized linear model for FIB using the negative binomial distribution:
     -  `glm.nb(FIB ~ previous FIB + rain + temperature + day of year + location + river activity)`
   - in progress
   
  
 **5. *Integrate Predictive Tool into beach monitoring/notification program***
 
   - This will depend on whether the tool is shown to be accurate & useful first. Then it will depend on collaboration with the city and obtaining a small amount of funding to further develop/validate/implement the model.
  
 **6. *Evaluate predictive tool over time***
 
   - New beach seasons should provide further training data and make predictions more accurate over time.
   - the model can be refined as new variables of interest are tested and incorporated into the model (eg. wildlife migrations)
  
---
  
  
# Technical details about dashboard
  
  
## Github repository


### Folders

  - Code: all the code I ran to tidy data, create visualizations, calculate statistics, create models.
  - Data - raw and tidy'd dataframes of input dataframes are here (input only)
  - Results - dataframes of compute results: summary tables, predictions, modelling outputs, etc. (output only)

### Scripts

**app.R** script has both the UI and the server chunks. The top of the file has some thematic elements and plotting functions that are reused for a bunch of plots in the server chunk; I plan on adjusting the plot themes later to create a minimal, notebook style. The UI layout consists mostly of nested tabSets until I figure out how to do reactive plots a bit more efficiently through drop down menus (eg for plot style in the coliform distributions analyses).

**parse.R** is the script for creating data-frames from public datasets. There are currently three main data sources: *beaches* city data; *weather* data from Ottawa airport; and *rivers* hydrological data from measuring stations at Britannia and Hog's Back (I think).
 
 **gloabl.R** eventually this script will contain all the static code that isn't UI or reactive server chunks, ie. it will contain all the plotting functions, thematic elements, figure captions, and body text.
 
NOTE TO SELF: this needs added info about how to convert all the mislabelled statuses in the original raw data tables

---
