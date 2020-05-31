# Ottawa Beaches dashboard

This is the repository for the Ottawa Beaches dashboard at https://jmoggridge.shinyapps.io/OttawaBeaches/.

---

## About 

**The goal of the project is to create a *forecast for beach swimming***: a tool that predicts whether fecal indicator bacteria (FIB, ie. *E. coli*) abundance will be in excess of safety thresholds in real-time and into the future.

I plan to generate forecast predictions for each beach during summer 2020 to assess the model accuracy. If the project is successful, the dashboard could serve as a template for jurisdictions to apply to their data for similar goals. The motivation for this project is curiosity; I am not affiliated with the city or being compensated by anyone for this work. I am currently a grad student in Bioinformatics; this side project provides an avenue for me to learn and apply statistics, coding, and science communication skills with real-world datasets, a tangible goal, and potential benefits for people in my hometown.

I plan on **modelling & forecasting for E. coli abundance during the 2020 season** based on environmental factors, location, previous day's count, etc. Predictions will be evaluated against the accuracy of the current persistence model for testing. If a predictive model is shown to be useful, I hope to publish the results and work in collaboration with the city to integrate modelling into their beach water monitoring system.

**The dashboard currently shows visualizations of beach water testing data from the City of Ottawa 2014-2019**. To my knowledge, no other comprehensive set of figures exists in the public domain for this data (as of May 29th, 2020); the dashboard should be very useful to anyone interested in beach water monitoring in the region.

---


## Creating a Predictive tool for Fecal Indicator Bacteria (FIB)


#### A six-step process from USGS to create the modelling system**

This is the guide I am using to create the prediction tool. see here: https://www.epa.gov/beach-tech/six-key-steps-developing-and-using-predictive-tools-your-beach. Below is the checklist and my notes about Ottawa beaches and how I'm applying the information from the USGS guide; through this I build the case for the predictive tool. Many other places have developed their tools with this guide and often it has been grad students (like me) doing the work for thesis projects.

**1. Evaluate the appropriateness of a FIB predictive tool**
  - *Is there a need for the tool?*
    - Ottawa beaches often close due to large E. coli counts (10% of total beach-days 2014-2019); recreational swimming carries the some risk of illness that we would like to minimize through closures while still maximizing safe swimming days.
    - The current daily testing system assumes a 'persistence model' with FIB remaining roughly constant over the testing interval (24hr)
       - in short: The advisory for today is based on yesterday's FIB. We don't *know* current level of risk for today.
    - but FIB could change substantially during the 24 hr lab time leading to an inaccurate advisory.
    - The main purpose of the tool is to predict events where FIB might surpass safety thresholds during testing lag-time.
    - The 'persistence model' might be complemented such a predictive tool
   
  - *Are beach characteristics compatible with predictive tools?*
      - 'The beach operates under a constant range of "normal" conditions'
        - Ottawa beaches conditions during summer are fairly stable with the exception of major precipitation events. Range of 'normal conditions' that do not vary greatly from year to year.
        - River beaches are mostly impacted by rainfall causing agricultual runoff and sewage outflows.
        - The number of dry days preceding rainfall influences the effect of rainfall ('first-flush' phenomenon).
    
      - 'Exceedences of beach notification thresholds occur occasionally but are not a chronic problem'
        - Ottawa beaches have ~10% FIB closures and ~5% rainfall closures - I think our beaches fit this description well.
        
      - 'FIB densities change over relatively short periods of time (time-lag problem)'
        - To be assessed and demonstrated
        - I looked at the 24hr differences between all observations and plotted these for each beach location.
        - FIB can change greatly over a 24 hr period. Persistence model prediction is that FIB_today = FIB_yesterday
        - Ie. 'persistent model' predictions vs actual timeseries by shifting the E.coli abundance lines 1 day to right and take the difference. Plot differences in a histogram.
        - Find number of days where persistence model provides accurate positive, false positive, accurate negative, false negative.

     - 'Sufficent amount of historical FIB and independent variables data exists'
       - Six years of beach data for 5 locations
       - lots of meterological data can be linked to FIB observations. 
         -Currently I have daily data but hourly data could improve the model later.
       - Some hydrology data: flow and level; for both rivers.
       - no data on beach use/ number of bathers?
       - might be able to find useful data about river turbity and chemistry: pH, solutes, etc. ?

      
**2. - Identify variables and collect data**
    - As noted above, 6 years of historical and daily testing for the future.
    - Identify variables of interest for modelling
      - Rainfall - weighted trailing sum (48 hr or 72 hr)
      - Dry days before rainfall
      - Temperature
      - time of year
      - location
      - previous FIB count at same location

    
 **3. - Perform Exploratory Data Analysis**
   - ***I am currently working on this stage of the process***
   - For historical data 2014-2019, this is mostly finished and presented in the dashboard already.
   - I did not want to clutter the dashboard too much, so have not added uninteresting plots.
   - 
  
 **4. - Develop and test a predictive model**
  
 **5. - Integrate Predictive Tool into beach monitoring/notification program**
   - This will depend on whether the tool is shown to be accurate first. Then it will depend on collaboration with the city and obtaining funding to implement the model for public advisories.
  
 **6. - Evaluate predictive tool over time**
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
