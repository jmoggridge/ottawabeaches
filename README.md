# Ottawa Beaches dashboard
---
This is the repository for the Ottawa Beaches dashboard at https://jmoggridge.shinyapps.io/OttawaBeaches/.

## About 

**The dashboard currently shows visualizations of beach water testing data from the City of Ottawa 2014-2019**. To my knowledge, no other comprehensive set of figures exists in the public domain for this data (as of May 29th, 2020); the dashboard should be very useful to anyone interested in beach water monitoring in the region.

**The goal of the project is to create a *forecast for beach swimming***: a tool that predicts whether E. coli abundance will be in excess of safety thresholds in real-time and into the future.  
I plan to generate forecast predictions for each beach during summer 2020 to assess the model accuracy. If the project is successful, the dashboard could serve as a template for jurisdictions to apply to their data for similar goals. The motivation for this project is curiosity; I am not affiliated with the city or being compensated by anyone for this work. I am currently a grad student in Bioinformatics; this side project provides an avenue for me to learn and apply statistics, coding, and science communication skills with real-world datasets, a tangible goal, and potential benefits for people in my hometown.

The dashboard is still a work in progress: I plan on **modelling & forecasting for E. coli abundance during the 2020 season** based on environmental factors, location, previous day's count, etc. Predictions will be evaluated against the accuracy of the current persistence model for testing. If a predictive model is shown to be useful, I hope to publish the results and work in collaboration with the city to integrate modelling into their beach water monitoring system.

Below are some details about the technical aspects of the process involved in creating the dashboard

## Process

**I am following a six-step process from USGS to create the modelling system**
see here:

The steps are :
  1 - 
  2
  3
  4
  5
  6


---
## Github repository


### Folders

  - Code: all the code I ran to tidy data, create visualizations, calculate statistics, create models.
  - Data - raw and tidy'd dataframes of input dataframes are here (input only)
  - Results - dataframes of compute results: summary tables, predictions, modelling outputs, etc. (output only)

### Scripts

The **app.R** script has both the UI and the server chunks. The top of the file has some thematic elements and plotting functions that are reused for a bunch of plots in the server chunk; I plan on adjusting the plot themes later to create a minimal, notebook style. The UI layout consists mostly of nested tabSets until I figure out how to do reactive plots a bit more efficiently through drop down menus (eg for plot style in the coliform distributions analyses).

**parse.R** is the script for creating data-frames from public datasets. There are currently three main data sources: *beaches* city data; *weather* data from Ottawa airport; and *rivers* hydrological data from measuring stations at Britannia and Hog's Back (I think).
 
 **gloabl.R** eventually this script will contain all the static code that isn't UI or reactive server chunks, ie. it will contain all the plotting functions, thematic elements, figure captions, and body text.
 
 
 transform the wide-format _ecoli.csv and _status.csv raw data into long-form table in _df.csv 
 
 NOTE TO SELF: this needs added info about how to convert all the mislabelled statuses in the original raw data tables

global.R  is a script containing all the text elements in the dashboard

---
**data/beaches_ .csv files**

The app.R file keeps three dataframes in memory:
beaches_df.csv is the main dataset with status and coliform counts with date and various date components for 2014-2019.
beaches_locations.csv contains the latitude and longitude coordinates of the beaches for the maps.
*future file enviro_df.csv* will contain daily weather and river data for the relevant time periods (to do)
individual datasets are in raw data folder

---
**Plans**

model 1: generalized linear model for E. coli counts using negative binomial distribution
  Significant coefficients: rain, location, day of year/month, temperature (interacts with rain)
  - bin predictions according to low, med, high predicted risk.
  
model 2: classifier for 100 and 200 E.coli per 100 mL threshold.
  - use learning to develop classifier that predicts whether E. coli count is above/below threshold.
  
---

**Bugs/Issues**
 - still having problems creating a good visualization that shows E. coli counts and weather together. Time series is the only thing I'm still trying to do with this.
 
 -data-tab: tables with means and ranges, for each beach and year; reactive to filters, with download button
    - get rid of the raw R stat summary and replace with markdown tables KableExtra.
 
 -
 
