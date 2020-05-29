# Ottawa Beaches dashboard
---

This is the repository for the Ottawa Beaches dashboard at https://jmoggridge.shinyapps.io/OttawaBeaches/.
The dashboard is still a work in progress as I figure out how to do modelling for E. coli abundance based on environmental factors, location, previous day's count, etc.
The end-goal of the project is to create a forecast for beach swimming: a tool that predicts whether E. coli abundance will be in excess of safety thresholds in real-time and into the future. I plan to generate forecast predictions for each beach during summer 2020 to assess the model accuracy. If the project is successful, the dashboard could serve as a template for jurisdictions to apply to their data for similar goals. The motivation for this project is curiosity; I am not affiliated with the city or being compensated by anyone for this work.

Below are some details about the technical aspects of the dashboard

---

**R scripts**

The app.R script has both the UI and the server chunks. The top of the file has some thematic elements and plotting functions that are reused for a bunch of plots in the server chunk; I plan on adjusting the plot themes later to create a minimal, notebook style. The UI layout consists mostly of nested tabSets until I figure out how to do reactive plots a bit more efficiently through drop down menus (eg for plot style in the coliform distributions analyses).

beaches_data_parse.R is the script I ran to match up and transform the wide-format _ecoli.csv and _status.csv raw data into long-form table in _df.csv # this needs added info about how to convert all the mislabelled statuses in the original raw data tables

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
 
