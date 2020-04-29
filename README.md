# ottawabeaches
OttawaBeaches_shiny_dashboard

Hi, this is the repository for the Ottawa Beaches dashboard at https://jmoggridge.shinyapps.io/OttawaBeaches/.
The dashboard is still a work in progress and will probably be updated on a semi-regular basis during May & June 2020.

**R scripts**
The app.R script has both the UI and the server chunks. The top of the file has some thematic elements and plotting functions that are reused for a bunch of plots in the server chunk; I plan on adjusting the plot themes later to create a minimal, notebook style. The UI layout consists mostly of nested tabSets until I figure out how to do reactive plots a bit more efficiently through drop down menus (eg for plot style in the coliform distributions analyses).

beaches_data_parse.R is the script I ran to match up and transform the wide-format _ecoli.csv and _status.csv raw data into long-form table in _df.csv # this needs added info about how to convert all the mislabelled statuses in the original raw data tables

**data/beaches_ .csv files**
The app.R file keeps three dataframes in memory:
beaches_df.csv is the main dataset with status and coliform counts with date and various date components for 2014-2019.
beaches_locations.csv contains the latitude and longitude coordinates of the beaches for the maps.
*future file enviro_df.csv* will contain daily weather and river data for the relevant time periods (to do)
individual datasets are in raw data folder

**Plans**
Currently the app is pretty unpolished and I have been pretty lazy about the texts in particular, they are mostly a placeholder lorem ipsem filler until I do the writing for the introduction and anaylsis tabs, which will probably be after I finish all the visualizations and reactive features pertaining to environmental data and statistical analysis. 

enviro data is needs to be tidyied up from unmatched river and weather observations (still need to do some filtering & avergaing)
this will go into beaches_parse.R enventually

The introduction tab will eventually cover the basis for coliform counts and the ecology and earth science of the river in brief with some non-rective visualizations/gifs to draw the reader into interacting with the app.

The analysis tab will get the weather and river data overlays that I'm working on as well as the reactive statistics tests that the reader can perform on their filtered data.
'Models' tab where the user can see either the negative binomial glm I tested and/or go ahead change the variables or distribution and test any model they please (within reason).  Ultimately a function where they can model coliform counts on changes in temp and precipitation; a visualization that shows how sensitive the model is to each parameter.
