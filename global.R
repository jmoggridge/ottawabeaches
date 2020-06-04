### App layout elements and styles

# makes all the horizontal line show up
hr <-  tags$head(tags$style(HTML("hr {border-top: 1px solid #000000;}")))



### Parse data:

# beaches data:
beaches <- read.csv("data/beaches.csv")
beaches$status <- factor(beaches$status, levels(beaches$status)[c(1,2,4,3)])
beaches$year <- as.factor(beaches$year)
beaches$month <- as.factor(beaches$month)
beaches$location <- as.factor(beaches$location)
beaches$month <- factor(beaches$month, levels=rev(levels(beaches$month)))

# levels(beaches$month) <- c("June", "July", "August")
# beaches$year <- factor(beaches$year, levels = rev(levels(beaches$year)))
# """for certain graphs: omit na or reverse levels on axis"""
# ecoli[is.na(ecoli)] <- 1

# geographic data
geo <- read.csv("data/locations.csv")
weather <- read.csv("data/weather.csv")

## turn these into reactive functions: add entries for 'all years', 'all locations'.

# tables with stats; round values for table presentation
table.status.sum <-  read.csv("results/tables/status.table.csv")
table.status.sum <- table.status.sum %>% mutate_if(is.numeric, ~round(., 1))
table.status.days <- table.status.sum[,c(1:6)]
names(table.status.days) <- c("Year", "Location","Swim","Rain", "E. coli", "Out of\nseason")
table.status.percent <- table.status.sum[,c(1,2,7:10)]
names(table.status.percent) <-  c("Year", "Location","Swim %","Rain %", "E. coli %", "Out of\nseason %")
rm(table.status.sum)

table.ecoli.sum <-  read.csv("results/tables/ecoli.table.csv")
table.ecoli.sum <- table.ecoli.sum %>% mutate_if(is.numeric, ~round(., 1))
names(table.ecoli.sum) <- c("Year", "Location","Median", "Min","Max", "Geo. mean", "Mean", "Std. dev")



############## Themes  ##############

# thematic elements
col_discrete <- scale_colour_carto_d(name = "", type = 'qualitative', direction = 1)
fill_discrete <- scale_fill_carto_d(name = "", type = 'qualitative', direction = 1)
basic_theme <- theme_void() + theme_tufte(12, "Avenir")
dark_theme <- theme_dark() + theme_linedraw()
facet_labels <- theme(strip.text = element_text(face = 'bold', colour = 'deepskyblue4', size = 12), #face = "bold"
                      strip.background = element_rect(fill = "white", colour = "white", size = 1))
no_facet_labs <- theme(strip.background = element_blank(), strip.text.x = element_blank())
legend_format <- theme(legend.text = element_text(size = 11)) #face = "bold",
base_x <- geom_hline(yintercept = 0)
base_y <- geom_vline(xintercept = 0)
log10x <- scale_x_continuous(trans='log10')

## Basic plot lines  for count threshold
threshold <- geom_hline(yintercept = c(100,200), linetype = "longdash", color = "black", size = 0.3, alpha = 0.7)
thresholdv <- geom_vline(xintercept=c(100, 200), linetype="longdash", color = "black", size = 0.3, alpha = 0.7)

# for timeseries first of months' lines
first_of_month <- geom_vline(xintercept = c(182, 213, 242), color = "darkgrey", alpha = 0.3, size = 2.5)
annot <- data.frame(x=c(170,200,230), y=seq(1015,1015,3), label = c('June', 'July', 'Aug'))




################ Plot functions ##############

## Violins
# violin plot template function
violin_plot <- function(data, x, y, group, fill){
  ggplot(data, aes(.data[[x]], data[[y]], group = data[[group]])) +
    geom_violin(aes(fill=data[[fill]]), size=0.5, alpha =0.8) +
    scale_fill_carto_d(name = fill, type = 'qualitative', direction = 1, guide=FALSE) +
    scale_y_log10() +
    threshold + basic_theme + no_facet_labs + base_y + base_x+
    theme(axis.text.x = element_text(face='bold', size=11))
}
# violin 'by year' grouped
violin_plotd <- function(data, x, y, group, fill){
  ggplot(data, aes(.data[[x]], data[[y]], group = data[[group]])) +
    geom_violin(aes(fill=data[[fill]]), size=0.5, alpha =0.8) +
    scale_fill_viridis_d("year", guide = FALSE) +
    scale_y_log10() +
    threshold + basic_theme + no_facet_labs + base_y + base_x+
    theme(axis.text.x = element_text(face='bold', size=11))
}
#  'by months' grouped
violin_plot.month <- function(data, x, y, group, fill){
  ggplot(data, aes(as.factor(.data[[x]]), data[[y]], group = as.factor(.data[[group]]))) +
    geom_violin(aes(fill=as.factor(.data[[fill]])), size=0.5, alpha =0.8) +
    scale_fill_discrete("month", guide = FALSE) +
    scale_y_log10() +
    threshold + basic_theme + no_facet_labs + base_y + base_x +
    theme(axis.text.x = element_text(face='bold', size=11))
}

## Histogram templates
hist_temp <- function(beaches, data, x, group, colour, nbins){
  ggplot(data, aes(.data[[x]], group = .data[[colour]], fill = .data[[colour]])) +
    geom_histogram(aes(x = .data[[x]], fill = .data[[colour]]), position = "stack", alpha = 0.99, bins = nbins) +
    # geom_density(aes(y=..count../nbins, fill = .data[[colour]], colour = .data[[colour]]), alpha = 0.08) +
    thresholdv + basic_theme + facet_labels + base_y + base_x+
    ylab("Number of days") + xlab("E. coli (cfu/100 mL)") +
    facet_grid(location~year)+
    theme(axis.text.x = element_text(angle = 50, hjust = 1))
}
hist_temp2 <- function(beaches, data, x, group, colour, nbins){
  ggplot(data, aes(.data[[x]], group = .data[[colour]], fill = .data[[colour]]), labels = data[[colour]]) +
    geom_histogram(aes(x = .data[[x]], fill = .data[[colour]]), position = "stack", alpha = 0.99, bins = nbins) +
    thresholdv + basic_theme + facet_labels + base_y + base_x +
    scale_x_continuous(trans='log10') +
    ylab("Number of days") +
    xlab("E. coli (cfu/100 mL)")+
    theme(legend.position = 'top', axis.text.x = element_text(angle = 50, hjust = 1))
}


## Timeseries Plots templates:
ts_plot <- function(data, julian, count, group, alpha){
  ggplot(data, aes(.data[[julian]], .data[[count]],group = data[[group]])) +
    geom_line(aes(group=.data[[group]], colour = .data[[group]]), size = 0.75, alpha = alpha) +
    geom_text(data = annot, aes(x = x, y = y, label = label, group = label),
              color = "black",
              size = 3, alpha = 0.8, angle = 0)+
    xlab('Day of year') + ylab('E. coli (cfu/100 mL)') +
    first_of_month + threshold + basic_theme + base_x +
    facet_grid(location~year) + facet_labels +
    theme(legend.position = 'top')
  }

ts_plot_grid <- function(data, julian, count, group, alpha){
  ggplot(data, aes(.data[[julian]], .data[[count]],group = data[[group]])) +
    geom_line(aes(group=.data[[group]], colour = .data[[group]]), size = 0.75, alpha = alpha) +
    scale_colour_carto_d(name = "", type = 'qualitative', direction = 1, limits = levels(as.factor(beaches$location)),
                         guide=FALSE) +
    xlab('Day of year') + ylab('E. coli (cfu/100 mL)') +
    first_of_month + threshold + basic_theme + facet_grid(location~year) + facet_labels +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

##################### Icons and Links #################################

#flatly_blue = #18bc9c
# sick_green: #90D13E

### ICONS:
icon.beach <- tags$div(HTML('<i class="fa fa-umbrella-beach" style = "color:#18bc9c;"></i>'))
icon.year <- tags$div(HTML('<i class="fa fa-history" style = "color:#18bc9c;"></i>'))
icon.status <- tags$div(HTML('<i class="fa fa-umbrella-beach" style = "color:#18bc9c;"></i>'))
icon.coliforms <- tags$div(HTML('<i class="fas fa-disease" style = "color:#18bc9c;"></i>'))
dis.tab <- tags$div(HTML('<i class="fas fa-disease"></i> E. coli'))
poo.tab <- tags$div(HTML('<i class="fal fa-poop"></i> E. coli'))

### LINKS AND DATA SOURCES
github.link <- a(href = "https://github.com/jmoggridge/ottawabeaches", HTML('OttawaBeaches GitHub <i class="fa fa-github" style = "color:#90D13E;"></i>'))
tweet.link <- a(href = "https://twitter.com/quaxlikeaduck", HTML('J Moggridge  <i class="fa fa-twitter" style = "color:#90D13E;"></i>'))
source.beach <- a(href = "https://open.ottawa.ca/datasets/beach-water-sampling", "City of Ottawa beach water sampling")
source.beach2 <- a(href = "https://open.ottawa.ca/datasets/beach-water-sampling", "beach water sampling data from five City of Ottawa public beaches")
source.weather <-  a(href = "https://climate.weather.gc.ca/climate_data/daily_data_e.html?hlyRange=2011-12-14%7C2020-04-22&dlyRange=2011-12-15%7C2020-04-22&mlyRange=%7C&StationID=49568&Prov=ON&urlExtension=_e.html&searchType=stnName&optLimit=yearRange&StartYear=1840&EndYear=2020&selRowPerPage=25&Line=14&searchMethod=contains&txtStationName=ottawa&timeframe=2&Day=22&Year=2014&Month=1#",
                     "climate.weather.gc.ca WMO ID: 71628")
# reference literature:

ref.sanborn <- "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3653650/"
ref.cityofottawa <- "https://www.arcgis.com/home/item.html?id=0bbb24d8a55946b5a3e50fbc24631cd2"
coliform.link <- a(href = "https://en.wikipedia.org/wiki/Coliform_bacteria", "Coliform bacteria")

# OTTAWA River AT BRITANNIA -> 02KF005 # RIDEAU River AT OTTAWA -> 02LA004
source.river <- a(href = "https://wateroffice.ec.gc.ca/report/data_availability_e.html?type=historical&station=02KF005&parameter_type=Flow+and+Level",
                  "Britannia, 02KF005")
source.river2 <- a(href = "https://wateroffice.ec.gc.ca/report/data_availability_e.html?type=historical&station=02LA004&parameter_type=Flow+and+Level",
                   "Rideau river, 02LA004")


################## Flavour texts ######################################################

### SIDEBAR TEXTS
title <- "Ottawa Beach Water Sampling"
apptitle <- span("Ottawa Beaches",
                         style = "color: #18bc9c; font-size: 30px; font-family: Avenir; font-weight: bold")
stitle <- p(" An interative dashboard for exploring closures and ", em("E. coli")," counts at City of Ottawa public beaches")

stitle2 <- p(tags$div(HTML('<i class="fa fa-umbrella-beach" style = "color:#18bc9c;"></i>
                       Interative dashboard for closures and <i>E. coli</i> counts from City of Ottawa public beaches
                     <i class="fa fa-umbrella-beach" style = "color:#18bc9c;"></i>')))
byline <- p(tags$div(h6('Created by',tweet.link, '. Code at', github.link, ". Dashboard licensed under ", a(href ="https://creativecommons.org/licenses/by-sa/4.0/","CC-SA-4.0.")),
                     h6('Beaches data source:', a(href = "https://open.ottawa.ca/datasets/beach-water-sampling", "City of Ottawa"),". Source information licensed under the",
                        a(ref="https://ottawa.ca/en/city-hall/get-know-your-city/open-data#open-data-licence-version-2-0",
                          "Open Government Licence – City of Ottawa."))
))
# title <- tags$div(HTML('Ottawa<i class="fa fa-umbrella-beach" style = "color:#90D13E;"></i>Beaches'))

### MAIN PANEL TEXTS

rain_2016_note <- h6(tags$b("*Note:")," Status data from 2016 did not indicate if a closure was due to rain or E. coli specifically, \
                     only if the beach was closed in season (labelled as 'E. coli' here).")

v.thresholds_note <- h6(tags$b("Note:"), "The vertical lines in each plot represent the first days of July, August, and Sept.\n \
The dashed lines represent the 100 & 200 cfu /100 mL thresholds; if the count is > 200 the beach is closed, \
or if the count is >100 for two consecutive days, the beach is closed.")

h.thresholds_note <-p(tags$b("Note:"), "The dashed lines represent the 100 & 200 E. coli (cfu/100mL) thresholds; if the count is > 200 the beach is closed, \
or if the count is >100 for two consecutive days, the beach is closed.")


fig3cap <- p(tags$b("A summary of swimming days for each beach by year."),
             "If not open, the beach may be closed due to rain, large E. coli counts, or due to later season start date at certain locations.", br())

status_text <- p(tags$b("Daily swim status and ",tags$b(em("E. coli"), "counts. ")),
                 "Daily observations are represented as dots with size according to the ", em("E. coli"),"count
                 and colour by swim status.  Each horizontal line represents a full season at a given beach.
                 Vertical grey lines separate the months June, July, and August.",
                 br()
                #  "When grouping the lines by year, note the pattern of ", em("E. coli"),"counts spiking at multiple beaches following rain.
                # Rainy summers* have many more closures and greater", em("E. coli"),"counts than others, especially 2017 ('the bummer summer');
                # additionally, we note the increased frequency of closures in August compared to June and July.", br(),br(),
                #  "When lines are grouped by beach, it becomes apparent that Petrie Island beaches and Mooney's bay tend
                # to get closed more often than Britannia and Westboro."
)

coliform_text <- p(tags$b("Timeseries of daily ", em("E. coli"),"counts at each beach. "),
                   "In each plot, a line is a full season at one beach
                    and the height of the line corresponds to the observed ", em("E. coli"),"count. ",
                   "A beach is closed if the E. coli count is greater than 200  per 100 mL, or if
                    counts remain above 100 per 100 mL for two days (these thresholds shown above as horizontal dotted lines).
                   Log(base 2) scaling helps expose variability in the lower ranges close to these thresholds.
                   1000 per 100 mL appears to be the limit of detection for the coliform count method used. "
)

dist_text <- p(tags$b("The distribution of ", em("E. coli"),"counts"),
               " from Ottawa public beaches during the summers 2014-2019. Use tabs to choose a style of
               frqeuency plot, then to group observations by beach, year, month, or beach & year to identify trends.
               We can see from these distributions that certain years (esp. 2017), times of year (August), and specific
               beaches (Petrie isl. and Mooney's Bay) can have to have greater E. coli abundances than others.")


######## NOTES PAGE TEXT BLOCKS #########3

notes_txt <-
  p(h4("Ottawa beach water quality sampling"), br(),
  "This dashboard presents", source.beach2," during the period 2014-2019.
    The data collected daily for each location are", tags$i(" Escerichia coli "), "counts
    and beach swimming status. Beach closures  are based on the result of the ", tags$i("E. coli"), "count from the previous day, or
    or recent heavy rainfall. The beaches season lasts from mid-June to late August. ",
  br(),

  h5(tags$b("Information from the City of Ottawa / Ottawa Public Health attached to dataset: ")),
     "'A no-swimming advisory will be issued if bacteria levels are over 200 E. coli per 100mL of water tested for one day;
    or if bacteria levels are over 100 E. coli per 100mL of water tested on two or more consecutive days.
    When a no-swim advisory is in effect, people should not swim due to the risk of getting a skin, ear, throat
    or even gastro-intestinal illness. A 24-hour no-swim advisory may be in place at the beaches after significant rainfall.
    Please note Ottawa Public Health collects beach water samples every day. The results take 18 to 24 hours to process
    in the laboratory, and as such, swim and no-swim advisories are based on sample results taken from the previous day.'",
     a(href=ref.cityofottawa,"Source"),
    br(),
    h6(em("*Note"),": Beach sampling data from 2016 did not indicate if a beach was closed due to rain or E. coli;
    in the absence of this information, observations have been labelled as 'E. coli' for simplicity;
       this occurs more frequently than closure due to rain in other years. "),
    # br(), br(),
     hr(),
     h4("E. coli testing and beach water safety"), br(),
    "Beach water can contain",a(href=ref.sanborn," a number of harmful microorganisms:"),
  tags$dl(tags$li("Bacteria:", em("Campylobacter, Salmonella, Shigella, etc.")),
          tags$li("Protozoans: ", em("Giardia"),"and", em("Cryptosporidium")),
           tags$li("Viruses: enterovirus, norovirus, and adenovirus")),

               "Public health organizations often measure the abundance of fecal indicator bacteria (FIB) to ascertain the health risk
    posed by beach water pathogens.  ",
               coliform.link," are often used as for beach water testing, but their presence isn't confirmation of fecal contamination.  ",
               a(href = "https://en.wikipedia.org/wiki/Escherichia_coli",tags$i("Escherichia coli")), "is a more specific indicator of
    fecal contamination than the more general 'coliforms', and is typically used as an FIB for freshwater samples.  ",
               em("Enterococcus "),
               "is used similarly as the fecal indicator species for marine habitats.  ",
               "FIB can be selectively cultured by using specific media and incubation protocols.
  Ottawa has a rigorous monitoring program with daily FIB testing at all locations specifically for ",em("E. coli.  "),
               hr(),
#
#                h4("Factors affecting beach water quality:"),br(),
#                tags$ul(tags$dt("Rainfall and temperature"),
#                        tags$dd("Temperature has a large effect on the rate at which microorganisms reproduce.
#             Heavy rains cause agriculural runoff and sewage outflows that contribute to the proliferation of pathogens.
#                     The effect of rainfall on E. coli count at Ottawa beaches is apparent in the 'time series > swim status' figure"),
#                        br(),
#                        tags$dt("Time of year"),
#                        tags$dd("Beach water coliform abundances tend to increase during late summer and early fall.
#                     This trend is seen at Ottawa public beaches where E. coli counts often spike during
#                     the final weeks of the swimming season. See E. coli count time-series figures and distribution of",
#                                tags$i("E. coli"), " by month (tabs:'time-series > E. coli > by year' and
#                     'statistics > E.coli distributions > by month)"),br(),
#                        tags$dt("Location"),
#                        tags$dd("Certain locations are more susceptible to fecal contamination from sewage outflows, agricultural runoff,
#                     and wildlife. While four of the five beaches (Britannia, Westboro and two Petrie beaches) are on the same river,
#                     the water quality at Petrie island locations is noticeably worse that either beaches upstream."),br(),
#
#                        tags$dt("River hydrology (flow and river level)"), br(),
#                        tags$dt("Wildlife & agriculture")
#                ), br(),

               #     h4(HTML('BEACH<i class="fa fa-umbrella-beach"
               #              style = "color:#90D13E;"></i>NET: a predictive model for beach water quality')),
    #            h4("Towards a predictive model for beach water safety"),
    #
    #            p("Ottawans love to swim in the many lakes and rivers around our beautiful city. Infection and illness
    # caused by pathogens and parasites is common in recreational swimmers.
    # Abundances of FIB in these bodies are extremely variable and can change rapidly.
    # The current culture-based counting approach to beach water testing requires a full day for processing;
    # real-time lab methods are generally impractical due to complexity and cost.
    # While factors affecting their levels (rain, temperature, runoff, sewage) are known,
    #  this knowledge is only leveraged when significant rainfall occurs.
    #  E. coli abundance can surpass safety thresholds in the absence of heavy rain, particularly in late summer.
    #  As such, using this empirical data for statistical modelling presents an attractive
    #   alternative to complement counting methods."),
    #
    #            p("Such a program has already been developed for Ohio beaches on Lakes Erie. This model, while not used to
    # decide closures, is used to predict when coliforms will surpass the threshold for closure.
    # Same-day predictions made by these models can complement current testing approaches, which are hampered
    #   by the time required to measure FIB."),
    #
    #
    #            p("Historical data collected by City of Ottawa, paired with weather observations, provides information
    # to train a statistical model that might indicate the likelihood of E. coli counts surpassing saftey thresholds on a given day.")

)
# p("From the data in this dashboard, I have created a generalized linear model that whether predicts E. coli abundance based on
#   weather, time of year, and beach location. When we perform tests on this model (ANOVA), we notice that rainfall is indeed
#   the strongest predictor of fecal contamination, as expected. Further, this model indicates that beach location
#   does indeed have a significant effect on the E. coli abundance."),
# br(),
# h4("Other considerations for the future"),
# tags$dl(
#   tags$li("Climate change effect on regional precipitation and temperature patterns"),
#   tags$li("Antimicrobial resistance and agricultural runoff")

dashboard_txt <-
  p(h4("About this dashboard"),
    "This dashboard was created with publicly available data from the City of Ottawa's open.ottawa.ca portal.
    The author is not affiliated with the City of Ottawa or Ottawa Public Health. ",  br(),
    "The html document was created using R language and libraries Shiny, Tidyverse, ggplot2, among others.
  Hosting is provided by the ShinyApps.io website.  Map from Leaflet.  Icons from FontAwesome.
  Code is available from the github link on the sidebar. Contents of this dashboard are under a creative commons license,
  feel free to reuse with attribution.  ", br(),

    h5(tags$b("Interpreting this dashboard")),
    "In season, beaches may be either open for swimming or closed to swimmers for several reasons;
  in this dashboard these swim statuses are referred to as:",
    tags$div(tags$ul(
      tags$li(em(" Open"), " - open for swimming"),
      tags$li(em(" E. coli"), " - closure due to a large abundance of bacteria"),
      tags$li(em(" Rain"), "- closure due to significant rainfall*."),
      tags$li(em(" Closed"), " - season has not started or has concluded (there are no associated counts for these data)")
    )),

    em('Colours'), "present in plots always represent the same groupings for consistency. All colour scales have been chosen
    to accomodate colourblindness - please contact me (the author) if you have any issues here - I am particularly interested in
    improving accessibility aspects of data visualizations.",
    tags$div(tags$ul(
      tags$li(em(" Yellow-green-purple scale"), " - time: year, month, or week"),
      tags$li(em(" Discrete scale 1: blue, green, pink, orange"), " - daily beach status"),
      tags$li(em(" Discrete scale 2: orange, purple, teal, lime, grey"), " - beach locations")
    ))
  )


############## REFERENCES LIST ##############

refs <- p(h4('References'),
          tags$div(tags$ol(
            tags$li(" Sanborn M, Takaro T. Recreational water-related illness: office management and prevention. Can Fam Physician. 2013;59(5):491‐495.
      https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3653650/  ")
            )))

##################################################################################################

### Plot graveyard

# # geom_density ridges

# density_ridges <- function(){
#   ggplot(beaches, aes(x=count, y=location, colour=location, fill=location)) +
#     geom_density_ridges(alpha = 0.5) + stat_density_ridges(quantile_lines = TRUE, alpha =0.2) +
#     scale_x_log10()+
#     scale_color_carto_d(name = fill, type = 'qualitative', direction = 1, guide=FALSE) +
#     scale_fill_carto_d(name = fill, type = 'qualitative', direction = 1, guide=FALSE) +
#     xlab("E. coli / 100 mL") +
#     facet_grid(~year) + basic_theme  + facet_labels +
#     theme(axis.title.y = element_blank())
# }

# output$ts_combo_beach <- renderPlot({
#   ggplot(beachesInput.ts(), aes(x = julian, y= year, group=year, fill=status, colour = status)) +
#     geom_point(aes(size = count+50), alpha = 0.7) + first_of_month + guides(shape = FALSE) +
#     scale_colour_discrete(name = "Status", direction = 1, labels = levels(beaches$status), limits = levels(beaches$status)) +
#     scale_fill_discrete(name = "Status", direction = 1, labels = levels(beaches$status), limits = levels(beaches$status)) +
#     scale_size_continuous("E. coli / 100 mL") +
#     facet_grid(location~.) + xlim(c(166,244)) + xlab("Days into year") + basic_theme + facet_labels + base_x+
#     theme(axis.text.x = element_text(angle = 50, hjust = 1),
#           legend.text = element_text(size= 10)) +
#     theme(legend.key.size = unit(5,"point"))+
#     theme(legend.position="top")
# })
