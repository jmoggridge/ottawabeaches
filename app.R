# Shiny app script for Ottawa Beaches dashboard.


# Reactive objects all run in the 'server' section
# Layout happens in the 'ui' section
# Remember to set the current working directory to: setwd("~/Dropbox/R coding/ottawa_beaches")
#
require(tidyverse)
require(shinythemes)
require(lubridate)
require(maps)
require(ggmap)
require(mapproj)
require(mapdata)
require(rgeos)
require(maptools)
require(sp)
require(raster)
require(rgdal)
require(dismo)
require(rcartocolor)
require(gganimate)
require(gifski)
require(kableExtra)
require(animation)
require(lattice)
require(proto)
require(png)
require(transformr)
require(ggthemes)

### Parse data:

# beaches data:
beaches <- read.csv("data/beaches.csv")
beaches$status <- factor(beaches$status, levels(beaches$status)[c(1,2,4,3)])
levels(beaches$status)[levels(beaches$status)=="Coliforms"] <- "E. coli"
beaches$year <- as.factor(beaches$year)

# """for certain graphs: omit na or reverse levels on axis"""
# ecoli[is.na(ecoli)] <- 1
# beaches$year <- factor(beaches$year, levels = rev(levels(beaches$year)))

# geographic data
geo <- read.csv("data/locations.csv")
weather <- read.csv("data/weather.csv")


### Themes

# thematic elements
col_discrete <- scale_colour_carto_d(name = "", type = 'qualitative', direction = 1)
fill_discrete <- scale_fill_carto_d(name = "", type = 'qualitative', direction = 1)
basic_theme <- theme_void() + theme_tufte(14, "Avenir")
dark_theme <- theme_dark() + theme_linedraw()
facet_labels <- theme(strip.text = element_text(face = "bold", colour = 'black', size=12),
                      strip.background = element_rect(fill = "white", colour = "white", size = 1))
no_facet_labs <- theme(strip.background = element_blank(), strip.text.x = element_blank())
legend_format <- theme(legend.text = element_text(face = "bold", size = 12))
base_x <- geom_hline(yintercept = 0)
base_y <- geom_vline(xintercept = 0)
log10x <- scale_x_continuous(trans='log10')

## Basic plot lines  for count threshold
threshold <- geom_hline(yintercept = c(100,200), linetype = "longdash", color = "black", size = 0.3, alpha = 0.7)
thresholdv <- geom_vline(xintercept=c(100, 200), linetype="longdash", color = "black", size = 0.3, alpha = 0.7)

# for timeseries first of months' lines
first_of_month <- geom_vline(xintercept = c(182, 213, 242), color = "darkgrey", alpha = 0.3, size = 2.5)
annot <- data.frame(x=c(170,200,230), y=seq(1015,1015,3), label = c('June', 'July', 'Aug'))

### Plot functions

## Violins
# violin plot template function
violin_plot <- function(data, x, y, group, fill){
  ggplot(data, aes(.data[[x]], data[[y]], group = data[[group]])) +
    geom_violin(aes(fill=data[[fill]]), size=0.5, alpha =0.8) +
    scale_fill_carto_d(name = fill, type = 'qualitative', direction = 1) +
    scale_y_log10() +
    threshold + basic_theme + no_facet_labs + base_y + base_x+
    theme(axis.text.x = element_text(face='bold', size=11))
}
# violin 'by year' grouped
violin_plotd <- function(data, x, y, group, fill){
  ggplot(data, aes(.data[[x]], data[[y]], group = data[[group]])) +
    geom_violin(aes(fill=data[[fill]]), size=0.5, alpha =0.8) +
    scale_fill_viridis_d("year") +
    scale_y_log10() +
    threshold + basic_theme + no_facet_labs + base_y + base_x+
    theme(axis.text.x = element_text(face='bold', size=11))
}
#  'by months' grouped
violin_plotf <- function(data, x, y, group, fill){
  ggplot(data, aes(as.factor(.data[[x]]), data[[y]], group = as.factor(.data[[group]]))) +
    geom_violin(aes(fill=as.factor(.data[[fill]])), size=0.5, alpha =0.8) +
    scale_fill_viridis_d("month") +
    scale_y_log10() +
    threshold + basic_theme + no_facet_labs + base_y + base_x+
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
    thresholdv + basic_theme + facet_labels + base_y + base_x+
    ylab("Number of days") +
    xlab("E. coli (cfu/100 mL)")
}

## Timeseries Plots templates:
ts_plot <- function(data, julian, count, group, alpha){
  ggplot(data, aes(.data[[julian]], .data[[count]],group = data[[group]])) +
    geom_line(aes(group=.data[[group]], colour = .data[[group]]), size = 0.75, alpha = alpha) +
    geom_text(data = annot, aes(x = x, y = y, label = label, group = label),
              color = "black",
              size = 3, alpha = 0.8, angle = 0)+
    xlab('Day of year') + ylab('E. coli (cfu/100 mL)') +
    first_of_month + threshold + basic_theme + base_x
}

#############################################################################################################################

### ICONS:
icon.beach <- tags$div(HTML('<i class="fa fa-umbrella-beach" style = "color:#90D13E;"></i>'))
icon.year <- tags$div(HTML('<i class="fa fa-history" style = "color:#90D13E;"></i>'))
icon.status <- tags$div(HTML('<i class="fa fa-umbrella-beach" style = "color:#90D13E;"></i>'))
icon.coliforms <- tags$div(HTML('<i class="fas fa-disease" style = "color:#90D13E;"></i>'))
dis.tab <- tags$div(HTML('<i class="fas fa-disease"></i> E. coli'))
poo.tab <- tags$div(HTML('<i class="fal fa-poop"></i> E. coli'))

### LINKS AND DATA SOURCES
github.link <- a(href = "https://github.com/jmoggridge/ottawabeaches", HTML('OttawaBeaches GitHub <i class="fa fa-github" style = "color:#90D13E;"></i>'))
tweet.link <- a(href = "https://twitter.com/quaxlikeaduck", HTML('J Moggridge  <i class="fa fa-twitter" style = "color:#90D13E;"></i>'))
source.beach <- a(href = "https://open.ottawa.ca/datasets/beach-water-sampling", "City of Ottawa beach water sampling")
source.beach2 <- a(href = "https://open.ottawa.ca/datasets/beach-water-sampling", "beach water sampling data from five City of Ottawa public beaches")
source.weather <-  a(href = "https://climate.weather.gc.ca/climate_data/daily_data_e.html?hlyRange=2011-12-14%7C2020-04-22&dlyRange=2011-12-15%7C2020-04-22&mlyRange=%7C&StationID=49568&Prov=ON&urlExtension=_e.html&searchType=stnName&optLimit=yearRange&StartYear=1840&EndYear=2020&selRowPerPage=25&Line=14&searchMethod=contains&txtStationName=ottawa&timeframe=2&Day=22&Year=2014&Month=1#",
                     "climate.weather.gc.ca WMO ID: 71628")

# OTTAWA River AT BRITANNIA -> 02KF005 # RIDEAU River AT OTTAWA -> 02LA004
source.river <- a(href = "https://wateroffice.ec.gc.ca/report/data_availability_e.html?type=historical&station=02KF005&parameter_type=Flow+and+Level",
                  "Britannia, 02KF005")
source.river2 <- a(href = "https://wateroffice.ec.gc.ca/report/data_availability_e.html?type=historical&station=02LA004&parameter_type=Flow+and+Level",
                   "Rideau river, 02LA004")


################## Flavour texts ######################################################

### SIDEBAR TEXTS
title <- "Ottawa Beaches"
stitle <- p(tags$div(HTML('<i class="fa fa-umbrella-beach" style = "color:#90D13E;"></i>
                     A dashboard of historical City of Ottawa public beach water sampling data 2014-2019
                     <i class="fa fa-umbrella-beach" style = "color:#90D13E;"></i>')))
byline <- p(tags$div('Created by',tweet.link, br(),'Code', github.link,
                     'Source:', a(href = "https://open.ottawa.ca/datasets/beach-water-sampling", "City of Ottawa"),
                     "Dashboard licensed under ", a(href ="https://creativecommons.org/licenses/by-sa/4.0/","CC-SA-4.0."),
                     "Contains information licensed under the",
                     a(ref="https://ottawa.ca/en/city-hall/get-know-your-city/open-data#open-data-licence-version-2-0",
                       "Open Government Licence – City of Ottawa.")))
# title <- tags$div(HTML('Ottawa<i class="fa fa-umbrella-beach" style = "color:#90D13E;"></i>Beaches'))

### MAIN PANEL TEXTS

rain_2016_note <- h6(tags$b("*Note:")," Status data from 2016 did not indicate if a closure was due to rain or E. coli specifically, \
                     only if the beach was closed in season. See 'Notes' for more details.")

v.thresholds_note <- h6(tags$b("Note:"), "The vertical lines in each plot represent the first days of July, August, and Sept.\n \
The dashed lines represent the 100 & 200 cfu /100 mL thresholds; if the count is > 200 the beach is closed, \
or if the count is >100 for two consecutive days, the beach is closed.")

h.thresholds_note <-p(tags$b("Note:"), "The dashed lines represent the 100 & 200 E. coli (cfu/100mL) thresholds; if the count is > 200 the beach is closed, \
or if the count is >100 for two consecutive days, the beach is closed.")


fig3cap <- p(tags$b("A summary of swimming days for each beach by year."),
              "If not open, the beach may be closed due to rain, large coliform counts, or due to later season start date at certain locations.", br())

status_text <- p(tags$b("Coliform count and beach status for each day at each beach"),
                " are shown by dots coloured by swim status and sized according to the coliform count.
                Each horizontal line represents a full season at a given beach.
                Use the tabs above group lines by beach or year.
                For totals of days by status see 'statistics > summary'.", br(),
                "When grouping the lines by year, note the pattern of coliform counts spiking at multiple beaches following rain.
                Rainy summers* have many more closures and greater E. coli counts than others, especially 2017 ('the bummer summer');
                additionally, we note the increased frequency of closures in August compared to June and July.", br(),
                "When lines are grouped by beach, it becomes apparent that Petrie Island beaches and Mooney's bay tend
                to get closed more often than Britannia and Westboro."
                )

coliform_text <- p(tags$b("Timeseries of daily coliform counts at each beach."),
                   "In each plot, a line is a full season at one beach
                    and the height of the line corresponds to the observed coliform count",
                    "A beach is closed if the E. coli count is greater than 200 cfu (colony-forming units) per 100 mL, or if
                    counts remain above 100 cfu/ 100 mL for two days (these thresholds shown above as horizontal dotted lines).
                   Log(base 2) scaling helps expose variability in the lower ranges close to these thresholds.
                   1000 cfu/ 100 mL appears to be the limit of detection for the coliform count method used."
                    )

dist_text <- p(tags$b("The distribution of E. coli counts"),
               " from Ottawa public beaches during the summers 2014-2019. Use tabs to choose a style of
               frqeuency plot, then to group observations by beach, year, month, or beach & year to identify trends.
               We can see from these distributions that certain years (esp. 2017), times of year (August), and specific
               beaches (Petrie isl. and Mooney's Bay) can have to have greater E. coli abundances than others.")


coliform.link <- a(href = "https://en.wikipedia.org/wiki/Coliform_bacteria", "Coliform bacteria")

notes_txt <- p(h4("Introduction"), br(),
    "This dashboard presents", source.beach2," during the period 2014-2019.
    All the beaches are located on the Ottawa river, exccept for Mooney's bay, which is located on the Rideau river.
    The beaches are open from mid-June to late August.",
    br(), br(),


    "The Ottawa Public Health beach water sampling dataset consists of daily", tags$i("Escerichia coli"), "counts (in colony forming units [cfu] per 100 mL)
    and beach swimming status. In season, beaches may be closed to swimmers for several reasons;
    here, these swim statuses are referred to as:",
    tags$b("Closed"), "- outside of the swim season for a given beach,",
    tags$b("Rain"), "- where closed due to significant rainfall, and ",
    tags$b("E. coli"), "-which means the beach was closed due to a large abundance of bacteria.

    Unfortunately, the data from 2016 do not indicate if a beach was closed due to rain or E. coli specifically; in the absence of these labels,
    these observations have been labelled as 'E. coli' as this occurs more frequently (in the future, weather observations will be
    added to disambiguate these labels).", br(), br(),

    h4("Background information from the City of Ottawa / Ottawa Public Health:"),
    "'A no-swimming advisory will be issued if bacteria levels are over 200 E. coli per 100mL of water tested for one day;
    or if bacteria levels are over 100 E. coli per 100mL of water tested on two or more consecutive days.
    When a no-swim advisory is in effect, people should not swim due to the risk of getting a skin, ear, throat
    or even gastro-intestinal illness.  A 24-hour no-swim advisory may be in place at the beaches after significant rainfall.  Please note Ottawa Public Health collects beach water samples every day. The results take 18 to 24 hours to process
    in the laboratory, and as such, swim and no-swim advisories are based on sample results taken from the previous day.'",
    br(),br(),

    h4("E. coli testing and beach water safety"),
    coliform.link," is a generic term for an array of fecal bacteria but their abundance is not necessarily indicative of fecal contamination;
    Ottawa tests specifically for ", tags$i("Escherichia coli"), "as their abundance serves as an more robust indicator for diverse pathogens
    including bacteria", tags$i("Campylobacter, Salmonella, Shigella"),
    ", protozoans ", tags$i("Giardia"),"and", tags$i("Cryptosporidium"),
    " as well as viruses (enterovirus, norovirus, and adenovirus) that cause illness in swimmers [1].",
    tags$i("E.coli"), "is a useful proxy species for measurement as it is not practical
    to count these organisms on a daily basis.",
    br(), br(),

    h4("Factors affecting beach water quality"), br(),
    tags$dl(tags$dt("Time of year and temperature"),
            tags$dd("Coliform abundances tend to increase during late summer and fall.
                    This trend is made clear in the E. coli count time-series figures ('time-series > E. coli > by year')
                    where we note that nearly every year sees E. coli counts spike during
                    the last week of the beach season. Similarly, when plotting the distributions of E. coli
                    counts we notice that they tend to be greater in August than June or July ('statistics > E.coli distributions > by month, fig. 3)"),
            br(),
            tags$dt("Rainfall"),
            tags$dd("Heavy rains cause agriculural runoff and sewage outflows that contribute to the proliferation of pathogens
                    In figure 1: daily beach status and E. coli count we can see that rain days are generally followed by
                    substantially increased E. coli abundance for several days.")),
    br(), br()
    )

dashboard_txt <- p(h4("About this dashboard"),
                   "This dashboard was created using the R programming language and the libraries Shiny, Tidyverse, ggplot2, and ggmap, among others.
                   The dashboard is hosted for free on the ShinyApps.io website. Icons by ", a(href="https://fontawesome.com/", "FontAwesome"),".
                   Code is available from the github link on the sidebar.")

refs <- p(h4('References'),
    "1. Sanborn M, Takaro T. Recreational water-related illness: office management and prevention. Can Fam Physician. 2013;59(5):491‐495.
      https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3653650/  ", br())

#############################################################################################################################
### UI half of app.R:

ui <- fluidPage(theme = shinytheme("flatly"),

  ### Sidebar layout ###
  sidebarLayout(
    sidebarPanel(
      titlePanel(title), p(stitle),
      br(),
      fluidRow(
        column(12, plotOutput(outputId = "map1"), ###, height = "360px", width = "95%"
        )),
      h4("Data filters:"),
      fluidRow(
        column(4, checkboxGroupInput("cbeach", label = "Beaches",
                                     choices = list("Britannia","Mooney's Bay" = "Mooneys","Petrie East" = "PetrieEast",
                                                    "Petrie River" = "PetrieRiver","Westboro"),
                                     selected = c("Westboro","Mooneys", "Britannia", "PetrieEast","PetrieRiver"))),
        column(3, checkboxGroupInput("cyear", label = "Year", choices = seq(2014,2019), selected = seq(2014,2019))),
        column(4,checkboxGroupInput("cstatus", label = "Status",
                                    choices = list("Open" = 'Swim', "E. coli" = 'E. coli', "Rain" = 'Rain', "Closed" = 'Closed'),
                                    selected = c('Swim', "E. coli", "Rain", "Closed")))
        ),
      h6(byline),

    ),

    ### Main Panels ###
    mainPanel(
      tabsetPanel(type = "tabs",
              tabPanel("Time-series", icon = icon('chart-line'), br(),
                   h4("Daily coliform counts and beach status"),
                   tabsetPanel(id = 'exploratory tabs', type = "tabs",
                       tabPanel("Swim status", icon = icon("swimmer"),
                                tabsetPanel(id = "ts1", type = 'pills',
                                            tabPanel("group by year", plotOutput(outputId = "ts_combo_year", height = "500px", width = "99%")),
                                            tabPanel("group by beach", plotOutput(outputId = "ts_combo_beach", height = "500px", width = "99%"))
                                ), status_text, rain_2016_note),
                       tabPanel("E. coli counts", icon = icon("poo"), h5("Plot timeseries lines by:"),
                                tabsetPanel(id='time series', type = "pills",
                                    tabPanel("Beach", br(), h5("\tY-axis scale:"),
                                             tabsetPanel(id='axis1', type = 'pills',
                                               tabPanel("Linear", br(), plotOutput(outputId = "ts_year", height = "600px", width = "95%")),
                                               tabPanel("Log2",br(),plotOutput(outputId = "ts_year_log", height = "600px", width = "95%")))),
                                    tabPanel("Year", br(), h5("\tY-axis scale:"),
                                             tabsetPanel(id='axis1', type = 'pills',
                                               tabPanel("Linear", br(), plotOutput(outputId = "ts_beach", height = "600px", width = "95%")),
                                               tabPanel("Log2", br(), plotOutput(outputId = "ts_beach_log", height = "600px", width = "95%")))),
                                    tabPanel("Beach & Year", br(), h5("\tY-axis scale:"),
                                             tabsetPanel(id='axis1', type = 'pills',
                                               tabPanel("Linear",br(), plotOutput(outputId = "ts_grid", height = "600px", width = "95%")),
                                               tabPanel("Log2",br(), plotOutput(outputId = "ts_grid_log", height = "600px", width = "95%"))))
                                ), br(), p(coliform_text)
                       ))),

              tabPanel("Statistics", icon= icon("calculator"), br(),
                   h4("Statistics"),
                   tabsetPanel(id = 'statisticstabs', type = "tabs",
                         tabPanel("Status Summary", icon = icon("exclamation-triangle"), plotOutput(outputId = "stackbars", height = "500px", width = "99%"),
                                  fig3cap),
                         tabPanel("E. coli Distribution", icon = icon("chart-area"), h5("Choose plot style:"),
                                  tabsetPanel(id='distributions', type = 'tabs',
                                        tabPanel("Histograms", h5("Group observations:"),
                                                 tabsetPanel(id='histograms', type = 'pills',
                                                       tabPanel("by season & beach", br(), plotOutput(outputId = 'histgrid', height = "500px", width = "99%")),
                                                       tabPanel("by beach", br(), plotOutput(outputId = 'histbeach', height = "500px", width = "99%")),
                                                       tabPanel("by year", br(), plotOutput(outputId = 'histyear', height = "500px", width = "99%")),
                                                       tabPanel("by month", br(), plotOutput(outputId = 'histmonth', height = "500px", width = "99%"))),
                                                 fluidRow(column(4, sliderInput(inputId = 'bins', label = "Select number of bins for histogram:",
                                                                         value = 10, min = 6, max = 20, step = 1)), column(1),
                                                          column(4, sliderInput(inputId = 'slider.count',
                                                                         label = "Set E. coli concentration range (cfu/100mL):",
                                                                         value = c(0,1000), min = 0, max = 1000, step = 50)))),

                                        tabPanel('Violin plots',
                                                 tabsetPanel(id='violin', type = 'pills',
                                                       tabPanel("by season & beach", br(),plotOutput(outputId = "violingrid", height = "700px", width = "99%")),
                                                       tabPanel("by beach", br(), plotOutput(outputId = "violinbeach", height = "350px", width = "99%")),
                                                       tabPanel("by year", br(), plotOutput(outputId = "violinyear", height = "350px", width = "99%")),
                                                       tabPanel("by month",br(), plotOutput(outputId = 'violinmonth', height = "350px", width = "99%"))),
                                                 sliderInput(inputId = 'slider.count2',
                                                             label = p("Set E. coli (cfu/100mL) range limits:"),
                                                             value = c(0,1000), min = 0, max = 1000, step = 25)),
                                        tabPanel("Box plots",
                                                 tabsetPanel(id='boxplot', type = 'pills',
                                                       tabPanel("by season & beach", br(), plotOutput(outputId = "box_grid", height = "600px", width = "95%"), br()),
                                                       tabPanel("by beach", br(),plotOutput(outputId = "box_beach", height = "600px", width = "95%"), br()),
                                                       tabPanel("by year", br(), plotOutput(outputId = "box_year", height = "600px", width = "95%"), br()),
                                                       tabPanel("by month",br(), plotOutput(outputId = "box_month", height = "600px", width = "95%"), br())
                                                       )),

                                        h4('Distributions of E. coli counts'),
                                        dist_text,
                                        h.thresholds_note))
                         )),
              # Insert geography tab here:#

              # RAW DATA TAB
              tabPanel("Data", icon = icon("list-ol"),br(),
                       h4('Choose dataset for table'),
                       tabsetPanel(id = 'data tables',
                                   tabPanel("Swim status and E. coli", br(),
                                            DT::dataTableOutput("table1"),br(),
                                            # Button Downloadable csv of selected dataset ----
                                            h4("Download the dataset used in this dashboard:"),
                                            downloadButton("downloadData", "Download"),),
                                   # tabPanel("Weather", DT::dataTableOutput("table2")),
                                   tabPanel("Coordinates", DT::dataTableOutput("table0")),
                                   tabPanel("Summary stats", br(),
                                            h5('Basic summary of dataset including weather'),br(),
                                            verbatimTextOutput(outputId = 'statsummary'),
                                            br(), br(),
                                            # h4('Mean E. coli (cfu/100mL) (+- standard deviation) by beach and year'),
                                            # h4('Summary of means and variance'),br(),
                                            # br(),
                                            # h4('Generalized linear model for E. coli (cfu/100mL)s using a negative binomial distribution'),
                                            br())),
                       br(), h4("Data sources"),
                       tags$ol(tags$li("E. coli counts and beach status", source.beach)
                         #FOR WHEN WEATHER AND HYDROLOGY ADDED
                         # tags$li("Weather at Ottawa Intl Ottawa Intl A 71628", source.weather),
                         # tags$li("River hydrology wateroffice.ec.gc.ca", source.river, source.river2)
                       ), br()
              ),

              tabPanel("Notes", icon = icon("sticky-note"), br(),
                       notes_txt, br(),
                       dashboard_txt, br(),
                       h4("Data sources"),
                       tags$ol(
                         tags$li("E. coli counts and beach status", source.beach),
                         tags$li("Weather at Ottawa Intl Ottawa Intl A 71628", source.weather),
                         tags$li("River hydrology wateroffice.ec.gc.ca", source.river, source.river2)), br(),
                       refs)
              )
      )))

server <- function(input, output){
  ### REACTIVE INPUTS
  # reactive - filter location, year, status (ie. factors)
  beachesInput <- reactive({
    beaches.sub = beaches
    beaches.sub = beaches.sub[beaches.sub$location %in% input$cbeach,]
    beaches.sub = beaches.sub[beaches.sub$year %in% input$cyear,]
    beaches.sub = beaches.sub[beaches.sub$status %in% input$cstatus,]
    return(beaches.sub)
  })
  # Range slider bar reactive input (integer counts)
  beachesRange <- reactive({
    beaches.range = beachesInput() %>%
      filter(count >= input$slider.count[1]) %>%
      filter(count <=  input$slider.count[2])
    return(beaches.range)
  })
  # Range slider bar reactive input (integer counts)
  beachesRange2 <- reactive({
    beaches.range = beachesInput() %>%
      filter(count >= input$slider.count2[1]) %>%
      filter(count <=  input$slider.count2[2])
    return(beaches.range)
  })
  # reactive; exclude locations from geographic data
  geoInput <- reactive({
    geo = geo[geo$Location %in% input$cbeach,]
    return(geo)
  })
  # controls span for fitlines
  smoothing <- reactive({
    smooth = input$slider.span
    return(smooth/100)
  })
  # controls span for fitlines
  transparency <- reactive({
    tp = input$slider.alpha
    return(tp)
  })
  bins <- reactive({
    bins <- input$bins
    return(bins)
  })

  ### TEXT FUNCTIONS
  # HTML hyperlinks
  output$twit <- renderUI({
    tagList(h5("app created by:", tweet))
  })


  ### FIGURES ###
  # Basic Map of beach locations
  output$map1 <- renderPlot({
    ggmap(get_map(location = c(-75.85, 45.25, -75.45, 45.60), source = "stamen", #c(-75.82, 45.3, -75.46, 45.55),
                  zoom = 11, maptype = "toner-lite")) +
      geom_point(data = geoInput(), aes(x = Long, y = Lat, colour = Location),
                  alpha = 1, size = 3) +
      scale_colour_carto_d(name = "", type = 'qualitative', direction = 1) +
      labs(x = "", y = "", title = "") +
      theme_grey() +
      theme(panel.background = element_rect(fill = "grey98"),
            legend.position = c(1.0, .45),
            legend.justification = c("right", "top"),
            legend.box.just = "right",
            legend.margin = margin(5, 5, 5, 5),
            legend.key = element_rect(colour = "grey90"),
            legend.text = element_text(size = rel(1)),
            axis.text = element_text(size = rel(0.75)),
            axis.text.x = element_text(angle = 0, vjust = 0.5),
            plot.title = element_text(size = rel(1.25)),
            # plot.margin = margin(0.2, 0.4, 0.2, 0.2, "cm"),
            plot.background = element_rect(
            fill = "grey98",
            colour = "grey",
            size = 0.2)
            )
  })



transparency <- 0.8
    ## Timeseries
  output$ts_year <- renderPlot({
    ts_plot(beachesInput(), 'julian', 'count', 'year', transparency) +
      scale_colour_viridis_d(name = "", limits = levels(beaches$year)) + facet_grid(location~.) + facet_labels
  })
  output$ts_year_log <- renderPlot({
    ts_plot(beachesInput(), 'julian', 'count', 'year', transparency ) +
      scale_y_continuous(trans = 'log2') +
      scale_colour_viridis_d(name = "", limits = levels(beaches$year)) +
      facet_grid(location~.) + facet_labels
  })
  output$ts_beach <- renderPlot({
    ts_plot(beachesInput(), 'julian', 'count', 'location', transparency) +
      scale_colour_carto_d(name = "", type = 'qualitative', direction = 1, limits = levels(as.factor(beaches$location))) +
      facet_grid(year~.) + facet_labels
  })
  output$ts_beach_log <- renderPlot({
    ts_plot(beachesInput(), 'julian', 'count', 'location', transparency) +
      scale_y_continuous(trans = 'log2') +
      scale_colour_carto_d(name = "", type = 'qualitative', direction = 1, limits = levels(as.factor(beaches$location))) +
      facet_grid(year~.) + facet_labels
  })
  output$ts_grid <- renderPlot({
    ts_plot(beachesInput(), 'julian', 'count', 'location', transparency) +
      scale_colour_carto_d(name = "", type = 'qualitative', direction = 1, limits = levels(as.factor(beaches$location))) +
      facet_grid(location~year) + facet_labels
  })
  output$ts_grid_log <- renderPlot({
    ts_plot(beachesInput(), 'julian', 'count', 'location', transparency) +
      scale_y_continuous(trans = 'log2') +
      scale_colour_carto_d(name = "", type = 'qualitative', direction = 1, limits = levels(as.factor(beaches$location))) +
      facet_grid(location~year) + facet_labels
  })


  ## Violin Plots
  output$violingrid <- renderPlot({
    violin_plot(beachesRange2(), 'year', 'count', 'year', 'location') +
      ylab("E. coli (cfu/100 mL)") +  xlab("Year") +
      facet_grid(location~.)
  })
  output$violinbeach <- renderPlot({
    violin_plot(beachesRange2(), 'location', 'count', 'location', 'location') +
      ylab("E. coli (cfu/100 mL)") +  xlab("Location")
  })
  output$violinyear <- renderPlot({
    violin_plotd(beachesRange2(), 'year', 'count', 'year', 'year') +
      ylab("E. coli (cfu/100 mL)") +  xlab("Year")
  })
  output$violinmonth <- renderPlot({
    violin_plotf(beachesRange2(), 'month', 'count', 'month', 'month') +
      ylab("E. coli (cfu/100 mL)") +  xlab("Month of year")
  })

  ## Histograms
  output$histgrid <- renderPlot({
    hist_temp(beaches, beachesRange(), 'count', 'year', 'location', input$bins) +
      scale_fill_carto_d(name = "location", type = 'qualitative', direction = 1, limits = levels(as.factor(beaches$location))) +
      log10x
  })
  output$histbeach <- renderPlot({
    hist_temp2(beaches, beachesRange(), 'count', 'location', 'year', input$bins) +
      scale_fill_viridis_d(name = "year", limits = levels(as.factor(beaches$year))) +
      scale_x_continuous(trans='log10') +
      facet_grid(location~.) +
      theme(axis.text.x = element_text(angle = 50, hjust = 1))
  })
  output$histyear <- renderPlot({
    hist_temp2(beaches, beachesRange(), 'count', 'year', 'location', input$bins) +
      scale_fill_carto_d(name = "location", type = 'qualitative', direction = 1, limits = levels(as.factor(beaches$location))) +
      scale_x_continuous(trans='log10') +
      facet_grid(year~.) +
      theme(axis.text.x = element_text(angle = 50, hjust = 1))
  })
  output$histmonth <- renderPlot({
    hist_temp2(beaches, beachesRange(), 'count', 'month', 'year', input$bins) +
      scale_fill_viridis_d(name = "year", limits = levels(as.factor(beaches$year))) +
      scale_x_continuous(trans='log10') +
      facet_grid(month~.) +
      theme(axis.text.x = element_text(angle = 50, hjust = 1))
  })

  ## BOX plots:

  output$box_grid <- renderPlot({
    ggplot(beachesInput(), aes(x= location, y = count, group = location)) +
      geom_jitter(aes(colour = status), size = 2, alpha = 0.67, width = 0.15) +
      geom_boxplot(outlier.shape = NA, alpha = 0.01, colour='black') +
      stat_summary(aes(location, count, group = as.factor(location)),
                   fun = mean, geom = 'point', size = 2.5, color = 'black') +
      stat_summary(aes(location, count, group = as.factor(location)),
                   fun = mean_cl_boot, geom = 'line', size = 2.5, color = 'black') +
      scale_fill_discrete(name = "", direction = 1, limits = levels(as.factor(beaches$status))) +
      basic_theme + threshold + base_x +
      facet_grid(~year) + facet_labels +
      theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 10)) +
      scale_y_log10() +
      xlab("Location") + ylab("E. coli (cfu/100mL)")
  })
  output$box_beach <- renderPlot({
    ggplot(beachesInput(), aes(x = location, y = count, group = location)) +
      geom_jitter(aes(colour = year, shape = status), size = 2, alpha=0.67, width = 0.25) +
      stat_summary(aes(location, count, group = as.factor(location)),
                   fun = mean, geom = 'point', size =2.5, color = 'black') +
      geom_boxplot(outlier.shape = NA, alpha = 0.01, colour='black') +
      scale_colour_viridis_d(limits = levels(as.factor(beaches$year))) +
      basic_theme + threshold + base_x +
      scale_y_log10() +
      xlab("Location") + ylab("E. coli (cfu/100mL)")
  })
  output$box_year <- renderPlot({
    ggplot(beachesInput(), aes(x = year, y = count)) +
      geom_jitter(aes(colour = location, shape = status), size = 2, alpha=0.77, width = 0.3) +
      stat_summary(aes(year, count, group = as.factor(year)),
                   fun = mean, geom = 'point', size =2.5, color = 'black') +
      geom_boxplot(outlier.shape = NA, alpha = 0.01, colour='black') +
      scale_color_carto_d(name = "location", direction = 1, limits = levels(as.factor(beaches$location))) +
      basic_theme + threshold + base_x +
      scale_y_log10() +
      xlab("Year") + ylab("E. coli (cfu/100mL)")
  })
  output$box_month <- renderPlot({
    ggplot(beachesInput(), aes(x = as.factor(month), y = count)) +
      geom_jitter(aes(colour = year, shape = status), size = 2, alpha=0.67, width = 0.3) +
      stat_summary(aes(as.factor(month), count, group = as.factor(month)),
                   fun = mean, geom = 'point', size =2.5, color = 'black') +
      geom_boxplot(outlier.shape = NA, alpha = 0.01, colour='black') +
      scale_colour_viridis_d(limits = levels(as.factor(beaches$year))) +
      basic_theme + threshold + base_x +
      scale_y_log10() +
      xlab("Month") + ylab("E. coli (cfu/100mL)")
  })


  ### STATUS BAR GRAPHS
  output$stackbars <- renderPlot({
    ggplot(beachesInput(), aes(x = location, fill = status)) +
      geom_bar(stat = 'count', aes(fill = status), alpha = 0.66) +
      geom_text(stat= 'count', aes(label=..count..), size = 4,  position= position_stack(0.5)) +
      geom_text(stat = 'count', aes(group = location, label=..count..), colour='black', size = 3, vjust=-1.666) +
      ylim(c(0,76)) +
      scale_fill_discrete(name = "status", direction = 1, limits = levels(beaches$status), labels = levels(beaches$status)) +
      facet_grid(~year) + #coord_flip() +
      xlab("Location") + ylab("Number of days") + basic_theme + facet_labels + base_x +
      theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 10))
  })

  # Status: time-series & w/ counts
  annot_sts <- data.frame(x=c(170,200,230), y=seq(2020,2020,3), label = c('June', 'July', 'Aug'))

  output$ts_status_beach <- renderPlot({
    ggplot(beachesInput(), aes(x = julian, y= year, fill=status, colour = status)) +
      geom_point(alpha = 1, size=2.65) +
      first_of_month + guides(shape = FALSE) +
      scale_colour_discrete(name = "status", direction = 1, labels = levels(beaches$status), limits = levels(beaches$status)) +
      scale_fill_discrete(name = "status", direction = 1, labels = levels(beaches$status), limits = levels(beaches$status)) +
      facet_grid(location~.) + ylab("Beach") + xlab("Days into year") + basic_theme + facet_labels +
      theme(axis.text.x = element_text(angle = 50, hjust = 1),
            legend.text = element_text(face = "bold", size = 10))
    })
  output$ts_status_year <- renderPlot({
    ggplot(beachesInput(), aes(x = julian, y = location, fill = status, colour = status)) +
      geom_point(alpha = 1, size = 2.65) + first_of_month + guides(shape = FALSE) +
      scale_colour_discrete(name = "Status", direction = 1, labels = levels(beaches$status), limits = levels(beaches$status)) +
      scale_fill_discrete(name = "Status", direction = 1, labels = levels(beaches$status), limits = levels(beaches$status)) +
      facet_grid(year~.) +  ylab("Beach") + xlab("Days into year") + basic_theme + facet_labels +
      theme(axis.text.x = element_text(angle = 50, hjust = 1),
            legend.text = element_text(face = "bold", size = 10))
  })
  # annot_sts <- data.frame(x=c(170,200,230), y=seq(2020,2020,3), label = c('June', 'July', 'Aug'))

  output$ts_combo_beach <- renderPlot({
    ggplot(beachesInput(), aes(x = julian, y= year, group=year, fill=status, colour = status)) +
      geom_point(aes(size = count+50), alpha = 0.7) + first_of_month + guides(shape = FALSE) +
      scale_colour_discrete(name = "Status", direction = 1, labels = levels(beaches$status), limits = levels(beaches$status)) +
      scale_fill_discrete(name = "Status", direction = 1, labels = levels(beaches$status), limits = levels(beaches$status)) +
      scale_size_continuous("E. coli (cfu/100mL)") +
      facet_grid(location~.) + xlim(c(166,244)) + xlab("Days into year") + basic_theme + facet_labels + base_x+
      theme(axis.text.x = element_text(angle = 50, hjust = 1),
            legend.text = element_text(face="bold", size= 10)) +
      theme(legend.key.size = unit(5,"point"))
  })

  output$ts_combo_year <- renderPlot({
    ggplot(beachesInput(), aes(x = julian, y= location, group=location, fill=status, colour = status)) +
      geom_point(aes(size = count+50), alpha = 0.7) +
      first_of_month +
      guides(shape = FALSE) +
      scale_colour_discrete(name = "Status", direction = 1, labels = levels(beaches$status), limits = levels(beaches$status)) +
      scale_fill_discrete(name = "Status", direction = 1, labels = levels(beaches$status), limits = levels(beaches$status)) +
      scale_size_continuous("E. coli (cfu/100mL)") +
      facet_grid(year~.) + xlim(c(166,244)) + xlab("Days into year") + basic_theme + facet_labels + base_x +
      theme(axis.text.x = element_text(angle = 50, hjust = 1),
            legend.text = element_text(face="bold", size= 10),
            legend.key.size = unit(5,"point"))
  })


  ### SUMMARY STATISTICS TABLES:
  #calculate stats above and don't worry about user's filters; add notes
  output$statsummary <- renderPrint({
    summary(beachesInput())
  })

  ### DATA TABLES
  output$table1 <- DT::renderDataTable(DT::datatable({
    beachesInput()
  }))
  output$table2 <- DT::renderDataTable(DT::datatable({
    weather
  }))
  output$table0 <- DT::renderDataTable(DT::datatable({
    geoInput()
  }))

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("beaches", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(beachesInput(), file, row.names = FALSE)
    }
  )

}
shinyApp(ui = ui, server = server)






### ANIMATION THAT TAKES TOO LONG TO DO ON SHINY APP
#
#   output$ts_combo_anim <- renderImage({
#     # A temp file to save the output.
#     # This file will be removed later by renderImage
#     outfile <- tempfile(fileext='.gif')
#     # now make the animation
#     p = ggplot(beachesInput(), aes(x = julian, y = location, group = seq_along(julian), fill=status, colour = status)) +
#       geom_point(aes(size = log10(count)), shape=16, alpha = 0.7) +
#       first_of_month +
#       guides(shape = FALSE) +
#       scale_colour_carto_d(name = "status", direction = 1, labels = levels(beaches$status), limits = levels(beaches$status)) +
#       scale_fill_carto_d(name = "status", direction = 1, labels = levels(beaches$status), limits = levels(beaches$status)) +
#       # scale_shape_discrete(name = "status") +
#       facet_grid(year~.) +
#       xlim(c(166,243)) +
#       ylab("Beach") + xlab("Days into year") +
#       basic_theme +
#       theme(strip.text = element_text(face="bold", colour = 'black', size=12),
#             strip.background = element_rect(fill="grey90", colour="black",size=1),
#             axis.text.x = element_text(angle = 50, hjust = 1),
#             legend.text = element_text(face="bold", size= 10)) +
#       theme(legend.key.size = unit(5,"point")) +
#       # labs(title = "{frame_time}th day of year") +
#       transition_reveal(julian) + ease_aes('cubic-in-out')
#
#     anim_save("outfile.gif", animate(p, height=500, width = 800, fps = 6, duration = 20)) # New
#
#     # Return a list containing the filename
#     list(src = "outfile.gif",
#          contentType = 'image/gif'
#          # width = 500,
#          # height = 500
#          # alt = "This is alternate text"
#     )
#   }, deleteFile = TRUE)


# Introduction text OLD
# output$intro <- renderUI({
#   str0 <- paste(
#   "This dashboard presents beach water sampling data from five City of Ottawa beaches during the period 2014-2019.
#   The beaches are open from mid-June to late August, depending on the location and conditions.
#   Links to raw data are available in the 'data' tab. \n
#   The beach water sampling data consists coliform counts (cfu/100 mL) and beach swimming status
#   Swimming statuses are coded as either 'open', 'closed'- means out of season, 'rain' - means closed due to rainfall,
#   and 'coliform' - which means the beach was closed due to abundance of coliforms.
#   ")
#   str1 <- paste("Coliforms <i>Escherichia coli</i> counts are used as indicator species to estimate the abundance of other harmful\
#                 species such as <i>Shigella, Giardia, etc</i>. E. coli (cfu/100mL)s are always expressed in count per 100 mL on this dashboard.")
#   str2 <- paste("The beaches are open from ~06/14 to 08/30 unless the daily coliform count is above the 200 cfu/ 100 mL threshold \
#   results in closure. These events have been logged as status:'Closed: high coliform' in this dashboard. \
#   Additionally, a beach is closed if counts are above 100 for two or more days.")
#   str3 <- paste("If there is substantial rainfall, the beach may be closed for that day; these events are coded as status:'Closed: Rain'")
#   str4 <- paste("The maximum measurable count is 1000 cfu/100mL, so any observation of 1000/ 100 mL may be beyond the range of measurement or 'off-the-charts'.")
#   str5 <- paste("The data for this app is publicly available through the City of Ottawa's open-data portal.
#                 The public dataset contains several ambiguously labelled observations (different spellings, etc.);
#                 I have done my best to tidy those up here but cannot guarantee that the data here is 100% faithful to the public records. \n
#                 If the status of an observation is 'Closed: out of season' then no count data is available for that date.")
#   str6 <- paste("Hi, I made this dashboard for fun and as an outlet to learn and apply programming and data analysis skills. I thought it\
#        would be interesting and valuable to explore some public data that isn't very accessible in it's current state. Beyond the trends in \
#        beach swim days and E. coli (cfu/100mL)s, I wanted to explore the impacts of environmental factors like weather and river level. ")
#   HTML(paste(str0, br(), str1, br(), str2,br(), str3, br(), str4, br(), str5, br(), sep = '<br/>'))
#   })



# p("https://www.sciencedirect.com/science/article/abs/pii/0043135486902034  asserts that a \
#   negative binomial distribution is appropriate for highly dispersed E. coli (cfu/100mL)s."),


# OLD FIGURE CAPTIONS
# coliform_explore1 <- p("By plotting with the data grouped by beach location, we can easily see if there are any beaches that \
#                         tend to have more peaks. We can see that the Petrie Island beaches and Mooney's Bay tend to \
#                        have more large counts than Britannia and Westboro beaches.")
# coliform_explore2 <- p("In contrast, by grouping the data by year, we can easily see if there are any years that stand out.\
#                        We can see that 2017 was a really good year to be an coliform, but a pretty bad year if you wanted to go for a swim
#                        There doesn't seem to be much of an annual trend, though. The peaks tend to occur randomly throughout the season")
# coliform_explore3 <- p("Did you notice the graphs much less 'noisy' when the data is grouped by year instead of grouped by location?
#                        the panels where all the lines belong to the same year appear more united than the panels grouped by location, right?")
# coliform_explore4 <- p("This effect tells us the counts at each beach tend to move up and down together over time.This makes sense \
#                        because most of the beaches are all on the Ottawa river, except for Mooney's Bay, which is on\
#                        the Rideau river (see map on sidebar). Note that the Ottawa river flows to the East, while the\
#                        Rideau river flows North and feeds into the Ottawa river")
# coliform_explore5 <- p("We can conclude, based on this data, that the unique locations of the beaches have less of an impact on the\
#                        coliform count than other factors that affect all beaches simultaneously. What could those factors be?\
#                        Could we find some other environmental data that might explain some of the variance in the coliform counts?", br(),
#                       "We'll try to find out in the 'Statistics ~ models' tab.")




# LARGER MAP
#   output$map2 <- renderPlot({
#     ggmap(get_map(location = c(-75.9, 45.34, -75.4, 45.55), source = "stamen",
#                   zoom = 12, maptype = "toner-lite")) +
#       geom_point(data = geoInput(), aes(x = Long, y = Lat, colour = Location),
#                  alpha = 1, size = 5) +
#       scale_colour_carto_d(name = "", type = 'qualitative', direction = 1) +
#       labs(x = "", y = "", title = "") +
#       theme_grey() +
#       theme(panel.background = element_rect(fill = "grey95"),
#             legend.position = c(1.0, .55),
#             legend.justification = c("right", "top"),
#             legend.box.just = "right",
#             legend.margin = margin(6, 6, 6, 6),
#             legend.key = element_rect(colour = "grey90"),
#             legend.text = element_text(size = rel(1)),
#             axis.text = element_text(size = rel(0.75)),
#             axis.text.x = element_text(angle = 0, vjust = 0.5),
#             plot.title = element_text(size = rel(1.25)),
#             plot.margin = margin(0.2, 0.4, 0.2, 0.2, "cm"),
#             plot.background = element_rect(
#               fill = "grey95",
#               colour = "black",
#               size = 0.5))
#   })


### ENIVRO LAYOUT< USE IN SEPERATE DASHBOARD
# tabPanel("Environment", icon = icon("sun"),
#          tabsetPanel(id='enviro', type = 'pills',
#                      tabPanel("Temperature and Precipitation",
#                               p("weather graph"),
#                               plotOutput(outputId = 'precip_years', height ='400px', width='100%'),
#                               plotOutput(outputId = 'rain_summers', height ='400px', width='100%')
#                               ),
#                      tabPanel("River hydrology",
#                               p('river graph')
#                               )
#          ),
#          enviro_intro
#          )
### Old geography tab
#               tabPanel("Geography", icon = icon("map-marked"), br(),
#                        h4('A larger map of the Ottawa region with beach locations marked:'),
#                        plotOutput(outputId = "map2", height = "600px", width = "95%"),
#                        br(),
#                        # h4('Annual temperature and precipitation patterns:'), br(),
#                        # icon("cloud-sun-rain"),
#                        # br(),
#                        # h4("Hydrology of the Ottawa and Rideau rivers"),
#                        # icon("water"),
#                        br(),
#                        ),

#
#   output$rain_summers <- renderPlot({
#     rain_summer_grid
#   })
#
#   output$precip_years <- renderPlot({
#     precipyears_lin
#   })


#weather

# rain_summer_grid <- ggplot(data = weather_summer, aes(x = julian,y = T.mean, ymin = T.min, ymax = T.max)) +
#   geom_linerange(data = weather_summer, alpha = 0.4, size =0.5) +
#   geom_point(aes(size = rain, colour = rain), alpha = 0.95, color='black', shape=1) +
#   geom_point(aes(size = rain, colour = rain), alpha = 0.95) +
#   facet_grid(year~.) +
#   ggtitle("Summer temperature mean/range and precipitation at Ottawa airport 2014 - 2019") +
#   ylab("Temperature (˚C)") + xlab("Day of year") + labs(color = 'Precipitation\n (mm)', size = "") +
#   scale_colour_viridis_c(begin = 0.1, direction = -1) +
#   basic_theme
#
# precipyears_lin <- ggplot(data = weather_precip, aes(x = date,y = T.mean, ymin = T.min, ymax = T.max)) +
#   geom_linerange(data = weather, alpha = 0.4, size =0.19) +
#   geom_point(data = weather, alpha = 0.4, size =0.3) +
#   geom_point(aes(size = precip, colour = precip), alpha = 0.8) +
#   ggtitle("Daily temperatures and precipitation at Ottawa airport 2014 - 2019") +
#   ylab("Temperature (˚C)") + xlab("Date") +
#   labs(color = 'Precipitation\n (mm)', size = "") +
#   scale_colour_viridis_c(begin = 0.1, direction = -1) + basic_theme






