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

### CSV data:
# geographic data
geo <- read.csv("beaches_locations.csv")
# ecoli count and beach status data
ecoli <- read.csv("beach_ecoli.csv", strip.white=TRUE)
ecoli <- ecoli %>%
  pivot_longer(names(ecoli)[-1], names_to = "location", values_to = "count")
opens <- read.csv("beach_status.csv", strip.white=TRUE)
opens <- opens %>%
  pivot_longer(names(opens)[-1], names_to = "location", values_to = "status")
# tidying data, especially dates
x<-as.factor(opens$status)
ecoli$status <- factor(x,levels(x)[c(1,4,3,2)])
ecoli$Date <- as.Date(ecoli$Date)
ecoli <- ecoli %>%
  dplyr::mutate(year = lubridate::year(ecoli$Date),
                month = lubridate::month(ecoli$Date),
                day = lubridate::day(ecoli$Date),
                julian = lubridate::yday(ecoli$Date))
ecoli$year <- as.factor(ecoli$year)
ecoli[is.na(ecoli)] <- 1

### PLOT THEMES
# thematic elements
col_discrete <- scale_colour_carto_d(name = "", type = 'qualitative', direction = 1)
fill_discrete <- scale_fill_carto_d(name = "", type = 'qualitative', direction = 1)
basic_theme <- theme_classic() + theme_linedraw()
dark_theme <- theme_dark() + theme_linedraw()
facet_labels <- theme(strip.text = element_text(face = "bold", colour = 'black', size=12), strip.background = element_rect(fill = "grey93", colour = "black",size=1))
no_facet_labs <- theme(strip.background = element_blank(), strip.text.x = element_blank())
legend_format <- theme(legend.text = element_text(face = "bold", size = 12))
## Basic plot lines
# for ecoli threshold
ecoli_threshold <- geom_hline(yintercept = 200, linetype = "longdash", color = "black", size = 0.35, alpha = 1)
ecoli_thresholdv <- geom_vline(xintercept=200, linetype="longdash", color = "black", size = 0.4, alpha = 0.9)
# for timeseries first of months' lines
first_of_month <- geom_vline(xintercept = c(182, 213, 242), color = "darkgrey", alpha = 0.3, size = 2.5)
annot <- data.frame(x=c(170,200,230), y=seq(1015,1015,3), label = c('June', 'July', 'Aug'))

# VIOLIN TEMPLATE
violin_plot <- function(data, x, y, group, fill){
  ggplot(data, aes(.data[[x]], data[[y]], group = data[[group]])) +
    geom_violin(aes(fill=data[[fill]]), size=0.5, alpha =0.8) +
    scale_fill_carto_d(name = fill, type = 'qualitative', direction = 1) +
    ecoli_threshold + basic_theme + no_facet_labs +
    theme(axis.text.x = element_text(face='bold', size=11))
}
violin_plotd <- function(data, x, y, group, fill){
  ggplot(data, aes(.data[[x]], data[[y]], group = data[[group]])) +
    geom_violin(aes(fill=data[[fill]]), size=0.5, alpha =0.8) +
    scale_fill_viridis_d() +
    ecoli_threshold + basic_theme + no_facet_labs +
    theme(axis.text.x = element_text(face='bold', size=11))
}

# VIOLIN TEMPLATE
violin_plotf <- function(data, x, y, group, fill){
  ggplot(data, aes(as.factor(.data[[x]]), data[[y]], group = as.factor(.data[[group]]))) +
    geom_violin(aes(fill=as.factor(.data[[fill]])), size=0.5, alpha =0.8) +
    scale_fill_viridis_d() +
    ecoli_threshold + basic_theme + no_facet_labs +
    theme(axis.text.x = element_text(face='bold', size=11))
}


# HISTOGRAM TEMPLATES
hist_temp <- function(ecoli, data, x, group, colour, nbins){
  ggplot(data, aes(.data[[x]], group = .data[[colour]], fill = .data[[colour]])) +
    geom_histogram(aes(x = .data[[x]], fill = .data[[colour]]), position = "stack", alpha = 0.99, bins = nbins) +
    # geom_density(aes(y=..count../nbins, fill = .data[[colour]], colour = .data[[colour]]), alpha = 0.08) +
    ecoli_thresholdv + basic_theme + facet_labels +
    ylab("Number of days") + xlab("E. coli count (cfu/100 mL)") +
    facet_grid(location~year)+
    theme(axis.text.x = element_text(angle = 50, hjust = 1))
}
hist_temp2 <- function(ecoli, data, x, group, colour, nbins){
  ggplot(data, aes(.data[[x]], group = .data[[colour]], fill = .data[[colour]]), labels = data[[colour]]) +
    geom_histogram(aes(x = .data[[x]], fill = .data[[colour]]), position = "stack", alpha = 0.99, bins = nbins) +
    ecoli_thresholdv + basic_theme + facet_labels +
    ylab("Number of days") +
    xlab("E. coli count (cfu/100 mL)")
}

## Timeseries Plots templates:
ts_plot <- function(data, julian, count, group, alpha){
  ggplot(data, aes(.data[[julian]], .data[[count]],group = data[[group]])) +
    geom_line(aes(group=.data[[group]], colour = .data[[group]]), size = 0.75, alpha = alpha) +
    geom_text(data = annot, aes(x = x, y = y, label = label, group = label),
              color = "navyblue",
              size = 3, alpha = 0.8, angle = 0)+
    xlab('Day of year') + ylab('E.coli count (cfu/100ml)') +
    first_of_month + ecoli_threshold + basic_theme
}



### UI
ui <- fluidPage(theme = shinytheme("yeti"),

  # Main layout (left/top -> sidebarPanel, center/right-> mainPanel)
  sidebarLayout(
    # sidebar menu
    sidebarPanel(
      titlePanel("Ottawa Beaches:"),
      h5(icon = icon('umbrella-beach'),"An interactive dashboard to explore publicly available", em("Escherichia coli"), "counts and closures data collected during 2014-2019"),
      # Author, source data
      uiOutput('twit'), uiOutput("link"), br(),
      h5("Data filters"),
      fluidRow(
        # data filters - beach location
        column(4, checkboxGroupInput("cbeach", label = "Beaches",
                                     choices = list("Britannia",
                                                    "Mooney's Bay" = "Mooneys",
                                                    "Petrie East" = "PetrieEast",
                                                    "Petrie River" = "PetrieRiver",
                                                    "Westboro"),
                                     selected = c("Westboro","Mooneys", "Britannia", "PetrieEast","PetrieRiver"))
               ),
        # Year filter
        column(3, checkboxGroupInput("cyear", label = "Year",
                                    choices = seq(2014,2019),
                                    selected = seq(2014,2019)
                                    )
               ),
      ),
      fluidRow(
        # status filter
        column(5,checkboxGroupInput("cstatus", label = "Status",
                                    choices = list("Open" = 'Swim',
                                                   "Closed: high E. coli" = 'NoSwim',
                                                   "Closed: rain" = 'Rain',
                                                   "Closed: no data" = 'Closed'),
                                    selected = c('Swim', "NoSwim", "Rain"))
               )
      ),


      # sidebar MAP
      h5("Currently selected locations"),
      plotOutput(outputId = "map1", height = "360px", width = "95%")
      ),

    mainPanel(br(),
      #   # Output: Tabset w/ plot, summary, and table
      tabsetPanel(type = "tabs",
                  tabPanel("Analysis", icon = icon('microscope'),
                           br(), h5("Choose data to analyze:"),
                           tabsetPanel(id = 'exploratory tabs', type = "tabs",
                                       tabPanel("Beach status", icon = icon("swimmer"),
                                                br(),
                                                h4('Beach swimming status'), br(),
                                                p("These plots display beach closures data for each summer at each beach during the period 2014-2019. Select either time-series (status by day),\
                                                or bar-graph (a summary total days by status during each season). Use 'Status' checkboxes to filter status conditions, beaches and years to evaluate."),
                                                br(),
                                                p("Choose timeseries or summary:"),
                                                tabsetPanel(id='status', type = 'tabs',
                                                            tabPanel("Time-series",
                                                                     br(),
                                                                     plotOutput(outputId = "ts_status", height = "500px", width = "85%"), br(),
                                                                     p("Try selecting/deselecting 'Status: open' to facilate easier interpretation of these figures"),
                                                                     p("From this plot of beach status by day, we see that rain tends to be followed by closures due to high E. coli counts.\
                                                                     We also note the pattern of closures: they tend to occur in clusters as opposed to being evenly spread throughout the season.\
                                                                     In particular, there are stretches in 2016 and 2017 when rain and/or high coliform counts resulted in multiple closures"), br()
                                                                     ),
                                                            tabPanel("Time-series with count",
                                                                     br(),
                                                                     tabsetPanel(id='stat_anim_ts', type = 'tabs',
                                                                                 tabPanel("static", br(),plotOutput(outputId = "ts_combo", height = "500px", width = "95%"), br()),
                                                                                 tabPanel("animated (slow)", br(),
                                                                                          imageOutput("ts_combo_anim", height = "500px", width = "95%"), br(),
                                                                                          h5("It takes ~2mins to render the gif, there is no status bar or hourglass icon, but it is loading."),
                                                                                          p("This page will generate an animation of all the data current selected in the sidebar:\
                                                                                          colours represent open/closed status, point size represents E. coli count (log base 10 scaling).\
                                                                                          Points are revealed along the horizontal 'day of year' axis, with each beach getting it's own line for each season.\
                                                                                          You can browse other tabs while the gif loads but no new tabs will load until the rendering is finished"),
                                                                                          br())
                                                                                 ),
                                                                     p("Try selecting/deselecting 'Status: open' to facilate easier interpretation of these figures"),
                                                                     p("From this plot of beach status by day, we see that rain tends to be followed by closures due to high E. coli counts.\
                                                                     We also note the pattern of closures: they tend to occur in clusters as opposed to being evenly spread throughout the season.\
                                                                     In particular, there are lengthy stretches in 2014 and 2017 when "),br(),
                                                                     ),
                                                            tabPanel("Beach status summary bars",
                                                                     br(),
                                                                     plotOutput(outputId = "stackbars", height = "500px", width = "85%"), br(),
                                                                     p("This plot shows the total number of days for each beach status by beach and year; it shows what proportion of the summer the\
                                                                     beach was open/closed due to rain or high E. coli count."), br(), br()
                                                                     )


                                                            ),
                                                p("note: closures in 2016 did not specify whether closure was due to rain or not, unfortunately."), br()
                                                ),
                                       tabPanel("E. coli:\nTime-series", icon = icon("chart-line"), br(),
                                                h4("E. coli count timeseries"), br(),
                                                p("These figures show the E. coli counts during  each summer (coloured lines) \
                                                  at each beach location. In each plot, a line represents the daily E. coli counts for one summer at one location.The height \
                                                  of the line indicates the E. coli count value for that beach on that date: \
                                                  any count over the 200 cfu/100 mL threshold results in closure to ensure the safety of swimmers.\
                                                  We see that counts frequently cross the safety threshold.  Lines can be grouped either by beach or by year to allow for visual comparison and trend finding. "),

                                                p("Group timeseries lines by:"),
                                                tabsetPanel(id='time series', type = "tabs",
                                                            tabPanel("Beach",br(), p("\ty-axis scale:"),
                                                                     tabsetPanel(id='axis1', type = 'pills',
                                                                                 tabPanel("Linear", br(), plotOutput(outputId = "ts_year", height = "600px", width = "95%")),
                                                                                 tabPanel("Log base 2",br(),plotOutput(outputId = "ts_year_log", height = "600px", width = "95%"))
                                                                                 )
                                                                     ),
                                                            tabPanel("Year", br(), p("\ty-axis scale:"),
                                                                     tabsetPanel(id='axis1', type = 'pills',
                                                                                 tabPanel("Linear", br(), plotOutput(outputId = "ts_beach", height = "600px", width = "95%")),
                                                                                 tabPanel("Log base 2", br(), plotOutput(outputId = "ts_beach_log", height = "600px", width = "95%"))
                                                                                 )
                                                                     ),
                                                            tabPanel("No groups", br(), p("\ty-axis scale:"),
                                                                     tabsetPanel(id='axis1', type = 'pills',
                                                                                 tabPanel("Linear",br(), plotOutput(outputId = "ts_grid", height = "600px", width = "95%")),
                                                                                 tabPanel("Log base 2",br(), plotOutput(outputId = "ts_grid_log", height = "600px", width = "95%"))
                                                                                 )
                                                                     )
                                                ),br(),
                                                p(tags$b("Note:"), "The vertical lines in each plot represent the first days of July, August, and Sept.\n \
                                                  The dashed line represents the E.coli count threshold for beach closure at 200 cfu (colony \
                                                  forming units) per 100 mL."),
                                                fluidRow(
                                                  column(4,sliderInput(inputId = 'slider.alpha',
                                                                       label = "Set line transparency",
                                                                       value = 0.9, min = 0.5, max = 1, step = 0.05)
                                                  )
                                                ),

                                                h4("Interpreting the E. coli count data"), br(),
                                                p("By plotting with the data grouped by beach location, we can easily see if there are any beaches that\
                                                tend to have more peaks. We can see that the Petrie Island beaches and Mooney's Bay tend to \
                                                have more large counts than Britannia and Westboro beaches."), br(),
                                                p("In contrast, by grouping the data by year, we can easily see if there are any years that stand out.\
                                                  We can see that 2017 was a really good year to be an E. coli, but a pretty bad year if you wanted to go for a swim"), br(),
                                                p("Did you notice the graphs much less 'noisy' when the data is grouped by year instead of grouped by location?
                                                The lines grouped by year tended to move in unison more than the lines gropued by location, right?"),
                                                p("This effect tells us the counts at each beach tend to move up and down together.This makes sense \
                                                because most of the beaches are all on the Ottawa river, except for Mooney's Bay, which is on\
                                                the Rideau river (see map on sidebar). Note that the Ottawa river flows to the East, while the\
                                                  Rideau river flows North and feeds into the Ottawa river"),
                                                p("We can conclude, based on this data, that the unique locations of the beaches have less of an impact on the\
                                                E. coli count than other factors that affect all beaches simultaneously. What could those factors be?\
                                                  Could we find some other environmental data that might explain the fluctuations in the E. coli counts?")
                                                ),
                                       tabPanel("E. coli:\nDistribution", icon = icon("chart-area"),br(),
                                                h4('E. coli count distributions'), br(),
                                                p('These plots represent the distribution of E. coli counts at each beach for each\
                                                summer during the period 2014-2019. They allow for further comparison between the\
                                                various beaches and different years, except here we are looking at the frequency of\
                                                E. coli count observations. By grouping data by different variables (eg. beach, year, month of year),\
                                                we can identify some trends in the data.'),
                                                br(),
                                                p("Choose plot style:"),
                                                tabsetPanel(id='distributions', type = 'tabs',
                                                            tabPanel("Histograms",
                                                                     br(), p("E. coli count histograms for selected data grouped by variable:"), br(),
                                                                     tabsetPanel(id='histograms', type = 'tabs',
                                                                                 tabPanel("by season & beach", br(), plotOutput(outputId = 'histgrid', height = "500px", width = "85%")),
                                                                                 tabPanel("by beach", br(), plotOutput(outputId = 'histbeach', height = "500px", width = "85%")),
                                                                                 tabPanel("by year", br(), plotOutput(outputId = 'histyear', height = "500px", width = "85%")),
                                                                                 tabPanel("by month", br(), plotOutput(outputId = 'histmonth', height = "500px", width = "85%"))
                                                                                 ), br(),
                                                                     fluidRow(
                                                                       column(4, sliderInput(inputId = 'bins',
                                                                                             label = "Select number of bins for histogram:",
                                                                                             value = 15, min = 5, max = 50, step = 5)),
                                                                       column(1),
                                                                       column(4, sliderInput(inputId = 'slider.count',
                                                                                          label = "Set E.coli count range:",
                                                                                          value = c(0,1000), min = 0, max = 1000, step = 50))
                                                                     ), br()
                                                                     ),
                                                            tabPanel('Violin plots',
                                                                     br(), p("E. coli count violigrams for selected data grouped by variable:"), br(),
                                                                     tabsetPanel(id='histograms', type = 'tabs',
                                                                                 tabPanel("by season & beach", br(),plotOutput(outputId = "violingrid", height = "700px", width = "95%")),
                                                                                 tabPanel("by beach", br(), plotOutput(outputId = "violinbeach", height = "350px", width = "95%")),
                                                                                 tabPanel("by year", br(), plotOutput(outputId = "violinyear", height = "350px", width = "95%")),
                                                                                 tabPanel("by month",br(), plotOutput(outputId = 'violinmonth', height = "350px", width = "95%"))
                                                                                 ), br(),
                                                                     p("Violin plots show the distribution of E. coli counts for each beach and year"), br(),
                                                                     sliderInput(inputId = 'slider.count2',
                                                                                 label = p("Set E.coli count range limits:"),
                                                                                 value = c(0,1000), min = 0, max = 1000, step = 50)
                                                                     ),
                                                            tabPanel("Box plots",
                                                                     br(),p('E. coli count box plots show the distribution of E. coli counts with the median, quartiles and min/max bar.'), br(),
                                                                     tabsetPanel(id='histograms', type = 'tabs',
                                                                                 tabPanel("by season & beach", br(), plotOutput(outputId = "box_grid", height = "600px", width = "95%"), br()),
                                                                                 tabPanel("by beach", br(),plotOutput(outputId = "box_beach", height = "600px", width = "95%"), br()),
                                                                                 tabPanel("by year", br(), plotOutput(outputId = "box_year", height = "600px", width = "95%"), br()),
                                                                                 tabPanel("by month",br(), plotOutput(outputId = "box_month", height = "600px", width = "95%"), br())
                                                                                 ), br(),

                                                                     )

                                                            ),
                                                p(tags$b("Note:"), "The dashed line represents the E.coli count threshold for beach closure at 200 cfu (colony \
                                                  forming units) per 100 mL. The larger black point represent the mean E. coli count for the group.")
                                                )
                                       ),
                           br(), br()
                           ),
                  tabPanel("Statistics", icon= icon("clipboard-list"), br(),
                           h4('Summary statistics'),
                           verbatimTextOutput(outputId = 'statsummary')
                           ),
                  tabPanel("Data", icon = icon("table"),br(),
                           h4('Tabular data'),
                           DT::dataTableOutput("table"), br(),
                           # Button
                           # Downloadable csv of selected dataset ----
                           h5("Download the dataset used in this dashboard:"),
                           downloadButton("downloadData", "Download"),
                           br(),br()
                           ),
                  tabPanel("Map", icon = icon("map-marked"), br(),
                           h4('A larger map of the Ottawa region with beach locations marked:'),
                           plotOutput(outputId = "map2", height = "600px", width = "95%")
                           ),
                  tabPanel("Notes", icon = icon("book"), br(),
                           h4('Notes about the dataset and dashboard'),br(),
                           htmlOutput("intro"), br(), br())
                  )
      )
    )
)

server <- function(input, output){
  ### REACTIVE INPUTS
  # reactive - filter location, year, status (ie. factors)
  ecoliInput <- reactive({
    ecoli.sub = ecoli
    ecoli.sub = ecoli.sub[ecoli.sub$location %in% input$cbeach,]
    ecoli.sub = ecoli.sub[ecoli.sub$year %in% input$cyear,]
    ecoli.sub = ecoli.sub[ecoli.sub$status %in% input$cstatus,]
    return(ecoli.sub)
  })
  # Range slider bar reactive input (integer counts)
  ecoliRange <- reactive({
    ecoli.range = ecoliInput() %>%
      filter(count >= input$slider.count[1]) %>%
      filter(count <=  input$slider.count[2])
    return(ecoli.range)
  })
  # Range slider bar reactive input (integer counts)
  ecoliRange2 <- reactive({
    ecoli.range = ecoliInput() %>%
      filter(count >= input$slider.count2[1]) %>%
      filter(count <=  input$slider.count2[2])
    return(ecoli.range)
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
  url <- a(href = "https://open.ottawa.ca/datasets/beach-water-sampling", "City of Ottawa beach water sampling")
  tweet <- a(href = "https://twitter.com/quaxlikeaduck", "JA Moggridge")
  output$link <- renderUI({
    tagList(h5("Source:", url))
  })
  output$twit <- renderUI({
    tagList(h5("app created by:", tweet))
  })
  # Introduction text
  output$intro <- renderUI({
    str0 <- paste("This dashboard presents data from five City of Ottawa beaches during the period 2014-2019, as well as concurrent meterological\
                  and river hydrology data. The beaches are open from mid-late\
                  June to late August, depending on the location and conditions. All the underlying data was collected and made public by the city and is available \
                  through their open-data portal. Weather and hydrology data was obtained from Environment Canada and sources are listed below.\
                  The beach water sampling data consists of coliform counts and beach swimming status. Best efforts were made to represent this data accurately\
                  here.")
    str1 <- paste("<i>Escherichia coli</i> counts are used as indicator species to estimate the abundance of other harmful\
                  species such as <i>Shigella, Giardia, etc</i>. Coliform counts are always expressed in count per 100 mL on this dashboard.")
    str2 <- paste("The beaches are open from ~06/14 to 08/30 unless an <i>E. coli</i> daily count above the 200 cfu/ 100 mL threshold \
    results in closure. These events have been logged as status:'Closed: high E.coli' in this dashboard. \
    Additionally, a beach is closed if counts are above 100 for two or more days.")
    str3 <- paste("If there is substantial rainfall, the beach may be closed for that day; these events are coded as status:'Closed: Rain'")
    str4 <- paste("The maximum measurable count is 1000 cfu/100mL, so any observation of 1000/ 100 mL may be beyond the range of measurement or 'off-the-charts'.")
    str5 <- paste("The data for this app is publicly available through the City of Ottawa's open-data portal.
                  The public dataset contains several ambiguously labelled observations (different spellings, etc.);
                  I have done my best to tidy those up here but cannot guarantee that the data here is 100% faithful to the public records. \n
                  If the status of an observation is 'Closed: out of season' then no count data is available for that date.")
    str6 <- paste("Hi, I made this dashboard for fun and as an outlet to learn and apply programming and data analysis skills. I thought it\
         would be interesting and valuable to explore some public data that isn't very accessible in it's current state. Beyond the trends in \
         beach swim days and coliform counts, I wanted to explore the impacts of environmental factors like weather and river level. ")
    HTML(paste(str0, br(), str1, br(), str2,br(), str3, br(), str4, br(), str5, br(), sep = '<br/>'))
    })

  ### FIGURES ###
  # Basic Map of beach locations
  output$map1 <- renderPlot({
    ggmap(get_map(location = c(-75.82, 45.3, -75.46, 45.55), source = "stamen",
                  zoom = 11, maptype = "toner-lite")) +
      geom_point(data = geoInput(), aes(x = Long, y = Lat, colour = Location),
                  alpha = 1, size = 3) +
      scale_colour_carto_d(name = "", type = 'qualitative', direction = 1) +
      labs(x = "", y = "", title = "") +
      theme_grey() +
      theme(panel.background = element_rect(fill = "grey95"),
            legend.position = c(1.0, .55),
            legend.justification = c("right", "top"),
            legend.box.just = "right",
            legend.margin = margin(6, 6, 6, 6),
            legend.key = element_rect(colour = "grey90"),
            legend.text = element_text(size = rel(1)),
            axis.text = element_text(size = rel(0.75)),
            axis.text.x = element_text(angle = 0, vjust = 0.5),
            plot.title = element_text(size = rel(1.25)),
            plot.margin = margin(0.2, 0.4, 0.2, 0.2, "cm"),
            plot.background = element_rect(
              fill = "grey95",
              colour = "black",
              size = 0.5))
  })

  output$map2 <- renderPlot({
    ggmap(get_map(location = c(-75.9, 45.34, -75.4, 45.55), source = "stamen",
                  zoom = 12, maptype = "toner-lite")) +
      geom_point(data = geoInput(), aes(x = Long, y = Lat, colour = Location),
                 alpha = 1, size = 5) +
      scale_colour_carto_d(name = "", type = 'qualitative', direction = 1) +
      labs(x = "", y = "", title = "") +
      theme_grey() +
      theme(panel.background = element_rect(fill = "grey95"),
            legend.position = c(1.0, .55),
            legend.justification = c("right", "top"),
            legend.box.just = "right",
            legend.margin = margin(6, 6, 6, 6),
            legend.key = element_rect(colour = "grey90"),
            legend.text = element_text(size = rel(1)),
            axis.text = element_text(size = rel(0.75)),
            axis.text.x = element_text(angle = 0, vjust = 0.5),
            plot.title = element_text(size = rel(1.25)),
            plot.margin = margin(0.2, 0.4, 0.2, 0.2, "cm"),
            plot.background = element_rect(
              fill = "grey95",
              colour = "black",
              size = 0.5))
  })

  ## Timeseries
  output$ts_year <- renderPlot({
    ts_plot(ecoliInput(), 'julian', 'count', 'year', transparency()) +
      scale_colour_viridis_d(name = "", limits = levels(ecoli$year)) +
      facet_grid(location~.) + facet_labels
  })
  output$ts_year_log <- renderPlot({
    ts_plot(ecoliInput(), 'julian', 'count', 'year', transparency()) +
      scale_y_continuous(trans = 'log2') +
      scale_colour_viridis_d(name = "", limits = levels(ecoli$year)) +
      facet_grid(location~.) + facet_labels
  })
  output$ts_beach <- renderPlot({
    ts_plot(ecoliInput(), 'julian', 'count', 'location', transparency()) +
      scale_colour_carto_d(name = "", type = 'qualitative', direction = 1, limits = levels(as.factor(ecoli$location))) +
      facet_grid(year~.) + facet_labels
  })
  output$ts_beach_log <- renderPlot({
    ts_plot(ecoliInput(), 'julian', 'count', 'location', transparency()) +
      scale_y_continuous(trans = 'log2') +
      scale_colour_carto_d(name = "", type = 'qualitative', direction = 1, limits = levels(as.factor(ecoli$location))) +
      facet_grid(year~.) + facet_labels
  })
  output$ts_grid <- renderPlot({
    ts_plot(ecoliInput(), 'julian', 'count', 'location', transparency()) +
      scale_colour_carto_d(name = "", type = 'qualitative', direction = 1,  limits = levels(as.factor(ecoli$location))) +
      facet_grid(location~year) + facet_labels
  })
  output$ts_grid_log <- renderPlot({
    ts_plot(ecoliInput(), 'julian', 'count', 'location', transparency()) +
      scale_y_continuous(trans = 'log2') +
      scale_colour_carto_d(name = "", type = 'qualitative', direction = 1,  limits = levels(as.factor(ecoli$location))) +
      facet_grid(location~year) + facet_labels
  })


  ## Violin Plots
  output$violingrid <- renderPlot({
    violin_plot(ecoliRange2(), 'year', 'count', 'year', 'location') +
      ylab("E.coli count (cfu/100 mL)") +  xlab("Year") +
      facet_grid(location~.)
  })
  output$violinbeach <- renderPlot({
    violin_plot(ecoliRange2(), 'location', 'count', 'location', 'location') +
      ylab("E.coli count (cfu/100 mL)") +  xlab("Location")
  })
  output$violinyear <- renderPlot({
    violin_plotd(ecoliRange2(), 'year', 'count', 'year', 'year') +
      ylab("E.coli count (cfu/100 mL)") +  xlab("Year")
  })
  output$violinmonth <- renderPlot({
    violin_plotf(ecoliRange2(), 'month', 'count', 'month', 'month') +
      ylab("E.coli count (cfu/100 mL)") +  xlab("Month of year")
  })

  ## Histograms
  output$histgrid <- renderPlot({
    hist_temp(ecoli, ecoliRange(), 'count', 'year', 'location', input$bins) +
      scale_fill_carto_d(name = "location", type = 'qualitative', direction = 1, limits = levels(as.factor(ecoli$location))) +
      scale_x_continuous(trans='log2')
  })
  output$histbeach <- renderPlot({
    hist_temp2(ecoli, ecoliRange(), 'count', 'location', 'year', input$bins) +
      scale_fill_viridis_d(name = "year", limits = levels(as.factor(ecoli$year))) +
      scale_x_continuous(trans='log2') +
      facet_grid(~location) +
      theme(axis.text.x = element_text(angle = 50, hjust = 1))
  })
  output$histyear <- renderPlot({
    hist_temp2(ecoli, ecoliRange(), 'count', 'year', 'location', input$bins) +
      scale_fill_carto_d(name = "location", type = 'qualitative', direction = 1, limits = levels(as.factor(ecoli$location))) +
      scale_x_continuous(trans='log2') +
      facet_grid(~year) +
      theme(axis.text.x = element_text(angle = 50, hjust = 1))
  })
  output$histmonth <- renderPlot({
    hist_temp2(ecoli, ecoliRange(), 'count', 'month', 'year', input$bins) +
      scale_fill_viridis_d(name = "year", limits = levels(as.factor(ecoli$year))) +
      scale_x_continuous(trans='log2') +
      facet_grid(~month) +
      theme(axis.text.x = element_text(angle = 50, hjust = 1))
  })

  ## BOX plots:

  output$box_grid <- renderPlot({
    ggplot(ecoliInput(), aes(x= location, y = count, group = location)) +
      geom_jitter(aes(colour = status), size = 1, alpha = 0.67, width = 0.15) +
      geom_boxplot(outlier.shape = NA, alpha = 0.01, colour='black') +
      stat_summary(aes(location, count, group = as.factor(location)),
                   fun = mean, geom = 'point', size = 2.5, color = 'black') +
      stat_summary(aes(location, count, group = as.factor(location)),
                   fun = mean_cl_boot, geom = 'line', size = 2.5, color = 'black') +
      scale_fill_carto_d(name = "", type = 'qualitative', direction = 1, limits = levels(as.factor(ecoli$status))) +
      basic_theme + ecoli_threshold +
      facet_grid(~year) + facet_labels +
      theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 10)) +
      scale_y_log10() +
      xlab("Location") + ylab("E.coli count")
  })
  output$box_beach <- renderPlot({
    ggplot(ecoliInput(), aes(x = location, y = count, group = location)) +
      geom_jitter(aes(colour = year, shape = status), size = 1.5, alpha=0.67, width = 0.25) +
      stat_summary(aes(location, count, group = as.factor(location)),
                   fun = mean, geom = 'point', size =2.5, color = 'black') +
      geom_boxplot(outlier.shape = NA, alpha = 0.01, colour='black') +
      scale_colour_viridis_d(limits = levels(as.factor(ecoli$year))) +
      basic_theme + ecoli_threshold +
      scale_y_log10() +
      xlab("Location") + ylab("E.coli count")
  })
  output$box_year <- renderPlot({
    ggplot(ecoliInput(), aes(x = year, y = count)) +
      geom_jitter(aes(colour = location, shape = status), size = 1.5, alpha=0.77, width = 0.3) +
      stat_summary(aes(year, count, group = as.factor(year)),
                   fun = mean, geom = 'point', size =2.5, color = 'black') +
      geom_boxplot(outlier.shape = NA, alpha = 0.01, colour='black') +
      scale_color_carto_d(name = "location", type = 'qualitative', direction = 1, limits = levels(as.factor(ecoli$location))) +
      basic_theme + ecoli_threshold +
      scale_y_log10() +
      xlab("Location") + ylab("E.coli count")
  })

  output$box_month <- renderPlot({
    ggplot(ecoliInput(), aes(x = as.factor(month), y = count)) +
      geom_jitter(aes(colour = year, shape = status), size = 1.5, alpha=0.67, width = 0.3) +
      stat_summary(aes(as.factor(month), count, group = as.factor(month)),
                   fun = mean, geom = 'point', size =2.5, color = 'black') +
      geom_boxplot(outlier.shape = NA, alpha = 0.01, colour='black') +
      scale_colour_viridis_d(limits = levels(as.factor(ecoli$year))) +
      basic_theme + ecoli_threshold +
      scale_y_log10() +
      xlab("Location") + ylab("E.coli count")
  })



  ### STATUS BAR GRAPHS
  output$stackbars <- renderPlot({
    ggplot(ecoliInput(), aes(x = location, fill = status)) +
      geom_bar(stat = 'count', aes(fill = status), alpha = 0.66) +
      geom_text(stat= 'count', aes(label=..count..), size = 4,  position= position_stack(0.5)) +
      geom_text(stat = 'count', aes(group = location, label=..count..), colour='navyblue', size = 3, hjust=-1.5) +
      ylim(c(0,76)) +
      scale_fill_carto_d(name = "", type = 'qualitative', direction = 1, limits = levels(ecoli$status), labels = levels(ecoli$status)) +
      facet_grid(year~.) + coord_flip() +
      xlab("Beach") +
      ylab("Number of days") +
      basic_theme + facet_labels +
      theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 10))
  })
  output$ts_status <- renderPlot({
    ggplot(ecoliInput(), aes(x = julian, y= year, fill=status, colour = status)) +
      geom_point(alpha = 1, size=2.65) +
      first_of_month +
      guides(shape = FALSE) +
      scale_colour_carto_d(name = "status", type = 'qualitative', direction = 1, labels = levels(ecoli$status), limits = levels(ecoli$status)) +
      facet_grid(location~.) +
      ylab("Beach") + xlab("Days into year") +
      basic_theme +
      theme(strip.text = element_text(face="bold", colour = 'black', size=12),
            strip.background = element_rect(fill="grey90", colour="black",size=1),
            axis.text.x = element_text(angle = 50, hjust = 1),
            legend.text = element_text(face="bold", size= 10))
    })

  output$ts_combo <- renderPlot({
    ggplot(ecoliInput(), aes(x = julian, y= year, group=year, fill=status, colour = status)) +
      geom_point(aes(size = log10(count)), alpha = 0.7) +
      first_of_month +
      guides(shape = FALSE) +
      scale_colour_carto_d(name = "Beach Status", direction = 1, labels = levels(ecoli$status), limits = levels(ecoli$status)) +
      facet_grid(location~.) +
      xlim(c(166,244)) +
      ylab("Year") + xlab("Days into year") +
      basic_theme +
      theme(strip.text = element_text(face="bold", colour = 'black', size=12),
            strip.background = element_rect(fill="grey90", colour="black",size=1),
            axis.text.x = element_text(angle = 50, hjust = 1),
            legend.text = element_text(face="bold", size= 10)) +
      theme(legend.key.size = unit(5,"point"))
  })

  output$ts_combo_anim <- renderImage({
    # A temp file to save the output.
    # This file will be removed later by renderImage
    outfile <- tempfile(fileext='.gif')
    # now make the animation
    p = ggplot(ecoliInput(), aes(x = julian, y = location, group = seq_along(julian), fill=status, colour = status)) +
      geom_point(aes(size = log10(count)), shape=16, alpha = 0.7) +
      first_of_month +
      guides(shape = FALSE) +
      scale_colour_carto_d(name = "Beach Status", direction = 1, labels = levels(ecoli$status), limits = levels(ecoli$status)) +
      facet_grid(year~.) +
      xlim(c(166,243)) +
      ylab("Beach") + xlab("Days into year") +
      basic_theme +
      theme(strip.text = element_text(face="bold", colour = 'black', size=12),
            strip.background = element_rect(fill="grey90", colour="black",size=1),
            axis.text.x = element_text(angle = 50, hjust = 1),
            legend.text = element_text(face="bold", size= 10)) +
      theme(legend.key.size = unit(5,"point")) +
      # labs(title = "{frame_time}th day of year") +
      transition_reveal(julian) + ease_aes('cubic-in-out')

    anim_save("outfile.gif", animate(p, height=500, width = 800, fps = 6, duration = 20)) # New

    # Return a list containing the filename
    list(src = "outfile.gif",
         contentType = 'image/gif'
         # width = 500,
         # height = 500
         # alt = "This is alternate text"
    )
  }, deleteFile = TRUE)

  ### SUMMARY STATISTICS TABLES:
  #calculate stats above and don't worry about user's filters; add notes
  output$statsummary <- renderPrint({
    summary(ecoliInput())
  })

  ### DATA TABLES
  output$table <- DT::renderDataTable(DT::datatable({
    ecoliInput()
  }))
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("beaches", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(ecoliInput(), file, row.names = FALSE)
    }
  )
}
shinyApp(ui = ui, server = server)