# Shiny app script for Ottawa Beaches dashboard.
# Reactive objects all run in the 'server' section
# Layout happens in the 'ui' section
# Remember to set the current working directory to: setwd("~/Dropbox/R coding/ottawa_beaches")

### libraries
require(tidyverse)
require(shinythemes)
require(rcartocolor)
require(lubridate)
require(ggbeeswarm)
require(leaflet)
require(kableExtra)
require(ggthemes)

require(transformr)

### old ggmap packages:
# require(maps)
# require(ggmap)
# require(mapproj)
# require(mapdata)
# require(rgeos)
# require(maptools)
# require(sp)
# require(raster)
# require(rgdal)
# require(dismo)

### if adding animations
# require(gganimate)
# require(gifski)
# require(animation)
# require(lattice)
# require(proto)
# require(png)

### All things are made in global.R and then made reactive in this app.R script
source("./global.R")


#flatly_blue = #18bc9c
# sick_green: #90D13E
#############################################################################################################################
### UI half of app.R:

ui <- fluidPage(theme = shinytheme("flatly"),

  ### Sidebar layout ###
  sidebarLayout(
    sidebarPanel(
      # titlePanel(title),
      titlePanel(title = apptitle), p(stitle), hr, hr(),
      # fluidRow(column(12, plotOutput(outputId = "map1"))), OLD Map
      fluidRow(leafletOutput("leaflet.map",width = "95%", height = "300px")),

      hr(), p(tags$b("Data filters:")),
      fluidRow(column(4, checkboxGroupInput("cbeach", label = "Beaches",
                                            choices = list("Britannia",
                                                           "Mooney's Bay" = "Mooneys",
                                                           "Petrie East" = "PetrieEast",
                                                           "Petrie River" = "PetrieRiver",
                                                           "Westboro"),
                                     selected = c("Westboro","Mooneys", "Britannia", "PetrieEast","PetrieRiver"))),
               column(3, checkboxGroupInput("cyear",label = "Year", choices = seq(2014,2019), selected = seq(2014,2019))),
               column(3, checkboxGroupInput("cstatus", label = "Status",
                                           choices = list("Open" = 'Swim',
                                                          "E.coli" = 'E. coli',
                                                          "Rain" = 'Rain',
                                                          "Closed" = 'Closed'),
                                    selected = c('Swim', "E. coli", "Rain", "Closed")
                                    ))
        ),
      p(hr(), byline, hr()),

    ),

    ### Main Panels ###
    mainPanel(
      tabsetPanel(id='maintabs', type = "tabs",
            tabPanel("Closures", icon = icon("swimmer"), br(),
                         tabsetPanel(id = "closures.tab", type = 'tabs',
                                     tabPanel("Beach status daily", icon = icon('chart-line'), br(),
                                              plotOutput(outputId = "ts_combo_year", height = "500px", width = "95%"),
                                              br(),status_text),
                                     tabPanel("Beach status summary", icon = icon('chart-bar'), br(),
                                              plotOutput(outputId = "stackbars", height = "400px", width = "99%"),
                                              fig3cap),
                                     tabPanel("Tables", icon = icon("list-ol"),
                                              h5(tags$b("Table:"), "Beach status days/percentage for each beach season"), br(),
                                              tabsetPanel(type="pills",
                                                          tabPanel("Total days", DT::dataTableOutput("table.status.days")),
                                                          tabPanel("Percentages", DT::dataTableOutput("table.status.percent"))
                                              )),
                                     rain_2016_note)),

            tabPanel("E. coli", icon = icon("poo"), br(),
                       tabsetPanel(id = "ts2", type = 'tabs',
                             tabPanel("Daily counts", icon = icon('chart-line'), br(),
                                    tabsetPanel(id='time series1', type = "pills",
                                                tabPanel("group by year", br(),
                                                         tabsetPanel(id='axis1', type = 'pills',
                                                                     tabPanel("Linear scale", br(), plotOutput(outputId = "ts_beach", height = "600px", width = "95%")),
                                                                     tabPanel("Log scale", br(), plotOutput(outputId = "ts_beach_log", height = "600px", width = "95%")))
                                                         ),
                                                tabPanel("group by beach", br(),
                                                         tabsetPanel(id='axis1', type = 'pills',
                                                                     tabPanel("Linear scale", br(), plotOutput(outputId = "ts_year", height = "600px", width = "95%")),
                                                                     tabPanel("Log scale",br(), plotOutput(outputId = "ts_year_log", height = "600px", width = "95%")))
                                                         ),
                                                tabPanel("show each season", br(),
                                                         tabsetPanel(id='axis1', type = 'pills',
                                                                     tabPanel("Linear scale",br(), plotOutput(outputId = "ts_grid", height = "600px", width = "95%")),
                                                                     tabPanel("Log scale",br(), plotOutput(outputId = "ts_grid_log", height = "600px", width = "95%")))
                                                         ),
                                                # daily ecoli footer
                                                br(), p(coliform_text), br(), br())
                                    ),

                             tabPanel("Count distributions", icon = icon("chart-area"), br(),
                                      tabsetPanel(id='distributions', type = 'tabs',
                                        tabPanel("Box plots",
                                                 tabsetPanel(id='boxplot', type = 'pills',
                                                             tabPanel("by season & beach", br(), plotOutput(outputId = "box_grid", height = "600px", width = "95%"), br()),
                                                             tabPanel("by beach", br(),plotOutput(outputId = "box_beach", height = "600px", width = "95%"), br()),
                                                             tabPanel("by year", br(), plotOutput(outputId = "box_year", height = "600px", width = "95%"), br()),
                                                             tabPanel("by month",br(), plotOutput(outputId = "box_month", height = "600px", width = "95%"), br())
                                                 )),
                                        tabPanel("Histograms",
                                                 tabsetPanel(id='histograms', type = 'pills',
                                                             tabPanel("by season & beach", br(), plotOutput(outputId = 'histgrid', height = "500px", width = "99%")),
                                                             tabPanel("by beach", br(), plotOutput(outputId = 'histbeach', height = "500px", width = "99%")),
                                                             tabPanel("by year", br(), plotOutput(outputId = 'histyear', height = "500px", width = "99%")),
                                                             tabPanel("by month", br(), plotOutput(outputId = 'histmonth', height = "500px", width = "99%"))),
                                                 fluidRow(column(4, sliderInput(inputId = 'bins', label = "Select number of bins for histogram:",
                                                                                value = 10, min = 6, max = 20, step = 1)), column(1),
                                                          column(4, sliderInput(inputId = 'slider.count',
                                                                                label = "Set E. coli concentration range (cfu/100mL):",
                                                                                value = c(0,1000), min = 0, max = 1000, step = 50))
                                                          )),

                                        tabPanel('Violin plots',
                                                 tabsetPanel(id='violin', type = 'pills',
                                                             tabPanel("by season & beach", br(),plotOutput(outputId = "violingrid", height = "700px", width = "99%")),
                                                             tabPanel("by beach", br(), plotOutput(outputId = "violinbeach", height = "350px", width = "99%")),
                                                             tabPanel("by year", br(), plotOutput(outputId = "violinyear", height = "350px", width = "99%")),
                                                             tabPanel("by month",br(), plotOutput(outputId = 'violinmonth', height = "350px", width = "99%"))),
                                                 sliderInput(inputId = 'slider.count2',
                                                             label = p("Set E. coli / 100 mL range limits:"),
                                                             value = c(0,1000), min = 0, max = 1000, step = 25)
                                                 ),
                                        # E.coli distributions caption #
                                        br(), dist_text, h.thresholds_note)
                                      ),
                             tabPanel("Descriptive stats",icon = icon("list-ol"), br(), h5("Table: E. coli count summary statistics by season"),
                                      DT::dataTableOutput("table.ecoli"), br()
                                      )
                             )
                     ),
            # RAW DATA TAB
            tabPanel("Data", icon = icon("list-ol"),br(),
                     h4('Choose dataset for table'),
                     tabsetPanel(id = 'data.tables',
                                 tabPanel("Full dataset", br(),
                                          DT::dataTableOutput("table.beaches"),br(),
                                          # Button Downloadable csv of selected dataset ----
                                          h5("Download the dataset used in this dashboard:"),
                                          downloadButton("downloadData", "Download")),
                               tabPanel("Coordinates", br(), DT::dataTableOutput("table.geo")),
                               tabPanel("Weather", DT::dataTableOutput("table.weather"))
                               ),
                   # Data footer #
                   br(), h4("Data sources"),
                   tags$ol(tags$li("E. coli counts and beach status", source.beach),
                           tags$li("Weather at Ottawa Intl Ottawa Intl A 71628", source.weather),
                           tags$li("River hydrology wateroffice.ec.gc.ca", source.river, source.river2)
                           ),br()
                   ),
          tabPanel("Notes", icon = icon("sticky-note"), br(),
                   notes_txt,
                   dashboard_txt, hr(),
                   h4("Data sources"),
                   tags$ol(tags$li("E. coli counts and beach status", source.beach),
                           tags$li("Weather at Ottawa Intl Ottawa Intl A 71628", source.weather),
                           tags$li("River hydrology wateroffice.ec.gc.ca", source.river, source.river2)
                           ),
                   hr(),refs, br(), br()
                   )
          )
      )
)
)

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
  beachesInput.ts <- reactive({
    beaches.sub = beachesInput()
    beaches.sub[is.na(beaches.sub)] <- 10
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
  # maptypes {terrain, terrain-background, terrain-labels, terrain-lines, toner, toner-2010, toner-2011, toner-background, toner-hybrid, toner-labels, toner-lines, toner-lite, or watercolor.}

  output$map1 <- renderPlot({
    ggmap(get_map(location = c(-75.85, 45.25, -75.45, 45.60), source = "stamen", #c(-75.82, 45.3, -75.46, 45.55),
                  zoom = 11, maptype = "toner-lite")) +
      geom_point(data = geoInput(), aes(x = Long, y = Lat, colour = Location),
                  alpha = 1, size = 3) +
      scale_color_carto_d(name = "Selected locations", direction = 1, limits = levels(as.factor(beaches$location))) +
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

## Leaflet
  # points <- eventReactive(input$recalc, {
  #
  # }, ignoreNULL = FALSE)

  output$leaflet.map <- renderLeaflet({
    leaflet() %>%
      # addTiles() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = cbind(geoInput()$Long,geoInput()$Lat),
                 label =  geoInput()$Location)
  })

transparency <- 0.85
    ## Timeseries
  output$ts_year <- renderPlot({
    ts_plot(beachesInput(), 'julian', 'count', 'year', transparency) +
      scale_colour_viridis_d(name = "", limits = levels(beaches$year)) + facet_grid(location~.) + facet_labels
  })
  output$ts_year_log <- renderPlot({
    ts_plot(beachesInput(), 'julian', 'count', 'year', transparency ) +
      scale_y_log10() +
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
      scale_y_log10()+
      scale_colour_carto_d(name = "", type = 'qualitative', direction = 1, limits = levels(as.factor(beaches$location))) +
      facet_grid(year~.) + facet_labels
  })
  output$ts_grid <- renderPlot({
    ts_plot_grid(beachesInput(), 'julian', 'count', 'location', transparency)
    })

  output$ts_grid_log <- renderPlot({
    ts_plot_grid(beachesInput(), 'julian', 'count', 'location', transparency) +
      scale_y_log10()
    })


  ## Violin Plots
  output$violingrid <- renderPlot({
    violin_plot(beachesRange2(), 'year', 'count', 'year', 'location') +
      ylab("E. coli (cfu/100 mL)") +  xlab("Year") +
      facet_grid(location~.)})

  output$violinbeach <- renderPlot({
    violin_plot(beachesRange2(), 'location', 'count', 'location', 'location') +
      ylab("E. coli (cfu/100 mL)") +  xlab("Location")})

  output$violinyear <- renderPlot({
    violin_plotd(beachesRange2(), 'year', 'count', 'year', 'year') +
      ylab("E. coli (cfu/100 mL)") +  xlab("Year")})

  output$violinmonth <- renderPlot({
    violin_plot.month(beachesRange2(), 'month', 'count', 'month', 'month') +
      ylab("E. coli (cfu/100 mL)") +  xlab("Month of year") })

  ## Histograms
  output$histgrid <- renderPlot({
    hist_temp(beaches, beachesRange(), 'count', 'year', 'location', input$bins) +
      scale_fill_carto_d(name = "location", type = 'qualitative', direction = 1,
                         guide = FALSE,
                         limits = levels(as.factor(beaches$location))) +
      log10x
  })
  output$histbeach <- renderPlot({
    hist_temp2(beaches, beachesRange(), 'count', 'location', 'year', input$bins) +
      scale_fill_viridis_d(name = "year", limits = levels(as.factor(beaches$year))) +
      facet_grid(location~.)
  })
  output$histyear <- renderPlot({
    hist_temp2(beaches, beachesRange(), 'count', 'year', 'location', input$bins) +
      scale_fill_carto_d(name = "location", type = 'qualitative', direction = 1, limits = levels(as.factor(beaches$location))) +
      facet_grid(year~.)
  })
  output$histmonth <- renderPlot({
    hist_temp2(beaches, beachesRange(), 'count', 'month', 'year', input$bins) +
      scale_fill_viridis_d(name = "year", limits = levels(as.factor(beaches$year))) +
      facet_grid(month~.)
  })

  ## BOX plots:

  output$box_grid <- renderPlot({
    ggplot(beachesInput(), aes(x= location, y = count)) +
      geom_beeswarm(aes(colour = status))+
      geom_boxplot(outlier.shape = NA, alpha = 0.01, colour='black', notch = TRUE) +
      stat_summary(aes(location, count, group = as.factor(location)),
                   fun = mean, geom = 'point', size = 2.5, color = 'black') +
      stat_summary(aes(location, count, group = as.factor(location)),
                   fun = mean_cl_boot, geom = 'line', size = 2, color = 'black') +
      scale_fill_discrete(name = "", direction = 1, limits = levels(as.factor(beaches$status))) +
      scale_colour_discrete(name = "", direction = 1, limits = levels(as.factor(beaches$status))) +
      basic_theme + threshold + base_x +
      facet_grid(~year) + facet_labels +
      theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 10),
            legend.position = 'top' ) +
      scale_y_log10() +
      xlab("Location") + ylab("E. coli / 100 mL")
  })
  output$box_beach <- renderPlot({
    ggplot(beachesInput(), aes(x = location, y = count)) +
      geom_beeswarm(aes(colour = status))+

      stat_summary(aes(location, count, group = as.factor(location)),
                   fun = mean, geom = 'point', size =2, color = 'black') +
      geom_boxplot(outlier.shape = NA, alpha = 0.01, colour='black', notch = TRUE) +
      scale_fill_discrete(name = "", direction = 1, limits = levels(as.factor(beaches$status))) +
      scale_colour_discrete(name = "", direction = 1, limits = levels(as.factor(beaches$status))) +
      basic_theme + threshold + base_x +
      scale_y_log10() +
      xlab("Location") + ylab("E. coli / 100 mL")
  })
  output$box_year <- renderPlot({
    ggplot(beachesInput(), aes(x = year, y = count)) +
      geom_beeswarm(aes(colour = status))+

      stat_summary(aes(year, count, group = as.factor(year)),
                   fun = mean, geom = 'point', size =2, color = 'black') +
      geom_boxplot(outlier.shape = NA, alpha = 0.01, colour='black', notch = TRUE) +
      scale_fill_discrete(name = "", direction = 1, limits = levels(as.factor(beaches$status))) +
      scale_colour_discrete(name = "", direction = 1, limits = levels(as.factor(beaches$status))) +
      basic_theme + threshold + base_x +
      scale_y_log10() +
      xlab("Year") + ylab("E. coli / 100 mL")
  })
  output$box_month <- renderPlot({
    ggplot(beachesInput(), aes(x = month, y = count)) +
      geom_beeswarm(aes(colour = status))+

      stat_summary(aes(as.factor(month), count, group = as.factor(month)),
                   fun = mean, geom = 'point', size =2, color = 'black') +
      geom_boxplot(outlier.shape = NA, alpha = 0.01, colour='black', notch = TRUE) +
      scale_fill_discrete(name = "", direction = 1, limits = levels(as.factor(beaches$status))) +
      scale_colour_discrete(name = "", direction = 1, limits = levels(as.factor(beaches$status))) +
      basic_theme + threshold + base_x +
      scale_y_log10() +
      xlab("Month") + ylab("E. coli / 100 mL")
  })


  ### STATUS BAR GRAPHS
  output$stackbars <- renderPlot({
    ggplot(beachesInput(), aes(x = location, fill = status)) +
      geom_bar(stat = 'count', aes(fill = status), alpha = 0.66) +
      geom_text(stat= 'count', aes(label=..count..), size = 3.5,  position= position_stack(0.5), colour = "black") +
      geom_text(stat = 'count', aes(group = location, label=..count..), colour='#18bc9c', size = 3, vjust=-1.666) +
      ylim(c(0,76)) +
      scale_fill_discrete(name = "status", direction = 1, limits = levels(beaches$status), labels = levels(beaches$status)) +
      facet_grid(~year) + # coord_flip() +
      ylab("Number of days") + basic_theme + facet_labels + base_x +
      theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 10),
            axis.title.x = element_blank(),
            legend.position="top")
  })

  # Status: time-series & w/ counts
  annot_sts <- data.frame(x=c(170,200,230), y=seq(2020,2020,3), label = c('June', 'July', 'Aug'))

### beach status and Ecoli timeseries graphs (front page)

  output$ts_combo_year <- renderPlot({
    ggplot(beachesInput.ts(), aes(x = julian, y= location, group=location, fill=status, colour = status)) +
      geom_point(aes(size = count+50), alpha = 0.7) +
      first_of_month +
      guides(shape = FALSE) +
      scale_colour_discrete(name = "Status", direction = 1, labels = levels(beaches$status), limits = levels(beaches$status)) +
      scale_fill_discrete(name = "Status", direction = 1, labels = levels(beaches$status), limits = levels(beaches$status)) +
      scale_size_continuous("E. coli / 100 mL") +
      facet_grid(year~.) + xlim(c(166,244)) + xlab("Day of year") + basic_theme + facet_labels + base_x +
      theme(axis.text.x = element_text(angle = 50, hjust = 1),
            axis.title.y = element_blank(),
            legend.text = element_text(size= 9),
            legend.key.size = unit(5,"point"))+
      theme(legend.position="top")
  })


  ### SUMMARY STATISTICS TABLES:
  #calculate stats above and don't worry about user's filters; add notes
  output$statsummary <- renderPrint({
    summary(beachesInput())
  })

  ### DATA TABLES
  output$table.beaches <- DT::renderDataTable(DT::datatable({
    beachesInput()}, rownames = FALSE))

  output$table.weather <- DT::renderDataTable(DT::datatable({
    weather}, rownames = FALSE))

  output$table.geo <- DT::renderDataTable(DT::datatable({
    geoInput()}, rownames = FALSE,
    options = list(paging = FALSE, autoWidth = TRUE, searching=FALSE)))

  output$table.status.days <-  DT::renderDataTable(DT::datatable({
    table.status.days}, rownames = FALSE,
    options = list(paging = FALSE, autoWidth = TRUE, searching=FALSE)))

  output$table.status.percent <-  DT::renderDataTable(DT::datatable({
    table.status.percent}, rownames = FALSE,
    options = list(paging = FALSE, autoWidth = TRUE, searching=FALSE)))

  output$table.ecoli <-  DT::renderDataTable(DT::datatable({
    table.ecoli.sum}, rownames = FALSE,
    options = list(paging = FALSE, autoWidth = TRUE, searching=FALSE)))


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



