# Climate In My Life
# Shiny App
# App script
# Tim Szewczyk


library(shiny); library(tidyverse); library(magrittr); theme_set(theme_bw())
source("fn.R")
clm <- load_climate_data("1850-01-01")

# Define UI for application
ui <- fluidPage(theme=shinythemes::shinytheme("flatly"),

    # Application title
    titlePanel("Earth's climate across lifetimes"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput(inputId="a.name",
                      label="Enter your name",
                      value="Albert E."),
            dateRangeInput(inputId="a.dates",
                           label="Enter the dates you've been alive",
                           start="1879-03-14", end="1955-04-18",
                           min="1850-01-01", max=Sys.Date()),
            tags$hr(),
            textInput(inputId="b.name",
                      label="Enter your name",
                      value="Justin B."),
            dateRangeInput(inputId="b.dates",
                           label="Enter the dates you've been alive",
                           start="1994-03-01", end=Sys.Date(),
                           min="1850-01-01", max=Sys.Date()),
            tags$hr(),
            radioButtons(inputId="dataset",
                         label="Use measurements for:",
                         choices=c("Land and ocean"="globe",
                                   "Land only"="land")),
            radioButtons(inputId="temp",
                         label="Show temperature as:",
                         choices=c("Anomaly from baseline"="anomaly",
                                   "Actual temperature"="actual")),
            dateInput(inputId="plotStart", 
                      label="Enter earliest date to plot", 
                      value="1850-01-01", min="1850-01-01", max="2020-01-01"),
            tags$br(),
            "Some text about how climate might have changed, comparing between different people, and stuff like that.",
        width=4),

        # Show the scatter plot with the dates and names
        mainPanel(
           plotOutput("climateScatter"), width=8
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Reactives
    ctr <- reactive({
        ifelse(input$temp=="anomaly", 
               0, 
               ifelse(input$dataset=="globe", 14.183, 8.61))
        })
    
    plot.dims <- reactive({
        tibble(x_min=input$plotStart, 
               x_max=max(input$a.dates, input$b.dates, 
                         clm$mo[[input$dataset]]$Year),
               x_span=as.numeric(as.Date(x_max)-as.Date(x_min)),
               y_min=min(with(clm$mo[[input$dataset]] %>%
                                  filter(Date >= input$plotStart),
                              AnomMo-UncertMo))+ctr(), 
               y_max=max(with(clm$mo[[input$dataset]] %>%
                                  filter(Date >= input$plotStart),
                              AnomMo+UncertMo))+ctr(),
               y_span=y_max-y_min)
    })
    
    names.df <- reactive({
        tibble(name=c(input$a.name, input$b.name), 
               bday=c(input$a.dates[1], input$b.dates[1]),
               dday=c(input$a.dates[2], input$b.dates[2]),
               mnday=c(mean.Date(as.Date(input$a.dates)),
                       mean.Date(as.Date(input$b.dates))),
               xpos=plot.dims()$x_max + plot.dims()$x_span*c(.02, .05),
               ypos=plot.dims()$y_min + plot.dims()$y_span*c(0.02, 0.03)) %>%
            mutate(avgYrT=map2_dbl(bday, dday, ~clm$yr[[input$dataset]] %>%
                                       filter(YrEnd >= .x) %>%
                                       filter(YrEnd <= .y) %$%
                                       mean(AnomMo)),
                   medYrT=map2_dbl(bday, dday,
                                   ~clm$yr[[input$dataset]] %>%
                                       filter(YrEnd >= .x) %>%
                                       filter(YrEnd <= .y) %$%
                                       median(AnomMo)),
                   minYr=map2_dbl(bday, dday, ~clm$yr[[input$dataset]] %>%
                                      filter(YrEnd >= .x) %>%
                                      filter(YrEnd <= .y) %>%
                                      arrange(AnomMo) %$%
                                      first(Year)),
                   minYrT=map2_dbl(bday, dday, ~clm$yr[[input$dataset]] %>%
                                       filter(YrEnd >= .x) %>%
                                       filter(YrEnd <= .y) %$%
                                       min(AnomMo)),
                   minYrAge=map2_int(bday, dday, ~clm$yr[[input$dataset]] %>%
                                         filter(YrEnd >= .x) %>%
                                         filter(YrEnd <= .y) %$%
                                         which.min(AnomMo)),
                   maxYr=map2_dbl(bday, dday, ~clm$yr[[input$dataset]] %>%
                                      filter(YrEnd >= .x) %>%
                                      filter(YrEnd <= .y) %>%
                                      arrange(AnomMo) %$% last(Year)),
                   maxYrT=map2_dbl(bday, dday, ~clm$yr[[input$dataset]] %>%
                                       filter(YrEnd >= .x) %>%
                                       filter(YrEnd <= .y) %$%
                                       max(AnomMo)),
                   maxYrAge=map2_int(bday, dday, ~clm$yr[[input$dataset]] %>%
                                         filter(YrEnd >= .x) %>%
                                         filter(YrEnd <= .y) %$%
                                         which.max(AnomMo)),
                   avgMo=map2_dbl(bday, dday, ~clm$mo[[input$dataset]] %>%
                                      filter(Date >= .x) %>%
                                      filter(Date <= .y) %$%
                                      mean(AnomMo)),
                   medMo=map2_dbl(bday, dday, ~clm$mo[[input$dataset]] %>%
                                      filter(Date >= .x) %>%
                                      filter(Date <= .y) %$%
                                      median(AnomMo)),
                   minMo=map2_chr(bday, dday, ~clm$mo[[input$dataset]] %>%
                                      filter(Date >= .x) %>%
                                      filter(Date <= .y) %>%
                                      arrange(AnomMo) %$%
                                      as.character(first(Date))),
                   minMoT=map2_dbl(bday, dday, ~clm$mo[[input$dataset]] %>%
                                       filter(Date >= .x) %>%
                                       filter(Date <= .y) %$%
                                       min(AnomMo)),
                   minMoAge=map2_int(bday, dday, ~clm$mo[[input$dataset]] %>%
                                         filter(Date >= .x) %>%
                                         filter(Date <= .y) %>%
                                         arrange(Date) %$%
                                         which.min(AnomMo))/12,
                   maxMo=map2_chr(bday, dday, ~clm$mo[[input$dataset]] %>%
                                      filter(Date >= .x) %>%
                                      filter(Date <= .y) %>%
                                      arrange(desc(AnomMo)) %$%
                                      as.character(first(Date))),
                   maxMoT=map2_dbl(bday, dday, ~clm$mo[[input$dataset]] %>%
                                       filter(Date >= .x) %>%
                                       filter(Date <= .y) %$%
                                       max(AnomMo)),
                   maxMoAge=trunc(map2_int(bday, dday, ~clm$mo[[input$dataset]] %>%
                                               filter(Date >= .x) %>%
                                               filter(Date <= .y) %>%
                                               arrange(Date) %$%
                                               which.max(AnomMo))/12))
    })
    
    output$climateScatter <- renderPlot({
        ggplot() + 
            geom_hline(yintercept=ctr(), colour="gray", linetype=2) +
            geom_hline(yintercept=ctr()+2, colour="gray", 
                       size=0.25, linetype=2) +
            geom_segment(data=clm$dec[[input$dataset]] %>%
                             filter(decadeStart >= input$plotStart &
                                        decadeEnd <= Sys.Date()),
                         aes(x=decadeStart, xend=decadeEnd,
                             y=AnomMo+ctr(), yend=AnomMo+ctr()),
                         colour="#fc4e2a", size=2, alpha=0.5) +
            geom_linerange(data=clm$mo[[input$dataset]] %>%
                               filter(Date >= input$plotStart),
                           aes(x=Date, 
                               ymin=AnomMo-UncertMo+ctr(), 
                               ymax=AnomMo+UncertMo+ctr()), 
                           size=0.25, alpha=0.2) + 
            geom_point(data=clm$mo[[input$dataset]] %>%
                           filter(Date >= input$plotStart),
                       aes(x=Date, y=AnomMo+ctr()), 
                       shape=1, size=0.5, alpha=0.2) +
            geom_segment(data=clm$yr[[input$dataset]] %>%
                             filter(YrStart >= input$plotStart),
                         aes(x=YrStart, xend=YrEnd, 
                             y=AnomMo+ctr(), yend=AnomMo+ctr()), 
                         colour="#b10026", size=1, alpha=0.5) +
            geom_rug(data=clm$mo[[input$dataset]] %>%
                         filter(Date >= input$plotStart),
                     aes(x=Date, colour=AnomMo+ctr()), sides="b") +
            # temp ranges
            geom_errorbar(data=names.df(), colour="grey30", 
                          width=plot.dims()$x_span*0.02,
                         aes(x=xpos, ymin=minYrT+ctr(), ymax=maxYrT+ctr())) +
            geom_text(data=names.df(), size=4, angle=90, vjust=0, 
                      nudge_x=-plot.dims()$x_span*0.01,
                       aes(x=xpos, y=(minYrT+maxYrT+ctr()*2)/2, label=name)) +
            geom_point(data=names.df(), colour="grey30", shape=1,
                       aes(x=xpos, y=medYrT+ctr())) +
            # year ranges
            geom_errorbar(data=names.df(), colour="grey30", 
                          width=plot.dims()$y_span*0.04,
                          aes(xmin=bday, xmax=dday, y=ypos)) +
            geom_text(data=names.df(), size=4, hjust=0.5,
                      nudge_y=plot.dims()$y_span*0.03,
                      aes(x=mnday, label=name, y=ypos)) +
            
            scale_colour_viridis_c(option="B") +
            scale_x_date("", date_labels="%Y", minor_breaks=NULL, 
                         limits=c(input$plotStart, Sys.Date()+365*12)) +
            labs(y=ifelse(input$temp=="anomaly", 
                          "Difference from 1950-1980 average (ºC)",
                          paste("Average", 
                                ifelse(input$dataset=="globe", 
                                       "global", 
                                       "land surface"),
                                "temperature (ºC)"))) +
            theme(panel.grid.major.x=element_line(colour="grey95", size=0.25),
                  panel.grid.major.y=element_blank(),
                  panel.grid.minor=element_blank(),
                  legend.position="none",
                  axis.text=element_text(size=12), 
                  axis.title=element_text(size=14))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
