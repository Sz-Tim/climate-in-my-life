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
                      label="Enter a name and lifetime",
                      value="Albert E."),
            dateRangeInput(inputId="a.dates",
                           label=NULL,
                           start="1879-03-14", end="1955-04-18",
                           min="1850-01-01", max=Sys.Date()),
            textInput(inputId="b.name",
                      label="Enter a name and lifetime",
                      value="Justin B."),
            dateRangeInput(inputId="b.dates",
                           label=NULL,
                           start="1994-03-01", end=Sys.Date(),
                           min="1850-01-01", max=Sys.Date()),
            radioButtons(inputId="life.span",
                         label="Summarise lifetimes by:",
                         choices=c("Months", "Years"),
                         selected="Years", 
                         inline=TRUE),
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
            checkboxGroupInput(inputId="timeRes", 
                               label="Show temperature averages for:",
                               choices=c("Months", "Years", "Decades"),
                               selected=c("Months", "Years", "Decades"),
                               inline=T),
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
                   medMoT=map2_dbl(bday, dday, ~clm$mo[[input$dataset]] %>%
                                       filter(Date >= .x) %>%
                                       filter(Date <= .y) %$%
                                       median(AnomMo)),
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
    
    legend.df <- reactive({
        tibble(label="Lifetime temperature\nrange & median year",
               x_pos=input$plotStart + plot.dims()$x_span*0.03,
               y_pos=plot.dims()$y_min + plot.dims()$y_span*0.95,
               y_pos_min=plot.dims()$y_min + plot.dims()$y_span*0.92,
               y_pos_max=plot.dims()$y_min + plot.dims()$y_span*0.98)
    })
    
    output$climateScatter <- renderPlot({
        ggplot() + 
            geom_hline(yintercept=ctr(), colour="gray", linetype=2) +
            geom_hline(yintercept=ctr()+2, colour="gray", 
                       size=0.25, linetype=2) +
            {if("Decades" %in% input$timeRes) {
                geom_segment(data=clm$dec[[input$dataset]] %>%
                                 filter(decadeStart >= input$plotStart &
                                            decadeEnd <= Sys.Date()),
                             aes(x=decadeStart, xend=decadeEnd,
                                 y=AnomMo+ctr(), yend=AnomMo+ctr()),
                             colour="#fc4e2a", size=2, alpha=0.5)
            }} + 
            {if("Months" %in% input$timeRes) {
                geom_linerange(data=clm$mo[[input$dataset]] %>%
                                   filter(Date >= input$plotStart),
                               aes(x=Date, 
                                   ymin=AnomMo-UncertMo+ctr(), 
                                   ymax=AnomMo+UncertMo+ctr()), 
                               size=0.25, alpha=0.2)
            }} +
            {if("Months" %in% input$timeRes) {
                geom_point(data=clm$mo[[input$dataset]] %>%
                               filter(Date >= input$plotStart),
                           aes(x=Date, y=AnomMo+ctr()), 
                           shape=1, size=0.5, alpha=0.2) 
            }} +
            {if("Years" %in% input$timeRes) {
                geom_segment(data=clm$yr[[input$dataset]] %>%
                                 filter(YrStart >= input$plotStart),
                             aes(x=YrStart, xend=YrEnd, 
                                 y=AnomMo+ctr(), yend=AnomMo+ctr()), 
                             colour="#b10026", size=1, alpha=0.7) 
            }} +
            geom_rug(data=clm$mo[[input$dataset]] %>%
                         filter(Date >= input$plotStart),
                     aes(x=Date, colour=AnomMo+ctr()), sides="b") +
            # temp ranges
            {if(input$life.span=="Months") {
                geom_errorbar(data=names.df(), colour="grey30", 
                              width=plot.dims()$x_span*0.02,
                              aes(x=xpos, ymin=minMoT+ctr(), ymax=maxMoT+ctr())) 
            } else {
                geom_errorbar(data=names.df(), colour="grey30", 
                              width=plot.dims()$x_span*0.02,
                              aes(x=xpos, ymin=minYrT+ctr(), ymax=maxYrT+ctr())) 
            }} + 
            {if(input$life.span=="Months") {
                geom_text(data=names.df(), size=4, angle=90, vjust=0, 
                          nudge_x=-plot.dims()$x_span*0.01,
                          aes(x=xpos, y=(minMoT+maxMoT+ctr()*2)/2, label=name))
            } else {
                geom_text(data=names.df(), size=4, angle=90, vjust=0, 
                          nudge_x=-plot.dims()$x_span*0.01,
                          aes(x=xpos, y=(minYrT+maxYrT+ctr()*2)/2, label=name))
            }} +
            {if(input$life.span=="Months") {
                geom_point(data=names.df(), colour="grey30", shape=1,
                           aes(x=xpos, y=medMoT+ctr()))
            } else {
                geom_point(data=names.df(), colour="grey30", shape=1,
                           aes(x=xpos, y=medYrT+ctr()))
            }} +
            
            # year ranges
            geom_errorbar(data=names.df(), colour="grey30", 
                          width=plot.dims()$y_span*0.04,
                          aes(xmin=bday, xmax=dday, y=ypos)) +
            geom_text(data=names.df(), size=4, hjust=0.5,
                      nudge_y=plot.dims()$y_span*0.03,
                      aes(x=mnday, label=name, y=ypos)) +
            # legend
            geom_point(data=legend.df(), shape=1, aes(x=x_pos, y=y_pos)) +
            geom_errorbar(data=legend.df(), width=plot.dims()$x_span*0.01,
                       aes(x=x_pos, ymin=y_pos_min, ymax=y_pos_max)) +
            geom_text(data=legend.df(), lineheight=0.8,
                      hjust=0, size=3.75, nudge_x=plot.dims()$x_span*0.02, 
                      aes(x=x_pos, y=y_pos, label=label)) +
            # scales 
            scale_colour_viridis_c(option="B") +
            scale_x_date("", date_labels="%Y", minor_breaks=NULL, 
                         limits=c(plot.dims()$x_min, 
                                  plot.dims()$x_max + plot.dims()$x_span*0.06)) +
            ylim(plot.dims()$y_min, plot.dims()$y_max) +
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
