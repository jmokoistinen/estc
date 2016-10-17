#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("Years etc whatnot"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("range.years",
                     "Years:",
                     min = 1450,
                     max = 1850,
                     value = c(1600, 1750))

      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         textOutput("text1"),
         plotOutput("yearPlot")
      )
   )
))

# Define server logic required to draw a histogram

# server data etc
source("helper_shinytesting.R")
library(ggplot2)

server <- shinyServer(function(input, output) {
   
   output$text1 <- renderText({
     sprintf("years low, %s years high %s",
             input$range.years[1],
             input$range.years[2])
   })
   
   output$yearPlot <- renderPlot({
     year.min <- input$range.years[1]
     year.max <- input$range.years[2]
     years  <- catalog.estc.amount.by.year$years
     titles <- catalog.estc.amount.by.year$titles
     year.min.index <- which(years == year.min)[[1]]
     year.max.index <- which(years == year.max)[[1]]
     years.subset  <- years[year.min.index:year.max.index]
     titles.subset <- titles[year.min.index:year.max.index]
     
     estc.subset <- data.frame(years = years.subset,
                               titles = titles.subset)
     
     qplot(years.subset, titles.subset, data = estc.subset)
   })
   
})

# Run the application 
shinyApp(ui = ui, server = server)

