#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

currentYear <- 2016
currentStudentFte <- 19229
fteYears <- c(2016, 2018:2020, 2025)

shinyServer(function(input, output) {
  exponentialGrowth <- function(year, baseFte, percentage) {
    decimalGrowth <- 1 + percentage / 100
    yearOffset <- year - currentYear
    return(baseFte * decimalGrowth ^ yearOffset)
  }

  studentFteGrowth <- reactive({ lapply(fteYears, exponentialGrowth, baseFte=currentStudentFte, percentage=input$studentFtePercentChange) })

  # An example of a ggplot2 bar graph that is successfully rendered on the UI.
  # Taken from: http://ggplot2.tidyverse.org/reference/geom_bar.html
  # Our real data, above, needs to be adapted to fit into this.
  output$ftePlot <- renderPlot({
    ggplot(mpg, aes(class)) + geom_bar()
  })
})
