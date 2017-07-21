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

  studentFteGrowth <- reactive({ sapply(fteYears, exponentialGrowth, baseFte=currentStudentFte, percentage=input$studentFtePercentChange) })
  df <- reactive({ data.frame(years=fteYears, fte=studentFteGrowth()) })

  # This graph is just an example using real data calculated reactively based on inputs.
  # We will not be graphing Student FTE vs. years in the final product.
  output$ftePlot <- renderPlot({
    ggplot(df(), aes(years, fte)) + geom_col() + scale_x_continuous(breaks = seq(min(fteYears), max(fteYears), by = 1))
  })
})
