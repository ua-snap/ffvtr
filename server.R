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
  tuitionFeesFTE <- reactive(
    list(
      6806, # 2016
      input$tuitionFeesFTE2018,
      input$tuitionFeesFTE2019,
      input$tuitionFeesFTE2020,
      input$tuitionFeesFTE2025
      )
    )
  totalStateAppropriation <- reactive(
    list(
      350, # 2016
      input$totalStateAppropriation2018,
      input$totalStateAppropriation2019,
      input$totalStateAppropriation2020,
      input$totalStateAppropriation2025
    )
  )
  output$studentFtes <- renderPrint({ studentFteGrowth() })
  output$tuitionFeesFte <- renderPrint({ tuitionFeesFTE() })
  output$totalStateAppropriation <- renderPrint({ totalStateAppropriation() })
})
