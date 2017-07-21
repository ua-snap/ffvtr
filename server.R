library(shiny)
currentYear <- 2016
currentStudentFte <- 19229
fteYears <- c(2016, 2018:2025)

shinyServer(function(input, output, session) {

  lapply(1:9, function(i, x, suffix="slider"){
    x <- x[[i]]
    num.lab <- x$inputId
    slider.lab <- paste0(x$inputId, suffix)
    observeEvent(input[[num.lab]], {
      updateSliderInput(session, slider.lab, value=input[[num.lab]])
    })
    observeEvent(input[[slider.lab]], {
      updateNumericInput(session, num.lab, value=input[[slider.lab]])
    })
  }, x=c(slider.args1, slider.args2, slider.args3))

    exponentialGrowth <- function(year, baseFte, percentage) {
    decimalGrowth <- 1 + percentage / 100
    yearOffset <- year - currentYear
    return(round(baseFte * decimalGrowth ^ yearOffset))
  }

  studentFteGrowth <- reactive({ sapply(fteYears, exponentialGrowth, baseFte=currentStudentFte, percentage=input$studentFtePercentChange) })
  tuitionFeesFTE <- reactive(
    sapply(
      c(
        6806, # 2016
        input$tuitionFeesFTE2018,
        input$tuitionFeesFTE2019,
        input$tuitionFeesFTE2020,
        approx(
          c(2020, 2025),
          c(input$tuitionFeesFTE2020, input$tuitionFeesFTE2025),
          c(2021:2024)
        )$y,
        input$tuitionFeesFTE2025
      ),
      round
    )
  )
  totalStateAppropriation <- reactive(
    sapply(
      c(
        350, # 2016
        input$totalStateAppropriation2018,
        input$totalStateAppropriation2019,
        input$totalStateAppropriation2020,
        approx(
          c(2020, 2025),
          c(input$totalStateAppropriation2020, input$totalStateAppropriation2025),
          c(2021:2024)
        )$y,
        input$totalStateAppropriation2025
      ),
      round
    )
  )
  output$studentFtes <- renderPrint({ studentFteGrowth() })
  output$tuitionFeesFte <- renderPrint({ tuitionFeesFTE() })
  output$totalStateAppropriation <- renderPrint({ totalStateAppropriation() })
})