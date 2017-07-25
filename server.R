library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(reshape2)

currentYear <- 2016
currentStudentFte <- 19229
fteYears <- c(2016, 2018:2025)

shinyServer(function(input, output, session) {

  lapply(1:9, function(i, x, suffix="slider"){
    x <- x[[i]]
    num.lab <- x$inputId
    slider.lab <- paste0(x$inputId, suffix)
    observeEvent(input[[num.lab]], {
      updateSliderInput(session, slider.lab, value = input[[num.lab]])
    })
    observeEvent(input[[slider.lab]], {
      updateNumericInput(session, num.lab, value = input[[slider.lab]])
    })
  }, x = c(slider.args1, slider.args2, slider.args3))

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

  stateAppropriationPerFte <- reactive({ round(totalStateAppropriation() * 1000000 / studentFteGrowth()) })
  totalTuitionFees <- reactive({ round(studentFteGrowth() * tuitionFeesFTE() / 1000000) })
  revenue <- reactive({ totalTuitionFees() + totalStateAppropriation() })

  # Build data frame for spreadsheet
  spreadsheetDf <- reactive({data.frame(
    Year = fteYears,
    studentFte = studentFteGrowth(),
    tuitionFees = tuitionFeesFTE(),
    stateAppropriation = totalStateAppropriation(),
    stateAppropriationPerFte = stateAppropriationPerFte(),
    totalTuitionFees = totalTuitionFees(),
    revenue = revenue()
  )})

  output$spreadsheet <- DT::renderDataTable(
    spreadsheetDf(),
    options = list(
      dom = 't',
      ordering = FALSE,
      columnDefs = list(
        list(
          targets = list(1, 4),
          render = JS(
            "function(data, type, row, meta) { return data.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ','); }"
          )
        )
      )
    ),
    rownames = FALSE,
    colnames = c(
      "Year",
      "Student FTE",
      "Tuition & Fees per Student FTE ($)",
      "Total State Appropriation (Million $)",
      "State Appropriation per FTE ($)",
      "Total Tuition & Fees (Million $)",
      "Revenue, Educational Cost (Million $)"
    )
  )

  compositeGraphDf <- reactive({ data.frame(years = fteYears, tuition = totalTuitionFees(), appropriation = totalStateAppropriation()) })
  compositeGraphDat <- reactive({ melt(compositeGraphDf(), id = "years") })

  output$compositePlot <- renderPlot({
    ggplot(compositeGraphDat(), aes(years, value, fill = variable)) +
      geom_col()+
      scale_x_continuous(breaks = seq(min(fteYears), max(fteYears), by = 1)) +
      ggtitle("Enrollment, Tuition & Fees, State Appropriations") +
      ylab("Million $") +
      scale_fill_manual(name = element_blank(), values = c("#e3593d", "#4575b5")) +
      theme(axis.title.x = element_blank())
  })

  appropriationsPlotDf <- reactive({ data.frame(years=fteYears, appropriation=stateAppropriationPerFte()) })
  appropriationsPlotDat <- reactive({ melt(appropriationsPlotDf(), id = "years") })

  output$appropriationsPlot <- renderPlot({
    ggplot(appropriationsPlotDat(), aes(years, value, fill = variable)) +
      geom_col() +
      scale_x_continuous(breaks = seq(min(fteYears), max(fteYears), by = 1)) +
      ggtitle("State Appropriations per FTE") +
      ylab("Thousand $") +
      scale_fill_manual(name = element_blank(), values = c("#13ad1b")) +
      theme(axis.title.x = element_blank())
  })
})