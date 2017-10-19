library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(reshape2)

currentYear <- 2017
currentStudentFte <- 19229
fteYears <- c(2017:2025)

exponentialGrowth <- function(years, baseFte, percentage) {
  decimalGrowth <- 1 + percentage / 100
  yearOffsets <- years - currentYear
  round(baseFte * decimalGrowth ^ yearOffsets)
}

shinyServer(function(input, output, session) {

  onRestore(function(state) {
    x <- c(slider.args1, slider.args2, slider.args3)
    lapply(seq_along(x), function(i, x){
      updateSliderInput(session, x[[i]]$inputId, value = state$values$x[[i]]$inputId)
    }, x = x)
  })

  lapply(1:9, function(i, x, suffix="slider"){
    x <- x[[i]]
    num.lab <- x$inputId
    slider.lab <- paste0(x$inputId, suffix)
    observeEvent(input[[num.lab]], {
      if(input[[num.lab]] != input[[slider.lab]])
        onevent("mouseleave", num.lab, updateSliderInput(session, slider.lab, value = input[[num.lab]]))
    })
    observeEvent(input[[slider.lab]], {
      if(input[[num.lab]] != input[[slider.lab]])
        updateNumericInput(session, num.lab, value = input[[slider.lab]])
    })
  }, x = c(slider.args1, slider.args2, slider.args3))
    
  studentFteGrowth <- reactive({ exponentialGrowth(fteYears, currentStudentFte, input$studentFtePercentChange) })

  tuitionFeesFTE <- reactive(
    round(
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
      )
    )
  )

  totalStateAppropriation <- reactive(
    round(
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
      )
    )
  )

  stateAppropriationPerFte <- reactive({round(totalStateAppropriation() * 1000000 / studentFteGrowth()) })
  totalTuitionFees <- reactive({round(studentFteGrowth() * tuitionFeesFTE() / 1000000) })
  revenue <- reactive({totalTuitionFees() + totalStateAppropriation() })

  # Build data frame for spreadsheet
  spreadsheetDf <- reactive({data.frame(
    year = fteYears,
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

  enrollmentGraphDf <- reactive({ data.frame(years = fteYears, enrollment = studentFteGrowth()) })
  enrollmentGraphDat <- reactive({ melt(enrollmentGraphDf(), id = "years") })

  output$enrollmentPlot <- renderPlot({
    ggplot(enrollmentGraphDat(), aes(years, value)) +
      geom_col() +
      scale_x_continuous(breaks = seq(min(fteYears), max(fteYears), by = 1)) +
      ggtitle("Enrollment") +
      ylab("Student FTEs") +
      theme(
        text = element_text(size = 17),
        axis.text = element_text(size = 15),
        axis.text.x = element_text(angle = -45, hjust = 0),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 20, margin = margin(0, 20, 0, 0)),
        legend.position = "none",
        plot.margin = unit(c(0.28, 0.5, 1.6, 0.5), "cm")
      )
  })

  compositeGraphDf <- reactive({ data.frame(years = fteYears, tuition = totalTuitionFees(), appropriation = totalStateAppropriation()) })
  compositeGraphDat <- reactive({ melt(compositeGraphDf(), id = "years") })

  output$compositePlot <- renderPlot({
    ggplot(compositeGraphDat(), aes(years, value, fill = variable)) +
      geom_col() +
      scale_x_continuous(breaks = seq(min(fteYears), max(fteYears), by = 1)) +
      ggtitle("Tuition & Fees, State Appropriations") +
      ylab("Million $") +
      scale_fill_manual(name = element_blank(), values = c("#e3593d", "#4575b5"), labels = c("Tuition   ", "Appropriation")) +
      theme(
        text = element_text(size = 17),
        axis.text = element_text(size = 15),
        axis.text.x = element_text(angle = -45, hjust = 0),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 20, margin = margin(0, 20, 0, 0)),
        legend.text = element_text(size = 17),
        legend.position = "bottom",
        plot.margin = unit(c(0.2, 0.5, 0.185, 0.5), "cm")
      )
  })

  appropriationsPlotDf <- reactive({ data.frame(years = fteYears, appropriation = stateAppropriationPerFte() / 1000) })
  appropriationsPlotDat <- reactive({ melt(appropriationsPlotDf(), id = "years") })

  output$appropriationsPlot <- renderPlot({
    ggplot(appropriationsPlotDat(), aes(years, value)) +
      geom_col() +
      scale_x_continuous(breaks = seq(min(fteYears), max(fteYears), by = 1)) +
      ggtitle("State Appropriations per FTE") +
      ylab("Thousand $") +
      theme(
        text = element_text(size = 17),
        axis.text = element_text(size = 15),
        axis.text.x = element_text(angle = -45, hjust = 0),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 20, margin = margin(0, 20, 0, 0)),
        legend.position = "none",
        plot.margin = unit(c(0.175, 0.5, 1.6, 0), "cm")
      )
  })
  
  observeEvent(input$reset, { 
    x <- c(slider.args1, slider.args2, slider.args3)
    lapply(seq_along(x), function(i, x){
      updateSliderInput(session, x[[i]]$inputId, value = x[[i]]$value)
    }, x = x)
  })
  
})
