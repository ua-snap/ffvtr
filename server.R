library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(reshape2)
library(scales)

currentYear <- 2017
fteYears <- c(2017:2025)

exponentialGrowth <- function(interval, initial, percentage) {
  decimalGrowth <- 1 + percentage / 100
  round(initial * decimalGrowth ^ interval)
}

shinyServer(function(input, output, session) {

  onRestore(function(state) {
    x <- c(fte_slider, tuition_sliders, appropriation_sliders)
    lapply(seq_along(x), function(i, x){
      updateSliderInput(session, x[[i]]$inputId, value = state$values$x[[i]]$inputId)
    }, x = x)
  })

  fte2017 <- 19229
  fte2018 <- 18652

  # For years 2019 - 2025
  fteThrough2025 <- reactive({ exponentialGrowth(1:7, fte2018, input$studentFtePercentChange) })

  studentFteGrowth <- reactive({
    c(
      fte2017,
      fte2018,
      fteThrough2025()
    )
  })

  tuition2017 <- 7020
  tuition2018 <- 7238
  tuition2019 <- reactive({ exponentialGrowth(1, tuition2018, input$tuitionFeesFTE2019) })
  tuition2020 <- reactive({ exponentialGrowth(1, tuition2019(), input$tuitionFeesFTE2020) })

  # For years 2021 - 2025
  tuitionThrough2025 <- reactive({ exponentialGrowth(1:5, tuition2020(), input$tuitionFeesFTE2025) })

  tuitionFeesFTE <- reactive({
    c(
      tuition2017,
      tuition2018,
      tuition2019(),
      tuition2020(),
      tuitionThrough2025()
    )
  })

  appropriation2017 <- 325
  appropriation2018 <- 317
  appropriation2019 <- reactive({ exponentialGrowth(1, appropriation2018, input$totalStateAppropriation2019) })
  appropriation2020 <- reactive({ exponentialGrowth(1, appropriation2019(), input$totalStateAppropriation2020) })

  # For years 2021 - 2025
  appropriationThrough2025 <- reactive({ exponentialGrowth(1:5, appropriation2020(), input$totalStateAppropriation2025) })

  totalStateAppropriation <- reactive({
    c(
      appropriation2017,
      appropriation2018,
      appropriation2019(),
      appropriation2020(),
      appropriationThrough2025()
    )
  })

  stateAppropriationPerFte <- reactive({round(totalStateAppropriation() * 1000000 / studentFteGrowth()) })
  totalTuitionFees <- reactive({round(studentFteGrowth() * tuitionFeesFTE() / 1000000) })
  revenue <- reactive({totalTuitionFees() + totalStateAppropriation() })

  # Build data frame for spreadsheet
  spreadsheetDf <- reactive({data.frame(
    year = fteYears,
    studentFte = studentFteGrowth(),
    tuitionFees = tuitionFeesFTE(),
    totalTuitionFees = totalTuitionFees(),
    stateAppropriationPerFte = stateAppropriationPerFte(),
    stateAppropriation = totalStateAppropriation(),
    revenue = revenue()
  )})

  output$spreadsheet <- DT::renderDataTable(
    spreadsheetDf(),
    options = list(
      dom = 't',
      ordering = FALSE,
      columnDefs = list(
        list(
          targets = list(1, 2, 4),
          render = JS(
            "function(data, type, row, meta) { return data.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ','); }"
          )
        )
      )
    ),
    escape = FALSE,
    rownames = FALSE,
    colnames = c(
      "Year",
      "Student FTE",
      "Tuition and Fees Price ($)",
      "Total Tuition & Fees (Million&nbsp;$)",
      "State Approp. per FTE&nbsp;($)",
      "Total State Approp. (Million&nbsp;$)",
      "Revenue (Million&nbsp;$)"
    )
  )

  enrollmentGraphDf <- reactive({ data.frame(years = fteYears, enrollment = studentFteGrowth()) })
  enrollmentGraphDat <- reactive({ melt(enrollmentGraphDf(), id = "years") })

  output$enrollmentPlot <- renderPlot({
    ggplot(enrollmentGraphDat(), aes(years, value)) +
      geom_col() +
      scale_x_continuous(breaks = seq(min(fteYears), max(fteYears), by = 1)) +
      scale_y_continuous(name="Student FTEs", labels = comma) +
      ggtitle("Enrollment") +
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

  appropriationsPlotDf <- reactive({ data.frame(years = fteYears, appropriation = stateAppropriationPerFte()) })
  appropriationsPlotDat <- reactive({ melt(appropriationsPlotDf(), id = "years") })

  output$appropriationsPlot <- renderPlot({
    ggplot(appropriationsPlotDat(), aes(years, value)) +
      geom_col() +
      scale_x_continuous(breaks = seq(min(fteYears), max(fteYears), by = 1)) +
      scale_y_continuous(name = "Thousand $", labels = comma) +
      ggtitle("State Appropriations per FTE") +
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
    x <- c(fte_slider, tuition_sliders, appropriation_sliders)
    lapply(seq_along(x), function(i, x){
      updateSliderInput(session, x[[i]]$inputId, value = x[[i]]$value)
    }, x = x)
  })
  
})
