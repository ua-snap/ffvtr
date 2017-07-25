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

  # The enrollment variable is many times larger than the other data on the graph. To plot it on
  # the same graph as the revenue bars without blowing the scale out of proportion, the enrollment
  # value and tick positions on the right-side y-axis are divided by the (somewhat arbitrary)
  # enrollmentScale variable. The labels, however, of the right-side y-axis tick marks use real
  # enrollment values before division, so the end result displayed to the user is accurate.
  enrollmentScale <- 50
  enrollmentTickNum <- 5

  compositeGraphDf <- reactive({data.frame(
    years = fteYears,
    enrollment = studentFteGrowth() / enrollmentScale,
    tuition = totalTuitionFees(),
    appropriation = totalStateAppropriation()
  )})

  compositeGraphDat <- reactive({ melt(compositeGraphDf(), id = "years") })

  maxEnrollment <- reactive({ max(studentFteGrowth()) })

  # Divide enrollment by the enrollmentScale variable defined above to bring the values down to
  # the scale of the revenue bars on the graph.
  secAxisTickPos <- reactive({ approx(c(0, maxEnrollment()) / enrollmentScale, n = enrollmentTickNum) })

  # The labels for the right-side y-axis ticks are set to undivided enrollment values.
  secAxisTickLabels <- reactive({ approx(c(0, maxEnrollment()), n = enrollmentTickNum) })

  output$compositePlot <- renderPlot({
    ggplot() +
      geom_col(mapping = aes(years, value, fill = variable), data = compositeGraphDat() %>% filter(variable == 'tuition' | variable == 'appropriation')) +
      geom_line(mapping = aes(years, value, fill = variable), data = compositeGraphDat() %>% filter(variable == 'enrollment')) +
      geom_point(mapping = aes(years, value, fill = variable), data = compositeGraphDat() %>% filter(variable == 'enrollment')) +
      scale_x_continuous(breaks = seq(min(fteYears), max(fteYears), by = 1)) +
      scale_y_continuous("Million $", sec.axis = sec_axis(~ ., name = "Enrollment", breaks = secAxisTickPos()$y, labels = secAxisTickLabels()$y)) +
      ggtitle("Enrollment, Tuition & Fees, State Appropriations") +
      scale_fill_manual(name = element_blank(), values = c("#4575b5", "#000000", "#e3593d")) +
      theme(axis.title.x = element_blank()) +
      guides(fill = guide_legend(override.aes = list(shape = NA)))
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