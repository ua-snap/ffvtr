library(shiny)

shinyUI(fluidPage(
  
  titlePanel("UA Financial Framework Visualization Tool"),
  sidebarLayout(
    
    sidebarPanel(
      sliderInput("studentFtePercentChange",
                  label = "% change student FTEs per year", min = -5, max = 5, value = 2),
      h4("Tuition and fees per student FTE ($)"),
      sliderInput("tuitionFeesFTE2018",
                  label = "2018", min = 5000, max = 15000, value = 7000),
      sliderInput("tuitionFeesFTE2019",
                  label = "2019", min = 5000, max = 15000, value = 7500),
      sliderInput("tuitionFeesFTE2020",
                  label = "2020", min = 5000, max = 15000, value = 8000),
      sliderInput("tuitionFeesFTE2025",
                  label = "2025", min = 5000, max = 15000, value = 10089),
      h4("Total State Appropriation (Million $)"),
      sliderInput("totalStateAppropriation2018",
                  label = "2018", min = 200, max = 500, value = 340),
      sliderInput("totalStateAppropriation2019",
                  label = "2019", min = 200, max = 500, value = 335),
      sliderInput("totalStateAppropriation2020",
                  label = "2020", min = 200, max = 500, value = 333),
      sliderInput("totalStateAppropriation2025",
                  label = "2025", min = 200, max = 500, value = 312)
    ),
    
    mainPanel(
      textOutput("studentFtes"),
      textOutput("tuitionFeesFte"),
      textOutput("totalStateAppropriation")
    )
  )
))
