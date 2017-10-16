
make_inputs_row <- function(components, col=3) {
  fluidRow(
    lapply(seq_along(components), function(i, components) {
      component <- components[[i]]
      slider <- do.call(sliderInput, c(component, width = "100%"))
      column(col, div(style = 'height:25px;'), slider)
    }, components = components)
  )
}

page_title <- "UA Financial Framework Visualization Tool"

function(request) {
  navbarPage(
    inverse = TRUE,
    fluid = TRUE,
    theme = "style.css",
    windowTitle = page_title,
    title = div(
      class = "navbar-content",
      span(
        class = "navbar-title",
        page_title
      ),
      span(
        class = "pull-right",
        actionButton("reset", label = "Reset Plots"),
        bookmarkButton()
      )
    ),
    useShinyjs(),
    fluidRow(
      column(6,
        h4("Percent Change in Student FTEs per Year"),
        make_inputs_row(fte_slider, 6),
        h4("Tuition and Fees per Student FTE ($)"),
        make_inputs_row(tuition_sliders),
        h4("Total State Appropriation (Million $)"),
        make_inputs_row(appropriation_sliders)
      ),
      column(6,
        DT::dataTableOutput("spreadsheet")
      )
    ),
    fluidRow(
      class = "graph-row",
      column(4, plotOutput("enrollmentPlot")),
      column(4, plotOutput("compositePlot")),
      column(4, plotOutput("appropriationsPlot"))
    )
  )
}
