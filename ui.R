
make_inputs_row <- function(x, col=3, labels="top", suffix="slider"){
  fluidRow(
    lapply(seq_along(x), function(i, x){
      x <- x[[i]]
      if(labels == "none") x$label <- ""
      numbox <- do.call(numericInput, c(x, width = "100%"))
      if(labels == "top") x$label <- ""
      x$inputId <- paste0(x$inputId, suffix)
      slider <- do.call(sliderInput, c(x, width = "100%"))
      column(col, div(numbox, style = 'height:50px;'), slider)
    }, x = x)
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
    fluidRow(
      column(6,
        h4("% change student FTEs per year"),
        make_inputs_row(slider.args1, 6, labels = "none"),
        h4("Tuition and Fees per Student FTE ($)"),
        make_inputs_row(slider.args2),
        h4("Total State Appropriation (Million $)"),
        make_inputs_row(slider.args3)
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
