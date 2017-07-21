
make_inputs_row <- function(x, col=3, labels="top", suffix="slider"){
  fluidRow(
    lapply(seq_along(x), function(i, x){
      x <- x[[i]]
      if(labels=="none") x$label <- ""
      numbox <- do.call(numericInput, c(x, width="100%"))
      if(labels=="top") x$label <- ""
      x$inputId <- paste0(x$inputId, suffix)
      slider <- do.call(sliderInput, c(x, width="100%"))
      column(col, div(numbox, style='height:50px;'), slider)
    }, x=x)
  )
}


shinyUI(fluidPage(
  titlePanel("UA Financial Framework Visualization Tool"),
  fluidPage(
    fluidRow(
      column(6,
             make_inputs_row(slider.args1, 6),
             make_inputs_row(slider.args2),
             make_inputs_row(slider.args3, labels='none')
      ),
      column(6,
             "placeholder for spreadsheet and graphs"
      )
    )
  )
))
