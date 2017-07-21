#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# Define server logic required to draw a histogram
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
  
})


