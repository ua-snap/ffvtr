library(shiny)
library(shinyjs)

enableBookmarking(store = "url")

slider.args1 <- list( # arguments for first row of sliders/numeric inputs
  list(inputId = 'studentFtePercentChange', label = "% change student FTEs per year", value = 2, min = -5, max = 5, step = 0.1)
)

slider.args2 <- list( # arguments for first row of sliders/numeric inputs
  list(inputId='tuitionFeesFTE2018', label='2018', value=7000, min=5000, max=15000, step=1),
  list(inputId='tuitionFeesFTE2019', label='2019', value=7500, min=5000, max=15000, step=1),
  list(inputId='tuitionFeesFTE2020', label='2020', value=8000, min=5000, max=15000, step=1),
  list(inputId='tuitionFeesFTE2025', label='2025', value=10089, min=5000, max=15000, step=1)
)

slider.args3 <- list( # arguments for second row of sliders/numeric inputs
  list(inputId='totalStateAppropriation2018', label='2018', value=340, min=200, max=500, step=1),
  list(inputId='totalStateAppropriation2019', label='2019', value=335, min=200, max=500, step=1),
  list(inputId='totalStateAppropriation2020', label='2020', value=333, min=200, max=500, step=1),
  list(inputId='totalStateAppropriation2025', label='2025', value=312, min=200, max=500, step=1)
)
