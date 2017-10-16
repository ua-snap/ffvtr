library(shiny)
library(shinyjs)

enableBookmarking(store = "url")

fte_slider <- list(
  list(inputId='studentFtePercentChange', label='% change student FTEs per year', value=2, min=-5, max=5, step=0.1)
)

tuition_sliders <- list(
  list(inputId='tuitionFeesFTE2019', label='% change in 2019', value=2, min=-5, max=5, step=0.1),
  list(inputId='tuitionFeesFTE2020', label='% change in 2020', value=2, min=-5, max=5, step=0.1),
  list(inputId='tuitionFeesFTE2025', label='% change through 2025', value=2, min=-5, max=5, step=0.1)
)

appropriation_sliders <- list(
  list(inputId='totalStateAppropriation2019', label='% change in 2019', value=2, min=-5, max=5, step=0.1),
  list(inputId='totalStateAppropriation2020', label='% change in 2020', value=2, min=-5, max=5, step=0.1),
  list(inputId='totalStateAppropriation2025', label='% change through 2025', value=2, min=-5, max=5, step=0.1)
)
