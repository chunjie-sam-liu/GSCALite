gexpOutput <- function() {
  # ns <- NS(id)
  column(
    width = 10, offset = 1,
    fluidRow(shinydashboard::tabBox(
      id = "GTEx_PLOT", title = "PLOT", width = 12,
      tabPanel(title = "GTEx expression", PlotInput(id = "GTEx_exp")) #,tabPanel(title = "GSVA score", PlotInput(id="GTEx_gsva"))
    ))
  )
}

fn_gexp_result <- function(.gexp){
  if (.gexp == TRUE) {
    gexpOutput()
  } else{
    column(
      width = 10, offset = 1,
      shiny::tags$div(style = "height=500px;", class = "jumbotron", shiny::tags$h2("This analysis is not selected"))
    )
  }
}