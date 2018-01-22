methOutput <- function() {
  # ns <- NS(id)
  column(
    width = 10, offset = 1,
    shinydashboard::tabBox(
            id = "snv_PLOT", title = "PLOT", width = 12,
            tabPanel(title = "Differential Methylation", PlotInput(id = "meth_diff")),
            tabPanel(title = "Methylation Survival", PlotInput(id = "meth_survival")),
            tabPanel(title = "Methylation to Expression", PlotInput(id = "meth_exp"))
          )
  )
}

fn_meth_result <- function(.meth){
  if (.meth == TRUE) {
    methOutput()
  } else{
    column(
      width = 10, offset = 1,
      tags$div("This analysis is not selected")
    )
  }
}