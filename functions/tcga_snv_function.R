snvOutput <- function() {
  # ns <- NS(id)
  column(
    width = 10, offset = 1,
    shinydashboard::tabBox(
      id = "snv_plot", title = "PLOT", width = 12,
      # bubble plot for tumor vs. normal
      tabPanel(title= "SNV percentage profile",PlotInput(id="snv_percentage")),
      tabPanel(title="SNV summary",imagePlotInput("snv_summary",width=700,height="100%")),
      tabPanel(title="SNV oncoplot",imagePlotInput("snv_oncoplot",width=700,height="100%")),
      tabPanel(title="SNV survival",PlotInput("snv_survival"))
    )
  )
}

fn_snv_result <- function(.snv){
  if (.snv == TRUE) {
    snvOutput()
  } else{
    column(
      width = 10, offset = 1,
      tags$div("This analysis is not selected")
    )
  }
}