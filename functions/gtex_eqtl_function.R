eqtlOutput <- function() {
  # ns <- NS(id)
  column(
    width = 10, offset = 1,
    fluidRow(GTEx_eqtl_Output("gtex_eqtl"))
  )
}

fn_eqtl_result <- function(.eqtl){
  if (.eqtl == TRUE) {
    eqtlOutput()
  } else{
    column(
      width = 10, offset = 1,
      shiny::tags$div(style = "height=500px;", class = "jumbotron", shiny::tags$h2("This analysis is not selected"))
    )
  }
}