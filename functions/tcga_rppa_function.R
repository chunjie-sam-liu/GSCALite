rppaOutput <- function() {
  # ns <- NS(id)
  column(
    width = 10, offset = 1,
    shinydashboard::tabBox(
      id = "rppa_PLOT", title = "", width = 12,
      tabPanel(title = "Global percentage", imagePlotInput(id = "rppa_pie", width = "100%", height = "100%")),
      tabPanel(title = "Heatmap percentage", imagePlotInput(id = "rppa_per", width = "100%", height = "100%")),
      tabPanel(
        title = "Relation network",
        imagePlotInput(id = "rppa_line", width = "100%", height = "100%"
      )
    )
    )
  )
}

fn_rppa_result <- function(.rppa){
  if (.rppa == TRUE) {
    rppaOutput()
  } else{
    column(
      width = 10, offset = 1,
      shiny::tags$div(style = "height=500px;", class = "jumbotron", shiny::tags$h2("This analysis is not selected"))
    )
  }
}