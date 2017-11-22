shinyBS::bsModal(
  id = "addReport_SPLOM_modal", 
  title = "SPLOM added to report", 
  trigger = "addReport_SPLOM",
  p("Scatter plot matrix was added to the report", addReport_modelTrivia),
  br(),
  uiOutput("addReport_SPLOM_modal_info")
)
