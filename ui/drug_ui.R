

tabItem(
  tabName = "drug", align = "center",
  
  fluidRow(style = "width:80%;", shiny::uiOutput(outputId = "ui_drug_welcome")),
  
  fluidRow(style = "width:80%", shiny::uiOutput(outputId = "ui_drug_help")),
  
  fluidRow(selectAndAnalysisInput("drug")),
  
  fluidRow(style = "width:80%", shiny::tags$hr(style = "width:80%")),
  
  fluidRow(style = "width:80%", shiny::uiOutput(outputId = "ui_drug_result")),

  # Load footer ----
  source(file.path(config$wd, "ui", "footer.R"), echo = FALSE, verbose = FALSE)$value
  
)