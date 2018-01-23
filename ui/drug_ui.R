

tabItem(
  tabName = "drug", align = "center",
  
  fluidRow(style = "width:80%;", shiny::uiOutput(outputId = "ui_drug_welcome")),
  
  fluidRow(shiny::uiOutput(outputId = "ui_drug_result")),
  # fluidRow(drugOutput("drug")),
  
  
  # Load footer ----
  source(file.path(config$wd, "ui", "footer.R"), echo = FALSE, verbose = FALSE)$value
  
)